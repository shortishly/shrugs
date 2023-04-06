%% Copyright (c) 2012-2023 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(shrugs_cli).


-behaviour(ssh_server_channel).
-define(EXEC_ERROR_STATUS, 255).
-export([handle_msg/2]).
-export([handle_ssh_msg/2]).
-export([init/1]).
-export([terminate/2]).
-include_lib("kernel/include/logger.hrl").


init(Arg) ->
    ?LOG_DEBUG(#{arg => Arg}),
    {ok, #{arg => Arg, channels => #{}, env => #{}, ports => #{}}}.


handle_msg({ssh_channel_up, _Channel, _Connection} = Msg, State) ->
    ?LOG_DEBUG(#{msg => Msg, state => State}),
    {ok, State};

handle_msg({'EXIT', Port, normal} = Msg, State) when is_port(Port) ->
    ?LOG_DEBUG(#{msg => Msg, state => State}),
    {ok, State};

handle_msg({Port, {data, Data}}, #{ports := Ports} = State)
  when is_port(Port), is_map_key(Port, Ports) ->
    ?LOG_DEBUG(#{port => Port, data => Data}),
    #{Port := {Connection, Channel}} = Ports,
    case ssh_connection:send(Connection,
                             Channel,
                             0,
                             Data) of
        ok ->
            {ok, State};

        {error, Reason} ->
            ?LOG_DEBUG(#{reason => Reason}),
            {ok, State}
    end;

handle_msg({Port, {exit_status, ExitStatus}} = Msg,
           #{ports := Ports, env := Env, channels := Channels} = State)
  when is_port(Port), is_map_key(Port, Ports) ->
    ?LOG_DEBUG(#{msg => Msg, state => State}),
    #{Port := {Connection, Channel} = CoCh} = Ports,
    ssh_connection:exit_status(Connection, Channel, ExitStatus),
    {stop,
     Channel,
     State#{channels := maps:remove(CoCh, Channels),
            env := maps:remove(CoCh, Env),
            ports := maps:remove(Port, Ports)}};

handle_msg({Port, {exit_status, _}} = Msg, State) when is_port(Port) ->
    ?LOG_DEBUG(#{msg => Msg, state => State}),
    {ok, State};

handle_msg(Msg, State) ->
    ?LOG_DEBUG(#{msg => Msg, state => State}),
    {ok, State}.


handle_ssh_msg({ssh_cm, Connection, {eof, _} = CM}, State) ->
    ?LOG_DEBUG(#{cm => CM, connection => Connection, state => State}),
    {ok, State};

handle_ssh_msg({ssh_cm,
                Connection,
                {data, Channel, _Type, Data} = CM},
               #{channels := Channels} = State)
  when is_map_key({Connection, Channel}, Channels) ->
    ?LOG_DEBUG(#{cm => CM,
                 connection => Connection,
                 state => State}),
    #{{Connection, Channel} := Port} = Channels,
    erlang:port_command(Port, Data),
    {ok, State};

handle_ssh_msg(
  {ssh_cm,
   Connection,
   {exec, Channel, WantReply, Command} = CM},
  #{channels := Channels, ports := Ports, env := Env} = State)
  when not(is_map_key({Connection, Channel}, Channels)) ->

    ?LOG_DEBUG(#{cm => CM,
                 connection => Connection,
                 state => State}),

    case string:split(Command, " ") of
        [Executable, QuotedRepo] when Executable == "git-receive-pack";
                                      Executable == "git-upload-archive";
                                      Executable == "git-upload-pack" ->

            try
                Port = erlang:open_port(
                         {spawn_executable, "/usr/bin/git"},
                         [stream,
                          {cd, "repos"},
                          {env,
                           maps:to_list(maps:get({Connection, Channel}, Env, #{}))},
                          {arg0, Executable},
                          {args, [unquote(QuotedRepo)]},
                          exit_status,
                          eof,
                          use_stdio,
                          binary]),

                ssh_connection:reply_request(Connection, WantReply, success, Channel),
                CoCh = {Connection, Channel},
                {ok,
                 State#{ports := Ports#{Port => CoCh},
                        channels := Channels#{CoCh => Port}}}
            catch
                Class:Exception:Stacktrace ->
                    ?LOG_ERROR(#{class => Class, error => Exception, stacktrace => Stacktrace}),
                    ssh_connection:reply_request(Connection, WantReply, failure, Channel),
                    ssh_connection:exit_status(Connection, Channel, ?EXEC_ERROR_STATUS),
                    _ = ssh_connection:send_eof(Connection, Channel),
                    {stop, Channel, State}
            end;

        _Otherwise ->
            case ssh_connection:send(Connection,
                                     Channel,
                                     1,
                                     "Denied.") of
                ok ->
                    ssh_connection:reply_request(Connection, WantReply, success, Channel),
                    ssh_connection:exit_status(Connection, Channel, ?EXEC_ERROR_STATUS),
                    ssh_connection:send_eof(Connection, Channel);

                {error, Reason} ->
                    ?LOG_DEBUG(#{reason => Reason})
            end,
            {stop, Channel, State}
    end;

handle_ssh_msg({ssh_cm, Connection, {shell, Channel, WantReply} = CM}, State) ->
    ?LOG_DEBUG(#{cm => CM,
                 connection => Connection,
                 state => State}),
    case ssh_connection:send(Connection,
                             Channel,
                             1,
                             "Denied.") of
        ok ->
            ssh_connection:reply_request(Connection, WantReply, success, Channel),
            ssh_connection:exit_status(Connection, Channel, ?EXEC_ERROR_STATUS),
            ssh_connection:send_eof(Connection, Channel);

        {error, Reason} ->
            ?LOG_DEBUG(#{reason => Reason})
    end,
    {stop, Channel, State};


handle_ssh_msg(
  {ssh_cm, Connection, {env, Channel, WantReply, Key, Value} = CM},
  State) ->
    ?LOG_DEBUG(#{cm => CM,
                 connection => Connection,
                 state => State}),
    ssh_connection:reply_request(Connection, WantReply, success, Channel),

    {ok,
     add_env({Connection, Channel},
             binary_to_list(Key),
             binary_to_list(Value),
             State)};

handle_ssh_msg({ssh_cm, Connection, {pty, Channel, WantReply, _} = CM}, State) ->
    ?LOG_DEBUG(#{cm => CM,
                 connection => Connection,
                 state => State}),
    ssh_connection:reply_request(Connection, WantReply, success, Channel),
    {ok, State};

handle_ssh_msg(Msg, State) ->
    ?LOG_DEBUG(#{msg => Msg, state => State}),
    {ok, State}.


terminate(Reason, State) ->
    ?LOG_DEBUG(#{reason => Reason, state => State}),
    ok.


unquote(Quoted) ->
    ?LOG_DEBUG(#{quoted => Quoted}),
    case string:trim(Quoted, both, "'") of
        [$/ | Unquoted] ->
            Unquoted;

        Unquoted ->
            Unquoted
    end.


add_env(CoCh, Key, Value, #{env := Env} = State) ->
    ?LOG_DEBUG(#{co_ch => CoCh, key => Key, value => Value, state => State}),
    case Env of
        #{CoCh := Existing} ->
            State#{env := Env#{CoCh := Existing#{Key => Value}}};

        #{} ->
            State#{env := Env#{CoCh => #{Key => Value}}}
    end.

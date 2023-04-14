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
    {ok,
     #{arg => Arg,
       channels => #{},
       envs => #{},
       pids => #{}}}.


handle_msg({ssh_channel_up, _Channel, _Connection} = Msg, State) ->
    ?LOG_DEBUG(#{msg => Msg, state => State}),
    {ok, State};

handle_msg({'DOWN', Pid, process, _, Reason} = Msg,
           #{channels := Channels, envs := Envs, pids := Pids} = State)
  when is_map_key(Pid, Pids) ->
    ?LOG_DEBUG(#{msg => Msg, state => State}),
    #{Pid := {Connection, Channel} = CoCh} = Pids,
    ssh_connection:exit_status(
      Connection,
      Channel,
      status(Reason)),
    _ = ssh_connection:send_eof(Connection, Channel),
    {stop,
     Channel,
     State#{channels := maps:remove(CoCh, Channels),
            envs := maps:remove(CoCh, Envs),
            pids := maps:remove(Pid, Pids)}};

handle_msg({stdout = FD, Pid, Data}, #{pids := Pids} = State)
  when is_map_key(Pid, Pids) ->
    ?LOG_DEBUG(#{fd => FD, pid => Pid, data => Data}),
    #{Pid := {Connection, Channel}} = Pids,
    case ssh_connection:send(Connection,
                             Channel,
                             0,
                             Data) of
        ok ->
            {ok, State};

        {error, Reason} ->
            ?LOG_DEBUG(#{reason => Reason}),
            {ok, State}
    end.


handle_ssh_msg({ssh_cm, Connection, {eof, Channel} = CM},
               #{channels := Channels} = State)
  when is_map_key({Connection, Channel}, Channels) ->
    ?LOG_DEBUG(#{cm => CM,
                 connection => Connection,
                 state => State}),
    #{{Connection, Channel} := {_, Pid}} = Channels,
    exec:send(Pid, eof),
    {ok, State};

handle_ssh_msg({ssh_cm,
                Connection,
                {data, Channel, _Type, Data} = CM},
               #{channels := Channels} = State)
  when is_map_key({Connection, Channel}, Channels) ->
    ?LOG_DEBUG(#{cm => CM,
                 connection => Connection,
                 state => State}),
    #{{Connection, Channel} := {_, Pid}} = Channels,
    exec:send(Pid, Data),
    {ok, State};

handle_ssh_msg(
  {ssh_cm,
   Connection,
   {exec, Channel, WantReply, Command} = CM},
  #{channels := Channels, pids := Pids, envs := Envs} = State)
  when is_map_key({Connection, Channel}, Envs),
       not(is_map_key({Connection, Channel}, Channels)) ->

    ?LOG_DEBUG(#{cm => CM,
                 connection => Connection,
                 state => State}),

    case string:split(Command, " ") of
        [Executable, QuotedRepo] when Executable == "git-receive-pack";
                                      Executable == "git-upload-archive";
                                      Executable == "git-upload-pack" ->


            ok = prepare_repo_dir(Executable, unquote(QuotedRepo)),

            case exec:run(
                   [Executable] ++ [unquote(QuotedRepo)],
                   [{executable, filename:join(shrugs_config:dir(bin), "git")},
                    monitor,
                    stdin,
                    stdout,
                    stderr,
                    {cd, shrugs_config:dir(repo)},
                    {env, maps:get({Connection, Channel}, Envs)}]) of

                {ok, Pid, OSPid} ->
                    ssh_connection:reply_request(
                      Connection,
                      WantReply,
                      success,
                      Channel),
                    CoCh = {Connection, Channel},
                    {ok,
                     State#{pids := Pids#{OSPid => CoCh},
                            channels := Channels#{CoCh => {Pid, OSPid}}}};

                {error, Reason} ->
                    ?LOG_ERROR(#{reason => Reason}),
                    ssh_connection:reply_request(Connection, WantReply, failure, Channel),
                    ssh_connection:exit_status(Connection, Channel, ?EXEC_ERROR_STATUS),
                    _ = ssh_connection:send_eof(Connection, Channel),
                    {stop, Channel, State}
            end;

        ["ls"] ->
            case file:list_dir(shrugs_config:dir(repo)) of
                {ok, []} ->
                    ssh_connection:reply_request(Connection, WantReply, success, Channel),
                    ssh_connection:exit_status(Connection, Channel, 0),
                    ssh_connection:send_eof(Connection, Channel);

                {ok, Repositories} ->
                    ssh_connection:reply_request(Connection, WantReply, success, Channel),
                    _ = ssh_connection:send(Connection,
                                            Channel,
                                            0,
                                            [lists:join("\n", Repositories), "\n"]),
                    ssh_connection:exit_status(Connection, Channel, 0),
                    ssh_connection:send_eof(Connection, Channel);

                {error, Reason} ->
                    ?LOG_DEBUG(#{reason => Reason})
            end,
            {stop, Channel, State};


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
    case shrugs_config:enabled(shell) of
        false ->
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

        true ->
            ssh_connection:reply_request(Connection, WantReply, success, Channel),
            shell:start(),
            {ok, State}
    end;


handle_ssh_msg(
  {ssh_cm, Connection, {env, Channel, WantReply, Key, Value} = CM},
  State) ->
    ?LOG_DEBUG(#{cm => CM,
                 connection => Connection,
                 state => State}),
    ssh_connection:reply_request(Connection, WantReply, success, Channel),
    {ok, add_env({Connection, Channel}, Key, Value, State)};

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
            ?LOG_DEBUG(#{unquoted => Unquoted}),
            Unquoted;

        Unquoted ->
            ?LOG_DEBUG(#{unquoted => Unquoted}),
            Unquoted
    end.


add_env(CoCh, Key, Value, #{envs := Envs} = State) ->
    ?LOG_DEBUG(#{co_ch => CoCh, key => Key, value => Value, state => State}),
    case Envs of
        #{CoCh := Existing} ->
            State#{envs := Envs#{CoCh := [{Key, Value} | Existing]}};

        #{} ->
            State#{envs := Envs#{CoCh => [{Key, Value}]}}
    end.


prepare_repo_dir("git-receive-pack", Repo) ->
    ?LOG_DEBUG(#{repo => Repo}),
    RepoDir = filename:join(shrugs_config:dir(repo), Repo),

    case filelib:is_dir(RepoDir) of
        false ->
            ok = filelib:ensure_dir(RepoDir),

            case shrugs_git:command(
                   ["init",
                    "--bare",
                    Repo]) of

                {ok, Detail} ->
                    ?LOG_DEBUG(#{detail => Detail}),
                    ok;

                {error, Detail} = Error ->
                    ?LOG_DEBUG(#{detail => Detail}),
                    Error
            end;
                
        true ->
            ok
    end;

prepare_repo_dir(_, _) ->
    ok.



status(normal) ->
    0;

status({status, Status}) ->
    exec:status(Status).

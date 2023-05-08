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


-module(shrugs_git).


-behaviour(gen_statem).
-export([callback_mode/0]).
-export([command/1]).
-export([command/3]).
-export([handle_event/4]).
-export([init/1]).
-export([start_link/0]).
-import(shrugs_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


start_link() ->
    gen_statem:start_link(
      {local, ?MODULE},
      ?MODULE,
      [],
      envy_gen:options(?MODULE)).


command(WorkingDir, Env, Args) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, WorkingDir, Env, Args}).


command(Args) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, Args}).


init([]) ->
    process_flag(trap_exit, true),
    {ok, ready, #{ports => #{}}}.


callback_mode() ->
    handle_event_function.


handle_event({call, From}, {command, WorkingDir, Env, Args}, _, _) ->
    {keep_state_and_data, nei({command, From, WorkingDir, Env, Args})};

handle_event({call, From}, {command, Args}, _, _) ->
    {keep_state_and_data,
     nei({command, From, shrugs_config:dir(repo), #{}, Args})};

handle_event(internal,
             {command, From, WorkingDir, Env, Args},
             _,
             #{ports := Ports} = Data) ->
    try

        Port = erlang:open_port(
                 {spawn_executable, filename:join(shrugs_config:dir(bin), "git")},
                 [stream,
                  {cd, WorkingDir},
                  {env, maps:to_list(Env)},
                  {args, Args},
                  exit_status,
                  eof,
                  use_stdio,
                  binary]),
        {keep_state, Data#{ports := Ports#{Port => #{from => From, rx => <<>>}}}}

    catch
        Class:Exception:Stacktrace ->
            ?LOG_ERROR(#{class => Class, error => Exception, stacktrace => Stacktrace}),
            {keep_state_and_data, {reply, From, {Class, Exception}}}
    end;

handle_event(info, {'EXIT', Port, normal}, _, _) when is_port(Port) ->
    keep_state_and_data;

handle_event(info, {Port, eof}, _, _) when is_port(Port) ->
    keep_state_and_data;

handle_event(info, {Port, {exit_status, Status}}, _, #{ports := Ports} = Data)
  when is_port(Port),
       is_map_key(Port, Ports) ->

    #{Port := #{from := From, rx := Rx}} = Ports,
    true = erlang:port_close(Port),
    {keep_state,
     Data#{ports := maps:remove(Port, Ports)},
     {reply,
      From,
      {status(Status), Rx}}};

handle_event(info, {Port, {data, Partial}}, _, #{ports := Ports} = Data)
  when is_port(Port),
       is_map_key(Port, Ports) ->

    #{Port := #{rx := Rx} = Metadata} = Ports,
    {keep_state, Data#{ports := Ports#{Port := Metadata#{rx := [Rx, Partial]}}}}.



status(0) -> ok;
status(_) -> error.

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


-module(shrugs_ssh_daemon).


-behaviour(gen_statem).
-export([callback_mode/0]).
-export([handle_event/4]).
-export([info/0]).
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


info() ->
    gen_statem:call(?MODULE, ?FUNCTION_NAME).


init([]) ->
    {ok, ready, #{}, nei(start_daemon)}.


callback_mode() ->
    handle_event_function.


handle_event({call, From}, info, _, #{daemon := Daemon}) ->
    {keep_state_and_data,
     {reply,
      From,
      case ssh:daemon_info(Daemon) of
          {ok, Info} ->
              maps:from_list(Info);

          {error, _} = Error ->
              Error
      end}};

handle_event(internal, start_daemon, _, Data) ->
    {_, KeyStore, worker, _} = shrugs_sup:get_child(shrugs_key_store),
    case ssh:daemon(
           shrugs_config:port(sshd),
           [{inet, inet},
            {subsystems, []},
            {ssh_cli, {shrugs_cli, []}},
            {shell, fun shell/2},
            {exec, {direct, fun exec/3}},
            {pwdfun, fun shrugs_key_store:pwd/2},
            {auth_methods, "publickey"},
            {key_cb, {shrugs_key_store, [KeyStore]}}]) of

        {ok, Daemon} ->
            {keep_state, Data#{daemon => Daemon}};

        {error, Reason} ->
            {stop, Reason}
    end.


shell(User, PeerAddr) ->
    ?LOG_DEBUG(#{user => User, peer_addr => PeerAddr}),
    shell:start().

exec(Cmd, User, ClientAddr) ->
    ?LOG_DEBUG(#{cmd => Cmd, user => User, client_addr => ClientAddr}),
    {ok, Cmd}.
    

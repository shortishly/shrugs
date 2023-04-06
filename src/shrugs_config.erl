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


-module(shrugs_config).


-export([authorized_keys/0]).
-export([enabled/1]).
-export([password/0]).
-export([port/1]).
-export([system_dir/0]).
-export([user_dir/0]).
-include_lib("kernel/include/logger.hrl").


port(sshd) ->
    envy(to_integer, port, 22).


enabled(sshd) ->
    envy(to_boolean, enabled, true).


system_dir() ->
    envy(to_list, system_dir, "/etc/ssh").


user_dir() ->
    case os:getenv("HOME") of
        false ->
            envy(to_list, user_dir, shrugs:priv_file("ssh/user"));

        Home ->
            envy(to_list, user_dir, filename:join(Home, ".ssh"))
    end.


authorized_keys() ->
    envy:get_env(shrugs, authorized_keys, [os_env]).


password() ->
    case secret("com.github.shortishly.shrugs.password") of
        {ok, Password} ->
            envy(to_list, password, Password);

        {error, _} ->
            envy:get_env(shrugs, password, [os_env])
    end.


secret(Name) ->
    case file:read_file(filename:join("/run/secrets/", Name)) of
        {error, _} = Error ->
            Error;

        {ok, Contents} ->
            {ok, binary_to_list(Contents)}
    end.


envy(To, Name, Default) ->
    envy:To(shrugs, Name, default(Default)).

default(Default) ->
    [os_env, app_env, {default, Default}].

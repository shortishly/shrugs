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
-export([branch/1]).
-export([dir/1]).
-export([enabled/1]).
-export([generate_key/1]).
-export([port/1]).
-export([timeout/1]).
-import(envy, [envy/1]).
-include_lib("kernel/include/logger.hrl").



branch(initial = Name) ->
    envy(#{caller => ?MODULE,
           names => [Name, ?FUNCTION_NAME],
           default => "main"}).


port(sshd = Name) ->
    envy(#{caller => ?MODULE,
           names => [Name, ?FUNCTION_NAME],
           default => 22}).


enabled(Name) ->
    envy(#{caller => ?MODULE,
           names => [Name, ?FUNCTION_NAME],
           default => true}).


dir(system = Name) ->
    envy(#{caller => ?MODULE,
           names => [Name, ?FUNCTION_NAME],
           default => "/etc/ssh"});

dir(repo = Name) ->
    envy(#{caller => ?MODULE,
           names => [Name, ?FUNCTION_NAME],
           default => "/repos"});

dir(bin = Name) ->
    envy(#{caller => ?MODULE,
           names => [Name, ?FUNCTION_NAME],
           default => "/bin"});

dir(user = Name) ->
    ?LOG_DEBUG(#{name => Name, home => os:getenv("HOME")}),
    envy(#{caller => ?MODULE,
           names => [Name, ?FUNCTION_NAME],
           default => case os:getenv("HOME") of
                          Home when Home == false; Home == "/"  ->
                              "/users";

                          Home ->
                              filename:join(Home, ".ssh")
                      end}).


timeout(watch = Name) ->
    envy(#{caller => ?MODULE,
           type => integer_or_atom,
           names => [Name, timeout],
           default => timer:seconds(5)});

timeout(Name) ->
    envy(#{caller => ?MODULE,
           type => integer_or_atom,
           names => [Name, timeout],
           default => infinity}).


authorized_keys() ->
    envy(#{caller => ?MODULE,
           type => binary,
           names => [?FUNCTION_NAME]}).


generate_key(param = Name) ->
    {ok, Application} = application:get_application(),

    case application:get_env(
           Application,
           shrugs_util:snake_case([?FUNCTION_NAME, Name])) of

        undefined ->
            [{namedCurve, ed25519}];

        {ok, Value} ->
            Value
    end.

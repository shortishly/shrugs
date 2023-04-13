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


-module(shrugs_key_store).


-behaviour(gen_statem).
-export([callback_mode/0]).
-export([handle_event/4]).
-export([host_key/2]).
-export([init/1]).
-export([is_auth_key/3]).
-export([pwd/2]).
-export([start_link/0]).
-import(shrugs_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("public_key/include/public_key.hrl").


start_link() ->
    gen_statem:start_link(
      {local, ?MODULE},
      ?MODULE,
      [],
      envy_gen:options(?MODULE)).


init([]) ->
    {ok, ready, #{key_store => ets:new(?MODULE, [private])}, nei(host_keys)}.


callback_mode() ->
    handle_event_function.

handle_event(internal, host_keys, _, #{key_store := KeyStore}) ->
    true = ets:insert_new(KeyStore, host_keys()),
    keep_state_and_data;

handle_event({call, From}, {add_auth_key, AuthKey, Comment}, _, #{key_store := KeyStore}) ->
    ets:insert(KeyStore, {{auth_key, AuthKey}, Comment}),
    {keep_state_and_data, {reply, From, ok}};

handle_event({call, From},
             {host_key, KeyType},
             _,
             #{key_store := KeyStore}) ->
    {keep_state_and_data,
     {reply,
      From,
      case ets:lookup(
             KeyStore,
             {host_key, key_type(KeyType)}) of

          [] ->
              {error, no_such_key};

          [{_, PrivateKey}] ->
              {ok, PrivateKey}
      end}};

handle_event({call, From}, {pwd, User, Password}, _, #{key_store := KeyStore}) ->
    {keep_state_and_data, {reply, From, ets:member(KeyStore, {auth, User, Password})}};

handle_event({call, From},
             {is_auth_key, Key, _},
             _,
             #{key_store := KeyStore}) ->
    case shrugs_config:enabled(authentication) of
        true ->
            {keep_state_and_data,
             {reply, From, ets:member(KeyStore, {auth_key, Key})}};

        false ->
            {keep_state_and_data, {reply, From, true}}
    end.


key_type(private, #'RSAPrivateKey'{}) ->
    'ssh-rsa';
key_type(private, #'DSAPrivateKey'{}) ->
    'ssh-dss';
key_type(private, #'ECPrivateKey'{parameters = {namedCurve, ?'id-Ed25519'}}) ->
    'ssh-ed25519';
key_type(private, #'ECPrivateKey'{parameters = {namedCurve, ?'id-Ed448'}}) ->
    'ssh-ed448';
key_type(private, #'ECPrivateKey'{parameters = {namedCurve, ?'secp256r1'}}) ->
    'ecdsa-sha2-nistp256';
key_type(private, #'ECPrivateKey'{parameters = {namedCurve, ?'secp384r1'}}) ->
    'ecdsa-sha2-nistp384';
key_type(private,#'ECPrivateKey'{parameters = {namedCurve, ?'secp521r1'}}) ->
    'ecdsa-sha2-nistp521';
key_type(PriPub, Key) ->
    error(badarg, [PriPub, Key]).


key_type('rsa-sha2-512') ->
    'ssh-rsa';
key_type('rsa-sha2-384') ->
    'ssh-rsa';
key_type('rsa-sha2-256') ->
    'ssh-rsa';
key_type(KeyType) ->
    KeyType.


host_key(Algorithm, _DaemonOptions) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, Algorithm}).


is_auth_key(PublicUserKey, User, _DaemonOptions) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, PublicUserKey, User}).


pwd(User, Password) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, User, Password}).


host_keys() ->
    case system_host_keys() of
        [] ->
            case file:consult(
                  filename:join(
                    shrugs_config:dir(system),
                    "shrugs_host_keys.terms")) of

                {ok, HostKeys} ->
                    HostKeys;

                {error, _} ->
                    HostKeys = lists:map(
                                 fun
                                     (Param) ->
                                         PrivateKey = public_key:generate_key(Param),
                                         ?LOG_DEBUG(#{private_key => PrivateKey}),

                                         KeyType = key_type(private, PrivateKey),
                                         ?LOG_DEBUG(#{param => Param, key_type => KeyType}), 
                                         {{host_key, KeyType}, PrivateKey}
                                 end,
                                 shrugs_config:generate_key(param)),

                    _ = file:write_file(
                          filename:join(
                            shrugs_config:dir(system),
                            "shrugs_host_keys.terms"),
                          [io_lib:fwrite("%% -*- mode: erlang -*-~n", []),
                           lists:map(
                             fun
                                 (Row) ->
                                     io_lib:fwrite("~p.~n", [Row])
                             end,
                             HostKeys)]),
                    HostKeys
            end;

        HostKeys ->
            HostKeys
    end.


system_host_keys() ->
    lists:filtermap(
      fun
          ({Key, _}) ->
              try
                  {true, {{host_key, key_type(private, Key)}, Key}}

              catch
                  error:badarg ->
                      ?LOG_DEBUG(#{key => Key}),
                      false
              end
      end,
      lists:flatmap(
        fun
            (Filename) ->
                ?LOG_DEBUG(#{filename => Filename}),

                case file:read_file(Filename) of
                    {ok, HostKey} ->
                        case ssh_file:decode(
                               HostKey, rfc4716_key) of
                            {error, Reason} ->
                                ?LOG_WARNING(
                                   #{filename => Filename,
                                     reason => Reason}),
                                [];

                            Decoded ->
                                Decoded
                        end;

                    {error, Reason} ->
                        ?LOG_WARNING(
                           #{filename => Filename,
                             reason => Reason}),
                        []
                end
        end,
        filelib:wildcard(
          filename:join(
            shrugs_config:dir(system),
            "ssh_host_*_key")))).

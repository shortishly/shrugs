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


-module(shrugs_users).


-export([callback_mode/0]).
-export([handle_event/4]).
-export([init/1]).
-export([start_link/0]).
-import(shrugs_statem, [nei/1]).
-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").


start_link() ->
    gen_statem:start_link(
      {local, ?MODULE},
      ?MODULE,
      [],
      envy_gen:options(?MODULE)).


init([]) ->
    case shrugs_config:enabled(authentication) of
        true ->
            {ok,
             ready,
             #{requests => gen_statem:reqids_new(),
               stats => #{}},
             nei(discover_keys)};

        false ->
            ignore
    end.


callback_mode() ->
    handle_event_function.


handle_event(internal, discover_keys, _, _) ->
    case os:getenv("SSH_PUBLIC_KEY_URL") of
        false ->
            try
                {keep_state_and_data,
                 nei({static, shrugs_config:authorized_keys()})}

            catch
                error:badarg ->
                    {keep_state_and_data,
                     nei({watch, shrugs_config:dir(user)})}
            end;

        URL ->
            case httpc:request(
                   get,
                   {URL, []},
                   [{timeout, 1_000}],
                   [{body_format, binary}]) of

                {ok, {_, _, Keys}} ->
                    {keep_state_and_data,
                     nei({static, Keys})};

                {error, Reason} ->
                    {stop, Reason}
            end
    end;

handle_event(internal, {static, EncodedKeys}, _, _) ->
    case ssh_file:decode(EncodedKeys, auth_keys) of
        {error, Reason} ->
            ?LOG_INFO(#{reason => Reason}),
            stop;

        Decoded ->
            {keep_state_and_data,
             lists:foldl(
               fun
                   ({AuthKey, Comment}, A) ->
                       [nei({add_auth_key, AuthKey, Comment}) | A]
               end,
               [nei(stop)],
               Decoded)}
    end;

handle_event(internal, stop, _, _) ->
    stop;

handle_event(internal, {watch, Directory}, _, #{stats := Stats} = Data)
  when is_map_key(Directory, Stats) ->
    #{Directory := Existing} = Stats,

    case file:read_link_info(Directory) of
        {ok, Existing} ->
            {keep_state_and_data,
             {{timeout, watch}, shrugs_config:timeout(watch), Directory}};

        {ok, New} ->
            case file:list_dir(Directory) of
                {ok, Filenames} ->
                    {keep_state,
                     Data#{stats := Stats#{Directory => New}},
                     lists:foldl(
                       fun
                           ("authorized_keys" = Filename, A) ->
                               [nei({stat, Directory, Filename}) | A];

                           (Filename, A) ->
                               case filename:extension(Filename) of
                                   ".pub" ->
                                       [nei({stat, Directory, Filename}) | A];

                                   _Otherwise ->
                                       A
                               end
                       end,
                       [{{timeout, watch}, shrugs_config:timeout(watch), Directory}],
                       Filenames)};

                {error, Reason} ->
                    ?LOG_WARNING(#{directory => Directory, reason => Reason}),
                    {keep_state_and_data,
                     {{timeout, watch},
                      shrugs_config:timeout(watch),
                      Directory}}
            end;

        {error, Reason} ->
            ?LOG_WARNING(#{directory => Directory, reason => Reason}),
            {keep_state_and_data,
             {{timeout, watch},
              shrugs_config:timeout(watch),
              Directory}}
    end;

handle_event(internal, {watch, Directory}, _, #{stats := Stats} = Data) ->
    case file:read_link_info(Directory) of
        {ok, New} ->
            case file:list_dir(Directory) of
                {ok, Filenames} ->
                    {keep_state,
                     Data#{stats := Stats#{Directory => New}},
                     lists:foldl(
                       fun
                           ("authorized_keys" = Filename, A) ->
                               [nei({stat, Directory, Filename}) | A];

                           (Filename, A) ->
                               case filename:extension(Filename) of
                                   ".pub" ->
                                       [nei({stat, Directory, Filename}) | A];

                                   _Otherwise ->
                                       A
                               end
                       end,
                       [{{timeout, watch}, shrugs_config:timeout(watch), Directory}],
                       Filenames)};

                {error, Reason} ->
                    ?LOG_WARNING(#{directory => Directory, reason => Reason}),
                    {keep_state_and_data,
                     {{timeout, watch},
                      shrugs_config:timeout(watch),
                      Directory}}
            end;

        {error, Reason} ->
            ?LOG_WARNING(#{directory => Directory, reason => Reason}),
            {keep_state_and_data,
             {{timeout, watch},
              shrugs_config:timeout(watch),
              Directory}}
    end;

handle_event({timeout, watch}, Directory, _, _) ->
    {keep_state_and_data, nei({watch, Directory})};

handle_event(internal,
             {stat, Directory, Filename},
             _,
             #{stats := Stats} = Data) when is_map_key(Filename, Stats) ->
    #{Filename := Existing} = Stats,

    case file:read_link_info(filename:join(Directory, Filename)) of

        {ok, Existing} ->
            keep_state_and_data;

        {ok, Updated} ->
            case file:read_file(filename:join(Directory, Filename)) of

                {ok, EncodedKeys} ->
                    case ssh_file:decode(EncodedKeys, auth_keys) of

                        {error, Reason} ->
                            ?LOG_INFO(#{directory => Directory,
                                filename => Filename,
                                        reason => Reason}),
                            {keep_state,
                             Data#{stats := Stats#{Filename := Updated}}};

                        Decoded ->
                            {keep_state,
                             Data#{stats := Stats#{Filename := Updated}},
                             lists:map(
                               fun
                                   ({AuthKey, Comment}) ->
                                       ?LOG_DEBUG(#{auth_key => AuthKey}),
                                       nei({add_auth_key, AuthKey, Comment})
                               end,
                               Decoded)}
                    end;
                
                {error, Reason} ->
                    ?LOG_INFO(#{directory => Directory,
                                filename => Filename,
                                reason => Reason}),
                    {keep_state, Data#{stats := Stats#{Filename := Updated}}}
            end;

        {error, Reason} ->
            ?LOG_INFO(#{directory => Directory,
                        filename => Filename,
                        reason => Reason}),
            keep_state_and_data
    end;


handle_event(internal,
             {stat, Directory, Filename},
             _,
             #{stats := Stats} = Data) ->
    case file:read_link_info(filename:join(Directory, Filename)) of

        {ok, New} ->
            case file:read_file(filename:join(Directory, Filename)) of

                {ok, EncodedKeys} ->
                    case ssh_file:decode(EncodedKeys, auth_keys) of

                        {error, Reason} ->
                            ?LOG_INFO(#{directory => Directory,
                                filename => Filename,
                                        reason => Reason}),
                            {keep_state,
                             Data#{stats := Stats#{Filename => New}}};

                        Decoded ->
                            {keep_state,
                             Data#{stats := Stats#{Filename => New}},
                             lists:map(
                               fun
                                   ({AuthKey, Comment}) ->
                                       ?LOG_DEBUG(#{auth_key => AuthKey}),
                                       nei({add_auth_key, AuthKey, Comment})
                               end,
                               Decoded)}
                    end;
                
                {error, Reason} ->
                    ?LOG_INFO(#{directory => Directory,
                                filename => Filename,
                                reason => Reason}),
                    {keep_state, Data#{stats := Stats#{Filename := New}}}
            end;

        {error, Reason} ->
            ?LOG_INFO(#{directory => Directory,
                        filename => Filename,
                        reason => Reason}),
            keep_state_and_data
    end;

handle_event(internal,
             {add_auth_key, _AuthKey, _Comment} = Label,
             _,
             #{requests := Requests} = Data) ->
    {keep_state,
     Data#{requests := gen_statem:send_request(
                         shrugs_key_store,
                         Label,
                         Label,
                         Requests)}};

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_statem:check_response(Msg, Existing, true) of
        {{reply, ok}, {add_auth_key, _, _}, Updated} ->
            {keep_state, Data#{requests := Updated}};

        {{error, {Reason, ServerRef}}, Label, UpdatedRequests} ->
                {stop,
                 #{reason => Reason,
                   server_ref => ServerRef,
                   label => Label},
                 Data#{requests := UpdatedRequests}}
    end.

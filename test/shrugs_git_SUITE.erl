%% Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
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


-module(shrugs_git_SUITE).


-compile(export_all).
-compile(nowarn_export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").


all() ->
    common:all(?MODULE).


init_per_suite(Config) ->
    BinDir = "/usr/bin",

    PrivDir = ?config(priv_dir, Config),

    RepoDir = filename:join(PrivDir, "repos"),
    SystemDir = filename:join(PrivDir, "system"),
    UserDir = filename:join(PrivDir, "users"),

    ok = file:make_dir(RepoDir),
    ok = file:make_dir(SystemDir),
    ok = file:make_dir(UserDir),

    _ = application:load(shrugs),
    application:set_env(shrugs, sshd_port, 0),
    application:set_env(shrugs, watch_timeout, 500),
    application:set_env(shrugs, system_dir, SystemDir),
    application:set_env(shrugs, repo_dir, RepoDir),
    application:set_env(shrugs, user_dir, UserDir),
    application:set_env(shrugs, bin_dir, BinDir),

    application:set_env(shrugs, git_trace, true),
    application:set_env(shrugs, key_store_trace, true),
    application:set_env(shrugs, ssh_daemon_trace, true),
    application:set_env(shrugs, users_trace, true),

    _ = application:load(grimsby),
    application:set_env(grimsby, port_trace, true),
    application:set_env(grimsby, command_trace, true),


    {ok, _} = application:ensure_all_started(shrugs),

    logger:set_handler_config(
      default,
      #{formatter => {logger_formatter,
                      #{template => [[logger_formatter, header],
                                     {pid, [" ", pid, ""], ""},
                                     {mfa, [" ", mfa, ":", line], ""},
                                     "\n",
                                     msg,
                                     "\n"],
                        legacy_header => true,
                        single_line => false}}}),

    logger:set_module_level([], debug),

    #{port := Port} = shrugs_ssh_daemon:info(),
    
    [{port, Port},
     {system_dir, SystemDir},
     {user_dir, UserDir},
     {repo_dir, RepoDir},
     {bin_dir, BinDir} | Config].


end_per_suite(_Config) ->
    ok = application:stop(shrugs).


unauth_push_test(Config) ->
    PrivDir = ?config(priv_dir, Config),

    KeyName = alpha(5),
    PrivateKey = filename:join(PrivDir, KeyName),
    ct:log(os:cmd("ssh-keygen -t ed25519 -N '' -f " ++ PrivateKey)),
    
    Repo = alpha(5),
    Content0 = list_to_binary(alpha(20)),
    RepoDir = filename:join(PrivDir, Repo),
    ok = file:make_dir(RepoDir),
    ok = file:write_file(
           filename:join(
             RepoDir,
             "content.txt"),
           Content0),

    Env = #{"GIT_SSH_COMMAND" => binary_to_list(
                                   iolist_to_binary(
                                     ["ssh",
                                      " -o UserKnownHostsFile=/dev/null",
                                      " -o IdentityFile='", PrivateKey, "'",
                                      " -o StrictHostKeyChecking=no",
                                      " -o LogLevel=QUIET"]))},

    {ok, _} = shrugs_git:command(RepoDir, Env, ["init", "."]),
    {ok, _} = shrugs_git:command(RepoDir, Env, ["add", "content.txt"]),
    {ok, _} = shrugs_git:command(RepoDir, Env, ["commit", "--message=''"]),


    URI = uri_string:recompose(
            #{host => "localhost",
              path => Repo,
              port => ?config(port, Config),
              scheme => "ssh"}),

    {error, _} = shrugs_git:command(
                RepoDir,
                Env,
                ["push", "--set-upstream", URI, "main"]).
    


push_clone_test(Config) -> 
    PrivDir = ?config(priv_dir, Config),

    KeyName = alpha(5),
    PrivateKey = filename:join(PrivDir, KeyName),
    ct:log(os:cmd("ssh-keygen -t ed25519 -N '' -f " ++ PrivateKey)),

    {ok, _} = file:copy(PrivateKey ++ ".pub",
                        filename:join(
                          ?config(user_dir, Config),
                          KeyName ++ ".pub")),
    
    %% wait for public key to be picked up from watched user directory...
    %%
    timer:sleep(trunc(shrugs_config:timeout(watch) * 1.5)),
    
    Repo = alpha(5),
    Content0 = list_to_binary(alpha(20)),
    RepoDir = filename:join(PrivDir, Repo),
    ok = file:make_dir(RepoDir),
    ok = file:write_file(
           filename:join(
             RepoDir,
             "content.txt"),
           Content0),

    Env = #{"GIT_SSH_COMMAND" => binary_to_list(
                                   iolist_to_binary(
                                     ["ssh",
                                      " -o UserKnownHostsFile=/dev/null",
                                      " -o IdentityFile='", PrivateKey, "'",
                                      " -o StrictHostKeyChecking=no",
                                      " -o LogLevel=QUIET"]))},

    {ok, _} = shrugs_git:command(RepoDir, Env, ["init", "."]),
    {ok, _} = shrugs_git:command(RepoDir, Env, ["add", "content.txt"]),
    {ok, _} = shrugs_git:command(RepoDir, Env, ["commit", "--message=''"]),


    URI = uri_string:recompose(
            #{host => "localhost",
              path => Repo,
              port => ?config(port, Config),
              scheme => "ssh"}),

    {ok, _} = shrugs_git:command(
                RepoDir,
                Env,
                ["push", "--set-upstream", URI, "main"]),

    Clone = alpha(5),

    {ok, _} = shrugs_git:command(
                PrivDir,
                Env,
                ["clone", URI, Clone]),

    {ok, Content0} = file:read_file(
                      filename:join(
                        [PrivDir,
                         Clone,
                         "content.txt"])),

    Content1 = list_to_binary(alpha(20)),

    ok = file:write_file(
           filename:join(
             [PrivDir,
              Clone,
              "content.txt"]),
           Content1),

    {ok, _} = shrugs_git:command(
                filename:join(
                  [PrivDir,
                   Clone]),
                Env,
                ["add", "content.txt"]),

    {ok, _} = shrugs_git:command(
                filename:join(
                  [PrivDir,
                   Clone]),
                Env,
                ["commit", "--message=''"]),

    {ok, _} = shrugs_git:command(
                filename:join(
                  [PrivDir,
                   Clone]),
                Env,
                ["push"]),

    {ok, _} = shrugs_git:command(
                RepoDir,
                Env,
                ["pull"]),


    {ok, Content1} = file:read_file(
                      filename:join(
                        [RepoDir,
                         "content.txt"])).


alpha(N) ->
    pick(N, lists:seq($a, $z)).


pick(N, Pool) ->
    ?FUNCTION_NAME(N, Pool, []).


pick(0, _, A) ->
    A;

pick(N, Pool, A) ->
    ?FUNCTION_NAME(N - 1,
                   Pool,
                   [lists:nth(rand:uniform(length(Pool)), Pool) | A]).


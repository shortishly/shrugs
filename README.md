# Shrugs: A Self Hosted Git SSH Server

[shrugs][shrugs] is an [Apache licensed][apache-license] self hosted
git server available as a [docker container][shrugs-container] written
in [Erlang][erlang-org] and [Rust][rust-lang-org].

Sometimes you just want to host some local changes before publishing
to the cloud, using a simple git server than can easily run on
something as small as a [Raspberry PI][raspberry-pi].

## Quick Start

You can run `shrugs` in a docker container with:

```shell
docker run \
  --name shrugs \
  -d \
  -p 22022:22 \
  ghcr.io/shortishly/shrugs
```

The container supports both `amd64` and  `arm64` architectures.

## Authentication

To copy your `authorized_keys` into the container for authentication:

```shell
docker cp ~/.ssh/authorized_keys shrugs:/users
```

Or copy the public key for individual users:

```shell
docker cp bob.pub shrugs:/users
docker cp alice.pub shrugs:/users
```

New keys ("*.pub" or "authorized_keys") in the users directory will be
picked up (by default) every 5 seconds.

## Push

Push any repository (including a new repository) into shrugs with:

```shell
mkdir demo
cd demo
git init .
echo "world" > hello.txt
git add hello.txt     
git commit --message='demo'
git branch -M main
git remote add origin ssh://localhost:22022/demo
git push --set-upstream origin main
```

## Clone

Clone an existing repository already in shrugs with:

```shell
git clone ssh://localhost:22022/demo abc
```

Add some more content and push to the existing repository:

```shell
cd abc
echo "God’s Own Country" > yorkshire.txt
git add yorkshire.txt
git commit --message='home'
git push origin main
```

## ls

To list (ls) the repositories stored in shrugs:

```shell
$ ssh -p 22022 localhost ls
demo
```

## ~/.ssh/config

With the addition of a host entry for shrugs in your `.ssh/config`:

```shell
host shrugs
     hostname localhost
     checkhostip no
     stricthostkeychecking no
     loglevel QUIET
     port 22022
```

Results in a bit less typing:

```shell
$ ssh shrugs ls
demo

$ git clone ssh://shrugs/demo
Cloning into 'demo'...
remote: Enumerating objects: 6, done.
remote: Counting objects: 100% (6/6), done.
remote: Compressing objects: 100% (3/3), done.
remote: Total 6 (delta 0), reused 0 (delta 0), pack-reused 0
Receiving objects: 100% (6/6), 466 bytes | 155.00 KiB/s, done.

$ cd demo
$ git remote -v
origin ssh://shrugs/demo (fetch)
origin ssh://shrugs/demo (push)
```

## compose.yaml

An example `compose.yaml` with volume mounts for `/repos` (git
repository storage), `/etc/ssh` (shrugs ssh host key), and `/users`
(user *public* keys).

```yaml
---
services:
  shrugs:
    image:
      ${SHRUGS_IMAGE:-ghcr.io/shortishly/shrugs:${SHRUGS_VERSION:-latest}}
    ports:
      - 22022:22
    pull_policy:
      ${PULL_POLICY:-always}
    environment:
      SHRUGS_KEY_STORE_TRACE: false
      SHRUGS_SSH_DAEMON_TRACE: false
      SHRUGS_USERS_TRACE: false
    volumes:
      - repos:/repos
      - host_keys:/etc/ssh
      - user_keys:/users
volumes:
  repos:
    driver: local
  host_keys:
    driver: local
  user_keys:
    driver: local
```

With the above you can copy keys with [docker compose
cp][docker-compose-cp] rather than [docker cp][docker-cp]:

```shell
docker compose cp bob.pub shrugs:/users
docker compose cp alice.pub shrugs:/users
```

## Directories

Shrugs uses the following directories in the container:

| Directory | Description                                              |
|-----------|----------------------------------------------------------|
| /users    | User public SSH keys in "*.pub" and/or "authorized_keys" |
| /repos    | Storage of git repositories                              |
| /etc/ssh  | SSH Host keys for Shrugs service                         |

## Environment

The following environment variables can be used for configuration:

|Variable|Default|Description|
|-|-|-|
|SHRUGS\_SSHD\_PORT|22|Incoming SSH connections on this port|
|SHRUGS\_AUTHENTICATION\_ENABLED|true|"false" will disable user authentication|
|SHRUGS\_SYSTEM\_DIR|/etc/ssh|This directory to find the host key|
|SHRUGS\_USER\_DIR|/users|This directory to find authorized user keys|
|SHRUGS\_REPO\_DIR|/repos|This directory to store git repositories|
|SHRUGS\_BIN\_DIR|/bin|This directory to find the git executable|
|SHRUGS\_WATCH\_TIMEOUT|5000|Check the repo dir every 5000ms for new user keys|
|SHRUGS\_INITIAL\_BRANCH|main|Initial branch name used with git --init --bare|

## Debug

The following environment variables can be used to enable debug logging:

|Variable|Default|Description|
|-|-|-|
|SHRUGS\_KEY\_STORE\_TRACE|false|Enables logging options from the key store|
|SHRUGS\_SSH_DAEMON\_TRACE|false|Enables logging of the SSH daemon|
|SHRUGS\_USERS\_TRACE|false|Enables logging of the users subsystem|

## Build

Erlang/OTP 25 is required.

```shell
make
```

A local docker container can be built with:

```shell
bin/build
```

[apache-license]: https://www.apache.org/licenses/LICENSE-2.0
[docker-compose-cp]: https://docs.docker.com/engine/reference/commandline/compose_cp/
[docker-cp]: https://docs.docker.com/engine/reference/commandline/cp/
[erlang-org]: https://www.erlang.org
[raspberry-pi]: https://www.raspberrypi.com
[rust-lang-org]: https://www.rust-lang.org
[shrugs-container]: https://github.com/shortishly/shrugs/pkgs/container/shrugs
[shrugs]: https://github.com/shortishly/shrugs

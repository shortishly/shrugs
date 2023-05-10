# secure shell remote user git server (shrugs)

A git server that you can push, clone or pull over ssh... ¯\\\_(ツ)\_/¯

You can run a docker container with:

```shell
docker run \
  --name shrugs \
  -d \
  -p 22022:22 \
  ghcr.io/shortishly/shrugs
```

Shrugs uses the following directories in the container:

| Directory | Description                                              |
|-----------|----------------------------------------------------------|
| /users    | User public SSH keys in "*.pub" and/or "authorized_keys" |
| /repos    | Storage of git repositories                              |
| /etc/ssh  | SSH Host keys for Shrugs service                         |

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

Push any repository (including a new repository) into shrugs with:

```shell
mkdir demo
cd demo
git init .
echo "world" > hello.txt
git add hello.txt     
git commit --message=''

git push --set-upstream ssh://localhost:22022/demo
```

Clone an existing repository already in shrugs with:

```shell
git clone ssh://localhost:22022/demo abc
```

Push to an existing repository:

```shell
cd abc
echo "God’s Own Country" > yorkshire.txt
git add yorkshire.txt
git commit --message=''
git push
```

To list (ls) the repositories stored in shrugs:

```shell
$ ssh -p 22022 localhost ls
demo
```

With a host entry for shrugs in your `.ssh/config`:

```shell
host shrugs
     hostname localhost
     checkhostip no
     stricthostkeychecking no
     loglevel QUIET
     port 22022
```

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

With the above compose you can copy keys with `docker compose cp`
rather than `docker cp`:

```shell
docker compose cp bob.pub shrugs:/users
docker compose cp alice.pub shrugs:/users
```

## Environment

The following environment variables can be used for configuration:

| Variable                        | Default    | Description                                       |
|---------------------------------|------------|---------------------------------------------------|
| SHRUGS\_SSHD\_PORT              | 22         | Incoming SSH connections on this port             |
| SHRUGS\_AUTHENTICATION\_ENABLED | true       | "false" will disable user authentication          |
| SHRUGS\_SYSTEM\_DIR             | /etc/ssh   | This directory to find the host key               |
| SHRUGS\_USER\_DIR               | /users     | This directory to find authorized user keys       |
| SHRUGS\_REPO\_DIR               | /repos     | This directory to store git repositories          |
| SHRUGS\_BIN\_DIR                | /bin       | This directory to find the git executable         |
| SHRUGS\_WATCH\_TIMEOUT          | 5000       | Check the repo dir every 5000ms for new user keys |
| SHRUGS\_INITIAL\_BRANCH         | main       | Initial branch name used with git --init --bare   |


## Debug

The following environment variables can be used to enable debug logging:

| Variable                        | Default    | Description                                |
|---------------------------------|------------|--------------------------------------------|
| SHRUGS\_KEY\_STORE\_TRACE       | false      | Enables logging options from the key store |
| SHRUGS\_SSH_DAEMON\_TRACE       | false      | Enables logging of the SSH daemon          |
| SHRUGS\_USERS\_TRACE            | false      | Enables logging of the users subsystem     |

## Build

Erlang/OTP 25 is required.

```shell
make
```

A local docker container can be built with:

```shell
bin/build
```

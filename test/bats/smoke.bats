#!/usr/bin/env bats
# -*- mode: shell-script; sh-shell: bash; -*-
# Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

setup() {
    KEY_DIR="$(cd "$(dirname "$BATS_TEST_FILENAME")" >/dev/null 2>&1 && pwd )"
    export KEY="${KEY_DIR}/bob"

    if ! [ -f "$KEY" ]; then
        ssh-keygen -t ed25519 -N '' -f "$KEY" -C "bob@example.com" >/dev/null 2>&1
        docker compose cp "$KEY.pub" shrugs:/users >/dev/null 2>&1
        sleep 5
    fi

    export GIT_SSH_COMMAND="ssh -o UserKnownHostsFile=/dev/null -o IdentityFile=$KEY -o StrictHostKeyChecking=no"
}

@test "auth_key_comments" {
    run ssh -o UserKnownHostsFile=/dev/null -o IdentityFile="$KEY" -o StrictHostKeyChecking=no -o LogLevel=QUIET -p 22022 localhost auth_key_comments
    [ "$output" = "[\"bob@example.com\"]." ]
}

@test "init_commit_clone" {
    git config init.defaultBranch main

    INIT_DIR=$(mktemp -d)
    git -C "$INIT_DIR" init
    echo "hello world" > "$INIT_DIR/greeting.txt"
    git -C "$INIT_DIR" add greeting.txt
    git -C "$INIT_DIR" commit -m "initial"
    git -C "$INIT_DIR" remote add origin "ssh://localhost:22022/$(basename "$INIT_DIR")"
    git -C "$INIT_DIR" push --set-upstream origin main

    ROOT_DIR=$(mktemp -d)
    git -C "$ROOT_DIR" clone "ssh://localhost:22022/$(basename "$INIT_DIR")"

    CLONE_DIR="$ROOT_DIR/$(basename "$INIT_DIR")"
    echo "God’s Own Country" > "$CLONE_DIR/yorkshire.txt"
    git -C "$CLONE_DIR" add yorkshire.txt
    git -C "$CLONE_DIR" commit --message='home'
    git -C "$CLONE_DIR" push origin main
}

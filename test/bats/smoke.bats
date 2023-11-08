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
    export KEY="bob"

    if ! [ -f "$KEY" ]; then
        ssh-keygen -t ed25519 -N '' -f "$KEY" -C "bob@example.com" >/dev/null 2>&1
        docker compose cp "$KEY.pub" shrugs:/users >/dev/null 2>&1
        sleep 5
    fi

    export GIT_SSH_COMMAND="ssh -o UserKnownHostsFile=/dev/null -o IdentityFile=$KEY -o StrictHostKeyChecking=no -o LogLevel=QUIET -p 22022"
}

@test "auth_key_comments" {
    run ssh -o UserKnownHostsFile=/dev/null -o IdentityFile="$KEY" -o StrictHostKeyChecking=no -o LogLevel=QUIET -p 22022 localhost auth_key_comments
    [ "$output" = "[\"bob@example.com\"]." ]
}

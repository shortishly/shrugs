#-*- mode: makefile-gmake -*-
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
#

PROJECT = shrugs
PROJECT_DESCRIPTION = Secure sHell Remote User Git Server
PROJECT_VERSION = ${shell git describe --tags}

DEPS += envy
DEPS += grimsby

RELX_TAR = 0

LOCAL_DEPS += crypto
LOCAL_DEPS += inets
LOCAL_DEPS += sasl
LOCAL_DEPS += ssh
LOCAL_DEPS += ssl


dep_envy = git https://github.com/shortishly/envy.git
dep_grimsby = git https://github.com/shortishly/grimsby.git

dep_envy_commit = 0.9.2
dep_grimsby_commit = 0.2.0


SHELL_OPTS += -config dev.config
SHELL_OPTS += -s $(PROJECT)
SHELL_OPTS += -s sync


SHELL_DEPS += sync


PLT_APPS += any
PLT_APPS += asn1
PLT_APPS += bbmustache
PLT_APPS += compiler
PLT_APPS += crypto
PLT_APPS += envy
PLT_APPS += grimsby
PLT_APPS += inets
PLT_APPS += mnesia
PLT_APPS += public_key
PLT_APPS += runtime_tools
PLT_APPS += ssl
PLT_APPS += stdlib
PLT_APPS += syntax_tools
PLT_APPS += tools
PLT_APPS += xmerl


BUILD_DEPS += relx

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)

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
PROJECT_VERSION = 0.1.0

DEPS = \
	envy \
	grimsby

RELX_TAR = 0

LOCAL_DEPS = \
	crypto \
	sasl \
	ssh


dep_envy = git https://github.com/shortishly/envy.git
dep_grimsby = git https://github.com/shortishly/grimsby.git

dep_envy_commit = 0.7.2


SHELL_OPTS = \
	-config dev.config \
	-s $(PROJECT) \
	-s sync


SHELL_DEPS = \
	sync


PLT_APPS = \
	any \
	asn1 \
	bbmustache \
	compiler \
	crypto \
	envy \
	grimsby \
	inets \
	mnesia \
	public_key \
	runtime_tools \
	ssl \
	stdlib \
	syntax_tools \
	tools \
	xmerl


BUILD_DEPS += relx

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)

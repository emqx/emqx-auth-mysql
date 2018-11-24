.PHONY: app tests

PROJECT = emqx_auth_mysql
PROJECT_DESCRIPTION = EMQ X Authentication/ACL with MySQL
PROJECT_VERSION = 3.0

DEPS = mysql ecpool clique emqx_passwd

dep_mysql       = git-emqx https://github.com/mysql-otp/mysql-otp 1.3.2
dep_ecpool      = git-emqx https://github.com/emqx/ecpool master
dep_clique      = git-emqx https://github.com/emqx/clique
dep_emqx_passwd = git-emqx https://github.com/emqx/emqx-passwd emqx30

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqtt/emqttd emqx30
dep_cuttlefish = git-emqx https://github.com/emqtt/cuttlefish emqx30

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +debug_info

TEST_DEPS = emqx_auth_username
dep_emqx_auth_username = git-emqx https://github.com/emqx/emqx-auth-username emqx30

TEST_ERLC_OPTS += +debug_info

COVER = true

define dep_fetch_git-emqx
	git clone -q --depth 1 -b $(call dep_commit,$(1)) -- $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1)) > /dev/null 2>&1; \
	cd $(DEPS_DIR)/$(call dep_name,$(1));
endef

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_mysql.conf -i priv/emqx_auth_mysql.schema -d data

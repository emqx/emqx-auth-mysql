.PHONY: app tests

PROJECT = emqx_auth_mysql
PROJECT_DESCRIPTION = EMQ X Authentication/ACL with MySQL
PROJECT_VERSION = 3.0

DEPS = mysql ecpool clique emqx_passwd

dep_mysql  = git https://github.com/mysql-otp/mysql-otp 1.3.2
dep_ecpool = git https://github.com/emqtt/ecpool master
dep_clique = git https://github.com/emqtt/clique
dep_emqx_passwd = git https://github.com/emqx/emqx-passwd emqx30

BUILD_DEPS = emqx cuttlefish
dep_emqx = git https://github.com/emqtt/emqttd emqx30
dep_cuttlefish = git https://github.com/emqtt/cuttlefish emqx30

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

TEST_DEPS = emqx_auth_username
dep_emqx_auth_username = git https://github.com/emqx/emqx-auth-username emqx30

TEST_ERLC_OPTS += +debug_info
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_mysql.conf -i priv/emqx_auth_mysql.schema -d data

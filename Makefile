PROJECT = emq_auth_mysql
PROJECT_DESCRIPTION = Authentication/ACL with MySQL
PROJECT_VERSION = 2.0

DEPS = mysql ecpool

dep_mysql    = git https://github.com/mysql-otp/mysql-otp 1.2.0
dep_ecpool   = git https://github.com/emqtt/ecpool master

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd master

TEST_DEPS = cuttlefish
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	cuttlefish -l info -e etc/ -c etc/emq_auth_mysql.conf -i priv/emq_auth_mysql.schema -d data


PROJECT = emqttd_auth_mysql
PROJECT_DESCRIPTION = emqttd Authentication/ACL against MySQL
PROJECT_VERSION = 1.1

DEPS = mysql ecpool emqttd 

dep_mysql  = git https://github.com/mysql-otp/mysql-otp 1.1.1
dep_ecpool = git https://github.com/emqtt/ecpool master
dep_emqttd = git https://github.com/emqtt/emqttd plus

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

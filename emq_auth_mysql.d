src/emq_auth_mysql.erl:: src/emq_auth_mysql_cli.erl; @touch $@
src/emq_auth_mysql_app.erl:: include/emq_auth_mysql.hrl src/emq_auth_mysql_cli.erl; @touch $@
src/emq_auth_mysql_cli.erl:: include/emq_auth_mysql.hrl; @touch $@
src/emq_auth_mysql_sup.erl:: include/emq_auth_mysql.hrl; @touch $@

COMPILE_FIRST += emq_auth_mysql_cli

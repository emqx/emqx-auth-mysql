%%--------------------------------------------------------------------
%% Copyright (c) 2012-2016 Feng Lee <feng@emqtt.io>.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emq_auth_mysql_SUITE).

-compile(export_all).

-define(PID, emq_auth_mysql).

-include_lib("emqttd/include/emqttd.hrl").

%%setp1 init table
-define(DROP_ACL_TABLE, <<"DROP TABLE IF EXISTS mqtt_acl">>).

-define(CREATE_ACL_TABLE, <<"CREATE TABLE mqtt_acl ("
                            "   id int(11) unsigned NOT NULL AUTO_INCREMENT,"
                            "   allow int(1) DEFAULT NULL COMMENT '0: deny, 1: allow',"
                            "   ipaddr varchar(60) DEFAULT NULL COMMENT 'IpAddress',"
                            "   username varchar(100) DEFAULT NULL COMMENT 'Username',"
                            "   clientid varchar(100) DEFAULT NULL COMMENT 'ClientId',"
                            "   access int(2) NOT NULL COMMENT '1: subscribe, 2: publish, 3: pubsub',"
                            "   topic varchar(100) NOT NULL DEFAULT '' COMMENT 'Topic Filter',",
                            "   PRIMARY KEY (`id`)"
                            ") ENGINE=InnoDB DEFAULT CHARSET=utf8">>).

-define(INIT_ACL, <<"INSERT INTO mqtt_acl (id, allow, ipaddr, username, clientid, access, topic)"
                    "VALUES  (1,1,NULL,'$all',NULL,2,'#'),"
	                        "(2,0,NULL,'$all',NULL,1,'$SYS/#'),"
	                        "(3,0,NULL,'$all',NULL,1,'eq #'),"
	                        "(5,1,'127.0.0.1',NULL,NULL,2,'$SYS/#'),"
	                        "(6,1,'127.0.0.1',NULL,NULL,2,'#'),"
	                        "(7,1,NULL,'dashboard',NULL,1,'$SYS/#')">>).

-define(DROP_AUTH_TABLE, <<"DROP TABLE IF EXISTS `mqtt_user`">>).

-define(CREATE_AUTH_TABLE, <<"CREATE TABLE `mqtt_user` ("
                             "`id` int(11) unsigned NOT NULL AUTO_INCREMENT,"
                             "`username` varchar(100) DEFAULT NULL,"
                             "`password` varchar(100) DEFAULT NULL,"
                             "`salt` varchar(20) DEFAULT NULL,"
                             "`is_superuser` tinyint(1) DEFAULT 0,"
                             "`created` datetime DEFAULT NULL,"
                             "PRIMARY KEY (`id`),"
                             "UNIQUE KEY `mqtt_username` (`username`)"
                             ") ENGINE=MyISAM DEFAULT CHARSET=utf8">>).

-define(INIT_AUTH, <<"INSERT INTO mqtt_user (id, username, password, salt, is_superuser, created)"
                     "VALUES  (1, 'testuser1', 'pass1', 'plain', 0, now())," 
                             "(2, 'testuser2', 'pass2', 'plain', 1, now())">>).

all() -> 
    [{group, emq_auth_mysql}].

groups() -> 
    [{emq_auth_mysql, [sequence],
     [check_acl,
      check_auth]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    application:start(lager),
    peg_com(DataDir),
    [start_apps(App, DataDir) || App <- [emqttd, emq_auth_mysql]],
    Config.

end_per_suite(_Config) ->
    application:stop(emq_auth_mysql),
    application:stop(ecpool),
    application:stop(mysql),
    application:stop(emqttd),
    emqttd_mnesia:ensure_stopped().

check_acl(_) ->
    init_acl_(),
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser">>},
    User2 = #mqtt_client{client_id = <<"client2">>, username = <<"xyz">>},
    deny = emqttd_access_control:check_acl(User1, subscribe, <<"users/testuser/1">>),
    deny = emqttd_access_control:check_acl(User2, subscribe, <<"a/b/c">>),
    deny  = emqttd_access_control:check_acl(User1, subscribe, <<"$SYS/testuser/1">>),
    deny  = emqttd_access_control:check_acl(User2, subscribe, <<"$SYS/testuser/1">>),
    drop_table_(?DROP_ACL_TABLE).


init_acl_() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    ok = mysql:query(Pid, ?DROP_ACL_TABLE),
    ok = mysql:query(Pid, ?CREATE_ACL_TABLE),
    ok = mysql:query(Pid, ?INIT_ACL).

check_auth(_) ->
    init_auth_(), 
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser1">>},

    User2 = #mqtt_client{client_id = <<"client2">>, username = <<"testuser2">>},
    
    User3 = #mqtt_client{client_id = <<"client3">>},
    {ok, false} = emqttd_access_control:auth(User1, <<"pass1">>),
    {error, _} = emqttd_access_control:auth(User1, <<"pass">>),
    {error,username_or_password_undefined} = emqttd_access_control:auth(User1, <<>>),
    
    {ok, true} = emqttd_access_control:auth(User2, <<"pass2">>),
    {error, username_or_password_undefined} = emqttd_access_control:auth(User2, <<>>),
    {error, password_error} = emqttd_access_control:auth(User2, <<"errorpwd">>),
    
    
    {error, _} = emqttd_access_control:auth(User3, <<"pwd">>),
    drop_table_(?DROP_AUTH_TABLE).

init_auth_() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    ok = mysql:query(Pid, ?DROP_AUTH_TABLE),
    ok = mysql:query(Pid, ?CREATE_AUTH_TABLE),
    ok = mysql:query(Pid, ?INIT_AUTH).

drop_table_(Tab) ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    ok = mysql:query(Pid, Tab).

start_apps(App, DataDir) ->
    Schema = cuttlefish_schema:files([filename:join([DataDir, atom_to_list(App) ++ ".schema"])]),
    Conf = conf_parse:file(filename:join([DataDir, atom_to_list(App) ++ ".conf"])),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals],
    application:ensure_all_started(App).

peg_com(DataDir) ->
    ParsePeg = file2(3, DataDir, "conf_parse.peg"),
    neotoma:file(ParsePeg),
    ParseErl = file2(3, DataDir, "conf_parse.erl"),
    compile:file(ParseErl, []),

    DurationPeg = file2(3, DataDir, "cuttlefish_duration_parse.peg"),
    neotoma:file(DurationPeg),
    DurationErl = file2(3, DataDir, "cuttlefish_duration_parse.erl"),
    compile:file(DurationErl, []).
    

file2(Times, Dir, FileName) when Times < 1 ->
    filename:join([Dir, "deps", "cuttlefish","src", FileName]);

file2(Times, Dir, FileName) ->
    Dir1 = filename:dirname(Dir),
    file2(Times - 1, Dir1, FileName).



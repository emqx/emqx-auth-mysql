%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_auth_mysql_SUITE).

-compile(export_all).

-define(PID, emqx_auth_mysql).

-define(APP, ?PID).

-include_lib("emqx/include/emqx.hrl").

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

%%setp1 init table
-define(DROP_ACL_TABLE, <<"DROP TABLE IF EXISTS mqtt_acl">>).

-define(CREATE_ACL_TABLE, <<"CREATE TABLE mqtt_acl ("
                            "   id int(11) unsigned NOT NULL AUTO_INCREMENT,"
                            "   allow int(1) DEFAULT NULL COMMENT '0: deny, 1: allow',"
                            "   ipaddr varchar(60) DEFAULT NULL COMMENT 'IpAddress',"
                            "   username varchar(100) DEFAULT NULL COMMENT 'Username',"
                            "   clientid varchar(100) DEFAULT NULL COMMENT 'ClientId',"
                            "   access int(2) NOT NULL COMMENT '1: subscribe, 2: publish, 3: pubsub',"
                            "   topic varchar(100) NOT NULL DEFAULT '' COMMENT 'Topic Filter',"
                            "   PRIMARY KEY (`id`)"
                            ") ENGINE=InnoDB DEFAULT CHARSET=utf8">>).

-define(INIT_ACL, <<"INSERT INTO mqtt_acl (id, allow, ipaddr, username, clientid, access, topic)"
                    "VALUES
                            (1,1,'127.0.0.1','u1','c1',1,'t1'),"
                            "(2,0,'127.0.0.1','u2','c2',1,'t1'),"
                            "(3,1,'10.10.0.110','u1','c1',1,'t1'),"
                            "(4,1,'127.0.0.1','u3','c3',3,'t1')">>).

-define(DROP_AUTH_TABLE, <<"DROP TABLE IF EXISTS `mqtt_user`">>).

-define(CREATE_AUTH_TABLE, <<"CREATE TABLE `mqtt_user` ("
                             "`id` int(11) unsigned NOT NULL AUTO_INCREMENT,"
                             "`username` varchar(100) DEFAULT NULL,"
                             "`password` varchar(100) DEFAULT NULL,"
                             "`salt` varchar(100) DEFAULT NULL,"
                             "`is_superuser` tinyint(1) DEFAULT 0,"
                             "`created` datetime DEFAULT NULL,"
                             "PRIMARY KEY (`id`),"
                             "UNIQUE KEY `mqtt_username` (`username`)"
                             ") ENGINE=MyISAM DEFAULT CHARSET=utf8">>).

-define(INIT_AUTH, <<"INSERT INTO mqtt_user (id, is_superuser, username, password, salt)"
                     "VALUES  (1, true, 'plain', 'plain', 'salt'),"
                             "(2, false, 'md5', '1bc29b36f623ba82aaf6724fd3b16718', 'salt'),"
                             "(3, false, 'sha', 'd8f4590320e1343a915b6394170650a8f35d6926', 'salt'),"
                             "(4, false, 'sha256', '5d5b09f6dcb2d53a5fffc60c4ac0d55fabdf556069d6631545f42aa6e3500f2e', 'salt'),"
                             "(5, false, 'pbkdf2_password', 'cdedb5281bb2f801565a1122b2563515', 'ATHENA.MIT.EDUraeburn'),"
                             "(6, false, 'bcrypt_foo', '$2a$12$sSS8Eg.ovVzaHzi1nUHYK.HbUIOdlQI0iS22Q5rd5z.JVVYH6sfm6', '$2a$12$sSS8Eg.ovVzaHzi1nUHYK.'),"
			     "(7, false, 'bcrypt', '$2y$16$rEVsDarhgHYB0TGnDFJzyu5f.T.Ha9iXMTk9J36NCMWWM7O16qyaK', 'salt'),"
                             "(8, false, 'bcrypt_wrong', '$2y$16$rEVsDarhgHYB0TGnDFJzyu', 'salt')">>).

all() ->
    [{group, emqx_auth_mysql_acl},
     {group, emqx_auth_mysql_auth},
     {group, emqx_auth_mysql}
    ].

groups() ->
    [{emqx_auth_mysql_auth, [sequence], [check_auth]},
     {emqx_auth_mysql_acl, [sequence], [check_acl, 
                                        acl_super]},
     {emqx_auth_mysql, [sequence], [comment_config, placeholders]}].

init_per_suite(Config) ->
    emqx_ct_helpers:start_apps([emqx_auth_mysql], fun set_special_configs/1),
    Config.

end_per_suite(_Config) ->
    drop_table_(?DROP_AUTH_TABLE),
    drop_table_(?DROP_ACL_TABLE),
    emqx_ct_helpers:stop_apps([emqx_auth_mysql]).

check_acl(_) ->
    init_acl_(),
    User1 = #{zone => external, client_id => <<"c1">>, username => <<"u1">>, peername => {{127,0,0,1}, 1}},
    User2 = #{zone => external, client_id => <<"c2">>, username => <<"u2">>, peername => {{127,0,0,1}, 1}},
    allow = emqx_access_control:check_acl(User1, subscribe, <<"t1">>),
    deny = emqx_access_control:check_acl(User2, subscribe, <<"t1">>),

    User3 = #{zone => external, peername => {{10,10,0,110}, 1}, client_id => <<"c1">>, username => <<"u1">>},
    User4 = #{zone => external, peername => {{10,10,10,110}, 1}, client_id => <<"c1">>, username => <<"u1">>},
    allow = emqx_access_control:check_acl(User3, subscribe, <<"t1">>),
    allow = emqx_access_control:check_acl(User3, subscribe, <<"t1">>),
    allow = emqx_access_control:check_acl(User3, subscribe, <<"t2">>),%% nomatch -> ignore -> emqx acl
    allow = emqx_access_control:check_acl(User4, subscribe, <<"t1">>),%% nomatch -> ignore -> emqx acl
    User5 = #{zone => external, peername => {{127,0,0,1}, 1}, client_id => <<"c3">>, username => <<"u3">>},
    allow = emqx_access_control:check_acl(User5, subscribe, <<"t1">>),
    allow = emqx_access_control:check_acl(User5, publish, <<"t1">>).

acl_super(_Config) ->
    init_auth_(),
    reload([{password_hash, plain}]),
    {ok, C} = emqx_client:start_link([{host, "localhost"},
                                      {client_id, <<"simpleClient">>},
                                      {username, <<"plain">>},
                                      {password, <<"plain">>}]),
    {ok, _} = emqx_client:connect(C),
    timer:sleep(10),
    emqx_client:subscribe(C, <<"TopicA">>, qos2),
    timer:sleep(1000),
    emqx_client:publish(C, <<"TopicA">>, <<"Payload">>, qos2),
    timer:sleep(1000),
    receive
        {publish, #{payload := Payload}} ->
            ?assertEqual(<<"Payload">>, Payload)
    after
        1000 ->
            ct:fail({receive_timeout, <<"Payload">>}),
            ok
    end,
    emqx_client:disconnect(C).

init_acl_() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    ok = mysql:query(Pid, ?DROP_ACL_TABLE),
    ok = mysql:query(Pid, ?CREATE_ACL_TABLE),
    ok = mysql:query(Pid, ?INIT_ACL).

check_auth(_) ->
    init_auth_(),
    Plain = #{client_id => <<"client1">>, username => <<"plain">>},
    Md5 = #{client_id => <<"md5">>, username => <<"md5">>},
    Sha = #{client_id => <<"sha">>, username => <<"sha">>},
    Sha256 = #{client_id => <<"sha256">>, username => <<"sha256">>},
    Pbkdf2 = #{client_id => <<"pbkdf2_password">>, username => <<"pbkdf2_password">>},
    BcryptFoo = #{client_id => <<"bcrypt_foo">>, username => <<"bcrypt_foo">>},
    User1 = #{client_id => <<"bcrypt_foo">>, username => <<"user">>},
    Bcrypt = #{client_id => <<"bcrypt">>, username => <<"bcrypt">>},
    BcryptWrong = #{client_id => <<"bcrypt_wrong">>, username => <<"bcrypt_wrong">>},
    reload([{password_hash, plain}]),
    {ok,#{is_superuser := true}} =
        emqx_access_control:authenticate(Plain#{password => <<"plain">>}),
    reload([{password_hash, md5}]),
    {ok,#{is_superuser := false}} =
        emqx_access_control:authenticate(Md5#{password => <<"md5">>}),
    reload([{password_hash, sha}]),
    {ok,#{is_superuser := false}} =
        emqx_access_control:authenticate(Sha#{password => <<"sha">>}),
    reload([{password_hash, sha256}]),
    {ok,#{is_superuser := false}} =
        emqx_access_control:authenticate(Sha256#{password => <<"sha256">>}),
    reload([{password_hash, bcrypt}]),
    {ok,#{is_superuser := false}} =
        emqx_access_control:authenticate(Bcrypt#{password => <<"password">>}),
    {error, not_authorized} =
        emqx_access_control:authenticate(BcryptWrong#{password => <<"password">>}),
    %%pbkdf2 sha
    reload([{password_hash, {pbkdf2, sha, 1, 16}},
            {auth_query, "select password, salt from mqtt_user where username = '%u' limit 1"}]),
    {ok,#{is_superuser := false}} =
        emqx_access_control:authenticate(Pbkdf2#{password => <<"password">>}),
    reload([{password_hash, {salt, bcrypt}}]),
    {ok,#{is_superuser := false}} =
        emqx_access_control:authenticate(BcryptFoo#{password => <<"foo">>}),
    {error, _} = emqx_access_control:authenticate(User1#{password => <<"foo">>}),
    {error, not_authorized} = emqx_access_control:authenticate(Bcrypt#{password => <<"password">>}).

comment_config(_) ->
    application:stop(?APP),
    [application:unset_env(?APP, Par) || Par <- [acl_query, auth_query]],
    application:start(?APP).

placeholders(_) ->
    ClientA = #{username => <<"plain">>, client_id => <<"plain">>},

    reload([{password_hash, plain},
            {auth_query, "select password from mqtt_user where username = '%u' and 'a_cn_val' = '%C' limit 1"}]),
    {error, not_authorized} =
        emqx_access_control:authenticate(ClientA#{password => <<"plain">>}),
    {error, not_authorized} =
        emqx_access_control:authenticate(ClientA#{password => <<"plain">>, cn => undefined}),
    {ok, _} =
        emqx_access_control:authenticate(ClientA#{password => <<"plain">>, cn => <<"a_cn_val">>}),

    reload([{auth_query, "select password from mqtt_user where username = '%u' and 'a_dn_val' = '%d' limit 1"}]),
    {error, not_authorized} =
        emqx_access_control:authenticate(ClientA#{password => <<"plain">>}),
    {error, not_authorized} =
        emqx_access_control:authenticate(ClientA#{password => <<"plain">>, dn => undefined}),
    {ok, _} =
        emqx_access_control:authenticate(ClientA#{password => <<"plain">>, dn => <<"a_dn_val">>}).

set_cmd(Key) ->
    emqx_cli_config:run(["config", "set", string:join(["auth.mysql", Key], "."), "--app=emqx_auth_mysql"]).

init_auth_() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    ok = mysql:query(Pid, ?DROP_AUTH_TABLE),
    ok = mysql:query(Pid, ?CREATE_AUTH_TABLE),
    ok = mysql:query(Pid, ?INIT_AUTH).

drop_table_(Tab) ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    ok = mysql:query(Pid, Tab).

reload(Config) when is_list(Config) ->
    ct:pal("~p: all configs before: ~p ", [?APP, application:get_all_env(?APP)]),
    ct:pal("~p: trying to reload config to: ~p ", [?APP, Config]),
    application:stop(?APP),
    [application:set_env(?APP, K, V) || {K, V} <- Config],
    ct:pal("~p: all configs after: ~p ", [?APP, application:get_all_env(?APP)]),
    application:start(?APP).

set_special_configs(emqx) ->
    application:set_env(emqx, allow_anonymous, false),
    application:set_env(emqx, enable_acl_cache, false),
    application:set_env(emqx, plugins_loaded_file,
                        emqx_ct_helpers:deps_path(emqx, "deps/emqx/test/emqx_SUITE_data/loaded_plugins"));
set_special_configs(_App) ->
    ok.

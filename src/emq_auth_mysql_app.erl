%%--------------------------------------------------------------------
%% Copyright (c) 2013-2018 EMQ Enterprise, Inc. (http://emqtt.io)
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
-module(emq_auth_mysql_app).

-behaviour(application).

-include("emq_auth_mysql.hrl").

-import(emq_auth_mysql_cli, [parse_query/1]).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).


%%--------------------------------------------------------------------
%% Application Callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    {ok, Sup} = emq_auth_mysql_sup:start_link(),
    if_enabled(auth_query, fun reg_authmod/1),
    if_enabled(acl_query,  fun reg_aclmod/1),
    emq_auth_mysql_config:register(),
    {ok, Sup}.

prep_stop(State) ->
    emqttd_access_control:unregister_mod(auth, emq_auth_mysql),
    emqttd_access_control:unregister_mod(acl, emq_acl_mysql),
    emq_auth_mysql_config:unregister(),
    State.

stop(_State) ->
    ok.

reg_authmod(AuthQuery) ->
    SuperQuery = parse_query(application:get_env(?APP, super_query, undefined)),
    {ok, HashType} = application:get_env(?APP, password_hash),
    AuthEnv = {AuthQuery, SuperQuery, HashType},
    emqttd_access_control:register_mod(auth, emq_auth_mysql, AuthEnv).

reg_aclmod(AclQuery) ->
    emqttd_access_control:register_mod(acl, emq_acl_mysql, AclQuery).

%%--------------------------------------------------------------------
%% Internal function
%%--------------------------------------------------------------------

if_enabled(Cfg, Fun) ->
    case application:get_env(?APP, Cfg) of
        {ok, Query} -> Fun(parse_query(Query));
        undefined   -> ok
    end.


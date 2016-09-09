%%--------------------------------------------------------------------
%% Copyright (c) 2016 Feng Lee <feng@emqtt.io>.
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

-module(emqttd_auth_mysql_app).

-behaviour(application).

-include("emqttd_auth_mysql.hrl").

-import(emqttd_auth_mysql_client, [parse_query/1]).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%%--------------------------------------------------------------------
%% Application Callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    gen_conf:init(?APP),
    {ok, Sup} = emqttd_auth_mysql_sup:start_link(),
    SuperQuery = parse_query(gen_conf:value(?APP, superquery, undefined)),
    ok = register_auth_mod(SuperQuery),
    ok = register_acl_mod(SuperQuery),
    {ok, Sup}.

register_auth_mod(SuperQuery) ->
    {ok, AuthQuery} = gen_conf:value(?APP, authquery),
    {ok, HashType}  = gen_conf:value(?APP, password_hash),
    AuthEnv = {SuperQuery, parse_query(AuthQuery), HashType},
    emqttd_access_control:register_mod(auth, emqttd_auth_mysql, AuthEnv).

register_acl_mod(SuperQuery) ->
    with_acl_enabled(fun(AclQuery) ->
        {ok, AclNomatch} = gen_conf:value(?APP, acl_nomatch),
        AclEnv = {SuperQuery, parse_query(AclQuery), AclNomatch},
        emqttd_access_control:register_mod(acl, emqttd_acl_mysql, AclEnv)
    end).

prep_stop(State) ->
    emqttd_access_control:unregister_mod(auth, emqttd_auth_mysql),
    with_acl_enabled(fun(_AclQuery) ->
        emqttd_access_control:unregister_mod(acl, emqttd_acl_mysql)
    end),
    State.

stop(_State) ->
    ok.

with_acl_enabled(Fun) ->
    case gen_conf:value(?APP, aclquery) of
        {ok, AclQuery} -> Fun(AclQuery);
        undefined      -> ok
    end.


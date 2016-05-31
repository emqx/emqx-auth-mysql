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

-module(emqttd_plugin_mysql_app).

-behaviour(application).

-import(emqttd_plugin_mysql, [parse_query/1]).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

-define(APP, emqttd_plugin_mysql).

%%--------------------------------------------------------------------
%% Application Callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqttd_plugin_mysql_sup:start_link(),
    SuperQuery = parse_query(application:get_env(?APP, superquery, undefined)),
    ok = register_auth_mod(SuperQuery), ok = register_acl_mod(SuperQuery),
    {ok, Sup}.

register_auth_mod(SuperQuery) ->
    {ok, AuthQuery} = application:get_env(?APP, authquery),
    {ok, HashType}  = application:get_env(?APP, password_hash),
    AuthEnv = {SuperQuery, parse_query(AuthQuery), HashType},
    emqttd_access_control:register_mod(auth, emqttd_auth_mysql, AuthEnv).

register_acl_mod(SuperQuery) ->
    with_acl_enabled(fun(AclQuery) ->
        {ok, AclNomatch} = application:get_env(?APP, acl_nomatch),
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
    case application:get_env(?APP, aclquery) of
        {ok, AclQuery} -> Fun(AclQuery);
        undefined      -> ok
    end.


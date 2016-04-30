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

%% @doc MySQL Authentication/ACL Application
-module(emqttd_auth_mysql_app).

-behaviour(application).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1]).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    {ok, AuthSql}  = application:get_env(?MODULE, authquery),
    {ok, HashType} = application:get_env(?MODULE, password_hash),
    ok = emqttd_access_control:register_mod(auth, emqttd_auth_mysql, {AuthSql, HashType}),
    with_acl_enabled(fun(AclSql) ->
        {ok, AclNomatch} = application:get_env(?MODULE, acl_nomatch),
        ok = emqttd_access_control:register_mod(acl, emqttd_acl_mysql, {AclSql, AclNomatch})
    end),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

prep_stop(State) ->
    emqttd_access_control:unregister_mod(auth, emqttd_auth_mysql),
    with_acl_enabled(fun(_AclSql) ->
        emqttd_access_control:unregister_mod(acl, emqttd_acl_mysql)
    end),
    State.

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% Supervisor callbacks(Dummy)
%%--------------------------------------------------------------------

init([]) ->

    {ok, Env} = application:get_env(emqttd_plugin_mysql, mysql_pool),

    Pool = ecpool:pool_spec(mysql_pool, mysql_pool, emqttd_mysql_pool, Env),

    {ok, { {one_for_all, 5, 100}, [Pool]} }.

with_acl_enabled(Fun) ->
    case application:get_env(?MODULE, aclquery) of
        {ok, AclSql} -> Fun(AclSql);
        undefined    -> ok
    end.


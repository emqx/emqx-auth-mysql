auth_mysql_app).

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
    if_enabled(authquery, fun(AuthQuery) ->
        SuperQuery = parse_query(gen_conf:value(?APP, superquery, undefined)),
        {ok, HashType}  = gen_conf:value(?APP, password_hash),
        AuthEnv = {parse_query(AuthQuery), SuperQuery, HashType},
        emqttd_access_control:register_mod(auth, emqttd_auth_mysql, AuthEnv)
    end),
    if_enabled(aclquery, fun(AclQuery) ->
        {ok, AclNomatch} = gen_conf:value(?APP, acl_nomatch),
        AclEnv = {parse_query(AclQuery), AclNomatch},
        emqttd_access_control:register_mod(acl, emqttd_acl_mysql, AclEnv)
    end),
    {ok, Sup}.

prep_stop(State) ->
    emqttd_access_control:unregister_mod(auth, emqttd_auth_mysql),
    emqttd_access_control:unregister_mod(acl, emqttd_acl_mysql),
    State.

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% Internal function
%%--------------------------------------------------------------------

if_enabled(Key, Fun) ->
    case gen_conf:value(?APP, Key) of
        {ok, Query} -> Fun(Query);
        undefined   -> ok
    end.


%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2012-2015 eMQTT.IO, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc
%%% emqttd MySQL Authentication/ACL Application.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(emqttd_plugin_mysql).

-author("Feng Lee <feng@emqtt.io>").

-behaviour(application).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1]).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

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

%%%=============================================================================
%%% Supervisor callbacks(Dummy)
%%%=============================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

with_acl_enabled(Fun) ->
    case application:get_env(?MODULE, aclquery) of
        {ok, AclSql} -> Fun(AclSql);
        undefined    -> ok
    end.


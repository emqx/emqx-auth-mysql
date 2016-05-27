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

%% @doc ACL with MySQL Database
-module(emqttd_acl_mysql).

-behaviour(emqttd_acl_mod).

-include("../../../include/emqttd.hrl").

%% ACL Callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

-record(state, {super_query, acl_query, acl_nomatch}).

init({SuperQuery, AclQuery, AclNomatch}) ->
    {ok, #state{super_query = SuperQuery, acl_query = AclQuery, acl_nomatch = AclNomatch}}.

check_acl({#mqtt_client{username = <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    {error, bad_username};

check_acl({Client, PubSub, Topic}, #state{super_query = SuperQuery,
                                          acl_query   = {AclSql, AclParams},
                                          acl_nomatch = Default}) ->

    case emqttd_plugin_mysql:is_superuser(SuperQuery, Client) of
        false -> case emqttd_plugin_mysql:query(AclSql, AclParams, Client) of
                    {ok, _Columns, []} ->
                        Default;
                    {ok, _Columns, Rows} ->
                        Rules = filter(PubSub, compile(Rows)),
                        case match(Client, Topic, Rules) of
                            {matched, allow} -> allow;
                            {matched, deny}  -> deny;
                            nomatch          -> Default
                        end
                end;
        true  ->
            allow
    end.

match(_Client, _Topic, []) ->
    nomatch;

match(Client, Topic, [Rule|Rules]) ->
    case emqttd_access_rule:match(Client, Topic, Rule) of
        nomatch -> match(Client, Topic, Rules);
        {matched, AllowDeny} -> {matched, AllowDeny}
    end.

filter(PubSub, Rules) ->
    [Term || Term = {_, _, Access, _} <- Rules, Access =:= PubSub orelse Access =:= pubsub].

compile(Rows) ->
    compile(Rows, []).
compile([], Acc) ->
    Acc;
compile([[Allow, IpAddr, Username, ClientId, Access, Topic]|T], Acc) ->
    Who  = who(IpAddr, Username, ClientId),
    Term = {allow(Allow), Who, access(Access), [topic(Topic)]},
    compile(T, [emqttd_access_rule:compile(Term) | Acc]).

who(_, <<"$all">>, _) ->
    all;
who(null, null, null) ->
    throw(undefined_who);
who(CIDR, Username, ClientId) ->
    Cols = [{ipaddr, b2l(CIDR)}, {user, Username}, {client, ClientId}],
    case [{C, V} || {C, V} <- Cols, V =/= null] of
        [Who] -> Who;
        Conds -> {'and', Conds}
    end.

allow(1)  -> allow;
allow(0)  -> deny;
allow(<<"1">>)  -> allow;
allow(<<"0">>)  -> deny.

access(1) -> subscribe;
access(2) -> publish;
access(3) -> pubsub;
access(<<"1">>) -> subscribe;
access(<<"2">>) -> publish;
access(<<"3">>) -> pubsub.

topic(<<"eq ", Topic/binary>>) ->
    {eq, Topic};
topic(Topic) ->
    Topic.

reload_acl(_State) ->
    ok.

description() ->
    "ACL with Mysql".

b2l(null) -> null;
b2l(B)    -> binary_to_list(B).


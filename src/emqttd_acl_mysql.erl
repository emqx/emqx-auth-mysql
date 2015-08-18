%%%-----------------------------------------------------------------------------
%%% @Copyright (C) 2012-2015, Feng Lee <feng@emqtt.io>
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
%%% ACL with MySQL Database.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(emqttd_acl_mysql).

-include_lib("emqttd/include/emqttd.hrl").

-behaviour(emqttd_acl_mod).

%% ACL callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

-record(state, {acl_sql, acl_nomatch}).

init({AclSql, AclNomatch}) ->
    {ok, #state{acl_sql = AclSql, acl_nomatch = AclNomatch}}.

check_acl({#mqtt_client{username = <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    {error, bad_username};

check_acl({Client = #mqtt_client{client_id = ClientId,
                                 username = Username,
                                 peername = {IpAddr, _}},
           PubSub, Topic}, #state{acl_sql = AclSql0}) ->

    Vars = [{"%u", Username}, {"%c", ClientId}, {"%a", inet_parse:ntoa(IpAddr)}],
    AclSql = lists:foldl(fun({Var, Val}, Sql) -> replvar(Sql, Var, Val) end, AclSql0, Vars),
    case emysql:select(AclSql) of
        {ok, []} ->
            allow;
        {ok, Rules} ->
            check_rule(Client, compile(Rules))
    end.

replvar(Sql, Var, Val) ->
    re:replace(Sql, Var, Val, [global, {return, list}]).

compile(Rules) ->
    compile(Rules, []).

compile([], Acc) ->
    Acc;
compile([Rule|T], Acc) ->
    Who  = who(g(ipaddr, Rule), g(username, Rule), g(clientid, Rule)),
    Term = {allow(g(allow, Rule)), Who, access(g(access, Rule)), topic(g(topic, Rule))},
    compile(T, [emqttd_access_rule:compile(Term) | Acc]).

who(_, <<"$all">>, _) ->
    all;
who(CIDR, undefined, undefined) ->
    {ipaddr, CIDR};
who(undefined, Username, undefined) ->
    {user, Username};
who(undefined, undefined, ClientId) ->
    {client, ClientId}.

allow(1)  -> allow;
allow(0)  -> deny.

access(1) -> subscribe;
access(2) -> publish;
access(3) -> pubsub.

topic(<<"eq ", Topic/binary>>) ->
    {eq, Topic};
topic(Topic) ->
    Topic.

reload_acl(_State) ->
    ok.

description() ->
    "ACL Module by Mysql".

g(K, L) ->
    proplists:get_value(K, L).


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
%%% @doc ACL with MySQL Database
%%%
%%% @author Feng Lee <feng@emqtt.io>
%%%-----------------------------------------------------------------------------
-module(emqttd_acl_mysql).

-behaviour(emqttd_acl_mod).

-include("../../../include/emqttd.hrl").

%% ACL callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

-record(state, {acl_sql, acl_nomatch}).

init({AclSql, AclNomatch}) ->
    {ok, #state{acl_sql = AclSql, acl_nomatch = AclNomatch}}.

check_acl({#mqtt_client{username = <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    {error, bad_username};

check_acl({Client, PubSub, Topic}, #state{acl_sql = AclSql,
                                          acl_nomatch = Default}) ->

    case emqttd_mysql_client:query(feed_var(Client, AclSql)) of
        {ok, _Columns, []} ->
            Default;
        {ok, _Columns, Rows} ->
            Rules = filter(PubSub, compile(Rows)),
            case match(Client, Topic, Rules) of
                {matched, allow} -> allow;
                {matched, deny}  -> deny;
                nomatch          -> Default
            end
    end.

feed_var(#mqtt_client{client_id = ClientId,
                      username  = Username,
                      peername  = {IpAddr, _}}, AclSql) ->
    Vars = [{"%u", Username}, {"%c", ClientId}, {"%a", inet_parse:ntoa(IpAddr)}],
    lists:foldl(fun({Var, Val}, Sql) -> feed_var(Sql, Var, Val) end, AclSql, Vars).

feed_var(Sql, Var, Val) ->
    re:replace(Sql, Var, Val, [global, {return, list}]).

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
    "ACL Module by Mysql".

b2l(null) -> null;
b2l(B)    -> binary_to_list(B).


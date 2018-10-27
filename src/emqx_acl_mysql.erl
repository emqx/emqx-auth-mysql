%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_acl_mysql).

-behaviour(emqx_acl_mod).

-include_lib("emqx/include/emqx.hrl").

%% ACL Callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

-record(state, {acl_query}).

init(AclQuery) ->
    {ok, #state{acl_query = AclQuery}}.

check_acl({#{username := <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    ignore;

check_acl({Credentials, PubSub, Topic}, #state{acl_query = {AclSql, AclParams}}) ->
    case emqx_auth_mysql_cli:query(AclSql, AclParams, Credentials) of
        {ok, _Columns, []} -> ignore;
        {ok, _Columns, Rows} ->
            Rules = filter(PubSub, compile(Rows)),
            case match(Credentials, Topic, Rules) of
                {matched, allow} -> allow;
                {matched, deny}  -> deny;
                nomatch          -> ignore
            end;
        {error, Reason} ->
            logger:error("Mysql check_acl error: ~p~n", [Reason]),
            ignore
    end.

match(_Credentials, _Topic, []) ->
    nomatch;

match(Credentials, Topic, [Rule|Rules]) ->
    case emqx_access_rule:match(Credentials, Topic, Rule) of
        nomatch ->
            match(Credentials, Topic, Rules);
        {matched, AllowDeny} ->
            {matched, AllowDeny}
    end.

filter(PubSub, Rules) ->
    [Term || Term = {_, _, Access, _} <- Rules,
             Access =:= PubSub orelse Access =:= pubsub].

compile(Rows) ->
    compile(Rows, []).
compile([], Acc) ->
    Acc;
compile([[Allow, IpAddr, Username, ClientId, Access, Topic]|T], Acc) ->
    Who  = who(IpAddr, Username, ClientId),
    Term = {allow(Allow), Who, access(Access), [topic(Topic)]},
    compile(T, [emqx_access_rule:compile(Term) | Acc]).

who(_, <<"$all">>, _) ->
    all;
who(null, null, null) ->
    throw(undefined_who);
who(CIDR, Username, ClientId) ->
    Cols = [{ipaddr, b2l(CIDR)}, {user, Username}, {client, ClientId}],
    case [{C, V} || {C, V} <- Cols, not empty(V)] of
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

empty(null) -> true;
empty("")   -> true;
empty(<<>>) -> true;
empty(_)    -> false.

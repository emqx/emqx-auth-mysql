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

%% @doc Authentication/ACL with MySQL Database.
-module(emqttd_plugin_mysql).

-behaviour(ecpool_worker).

-include("../../../include/emqttd.hrl").

-export([is_superuser/2, parse_query/1, connect/1, query/3]).

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | {string(), list()}, mqtt_client()) -> boolean()).
is_superuser(undefined, _Client) ->
    false;
is_superuser({SuperSql, Params}, Client) ->
    case query(SuperSql, Params, Client) of
        {ok, [_Super], [[1]]} ->
            true;
        {ok, [_Super], [[_False]]} ->
            false;
        {ok, [_Super], []} ->
            false;
        {error, _Error} ->
            false
    end.

%%--------------------------------------------------------------------
%% Avoid SQL Injection: Parse SQL to Parameter Query.
%%--------------------------------------------------------------------

parse_query(undefined) ->
    undefined;
parse_query(Sql) ->
    case re:run(Sql, "'%[uca]'", [global, {capture, all, list}]) of
        {match, Variables} ->
            Params = [Var || [Var] <- Variables],
            {re:replace(Sql, "'%[uca]'", "?", [global, {return, list}]), Params};
        nomatch ->
            {Sql, []}
    end.

%%--------------------------------------------------------------------
%% MySQL Connect/Query
%%--------------------------------------------------------------------

connect(Options) ->
    mysql:start_link(Options).

query(Sql, Params, Client) ->
    ecpool:with_client(?MODULE, fun(C) -> mysql:query(C, Sql, replvar(Params, Client)) end).

replvar(Params, Client) ->
    replvar(Params, Client, []).

replvar([], Client, Acc) ->
    lists:reverse(Acc);
replvar(["'%u'" | Params], Client = #mqtt_client{username = Username}, Acc) ->
    replvar(Params, Client, [Username | Acc]);
replvar(["'%c'" | Params], Client = #mqtt_client{client_id = ClientId}, Acc) ->
    replvar(Params, Client, [ClientId | Acc]);
replvar(["'%a'" | Params], Client = #mqtt_client{peername = {IpAddr, _}}, Acc) ->
    replvar(Params, Client, [inet_parse:ntoa(IpAddr) | Acc]);
replvar([Param | Params], Client, Acc) ->
    replvar(Params, Client, [Param | Acc]).


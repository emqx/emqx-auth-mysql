%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_auth_mysql_cli).

-behaviour(ecpool_worker).

-include("emqx_auth_mysql.hrl").
-include_lib("emqx/include/emqx.hrl").

-export([parse_query/1]).
-export([connect/1]).
-export([query/3]).

%%--------------------------------------------------------------------
%% Avoid SQL Injection: Parse SQL to Parameter Query.
%%--------------------------------------------------------------------

parse_query(undefined) ->
    undefined;
parse_query(Sql) ->
    case re:run(Sql, "'%[ucCad]'", [global, {capture, all, list}]) of
        {match, Variables} ->
            Params = [Var || [Var] <- Variables],
            {re:replace(Sql, "'%[ucCad]'", "?", [global, {return, list}]), Params};
        nomatch ->
            {Sql, []}
    end.

%%--------------------------------------------------------------------
%% MySQL Connect/Query
%%--------------------------------------------------------------------

connect(Options) ->
    mysql:start_link(Options).

query(Sql, Params, Credentials) ->
    ecpool:with_client(?APP, fun(C) -> mysql:query(C, Sql, replvar(Params, Credentials)) end).

replvar(Params, Credentials) ->
    replvar(Params, Credentials, []).

replvar([], _Credentials, Acc) ->
    lists:reverse(Acc);
replvar(["'%u'" | Params], Credentials, Acc) ->
    replvar(Params, Credentials, [safe_get(username, Credentials) | Acc]);
replvar(["'%c'" | Params], Credentials, Acc) ->
    replvar(Params, Credentials, [safe_get(client_id, Credentials) | Acc]);
replvar(["'%a'" | Params], Credentials, Acc) ->
    IpAddr = case Credentials of
                 #{peername := {Ip, _}} -> inet_parse:ntoa(Ip);
                 _ -> <<"undefined">>
             end,
    replvar(Params, Credentials, [IpAddr | Acc]);
replvar(["'%C'" | Params], Credentials, Acc) ->
    replvar(Params, Credentials, [safe_get(cn, Credentials)| Acc]);
replvar(["'%d'" | Params], Credentials, Acc) ->
    replvar(Params, Credentials, [safe_get(dn, Credentials)| Acc]);
replvar([Param | Params], Credentials, Acc) ->
    replvar(Params, Credentials, [Param | Acc]).

safe_get(K, Credentials) ->
    bin(maps:get(K, Credentials, undefined)).

bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(B) when is_binary(B) -> B;
bin(X) -> X.


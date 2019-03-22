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

-module (emqx_auth_mysql_cfg).

-include("emqx_auth_mysql.hrl").

-export ([ register/0
         , unregister/0
         ]).

register() ->
    clique_config:load_schema([code:priv_dir(?APP)], ?APP),
    register_formatter(),
    register_config().

unregister() ->
    unregister_formatter(),
    unregister_config(),
    clique_config:unload_schema(?APP).

register_formatter() ->
    [clique:register_formatter(cuttlefish_variable:tokenize(Key),
     fun formatter_callback/2) || Key <- keys()].

formatter_callback([_, _, "server"], Params) ->
    lists:concat([proplists:get_value(host, Params), ":", proplists:get_value(port, Params)]);
formatter_callback([_, _, "pool"], Params) ->
    proplists:get_value(pool_size, Params);
formatter_callback([_, _, "password_hash"], Params) when is_atom(Params) ->
    Params;
formatter_callback([_, _, "password_hash"], Params) when is_tuple(Params) ->
    format(tuple_to_list(Params));
formatter_callback([_, _, Key], Params) ->
    proplists:get_value(list_to_atom(Key), Params).

unregister_formatter() ->
    [clique:unregister_formatter(cuttlefish_variable:tokenize(Key)) || Key <- keys()].

register_config() ->
    Keys = keys(),
    [clique:register_config(Key , fun config_callback/2) || Key <- Keys],
    clique:register_config_whitelist(Keys, ?APP).

config_callback([_, _, "server"], Value0) ->
    {Host, Port} = parse_servers(Value0),
    {ok, Env} = application:get_env(?APP, server),
    Env1 = lists:keyreplace(host, 1, Env, {host, Host}),
    Env2 = lists:keyreplace(port, 1, Env1, {port, Port}),
    application:set_env(?APP, server, Env2),
    " successfully\n";
config_callback([_, _, "pool"], Value) ->
    {ok, Env} = application:get_env(?APP, server),
    application:set_env(?APP, server, lists:keyreplace(pool_size, 1, Env, {pool_size, Value})),
    " successfully\n";
config_callback([_, _, "username"], Value) ->
    {ok, Env} = application:get_env(?APP, server),
    application:set_env(?APP, server, lists:keyreplace(user, 1, Env, {user, Value})),
    " successfully\n";
config_callback([_, _, "password_hash"], Value0) ->
    Value = parse_password_hash(Value0),
    application:set_env(?APP, password_hash, Value),
    " successfully\n";
config_callback([_, _, Key0], Value) ->
    Key = list_to_atom(Key0),
    {ok, Env} = application:get_env(?APP, server),
    application:set_env(?APP, server, lists:keyreplace(Key, 1, Env, {Key, Value})),
    " successfully\n".

unregister_config() ->
    Keys = keys(),
    [clique:unregister_config(Key) || Key <- Keys],
    clique:unregister_config_whitelist(Keys, ?APP).

keys() ->
    ["auth.mysql.server",
     "auth.mysql.pool",
     "auth.mysql.username",
     "auth.mysql.password",
     "auth.mysql.database",
     "auth.mysql.password_hash"].

format(Value) ->
    format(Value, "").
format([Head], Acc) ->
    lists:concat([Acc, Head]);
format([Head | Tail], Acc) ->
    format(Tail, Acc ++ lists:concat([Head, ","])).

parse_password_hash(Value) ->
    case string:tokens(Value, ",") of
          [Hash]           -> list_to_atom(Hash);
          [Prefix, Suffix] -> {list_to_atom(Prefix), list_to_atom(Suffix)};
          [Hash, MacFun, Iterations, Dklen] -> {list_to_atom(Hash),
                                                list_to_atom(MacFun),
                                                list_to_integer(Iterations),
                                                list_to_integer(Dklen)};
          _                -> plain
    end.

parse_servers(Value) ->
    case string:tokens(Value, ":") of
        [Domain]       -> {Domain, 3306};
        [Domain, Port] -> {Domain, list_to_integer(Port)}
    end.


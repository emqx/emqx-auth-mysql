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

-module(emqx_auth_mysql).

-include_lib("emqx/include/emqx.hrl").

-export([  check/2
         , description/0
         ]).

-define(EMPTY(Username), (Username =:= undefined orelse Username =:= <<>>)).

check(Credentials = #{username := Username, password := Password}, _State) 
  when ?EMPTY(Username); ?EMPTY(Password) ->
    {ok, Credentials#{auth_result => bad_username_or_password}};

check(Credentials = #{password := Password}, #{auth_query  := {AuthSql, AuthParams},
                                               super_query := SuperQuery,
                                               hash_type   := HashType}) ->
    CheckPass = case emqx_auth_mysql_cli:query(AuthSql, AuthParams, Credentials) of
                    {ok, [<<"password">>], [[PassHash]]} ->
                        check_pass({PassHash, Password}, HashType);
                    {ok, [<<"password">>, <<"salt">>], [[PassHash, Salt]]} ->
                        check_pass({PassHash, Salt, Password}, HashType);
                    {ok, _Columns, []} ->
                        {error, not_found};
                    {error, Reason} ->
                        logger:error("Mysql query '~p' failed: ~p", [AuthSql, Reason]),
                        {error, not_found}
                end,
    case CheckPass of
        ok -> {stop, Credentials#{is_superuser => is_superuser(SuperQuery, Credentials),
                                  auth_result => success}};
        {error, not_found} -> ok;
        {error, ResultCode} ->
            logger:error("Auth from mysql failed: ~p", [ResultCode]),
            {stop, Credentials#{auth_result => ResultCode}}
    end;
check(Credentials, Config) ->
    ResultCode = insufficient_credentials,
    logger:error("Auth from mysql failed: ~p, Configs: ~p", [ResultCode, Config]),
    {ok, Credentials#{auth_result => ResultCode}}.

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | {string(), list()}, emqx_types:credentials()) -> boolean()).
is_superuser(undefined, _Credentials) -> false;
is_superuser({SuperSql, Params}, Credentials) ->
    case emqx_auth_mysql_cli:query(SuperSql, Params, Credentials) of
        {ok, [_Super], [[1]]} ->
            true;
        {ok, [_Super], [[_False]]} ->
            false;
        {ok, [_Super], []} ->
            false;
        {error, _Error} ->
            false
    end.

check_pass(Password, HashType) ->
    case emqx_passwd:check_pass(Password, HashType) of
        ok -> ok;
        {error, _Reason} -> {error, not_authorized}
    end.

description() -> "Authentication with MySQL".


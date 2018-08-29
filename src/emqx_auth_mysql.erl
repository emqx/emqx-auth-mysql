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

-module(emqx_auth_mysql).

-behaviour(emqx_auth_mod).

-include_lib("emqx/include/emqx.hrl").

-export([init/1, check/3, description/0]).

-record(state, {auth_query, super_query, hash_type}).

-define(EMPTY(Username), (Username =:= undefined orelse Username =:= <<>>)).

init({AuthQuery, SuperQuery, HashType}) ->
    {ok, #state{auth_query = AuthQuery, super_query = SuperQuery, hash_type = HashType}}.

check(#{username := Username}, Password, _State) when ?EMPTY(Username); ?EMPTY(Password) ->
    {error, username_or_password_undefined};

check(Credentials, Password, #state{auth_query  = {AuthSql, AuthParams},
                                    super_query = SuperQuery,
                                    hash_type   = HashType}) ->
    Result = case emqx_auth_mysql_cli:query(AuthSql, AuthParams, Credentials) of
                 {ok, [<<"password">>], [[PassHash]]} ->
                     check_pass(PassHash, Password, HashType);
                 {ok, [<<"password">>, <<"salt">>], [[PassHash, Salt]]} ->
                     check_pass(PassHash, Salt, Password, HashType);
                 {ok, _Columns, []} ->
                     ignore;
                 {error, Reason} ->
                     {error, Reason}
             end,
    case Result of ok -> {ok, is_superuser(SuperQuery, Credentials)}; Error -> Error end.

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | {string(), list()}, emqx_types:credentials()) -> boolean()).
is_superuser(undefined, _Credentials) ->
    false;
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

check_pass(PassHash, Password, HashType) ->
    check_pass(PassHash, emqx_passwd:hash(HashType, Password)).
check_pass(PassHash, Salt, Password, {pbkdf2, Macfun, Iterations, Dklen}) ->
    check_pass(PassHash, emqx_passwd:hash(pbkdf2, {Salt, Password, Macfun, Iterations, Dklen}));
check_pass(PassHash, Salt, Password, {salt, bcrypt}) ->
    check_pass(PassHash, emqx_passwd:hash(bcrypt, {Salt, Password}));
check_pass(PassHash, Salt, Password, {salt, HashType}) ->
    check_pass(PassHash, emqx_passwd:hash(HashType, <<Salt/binary, Password/binary>>));
check_pass(PassHash, Salt, Password, {HashType, salt}) ->
    check_pass(PassHash, emqx_passwd:hash(HashType, <<Password/binary, Salt/binary>>)).

check_pass(PassHash, PassHash) -> ok;
check_pass(_Hash1, _Hash2)     -> {error, password_error}.

description() ->
    "Authentication with MySQL".


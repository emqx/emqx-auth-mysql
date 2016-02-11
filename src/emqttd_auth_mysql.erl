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

%% @doc Authentication with MySQL Database.
-module(emqttd_auth_mysql).

-behaviour(emqttd_auth_mod).

-include("../../../include/emqttd.hrl").

-export([init/1, check/3, description/0]).

-record(state, {auth_sql, hash_type}).

-define(EMPTY(Username), (Username =:= undefined orelse Username =:= <<>>)).

init({AuthSql, HashType}) -> 
    {ok, #state{auth_sql = AuthSql, hash_type = HashType}}.

check(#mqtt_client{username = Username}, Password, _State)
    when ?EMPTY(Username) orelse ?EMPTY(Password) ->
    {error, undefined};

check(#mqtt_client{username = Username}, Password,
        #state{auth_sql = AuthSql, hash_type = HashType}) ->
    case emqttd_mysql_pool:query(replvar(AuthSql, Username)) of
        {ok, [<<"password">>], [[PassHash]]} ->
            check_pass(PassHash, Password, HashType);
        {ok, [<<"password">>, <<"salt">>], [[PassHash, Salt]]} ->
            check_pass(PassHash, Salt, Password, HashType);
        {ok, []} ->
            {error, notfound};
        {error, Error} ->
            {error, Error}
    end.

replvar(AuthSql, Username) ->
    re:replace(AuthSql, "%u", Username, [global, {return, list}]).

check_pass(PassHash, Password, HashType) ->
    check_pass(PassHash, hash(HashType, Password)).
check_pass(PassHash, Salt, Password, {salt, HashType}) ->
    check_pass(PassHash, hash(HashType, <<Salt/binary, Password/binary>>));
check_pass(PassHash, Salt, Password, {HashType, salt}) ->
    check_pass(PassHash, hash(HashType, <<Password/binary, Salt/binary>>)).

check_pass(PassHash, PassHash) -> ok;
check_pass(_, _)               -> {error, password_error}.

description() -> "Authentication by MySQL".

hash(Type, Password) -> emqttd_auth_mod:passwd_hash(Type, Password).


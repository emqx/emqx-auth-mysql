%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2012-2016 eMQTT.IO, All Rights Reserved.
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
%%% @doc Authentication with MySQL Database.
%%%
%%% @author Feng Lee <feng@emqtt.io>
%%%-----------------------------------------------------------------------------
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


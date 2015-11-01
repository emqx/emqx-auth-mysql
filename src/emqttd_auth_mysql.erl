%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2012-2015 eMQTT.IO, All Rights Reserved.
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
%%% Authentication with MySQL Database.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(emqttd_auth_mysql).

-author("Feng Lee <feng@emqtt.io>").

-include("../../../include/emqttd.hrl").

-behaviour(emqttd_auth_mod).

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
    case emysql:sqlquery(replvar(AuthSql, Username)) of
        {ok, [Record]} ->
            check_pass(lists:sort(Record), Password, HashType);
        {ok, []} ->
            {error, notfound}
    end.

replvar(AuthSql, Username) ->
    re:replace(AuthSql, "%u", Username, [global, {return, list}]).

check_pass([{password, PassHash}], Password, HashType) ->
    case PassHash =:= hash(HashType, Password) of
        true  -> ok;
        false -> {error, password_error}
    end;
check_pass([{password, PassHash}, {salt, Salt}], Password, {salt, HashType}) ->
    case PassHash =:= hash(HashType, <<Salt/binary, Password/binary>>) of
        true  -> ok;
        false -> {error, password_error}
    end;
check_pass([{password, PassHash}, {salt, Salt}], Password, {HashType, salt}) ->
    case PassHash =:= hash(HashType, <<Password/binary, Salt/binary>>) of
        true  -> ok;
        false -> {error, password_error}
    end.

description() -> "Authentication by MySQL".

hash(plain,  Password)  ->
    Password;
hash(md5,    Password)  ->
    hexstring(crypto:hash(md5, Password));
hash(sha,    Password)  ->
    hexstring(crypto:hash(sha, Password));
hash(sha256, Password)  ->
    hexstring(crypto:hash(sha256, Password)).

hexstring(<<X:128/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~64.16.0b", [X])).


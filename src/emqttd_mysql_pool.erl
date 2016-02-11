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

%% @doc emqttd mysql connection pool client
-module(emqttd_mysql_pool).

-behaviour(ecpool_worker).

-export([connect/1, client/0, query/1, query/2, query/3]).

-define(POOL, mysql_pool).

connect(Options) ->
    mysql:start_link(Options).

client() ->
    ecpool:get_client(?POOL).

query(Query) ->
    ecpool:with_client(?POOL, fun(C) -> mysql:query(C, Query) end).

query(Query, Params) ->
    ecpool:with_client(?POOL, fun(C) -> mysql:query(C, Query, Params) end).

query(Query, Params, Timeout) ->
    ecpool:with_client(?POOL, fun(C) -> mysql:query(C, Query, Params, Timeout) end).


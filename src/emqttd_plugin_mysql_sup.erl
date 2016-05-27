%%--------------------------------------------------------------------
%% Copyright (c) 2016 Feng Lee <feng@emqtt.io>.
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

-module(emqttd_plugin_mysql_sup).

-behaviour(supervisor).

-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(APP, emqttd_plugin_mysql).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

init([]) ->

    {ok, Env} = application:get_env(?APP, mysql_pool),

    PoolSpec = ecpool:pool_spec(?APP, ?APP, ?APP, Env),

    {ok, { {one_for_all, 10, 100}, [PoolSpec]} }.


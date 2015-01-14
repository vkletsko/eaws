-module(eaws_app).

-behaviour(application).
-behaviour(supervisor).

%% app 
-export([start/2, stop/1]).

%% supervisor.
-export([start_link/0]).
-export([init/1]).

start(_Type, _Args) -> eaws_app:start_link().
stop(_Client) -> ok.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([])  ->
    {ok, {{one_for_one, 250, 50}, []}}.

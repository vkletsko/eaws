-module(eaws_app).

-behaviour(application).
-behaviour(supervisor).

%% app 
-export([start/2, stop/1]).

%% supervisor.
-export([start_link/0]).
-export([init/1]).

-spec start(atom(), [any()]) -> {ok, pid()}.
start(_Type, _Args) ->
  eaws_app:start_link().

-spec stop(any()) -> ok.
stop(_Client) -> ok.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec(init([Args :: term()]) ->
  ignore | {ok,
    { {'one_for_all',non_neg_integer(),pos_integer()}
    | {'one_for_one',non_neg_integer(),pos_integer()}
    | {'rest_for_one',non_neg_integer(),pos_integer()}
    | {'simple_one_for_one',non_neg_integer(),pos_integer()}
    | #{},
      [{_, {atom() | tuple(), atom(), 'undefined' |
      [any()]}, 'permanent' | 'temporary' | 'transient','brutal_kill' | 'infinity' | non_neg_integer(),
        'supervisor' | 'worker','dynamic' | [atom() | tuple()]} | #{}]}}).
init([])  ->
    {ok, {{one_for_one, 250, 50}, []}}.

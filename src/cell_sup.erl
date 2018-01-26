%%%-------------------------------------------------------------------
%% @doc Cell supervisor
%% @end
%%%-------------------------------------------------------------------

-module(cell_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0,
  children/0,
  raiseTheDead/1,
  kill/1,
  killAll/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

children() ->
  [Pid || {_, Pid, _, _} <- supervisor:which_children(cell_sup)].

raiseTheDead(Positions) ->
  [zombie_registry:insert(Pos, raiseOne(Pos)) || Pos <- Positions], ok.

raiseOne(Position) ->
  {ok, Pid} = supervisor:start_child(cell_sup, [Position]),
  Pid.

killAll() ->
  [kill(X) || X <- children()],
  zombie_registry:reset().

kill(Pid) ->
  supervisor:terminate_child(cell_sup, Pid).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
  io:format("Cell Supervisor started ~n"),
  SupFlags = #{strategy => simple_one_for_one, intensity => 5, period => 5},

  Cell =
    #{id => cell,
      start => {cell, start_link, []},
      restart => transient,
      shutdown => 2000,
      type => worker,
      modules => [cell]
    },

  ChildSpecs = [Cell],

  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================




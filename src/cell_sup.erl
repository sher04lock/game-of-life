%%%-------------------------------------------------------------------
%% @doc Lab12 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cell_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0,
  children/0,
  raiseOne/1,
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

%%which_children(SupRef) -> [{Id, Child, Type, Modules}]
children() ->
  lists:map(
    fun({_, Pid, _, _}) -> Pid end,
    supervisor:which_children(cell_sup)).

raiseOne(Position) ->
  {ok, Pid} = supervisor:start_child(cell_sup, [Position]),
  Pid.

raiseTheDead(Positions) ->
  lists:map(fun(Pos) ->
    register:insert(Pos,supervisor:start_child(cell_sup, [Pos])) end, Positions).

kill(Pid) ->
  supervisor:terminate_child(cell_sup, Pid).

killAll() ->
  Pids = lists:map(fun({_, Pid, _, _}) -> Pid end, supervisor:which_children(cell_sup)),
  [supervisor:terminate_child(cell_sup, X) || X <- Pids],
  register:reset().


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  io:format("Cell Supervisor started ~n"),
  SupFlags = #{strategy => simple_one_for_one, intensity => 5, period => 5},

  Cell =
    #{id => cell,
      start => {cell, start_link, []},
      restart => transient,   % optional
      shutdown => 2000, % optional
      type => worker,       % optional
      modules => [cell]   % optional
    },

  ChildSpecs = [Cell],

  {ok, {SupFlags, ChildSpecs}}.
% {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================




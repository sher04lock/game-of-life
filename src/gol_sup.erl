%%%-------------------------------------------------------------------
%% @doc GOL top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(gol_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================


init([]) ->
  SupFlags = #{strategy => one_for_all, intensity => 1, period => 5},

  CellSupervisor =
    #{id => cell_sup,
      start => {cell_sup, start_link, []},
      restart => transient,
      shutdown => 2000,
      type => supervisor,
      modules => [cell_sup]
    },

  ZombieRegistry =
    #{id => zombie_registry,
      start => {zombie_registry, start, []},
      restart => transient,
      shutdown => 2000,
      type => worker,
      modules => [zombie_registry]
    },

  Universe =
    #{id => universe,
      start => {universe, start, []},
      restart => transient,
      shutdown => 2000,
      type => worker,
      modules => [universe]
    },

  ChildSpecs = [ZombieRegistry, Universe, CellSupervisor],

  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================




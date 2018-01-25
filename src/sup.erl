%%%-------------------------------------------------------------------
%% @doc GOL top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sup).

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

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

  CellSupervisor =
    #{id => cell_sup,
      start => {cell_sup, start_link, []},
      restart => permanent,   % optional
      shutdown => 2000, % optional
      type => supervisor,       % optional
      modules => [cell_sup]   % optional
    },

  Register =
    #{id => register,
      start => {register, start, []},
      restart => permanent,   % optional
      shutdown => 2000, % optional
      type => worker,       % optional
      modules => [register]   % optional
    },

  Universe =
    #{id => universe,
      start => {universe, start, []},
      restart => permanent,   % optional
      shutdown => 2000, % optional
      type => worker,       % optional
      modules => [universe]   % optional
    },

  ChildSpecs = [Register, Universe, CellSupervisor],

  {ok, {SupFlags, ChildSpecs}}.
% {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================




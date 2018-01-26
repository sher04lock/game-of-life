-module(cell).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

-export([start_link/1, tick/0]).

-define(OFFSETS,
  [
    {-1, -1}, {0, -1}, {1, -1},
    {-1, 0}, {1, 0},
    {-1, 1}, {0, 1}, {1, 1}
  ]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Position) ->
  gen_server:start_link(?MODULE, Position, []).

tick() -> gen_server:call(?MODULE, tick).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Position) ->
  io:format("Cell born ~p at ~p ~n", [self(), Position]),
  zombie_registry:insert(Position, self()),
  {ok, Position}.


handle_call(tick, _, Position) ->

  Neighbours = get_neighbours(Position),
  AliveNeighbours = filter_alive(Neighbours),

  % raise neighbouring cells
  ZombiesToRiseFromDead = lists:filter(
    fun(X) -> length(filter_alive(get_neighbours(X))) == 3 end,
    Neighbours),

  % choose own faith
  MyFaith = case length(AliveNeighbours) of
              2 -> [];
              3 -> [];
              _ -> [Position]
            end,

  {reply, {ZombiesToRiseFromDead, MyFaith}, Position}.


%%====================================================================
%% Internal functions
%%====================================================================

get_neighbours({X, Y}) ->
  lists:map(fun({Dx, Dy}) -> {Dx + X, Dy + Y} end, ?OFFSETS).

filter_alive(Positions) ->
  lists:filter(fun(X) -> zombie_registry:find(X) end, Positions).


handle_cast(_Message, State) ->
  io:format("What should I do with that? ~p ~n", [_Message]),
  {noreply, State}.


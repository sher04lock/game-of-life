-module(cell).
-behaviour(gen_server).

-compile(export_all).

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
  io:format("~p:~p Cell has started ~p ~p ~n", [?MODULE, ?LINE, self(), Position]),
  register:insert(Position, self()),
  {ok, Position}.


handle_call(tick, From, Position) ->

  Neighbours = get_neighbours(Position),
  AliveNeighbours = filter_alive(Neighbours),
  % zbierz zombie do ozywienia

  ZombiesToRiseFromDead = lists:filter(fun(X) -> length(filter_alive(get_neighbours(X))) == 3 end, Neighbours),

  % zadecyduj o sobie
  MyFaith = case length(AliveNeighbours) of
            2 -> [];
            3 -> [];
            _ -> [Position]
          end,

  {reply, {ZombiesToRiseFromDead, MyFaith}, Position}.

get_neighbours({X, Y}) ->
  lists:map(fun({Dx, Dy}) -> {Dx + X, Dy + Y} end, ?OFFSETS).

filter_alive(Positions) ->
  lists:filter(fun(X) -> register:find(X) end, Positions).

terminate(Reason, State) ->
  io:format("~p:~p ~p has terminated by reason ~p ~p ~n", [?MODULE, ?LINE, self(), Reason, State]),
  ok.

code_change(Vsn, State, Ext) ->
  io:format("~p:~p code_change ~p ~p ~p ~n", [?MODULE, ?LINE, Vsn, State, Ext]), {ok, State}.


handle_cast(_Message, State) ->
  io:format("What should I do with that? ~p ~n", [_Message]),
  {noreply, State}.

handle_info(Info, State) ->
  io:format("~p:~p handle_info ~p ~p  ~n", [?MODULE, ?LINE, Info, State]), {noreply, State}.

format_status(normal, [PDict, State]) -> {normal, [PDict, State]}.

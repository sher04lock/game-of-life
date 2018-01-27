-module(universe).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

-export([start/0, tick/0, raiseTheDead/1, blinker/0, oneForThree/0, destroy/0]).


%%%===================================================================
%%% API
%%%===================================================================

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

tick() -> gen_server:call(?MODULE, tick).

%% sample positions
blinker() -> raiseTheDead([{0, 0}, {1, 0}, {2, 0}]).

oneForThree() -> raiseTheDead([{0, 0}, {0, 2}, {2, 0}]).

raiseTheDead(Positions) -> cell_sup:raiseTheDead(Positions).

destroy() -> cell_sup:killAll().

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  Generation = 0,
  io:format("Universe created...~n", []),
  {ok, Generation}.


handle_call(tick, _From, Generation) ->

  % tick each cell and fetch responses
  Responses = [gen_server:call(Pid, tick) || Pid <- cell_sup:children()],

  % split dead & alive cells
  {ToLive, ToDie} = lists:foldr(
    fun({X, Y}, {AccX, AccY}) -> {X ++ AccX, Y ++ AccY} end, {[], []}, Responses),

  % bring life to the Zombies!
  UniqueToLive = utils:remove_duplicates(ToLive),
  raiseDead(UniqueToLive),
  % kill unlucky
  killAlive(ToDie),

  {reply, #{die => ToDie, born => UniqueToLive}, Generation + 1};


handle_call(_Message, _From, Generation) ->
  {reply, error, Generation}.


%%====================================================================
%% Internal functions
%%====================================================================

raiseDead(Positions) ->
  cell_sup:raiseTheDead(Positions).

killAlive(Positions) ->
  lists:map(fun(Pos) ->
    cell_sup:kill(zombie_registry:get(Pos)),
    zombie_registry:remove(Pos) end, Positions).

% ----------------------------- asynchronous ------------------------

handle_cast(_Message, Gen) ->
  io:format("What should I do with that? ~p ~n", [_Message]),
  {noreply, Gen}.

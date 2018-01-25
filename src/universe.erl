-module(universe).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

-export([start/0, tick/0]).

-record(state, {register, to_live, to_die}).

%%%===================================================================
%%% API
%%%===================================================================

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

tick() -> gen_server:call(?MODULE, tick).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

% This is called when a connection is made to the server
init([]) ->
  Generation = 0, % nie tu jest register deklu, inny stan tu rano daj
  io:format("Universe created.~n", []),
  {ok, Generation}.


handle_call(tick, _From, Generation) ->

  Pids = cell_sup:children(),
  Responses = lists:map(fun(X) -> gen_server:call(X, tick) end, Pids),

%%  extract lists to die, live
%%  [{[ToLive],[ToDie]}]

  {ToLive, ToDie} = lists:foldr(
    fun({X, Y}, {AccX, AccY}) -> {X ++ AccX, Y ++ AccY} end, {[], []}, Responses),

  UniqueToLive = remove_duplicates(ToLive),

%%  bring life the Zombies!
  raiseTheDead(UniqueToLive),
%%  kill unlucky
  killAlive(ToDie),

  {reply, {ToLive, ToDie}, Generation + 1};


% if not defined
handle_call(_Message, _From, _) ->
  {reply, error, []}.


% ----------------------------- functions------------------------------

remove_duplicates(List) -> sets:to_list(sets:from_list(List)).

raiseTheDead(Positions) ->
  lists:map(fun(Pos) ->
    register:insert(Pos, cell_sup:raiseOne(Pos)) end, Positions).

killAlive(Positions) ->
  lists:map(fun(Pos) ->
    cell_sup:kill(register:get(Pos)),
    register:remove(Pos) end, Positions).

% ----------------------------- asynchronous ------------------------------

handle_cast(_Message, State) ->
  io:format("What should I do with that? ~p ~n", [_Message]),
  {noreply, State}.



terminate(_Reason, _State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

% handle_cast(_Message, State) -> {noreply, State}.
% handle_info(_Message, State) -> {noreply, State}.
% terminate(_Reason, _Library) -> ok.
% code_change(_OldVersion, State, _Extra) -> {ok, State}.
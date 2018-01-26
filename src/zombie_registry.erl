-module(zombie_registry).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

-export([start/0, insert/2, find/1, get/1, remove/1, count_alive/0, reset/0]).

%%%===================================================================
%%% API
%%%===================================================================

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(Position, Pid) -> gen_server:call(?MODULE, {insert, Position, Pid}).

find(Position) -> gen_server:call(?MODULE, {find, Position}).

get(Position) -> gen_server:call(?MODULE, {get, Position}).

remove(Position) -> gen_server:call(?MODULE, {remove, Position}).

count_alive() -> gen_server:call(?MODULE, count_alive).

reset() -> gen_server:call(?MODULE, reset).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
  Registry = maps:new(),
  io:format("Zombie registry opened...~n", []),
  {ok, Registry}.


handle_call({insert, Position, Pid}, _From, Registry) ->
  case maps:find(Position, Registry) of
    {ok, _} ->
      NewRegister = Registry,
      {cell_alive, Position};
    error ->
      NewRegister = maps:put(Position, Pid, Registry)
  end,
  {reply, {ok, Position, Pid}, NewRegister};

handle_call({find, Position}, _From, Registry) ->
  Response = case maps:find(Position, Registry) of
               {ok, _} ->
                 true;
               _ ->
                 false
             end,
  {reply, Response, Registry};

handle_call({get, Position}, _From, Registry) ->
  Response = case maps:get(Position, Registry) of
               {badkey, _} ->
                 {cell_dead};
               Pid ->
                 Pid
             end,
  {reply, Response, Registry};


handle_call({remove, Position}, _From, Registry) ->
  NewRegister = maps:remove(Position, Registry),
  {reply, NewRegister, NewRegister};

handle_call(count_alive, _From, Registry) ->
  Response = maps:size(Registry),
  {reply, Response, Registry};

handle_call(reset, _From, _) ->
  {reply, ok, maps:new()};


handle_call(_Message, _From, Registry) ->
  {reply, error, Registry}.

% ----------------------------- asynchronous ------------------------------

handle_cast(_Message, State) -> {noreply, State}.


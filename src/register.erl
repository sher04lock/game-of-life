-module(register).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

-export([start/0, insert/2, find/1, get/1, remove/1, count_alive/0, reset/0]).


% These are all wrappers for calls to the server
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(Key, Value) -> gen_server:call(?MODULE, {insert, Key, Value}).
find(Key) -> gen_server:call(?MODULE, {find, Key}).
get(Key) -> gen_server:call(?MODULE, {get, Key}).
remove(Key) -> gen_server:call(?MODULE, {remove, Key}).
count_alive() -> gen_server:call(?MODULE, count_alive).
reset() -> gen_server:call(?MODULE, reset).


% This is called when a connection is made to the server
init([]) ->
  Register = maps:new(),
  io:format("Register started!~n", []),
  {ok, Register}.


% handle_call is invoked in response to gen_server:call
handle_call({insert, Key, Value}, _From, Register) ->
  Response = case maps:find(Key, Register) of
               {ok, _} ->
                 NewRegister = Register,
                 {cell_alive, Key};
               error ->
                 NewRegister = maps:put(Key, Value, Register)
             end,
  {reply, Response, NewRegister};

handle_call({find, Key}, _From, Register) ->
  Response = case maps:find(Key, Register) of
               {ok, _} ->
                 true;
               _ ->
                 false
             end,
  {reply, Response, Register};

handle_call({get, Key}, _From, Register) ->
  Response = case maps:get(Key, Register) of
               Value ->
                 Value;
               _ ->
                 {cell_dead}
             end,
  {reply, Response, Register};


handle_call({remove, Key}, _From, Register) ->
  NewRegister = maps:remove(Key, Register),
  {reply, NewRegister, NewRegister};

handle_call(count_alive, _From, Register) ->
  Response = maps:size(Register),
  {reply, Response, Register};

handle_call(reset, _From, _) ->
  {reply, ok, maps:new()};

% if not defined
handle_call(_Message, _From, Register) ->
  {reply, error, Register}.

% ----------------------------- asynchronous ------------------------------
handle_cast(_Message, State) -> {noreply, State}.

terminate(_Reason, _State) ->
  ok.


% handle_info(_Message, State) -> {noreply, State}.
% terminate(_Reason, _Library) -> ok.
% code_change(_OldVersion, State, _Extra) -> {ok, State}.
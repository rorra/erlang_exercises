-module(misc_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, add/2, stop/0, subtract/2]).

%% Client functions
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(First, Second) ->
  gen_server:call(?MODULE, {add, First, Second}).

subtract(First, Second) ->
  gen_server:call(?MODULE, {subtract, First, Second}).

stop() ->
  gen_server:cast(?MODULE, stop).

%% Server functions

init([]) ->
  {ok, []}.

handle_call({subtract, First, Second}, _From, State) ->
  {reply, {ok, First - Second}, State};

handle_call({add, First, Second}, _From, State) ->
  {reply, {ok, First + Second}, State};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  io:format("Info message received: ~p~n", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("Server is stopping...~n"),
  ok.

code_change(_OldSvn, State, _Extra) ->
  {ok, State}.
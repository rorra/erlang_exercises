-module(misc_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

init([]) ->
  {ok, []}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldSvn, State, _Extra) ->
  {ok, State}.
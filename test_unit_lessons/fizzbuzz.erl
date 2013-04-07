-module(fizzbuzz).

-export([start/0, calc/2, shutdown/1]).

calc(Pid, Value) ->
    Pid ! {analyze, self(), Value},
    receive
	Result ->
	    Result
    end.

start() ->
    spawn(fun() ->
		  loop() end).

shutdown(Pid) ->
    Pid ! shutdown.

analyze(N) ->
    if N rem 15 == 0 ->
	    fizzbuzz;
       N rem 3 == 0 ->
	    fizz;
       N rem 5 == 0 ->
	    buzz;
       true ->
	    N
    end.

loop() ->
    receive
	{analyze, Caller, Value} ->
	    Caller ! analyze(Value),
	    loop();
	shutdown ->
	    io:format("Shutting down...~n"),
	    ok
    end.


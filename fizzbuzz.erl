-module(fizzbuzz).

-export([analyze/1]).

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

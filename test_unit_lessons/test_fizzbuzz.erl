-module(test_fizzbuzz).

-include_lib("eunit/include/eunit.hrl").

server_test_() ->
    {setup, fun() -> fizzbuzz:start() end,
     fun(Pid) -> fizzbuzz:shutdown(Pid) end,
     fun generate_analyze_test/1}.
	      
generate_analyze_test(Pid) ->
    [?_assertEqual(1, fizzbuzz:calc(Pid, 1)),
     ?_assertEqual(fizz, fizzbuzz:calc(Pid, 3)),
     ?_assertEqual(buzz, fizzbuzz:calc(Pid, 5)),
     ?_assertEqual(fizzbuzz, fizzbuzz:calc(Pid, 15))].

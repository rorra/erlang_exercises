-module(test_fizzbuzz).

-include_lib("eunit/include/eunit.hrl").

analyze_test() ->
    fizzbuzz:analyze(1).

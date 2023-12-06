-module(day@@DAY@@).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Input) ->
    ?v(length(Input)).

part2(Input) ->
    ?v(length(Input)).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({x, x}, ?solve_ex(1))
    , ?_assertEqual({x, x}, ?solve())
    ].

-module(day6).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(Input), part2(Input)}.

part1(Input) ->
    [Times, Distances] = ?ints_lines(Input),
    Races = lists:zip(Times, Distances),
    aoc:product([count_ways_to_win(T, D) || {T, D} <- Races]).

part2(Input) ->
    [L1, L2] = ?lines(Input),
    Time     = ?int([C || C <- L1, ?is_digit(C)]),
    Distance = ?int([C || C <- L2, ?is_digit(C)]),
    count_ways_to_win(Time, Distance).

count_ways_to_win(Time, Record) ->
    length(lists:filter(
             fun(HoldTime) ->
                     Distance = (Time - HoldTime) * HoldTime,
                     Distance > Record
             end,
             lists:seq(1, Time-1))).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({x, x}, ?solve_ex(1))
    , ?_assertEqual({x, x}, ?solve())
    ].

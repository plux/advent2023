-module(day11).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    sum_distances(Lines, 1).

part2(Lines) ->
    sum_distances(Lines, 999999).

sum_distances(Lines, M) ->
    Grid = ?grid(Lines),
    Galaxies = [P || {P, $#} <- maps:to_list(Grid)],
    EmptyRows = empty_lines(Lines),
    EmptyCols = empty_lines(aoc:transpose(Lines)),
    calc(Galaxies, EmptyRows, EmptyCols, M).

calc([], _, _, _M) ->
    0;
calc([{X1, Y1}|Galaxies], EmptyRows, EmptyCols, M) ->
    D1 = [abs(X1-X2) + space(X1, X2, EmptyCols) * M || {X2, _} <- Galaxies],
    D2 = [abs(Y1-Y2) + space(Y1, Y2, EmptyRows) * M || {_, Y2} <- Galaxies],
    lists:sum(D1) + lists:sum(D2) + calc(Galaxies, EmptyRows, EmptyCols, M).

space(A, B, Empty) ->
    length([E || E <- Empty, min(A, B) < E, E < max(A, B)]).

empty_lines(Lines) ->
    [N || {N, Line} <- aoc:enumerate(Lines), is_empty(Line)].

is_empty(Line) ->
    not lists:any(fun(C) -> C == $# end, Line).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({374, 82000210}, ?solve_ex(1))
    , ?_assertEqual({9543156, 625243292686}, ?solve())
    ].

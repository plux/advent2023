-module(day9).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?ints_lines(Input)), part2(?ints_lines(Input))}.

part1(Lines) ->
    lists:sum([calc([lists:last(Line)], next_line(Line)) || Line <- Lines]).

part2(Lines0) ->
    Lines = lists:map(fun lists:reverse/1, Lines0),
    lists:sum([calc([lists:last(Line)], next_line(Line)) || Line <- Lines]).


calc(Vals, CurrLine) ->
    case lists:all(fun(X) -> X == 0 end, CurrLine) of
        true ->
            extrapolate([lists:last(CurrLine)|Vals]);
        false ->
            calc([lists:last(CurrLine)|Vals], next_line(CurrLine))
    end.

extrapolate([Val]) ->
    Val;
extrapolate([CurrVal, PrevVal|Rest]) ->
    extrapolate([PrevVal + CurrVal|Rest]).

next_line([_]) ->
    [];
next_line([Curr,Next|Rest]) ->
    [Next-Curr|next_line([Next|Rest])].

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({114, 2}, ?solve_ex(1))
    , ?_assertEqual({1995001648, 988}, ?solve())
    ].

-module(day13).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?split(Input,"\n\n")), part2(?split(Input, "\n\n"))}.

part1(Input) ->
    lists:sum([calc(I) || I <- Input]).

part2(Input) ->
    x.

calc(I) ->
    calc(?lines(I), 0) + calc(aoc:transpose(?lines(I)), 0) * 100.

calc(Lines, Smudges) ->
    Reflections = [find_reflections(tl(L), [hd(L)]) || L <- Lines],
    find_reflection_pos(Reflections, Smudges).

find_reflections([], _) ->
    [];
find_reflections([H|T] = Right, Left) ->
    Found = case string:prefix(Right, Left) of
                nomatch ->
                    case string:prefix(Left, Right) of
                        nomatch ->
                            [];
                        _ ->
                            [length(Left)]
                    end;
                _ ->
                    [length(Left)]
            end,
    Found ++ find_reflections(T, [H|Left]).

find_reflection_pos(Refs, Smudges) ->
    Expected = length(Refs) - Smudges,
    Counter = counter:count(lists:flatten(Refs)),
    case [L || {L, N} <- maps:to_list(Counter), N == Expected] of
        [] ->
            0;
        [L] ->
            L
    end.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({405, x}, ?solve_ex(1))
    , ?_assertEqual({37025, x}, ?solve())
    ].

-module(day1).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
   {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    DigLines = [[C || C <- Line, is_digit(C)] || Line <- Lines],
    lists:sum([?int([hd(L), lists:last(L)]) || L <- DigLines, length(L) > 0]).

part2(Lines) ->
    DigLines = [to_digits(Line) || Line <- Lines],
    lists:sum([?int([hd(L), lists:last(L)]) || L <- DigLines, length(L) > 0]).

to_digits(Line) ->
    case Line of
        "one"   ++ _ -> [$1|to_digits(tl(Line))];
        "two"   ++ _ -> [$2|to_digits(tl(Line))];
        "three" ++ _ -> [$3|to_digits(tl(Line))];
        "four"  ++ _ -> [$4|to_digits(tl(Line))];
        "five"  ++ _ -> [$5|to_digits(tl(Line))];
        "six"   ++ _ -> [$6|to_digits(tl(Line))];
        "seven" ++ _ -> [$7|to_digits(tl(Line))];
        "eight" ++ _ -> [$8|to_digits(tl(Line))];
        "nine"  ++ _ -> [$9|to_digits(tl(Line))];
        [C|T] ->
            case is_digit(C) of
                true -> [C|to_digits(T)];
                false -> to_digits(T)
            end;
        [] ->
            []
    end.

is_digit(C) ->
    C >= $1 andalso C =< $9.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({142, 142}, ?solve_ex(1))
    , ?_assertEqual({209, 281}, ?solve_ex(2))
    , ?_assertEqual({54388, 53515}, ?solve())
    ].

-module(day7).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?words_lines(Input)), part2(?words_lines(Input))}.

parse(Input, Part) ->
    [{[val(C, Part) || C <- Hand], Hand, ?int(Bid)} || [Hand, Bid] <- Input].

part1(Input) ->
    calc_total_winnings(parse(Input, p1)).

part2(Input) ->
    calc_total_winnings(parse(Input, p2)).

calc_total_winnings(Hands) ->
    SortedHands = aoc:enumerate(lists:sort(fun lt_hand/2, Hands)),
    lists:sum([(I + 1) * Bid || {I, {_, _, Bid}} <- SortedHands]).

lt_hand({A, _, _}, {B, _, _}) ->
    ValA = hand_kind(A),
    ValB = hand_kind(B),
    case ValA == ValB of
        true ->
            A < B;
        false ->
            ValA < ValB
    end.

hand_kind(Hand) ->
    Count0 = counter:count(Hand),
    NumJokers = maps:get(1, Count0, 0),
    Count1 = Count0#{1 => 0},
    {Best, _NumBest} = counter:max(Count1),
    Count2 = counter:incr(Count1, Best, NumJokers),
    case counter:most_common(Count2, maps:size(Count2)) of
        [{_, 5}|_]         -> 7;
        [{_, 4}|_]         -> 6;
        [{_, 3}, {_, 2}|_] -> 5;
        [{_, 3}|_]         -> 4;
        [{_, 2}, {_, 2}|_] -> 3;
        [{_, 2}|_]         -> 2;
        [{_, 1}|_]         -> 1
    end.

val(X, Part) ->
    case X of
        $A -> 14;
        $K -> 13;
        $Q -> 12;
        $J when Part == p1 -> 11;
        $J when Part == p2 -> 1;
        $T -> 10;
        _  -> X - $0
    end.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({6440, 5905}, ?solve_ex(1))
    , ?_assertEqual({254024898, 254115617}, ?solve())
    ].

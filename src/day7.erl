-module(day7).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

parse(Input) ->
    [{[val(C) || C <- Hand], Hand, ?int(Bid)} || [Hand, Bid] <- ?words_lines(Input)].

part1(Input) ->
    SortedHands = aoc:enumerate(lists:sort(fun lt_hand/2, Input)),
    L = [{Hand, Bid} || {I, {_, Hand, Bid}} <- SortedHands],
    ?v(L),
    lists:sum([(I+1)*Bid || {I, {_, _, Bid}} <- SortedHands]).

part2(Input) ->
    ok.

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
    Count = counter:count(Hand),
    case counter:most_common(Count, maps:size(Count)) of
        [{_, 5}]        -> 7;
        [{_, 4}|_]      -> 6;
        [{_, 3}|{_, 2}] -> 5;
        [{_, 3}|_]      -> 4;
        [{_, 2}|{_, 2}] -> 3;
        [{_, 2}|_]      -> 2;
        _               -> 1
    end.

val(X) ->
    case X of
        $A -> 14;
        $K -> 13;
        $Q -> 12;
        $J -> 11;
        $T -> 10;
        $9 -> 9;
        $8 -> 8;
        $7 -> 7;
        $6 -> 6;
        $5 -> 5;
        $4 -> 4;
        $3 -> 3;
        $2 -> 2
    end.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({6440, x}, ?solve_ex(1))
    %% , ?_assertEqual({1, x}, ?solve())
    ].

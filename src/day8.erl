-module(day8).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

parse([Instrs | Lines]) ->
    Map0 = [string:lexemes(Line, " =(),") || Line <- Lines],
    Map = lists:foldl(fun([Node, L, R], Acc) ->
                              Acc#{{Node, $L} => L,
                                   {Node, $R} => R}
                      end, #{}, Map0),
    {Instrs, Map}.

part1(Input) ->
    {Instrs, Map} = parse(Input),
    walk(Instrs, "AAA", Instrs, Map, p1).

walk(_, "ZZZ", _Instrs, _Map, p1) ->
    0;
walk(_, [_, _, $Z], _Instrs, _Map, p2) ->
    0;
walk([], Curr, Instrs, Map, Part) ->
    walk(Instrs, Curr, Instrs, Map, Part);
walk([Instr|Rest], Curr, Instrs, Map, Part) ->
    Next = maps:get({Curr, Instr}, Map),
    1 + walk(Rest, Next, Instrs, Map, Part).

calc_total_steps(A, B, _N) when A rem B == 0 ->
    A;
calc_total_steps(A, B, N) ->
    calc_total_steps(A+N, B, N).

part2(Input) ->
    {Instrs, Map} = parse(Input),
    Starts = lists:uniq([Start || {[_, _, $A] = Start, _} <- maps:keys(Map)]),
    AllSteps = [walk(Instrs, Start, Instrs, Map, p2) || Start <- Starts],
    aoc:lcm(AllSteps).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({2, 2}, ?solve_ex(1))
    , ?_assertEqual({6, 6}, ?solve_ex(2))
    , ?_assertEqual({18113, 12315788159977}, ?solve())
    ].

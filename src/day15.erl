-module(day15).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?split(Input, ",")), part2(?split(Input, ","))}.

part1(Input) ->
    lists:sum([hash(S) || S <- Input]).

part2(Input) ->
    Boxes = lists:foldl(fun eval/2, #{}, Input),
    calc_focus_power(Boxes).

eval(Op, Boxes) ->
    case parse_op(Op) of
        {assign, Name, Value, Hash} ->
            Box0 = maps:get(Hash, Boxes, []),
            Box = case lists:keyfind(Name, 1, Box0) of
                      {Name, _Value0} ->
                          lists:keyreplace(Name, 1, Box0, {Name, Value});
                      false ->
                          Box0 ++ [{Name, Value}]
                  end,
            Boxes#{Hash => Box};
        {sub, Name, Hash} ->
            Box0 = maps:get(Hash, Boxes, []),
            Box = lists:keydelete(Name, 1, Box0),
            Boxes#{Hash => Box}
    end.

calc_focus_power(X) ->
    maps:fold(
      fun(N, Box, Acc0) ->
              {_, Acc} = lists:foldl(
                           fun({_Name, Value}, {Slot, Acc1})  ->
                                   Power = ((N+1) * Slot * Value),
                                   {Slot + 1, Acc1 + Power}
                           end, {1, Acc0}, Box),
              Acc
      end, 0, X).

parse_op(S) ->
    case ?split(S, "=") of
        [S] ->
            [$-|Var0] = lists:reverse(S),
            Var = lists:reverse(Var0),
            {sub, Var, hash(Var)};
        [Var, Num] ->
            {assign, Var, list_to_integer(Num), hash(Var)}
    end.

hash(S) ->
    hash(S, 0).

hash([], Acc) ->
    Acc;
hash([C|Rest], Acc) ->
    hash(Rest, ((Acc + C) * 17) rem 256).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({1320, 145}, ?solve_ex(1))
    , ?_assertEqual({510388, 291774}, ?solve())
    ].

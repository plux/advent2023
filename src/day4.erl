-module(day4).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    erase(),
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    Cards = [parse_line(Line) || Line <- Lines],
    lists:sum([score(Card) || Card <- Cards]).

score(Card) ->
    case count_winning_numbers(Card) of
        0 ->
            0;
        Count ->
            1 bsl (Count - 1)
    end.

count_winning_numbers({_, Winning, MyNumbers}) ->
    length([N || N <- MyNumbers, lists:member(N, Winning)]).

parse_line(Line) ->
    [L, R] = ?split(Line, "|"),
    [Card | Winning] = ?ints(L),
    MyNumbers = ?ints(R),
    {Card, Winning, MyNumbers}.

part2(Lines) ->
    Cards = [parse_line(Line) || Line <- Lines],
    {Next, _, _} = lists:unzip3(Cards),
    calc(Next, Cards).

calc([], _Cards) ->
    0;
calc(Next0, Cards) ->
    Next = lists:flatten([next(C, Cards) || C <- Next0]),
    length(Next0) + calc(Next, Cards).

next(CardId, Cards) ->
    ?memo(CardId,
          begin
              Count = count_winning_numbers(lists:keyfind(CardId, 1, Cards)),
              lists:seq(CardId+1, CardId + Count)
          end
      ).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({13, 30}, ?solve_ex(1))
    , ?_assertEqual({24542, 8736438}, ?solve())
    ].

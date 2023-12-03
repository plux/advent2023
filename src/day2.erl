-module(day2).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
   {part1(parse(Input)), part2(parse(Input))}.

part1(Lines) ->
    lists:sum([Id || {Id, Game} <- Lines, is_possible(Game)]).

part2(Lines) ->
    lists:sum([power(Game) || {_, Game} <- Lines]).

parse(Input) ->
    [parse_line(Line) || Line <- ?lines(Input)].

parse_line(Line) ->
    [_, Id | G] = re:split(Line, "[,;: ]", [{return, list}]),
    Chunks = ?chunks([X || X <- G, X /= ""], 2),
    Game = [{?int(N), C} || [N, C] <- Chunks],
    {?int(Id), Game}.

is_possible(Game) ->
    NumCubes = #{"red" => 12, "green" => 13, "blue" => 14},
    lists:all(fun({N, Color}) ->
                      N =< maps:get(Color, NumCubes, 0)
              end, Game).
power(Game) ->
    Maxes = lists:foldl(fun({N, Color}, Acc) ->
                                Max = max(maps:get(Color, Acc, 0), N),
                                Acc#{Color => Max}
                        end, #{}, Game),
    aoc:product(maps:values(Maxes)).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({8, 2286}, ?solve_ex(1))
    , ?_assertEqual({2085, 79315}, ?solve())
    ].

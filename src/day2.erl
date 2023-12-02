-module(day2).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
   {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    Games = [parse_line(Line) || Line <- Lines],
    lists:sum([Id || {Id, Game} <- Games, is_possible(Game)]).

part2(Lines) ->
    Games = [parse_line(Line) || Line <- Lines],
    lists:sum([power(Game) || {_Id, Game} <- Games]).

parse_line(Line) ->
    [IdStr, GameStr] = ?split(Line, ":"),
    [Id] = ?ints(IdStr),
    Game = lists:map(
             fun(Sub) ->
                     Cubes = [?split(string:strip(Cube), " ") ||
                                 Cube <- ?split(Sub, ",")],
                     [{Color, ?int(Num)} || [Num, Color] <- Cubes]
             end, ?split(GameStr, ";")),
    {Id, Game}.

is_possible(Game) ->
    NumCubes = #{"red" => 12, "green" => 13, "blue" => 14},
    lists:all(fun(SubGame) ->
                       lists:all(fun ({Color, N}) ->
                                         N =< maps:get(Color, NumCubes, 0)
                                 end, SubGame)
              end, Game).

power(Game) ->
    Maxes = lists:foldl(
              fun(SubGame, Acc) ->
                      lists:foldl(
                        fun({Color, N}, SubAcc) ->
                                Max = max(maps:get(Color, SubAcc, 0), N),
                                SubAcc#{Color => Max}
                        end, Acc, SubGame)
              end, #{}, Game),
    lists:foldl(fun erlang:'*'/2, 1, maps:values(Maxes)).



-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({8, 2286}, ?solve_ex(1))
    , ?_assertEqual({2085, 79315}, ?solve())
    ].

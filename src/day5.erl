-module(day5).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

part1({Seeds, Maps}) ->
    lists:min([follow_maps(Seed, Maps) || Seed <- Seeds]).

part2({Seeds, Maps}) ->
    follow_maps_ranges(?chunks(Seeds, 2), Maps, []).

follow_maps_ranges([], _Maps, Best) ->
    Best;
follow_maps_ranges([[From, Size]|Ranges], Maps, Best0) ->
    Ctx = {From, From+Size, Maps},
    Step = Size div 10,
    Best = follow_maps_range_coarse(From, Ctx, From, Step, infinity),
    follow_maps_ranges(Ranges, Maps, min(Best, Best0)).

follow_maps_range_coarse(Source, {_Min, Max, Maps}, _BestSeed, 0, Best) ->
    %% Do the final search to find the right seed
    follow_maps_range(Source, Max, Maps, Best);
follow_maps_range_coarse(Source0, {Min, Max, Maps}, BestSeed, Step, Best)
  when Source0 > Max ->
    %% We overstepped, search around the best seed with smaller steps
    Ctx = {Min, min(BestSeed + Step, Max), Maps},
    Source = max(Min, BestSeed - Step),
    SmallerStep = Step div 10,
    follow_maps_range_coarse(Source, Ctx, BestSeed, SmallerStep, Best);
follow_maps_range_coarse(Source, {_, _, Maps} = Ctx, BestSeed, Step, Best) ->
    Location = follow_maps(Source, Maps),
    case Location < Best of
        true ->
            follow_maps_range_coarse(Source + Step, Ctx, Source, Step, Location);
        false ->
            follow_maps_range_coarse(Source + Step, Ctx, BestSeed, Step, Best)
    end.

follow_maps_range(Same, Same,_Maps, Best) ->
    Best;
follow_maps_range(Source, Max, Maps, Best) ->
    Location = follow_maps(Source, Maps),
    follow_maps_range(Source+1, Max, Maps, min(Best, Location)).

follow_maps(Location, []) ->
    Location;
follow_maps(Source, [Map|Maps]) ->
    Dest = follow(Source, Map),
    follow_maps(Dest, Maps).

follow(N, []) ->
    N;
follow(N, [[To, From, Size]|_]) when From =< N, N < From + Size ->
    N - From + To;
follow(N, [_|Rest]) ->
    follow(N, Rest).

parse(Input) ->
    [Seeds|Rest] = ?split(Input, "\n\n"),
    Maps = [tl(?ints_lines(M)) || M <- Rest],
    {?ints(Seeds), Maps}.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({35, 46}, ?solve_ex(1))
    , ?_assertEqual({379811651,27992443}, ?solve())
    ].

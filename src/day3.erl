-module(day3).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(parse(Input)), part2(parse(Input))}.

parse(Input) ->
    ?grid(?lines(Input)).

part1(Grid) ->
    Dim = dimensions(Grid),
    Nums = find_nums({0, 0}, Dim, Grid, "", false, []),
    lists:sum(Nums).

part2(Grid) ->
    Dim = dimensions(Grid),
    Nums = find_nums2({0, 0}, Dim, Grid, "", [], []),
    {Cogs, _} = lists:unzip(Nums),
    lists:foldl(fun(Cog, Acc) ->
                      case [N || {Pos, N} <- Nums, Pos == Cog] of
                          [X, Y] -> X*Y + Acc;
                          _      -> Acc
                      end
              end, 0, lists:uniq(Cogs)).

find_nums({_X, Y}, {_W, H}, _Grid, _Curr, _IsMatch, Seen) when Y > H ->
    Seen;
find_nums({X, Y}, {W, H}, Grid, Curr, IsMatch, Seen) when X > W ->
    find_nums({0, Y+1}, {W, H}, Grid, "", false, add_seen(Curr, IsMatch, Seen));
find_nums({X, Y} = Pos, Dim, Grid, Curr, IsMatch0, Seen) ->
    case maps:get(Pos, Grid) of
        C when ?is_digit(C) ->
            IsMatch = check_match(IsMatch0, Pos, Grid),
            find_nums({X+1, Y}, Dim, Grid, [C|Curr], IsMatch, Seen);
        _ ->
            find_nums({X+1, Y}, Dim, Grid, "", false,
                      add_seen(Curr, IsMatch0, Seen))
    end.

add_seen("", _, Seen) ->
    Seen;
add_seen(Curr, true, Seen) ->
    [?int(lists:reverse(Curr))|Seen];
add_seen(_Curr, false, Seen) ->
    Seen.

check_match(true, _, _Grid) ->
    true;
check_match(_, Pos, Grid) ->
    lists:any(
      fun(P) ->
              case maps:get(P, Grid, $.) of
                  $. ->
                      false;
                  C when ?is_digit(C) ->
                      false;
                  _ ->
                      true
              end
      end, aoc:neighbors_8(Pos)).


find_nums2({_X, Y}, {_W, H}, _Grid, _Curr, _Matches, Seen) when Y > H ->
    Seen;
find_nums2({X, Y}, {W, H}, Grid, Curr, Matches, Seen) when X > W ->
    find_nums2({0, Y+1}, {W, H}, Grid, "", [], add_seen2(Curr, Matches, Seen));
find_nums2({X, Y} = Pos, Dim, Grid, Curr, Matches0, Seen) ->
    case maps:get(Pos, Grid) of
        C when ?is_digit(C) ->
            Matches = check_match2(Matches0, Pos, Grid),
            find_nums2({X+1, Y}, Dim, Grid, [C|Curr], Matches, Seen);
        _ ->
            find_nums2({X+1, Y}, Dim, Grid, "", [], add_seen2(Curr, Matches0, Seen))
    end.

check_match2(Matches, Pos, Grid) ->
    Matches ++ [P || P <- aoc:neighbors_8(Pos), $* == maps:get(P, Grid, $.)].

add_seen2("", _, Seen) ->
    Seen;
add_seen2(Curr, Matches, Seen) ->
    lists:uniq([{M, ?int(lists:reverse(Curr))} || M <- Matches] ++ Seen).

dimensions(Grid) ->
    lists:max(maps:keys(Grid)).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({4361,467835}, ?solve_ex(1))
    , ?_assertEqual({550934,81997870}, ?solve())
    ].

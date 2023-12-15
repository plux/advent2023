-module(day14).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    erase(),
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Input) ->
    G = tilt(?grid(Input), ?north),
    score(G).

part2(Input) ->
    tilt_cycle(?grid(Input)).

tilt(G, Dir) ->
    NewG =
        maps:fold(
          fun(Pos, $O, Acc) ->
                  NewPos = add(Pos, Dir),
                  case maps:get(NewPos, G, $#) of
                      $. ->
                          move_stone(Pos, Dir, Acc#{Pos => $.});
                      _ ->
                          Acc
                  end;
             (_Pos, _, Acc) ->
                  Acc
          end, G, G),
    case NewG == G of
        true ->
            G;
        false ->
            tilt(NewG, Dir)
    end.

move_stone(Pos, Dir, G) ->
    NewPos = add(Pos, Dir),
    case maps:get(NewPos, G, $#) of
        $. ->
            move_stone(NewPos, Dir, G);
        _ ->
            G#{Pos => $O }
    end.

add({X, Y}, {DX, DY}) ->
    {X + DX, Y + DY}.

score(G) ->
    {_W, H} = lists:max(maps:keys(G)),
    maps:fold(
     fun({_, Y}, $O, Acc) ->
             Acc + H - Y + 1;
        (_Pos, _, Acc) ->
             Acc
     end, 0, G).

-define(directions, [?north, ?west, ?south, ?east]).
-define(cycles, 1000000000).

tilt_cycle(G) ->
    tilt_cycle(G, ?directions, 1, #{}).

tilt_cycle(G, [], ?cycles, _Grids) ->
    score(G);
tilt_cycle(G, [], N, Grids) ->
    case maps:find(G, Grids) of
        {ok, CycleStart} ->
            SkipN = skip(N, N - CycleStart),
            tilt_cycle(G, ?directions, SkipN+1, #{});
        error ->
            tilt_cycle(G, ?directions, N+1, Grids#{G => N})
    end;
tilt_cycle(G0, [Dir|Rest], N, Grids) ->
    G = tilt(G0, Dir),
    tilt_cycle(G, Rest, N, Grids).

skip(N, Cycle) when N + Cycle < ?cycles ->
    skip(N + Cycle, Cycle);
skip(N, _Cycle) ->
    N.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual({136, 64}, ?solve_ex(1))
    , ?_assertEqual({109345, 112452}, ?solve())
    ].

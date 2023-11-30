-module(advent2023).

-export([main/1]).
-export([solve/1]).
-export([solve/2]).

%% escript Entry point
main([]) ->
    {ok, Files} = file:list_dir("input"),
    solve(Files),
    erlang:halt(0);
main([Arg]) ->
    solve([Arg ++ ".txt"]),
    erlang:halt(0).

solve(Files) ->
    {T, _} = timer:tc(fun() -> aoc:solve(Files) end),
    io:format("Total duration: ~p ms\n",
              [trunc(math:ceil(T / 1000))]).

solve(Mod, File) ->
    aoc:solve(Mod, File).

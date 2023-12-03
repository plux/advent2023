-define(lines(X), aoc:lines(X)).
-define(words(X), aoc:words(X)).
-define(int(X), aoc:int(X)).
-define(ints(X), aoc:ints(X)).
-define(solve(), aoc:solve(?MODULE)).
-define(grid(X), aoc:grid(X)).
-define(words_lines(X), [?words(Line) || Line <- ?lines(X)]).
-define(ints_lines(X), [?ints(Line) || Line <- ?lines(X)]).
-define(int_lines(X), [?int(Line) || Line <- ?lines(X)]).
-define(split(X, Sep), aoc:split(X, Sep)).
-define(chunks(L, N), aoc:chunks(L, N)).
-define(chunk_every(L, N), aoc:chunk_every(L, N)).
-define(uniq(L), lists:uniq(L)).
-define(find_index(Pred, L), aoc:find_index(Pred, L)).
-define(solve_ex(X),
        aoc:solve(?MODULE, "input_ex/" ++ ?MODULE_STRING ++ "_"
                  ++ integer_to_list(X) ++ ".txt")).
-define(take(N, L), lists:sublist(L, N)).
-define(l(Fmt, Args), io:format("DEBUG: ~p:~p:~p - " ++ Fmt ++ "\n",
                                [?MODULE,
                                 ?FUNCTION_NAME,
                                 ?LINE|Args])).
-define(l(Fmt), ?l(Fmt, [])).
-define(v(X), begin ?l("~s = ~p\n", [(??X), (X)]), (X) end).

%%      N              {0,-1}
%%   NW | NE      {-1,-1}|{1,-1}
%% W ---+--- E {-1,0}----+----{1,0}
%%   SW | SE       {-1,1}|{1,1}
%%      S              {0,1}

-define(west,  {-1,  0}).
-define(east,  { 1,  0}).
-define(north, { 0, -1}).
-define(south, { 0,  1}).

-define(nw,  {-1, -1}).
-define(ne,  { 1, -1}).
-define(sw,  {-1,  1}).
-define(se,  { 1,  1}).

-define(manhattan(X, Y), abs(X) + abs(Y)).

-define(memo(X, Fun),
    case get({?MODULE, X}) of
        undefined ->
            Res = begin Fun end,
            put({?MODULE, X}, Res),
            Res;
        Res ->
            Res
    end).

-define(is_digit(C), C >= $0, C =< $9).

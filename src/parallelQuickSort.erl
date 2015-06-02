-module(parallelQuickSort).
-export([pqsort/1, test_loop/1, test_loop/3]).

 %                    %
%%%%%%%%%%%%%%%%%%%%%%%%
 % Parallel QuickSort %
%%%%%%%%%%%%%%%%%%%%%%%%
 %                    %

pmap(F, L) ->
    S = self(),
    Pids = lists:map(fun(I) -> spawn(fun() -> pmap_f(S, F, I) end) end, L),
    pmap_gather(Pids).

pmap_gather([H|T]) ->
    receive
        {H, Ret} -> [Ret|pmap_gather(T)]
    end;
pmap_gather([]) ->
    [].

pmap_f(Parent, F, I) ->
    Parent ! {self(), (catch F(I))}.

pqsort([]) -> [];
pqsort([Pivot]) -> [Pivot];
pqsort([Pivot|Rest]) ->
    % io:format("+", []),
    Left = [X || X <- Rest, X < Pivot],
    Right = [Y || Y <- Rest, Y >= Pivot],
    [SortedLeft, SortedRight] = pmap(fun pqsort/1, [Left, Right]),
    % io:format("-", []),
    SortedLeft ++ [Pivot] ++ SortedRight.

%%%%%%%%%%%%%%%%%%%
%% PROFILE STUFF %%
%%%%%%%%%%%%%%%%%%%

profile(Parameters) ->
    timer:tc(parallelQuickSort, pqsort, [Parameters]).

test_quickSort(Index, Len, Max) ->
    File = io_lib:fwrite("./paralllelQuickSort.~w.log", [Index]),
    Array = [random:uniform(Max) || _ <- lists:seq(1, Len)],
    file:write_file(File, io_lib:fwrite("Len: ~w\nMax: ~w\nArray: ~w\n", [Len, Max, Array])),
    {Time, Result} = profile(Array),
    file:write_file(File, io_lib:fwrite("Time: ~w\nResult: ~w", [Time, Result]), [append]).

test_looper(Index, Len, Max) when Index > 0 ->
    test_quickSort(Index, Len, Max),
    test_looper(Index-1, Len, Max);
test_looper(Index, _, _) when Index =< 0 ->
    0.

granted_int(Num) when erlang:is_integer(Num) -> 
    Num;
granted_int(Num) when erlang:is_list(Num) -> 
    {NewNum, _} = string:to_integer(Num),
    NewNum;
granted_int(Num) when erlang:is_atom(Num) -> 
    granted_int(erlang:atom_to_list(Num)).

test_loop(Num, Len, Max) ->
    random:seed(1,2,3),
    test_looper(granted_int(Num), granted_int(Len), granted_int(Max)).

test_loop([Num, Len, Max]) ->
    test_loop(Num, Len, Max).


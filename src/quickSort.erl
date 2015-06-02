-module(quickSort).
-export([quickSort/1, test_loop/3, granted_int/1]).

% Sequential QuickSort

quickSort(List) when length(List) =< 1 ->
    List;
quickSort([Pivot | List]) ->
    quickSort(quickSortMin(List, Pivot)) ++ [Pivot] ++ quickSort(quickSortMax(List, Pivot)).

quickSortMin([First], Pivot) when First =< Pivot ->
    [First];
quickSortMin([First], Pivot) ->
    [];
quickSortMin([First | Tail], Pivot) when First =< Pivot ->
    [First] ++ quickSortMin(Tail, Pivot);
quickSortMin([First | Tail], Pivot) ->
    quickSortMin(Tail, Pivot).

quickSortMax([First], Pivot) when First > Pivot ->
    [First];
quickSortMax([First], Pivot) ->
    [];
quickSortMax([First | Tail], Pivot) when First > Pivot ->
    [First] ++ quickSortMax(Tail, Pivot);
quickSortMax([First | Tail], Pivot) ->
    quickSortMax(Tail, Pivot).

profile(Parameters) ->
    timer:tc(quickSort, quickSort, [Parameters]).

test_quickSort(Index, Len, Max) ->
    File = io_lib:fwrite("./quickSort.~w.log", [Index]),
    Array = [random:uniform(Max) || _ <- lists:seq(1, Len)],
    file:write_file(File, io_lib:fwrite("Len: ~w\nMax: ~w\nArray: ~w\n", [Len, Max, Array])),
    {Time, Result} = profile(Array),
    file:write_file(File, io_lib:fwrite("Time: ~w\nResult: ~w", [Time, Result]), [append]).

test_looper(Index, Len, Max) when Index > 0 ->
    test_quickSort(Index, Len, Max),
    test_looper(Index-1, Len, Max);
test_looper(Index, Len, Max) when Index =< 0 ->
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


-module(quickSort).
-export([quickSort/1, test_loop/1, test_loop/3]).

 %                     %
%%%%%%%%%%%%%%%%%%%%%%%%%
 % Sequential QuickSort %
%%%%%%%%%%%%%%%%%%%%%%%%%
 %                     %

quickSort(List) when length(List) =< 1 ->
    List;
quickSort([Pivot | List]) ->
    quickSort(quickSortMin(List, Pivot)) ++ [Pivot] ++ quickSort(quickSortMax(List, Pivot)).


quickSortMin([First], Pivot) when First =< Pivot ->
    [First];
quickSortMin([_], _) ->
    [];
quickSortMin([First | Tail], Pivot) when First =< Pivot ->
    [First] ++ quickSortMin(Tail, Pivot);
quickSortMin([_ | Tail], Pivot) ->
    quickSortMin(Tail, Pivot).


quickSortMax([First], Pivot) when First > Pivot ->
    [First];
quickSortMax([_], _) ->
    [];
quickSortMax([First | Tail], Pivot) when First > Pivot ->
    [First] ++ quickSortMax(Tail, Pivot);
quickSortMax([_ | Tail], Pivot) ->
    quickSortMax(Tail, Pivot).

%%%%%%%%%%%%%%%%%%%
%% PROFILE STUFF %%
%%%%%%%%%%%%%%%%%%%

profile(Parameters) ->
    timer:tc(quickSort, quickSort, [Parameters]).

test_quickSort(List, PerfFile) ->
    {Time, _} = profile(List),
    file:write(PerfFile, io_lib:fwrite("~w\n", [Time])).

test_looper(_, 0, _) ->
    0;
test_looper(List, Index, PerfFile) ->
    test_quickSort(List, PerfFile),
    test_looper(List, Index-1, PerfFile).

granted_int(Num) when erlang:is_integer(Num) -> 
    Num;
granted_int(Num) when erlang:is_list(Num) -> 
    {NewNum, _} = string:to_integer(Num),
    NewNum;
granted_int(Num) when erlang:is_atom(Num) -> 
    granted_int(erlang:atom_to_list(Num)).

problemFromFileAux(_, eof) ->
    [];
problemFromFileAux(Device, Line) ->
    [granted_int(Line)] ++ problemFromFileAux(Device, io:get_line(Device, "")).

problemFromFileAux(Device) ->
    problemFromFileAux(Device, io:get_line(Device, "")).

problemFromFile(Filename) ->
    {ok, File} = file:open(Filename, [read]),
    problemFromFileAux(File).

test_loop(ProblemFile, Repetitions, PerfFile) ->
    {ok, PFile} = file:open(PerfFile, [write]),
    Result = test_looper(problemFromFile(ProblemFile), granted_int(Repetitions), PFile),
    file:close(PFile),
    Result.

test_loop([ProblemFile, Repetitions, PerfFile]) ->
    test_loop(ProblemFile, Repetitions, PerfFile).


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


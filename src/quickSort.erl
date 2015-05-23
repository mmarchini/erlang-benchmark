-module(sorters).
-export([quickSort/1]).

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


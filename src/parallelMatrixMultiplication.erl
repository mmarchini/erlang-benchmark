-module(parallelMatrixMultiplication).
-export([matrixMultiplication/3, matrixMultiplicationSpawn/3, test_loop/1, test_loop/5]).

gather([H|T]) ->
    receive
        {H, Ret} -> Ret ++ gather(T)
    end;
gather([]) ->
    [].


matrixMultiplication(A, B, P) ->
    RotB = rotateMatrix(B),
    gather([spawn(parallelMatrixMultiplication, matrixMultiplicationSpawn, [self(), ScatA, RotB]) || ScatA <- scatter(A, P)]).

matrixMultiplicationSpawn(S, A, RotB) ->
    S ! {self(), multi1(A, RotB)}.

scatter([], 0) ->
    [];
scatter(List, Pieces) ->
    {Scattered, Tail} = scatterAux(List, length(List) div Pieces),
    [Scattered] ++ scatter(Tail, Pieces-1).

scatterAux(RealTail, 0) ->
    {[], RealTail};
scatterAux([Elem | Tail], Left) ->
    {Scattered, RealTail} = scatterAux(Tail, Left-1),
    {[Elem] ++ Scattered, RealTail}.

multi1([], _) ->
    [];
multi1([Row | Tail], Matrix) ->
    [multi2(Row, Matrix)] ++ multi1(Tail, Matrix).

multi2(_, []) ->
    [];
multi2(Row, [Col | Tail]) ->
    [vectorMultiplication(Row, Col)] ++ multi2(Row, Tail).

rotateMatrix([[] | _]) ->
    [];
rotateMatrix([]) ->
    [];
rotateMatrix(Matrix) ->
    {Column, Tail} = getColumn(Matrix),
    [Column] ++ rotateMatrix(Tail).

getColumn(Matrix) ->
    {[VCol || [VCol |  _  ] <- Matrix], [Tail || [  _  | Tail] <- Matrix]}.

vectorMultiplication([], []) ->
    0;
vectorMultiplication([A | ATail], [B | BTail]) ->
    (A * B) + vectorMultiplication(ATail, BTail).
    
    
%%%%%%%%%%%%%%%%%%%
%% PROFILE STUFF %%
%%%%%%%%%%%%%%%%%%%

profile(A, B, P) ->
    timer:tc(parallelMatrixMultiplication, matrixMultiplication, [A, B, P]).

test_matrixMultiplication(A, B, P, PerfFile) ->
    {Time, Result} = profile(A, B, P),
    file:write(PerfFile, io_lib:fwrite("~w\n", [Time])), Result.

test_looper(_, _, _, 0, _) ->
    ok;
test_looper(A, B, P, Index, PerfFile) ->
    test_matrixMultiplication(A, B, P, PerfFile),
    test_looper(A, B, P, Index-1, PerfFile).

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
    [[granted_int(Num) || Num <- string:tokens(Line, " ")]] ++ problemFromFileAux(Device, io:get_line(Device, "")).

problemFromFileAux(Device) ->
    problemFromFileAux(Device, io:get_line(Device, "")).

matrixFromFile(Filename) ->
    {ok, File} = file:open(Filename, [read]),
    Matrix = problemFromFileAux(File),
    file:close(File),
    Matrix.

test_loop(AFile, BFile, P, Repetitions, PerfFile) ->
    {ok, PFile} = file:open(PerfFile, [write]),
    Result = test_looper(matrixFromFile(AFile), matrixFromFile(BFile), granted_int(P), granted_int(Repetitions), PFile),
    file:close(PFile),
    Result.

test_loop([AFile, BFile, P, Repetitions, PerfFile]) ->
    test_loop(AFile, BFile, P, Repetitions, PerfFile).


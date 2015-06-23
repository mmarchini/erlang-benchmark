-module(matrixMultiplication).
-export([matrixMultiplication/2, test_loop/1, test_loop/4]).

matrixMultiplication(A, B) ->
    multi1(A, rotateMatrix(B)).

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

profile(A, B) ->
    timer:tc(matrixMultiplication, matrixMultiplication, [A, B]).

test_matrixMultiplication(A, B, PerfFile) ->
    {Time, Result} = profile(A, B),
    file:write(PerfFile, io_lib:fwrite("~w\n", [Time])), Result.

test_looper(_, _, 0, _) ->
    ok;
test_looper(A, B, Index, PerfFile) ->
    test_matrixMultiplication(A, B, PerfFile),
    test_looper(A, B, Index-1, PerfFile).

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
    [[granted_int(Num) || Num <- string:tokens(Line, ",")]] ++ problemFromFileAux(Device, io:get_line(Device, "")).

problemFromFileAux(Device) ->
    problemFromFileAux(Device, io:get_line(Device, "")).

matrixFromFile(Filename) ->
    {ok, File} = file:open(Filename, [read]),
    Matrix = problemFromFileAux(File),
    file:close(File),
    Matrix.

test_loop(AFile, BFile, Repetitions, PerfFile) ->
    {ok, PFile} = file:open(PerfFile, [write]),
    Result = test_looper(matrixFromFile(AFile), matrixFromFile(BFile), granted_int(Repetitions), PFile),
    file:close(PFile),
    Result.

test_loop([AFile, BFile, Repetitions, PerfFile]) ->
    test_loop(AFile, BFile, Repetitions, PerfFile).


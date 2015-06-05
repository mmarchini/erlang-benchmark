-module(parallelMatrixMultiplication).
-export([matrixMultiplication/3, matrixMultiplicationSpawn/3, scatter/2]).

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
    

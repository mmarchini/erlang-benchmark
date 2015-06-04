-module(matrixMultiplication).
-export([rotateMatrix/1, matrixMultiplication/2, vectorMultiplication/2, multi1/2, multi2/2]).

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
    

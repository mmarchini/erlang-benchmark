-module(queensProblem).
-import(lists, [seq/2]).
-export([queensResolver/1]).

% Tabuleiro NxN
% N² possibilidades de posicionamento inicial (primeira peça)
% Quantidade máxima de processos: N²
% Restrição de processos: N²/P deve ser um número interio

queensResolver(N) ->
    queensResolverAux(N, 0, 0).

queensResolverAux(N, _, Column) when (Column == N) ->
    [];
queensResolverAux(N, Row, Column) when Row == N ->
    queensResolverAux(N, 0, Column+1);
queensResolverAux(N, Row, Column) ->
    lists:umerge(evaluateSolution(N, Row, Column), queensResolverAux(N, Row+1, Column)).

evaluateSolution(N, Row, Column) -> 
    evaluator(seq(0, N-1) -- [Row], seq(0, N-1) -- [Column], [{Row, Column}]).

evaluator([], [], Pieces) ->
    [lists:keysort(2, Pieces)];
evaluator(Rows, Columns, _) when length(Rows) /= length(Columns) ->
    [];
evaluator(Rows, [Column | Columns], Pieces) ->
    lists:umerge([evaluator(Rows -- [Row], Columns, Pieces ++ [{Row, Column}]) || Row <- Rows, lists:all(fun ({PRow, PCol}) -> abs(PRow - Row) /= abs(PCol - Column) end, Pieces)]).


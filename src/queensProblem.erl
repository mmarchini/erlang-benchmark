-module(queensProblem).
-import(lists, [seq/2]).
-export([queensResolver/1, profile/1, test_loop/1, test_loop/3]).

% Tabuleiro NxN
% N² possibilidades de posicionamento inicial (primeira peça)
% Quantidade máxima de processos: N²
% Restrição de processos: N²/P deve ser um número interio

queensResolver(N) ->
    queensResolverAux(N, 0, 0).

queensResolverAux(N, _, Column) when (Column == N) ->
    [];
queensResolverAux(N, Row, Column) when Row == N ->
    [];%queensResolverAux(N, 0, Column+1);
queensResolverAux(N, Row, Column) ->
    evaluateSolution(N, Row, Column) ++ queensResolverAux(N, Row+1, Column).

evaluateSolution(N, Row, Column) -> 
    evaluator(seq(0, N-1) -- [Row], seq(0, N-1) -- [Column], [{Row, Column}]).

evaluator([], [], Pieces) ->
    [Pieces];
evaluator(Rows, [Column | Columns], Pieces) when length(Rows) == length(Columns)+1 ->
    lists:merge([evaluator(Rows -- [Row], Columns, Pieces ++ [{Row, Column}]) || Row <- Rows, lists:all(fun ({PRow, PCol}) -> abs(PRow - Row) /= abs(PCol - Column) end, Pieces)]).

%%%%%%%%%%%%%%%%%%%
%% PROFILE STUFF %%
%%%%%%%%%%%%%%%%%%%

profile(N) ->
    timer:tc(queensProblem, queensResolver, [N]).

test_queensProblem(N, PerfFile) ->
    {Time, _} = profile(N),
    file:write(PerfFile, io_lib:fwrite("~w\n", [Time])).

test_looper(_, 0, _) ->
    0;
test_looper(N, Index, PerfFile) ->
    test_queensProblem(N, PerfFile),
    test_looper(N, Index-1, PerfFile).

granted_int(Num) when erlang:is_integer(Num) -> 
    Num;
granted_int(Num) when erlang:is_list(Num) -> 
    {NewNum, _} = string:to_integer(Num),
    NewNum;
granted_int(Num) when erlang:is_atom(Num) -> 
    granted_int(erlang:atom_to_list(Num)).

test_loop(N, Repetitions, PerfFile) ->
    {ok, PFile} = file:open(PerfFile, [write]),
    Result=test_looper(granted_int(N), granted_int(Repetitions), PFile),
    file:close(PFile),
    Result.

test_loop([N, Repetitions, PerfFile]) ->
    test_loop(N, Repetitions, PerfFile).


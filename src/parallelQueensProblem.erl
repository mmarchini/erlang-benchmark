-module(parallelQueensProblem).
-import(lists, [seq/2]).
-export([queensResolver/2, queensResolverAux/4, queenResolverSpawn/5, test_loop/1, test_loop/4]).

% Tabuleiro NxN
% N² possibilidades de posicionamento inicial (primeira peça)
% Paralelismo será feito "distribuindo colunas"
% Quantidade máxima de processos: N
% Restrição de processos: N/P deve ser um número interio

gather([H|T]) ->
    receive
        {H, Ret} -> lists:umerge(Ret, gather(T))
    end;
gather([]) ->
    [].

queensResolver(N, P) when P > N ->
    error(processes_overflow);
queensResolver(N, P) when N rem P /= 0 ->
    error(invalid_processes);
queensResolver(N, P) ->
    Mult = (N div P),
    S = self(),
    gather([spawn(parallelQueensProblem, queenResolverSpawn, [S, N, (Index+1)*Mult, 0, Index*Mult]) || Index <- seq(0, P-1)]).

queenResolverSpawn(S, N, ColEnd, Row, Column) ->
    S ! {self(), queensResolverAux(N, ColEnd, Row, Column)}.

queensResolverAux(_, ColEnd, _, Column) when (Column == ColEnd) ->
    [];
queensResolverAux(N, ColEnd, Row, Column) when Row == N ->
    queensResolverAux(N, ColEnd, 0, Column+1);
queensResolverAux(N, ColEnd, Row, Column) ->
    lists:umerge(evaluateSolution(N, Row, Column), queensResolverAux(N, ColEnd, Row+1, Column)).

evaluateSolution(N, Row, Column) -> 
    evaluator(seq(0, N-1) -- [Row], seq(0, N-1) -- [Column], [{Row, Column}]).

evaluator([], [], Pieces) ->
    [lists:keysort(2, Pieces)];
evaluator(Rows, Columns, _) when length(Rows) /= length(Columns) ->
    [];
evaluator(Rows, [Column | Columns], Pieces) ->
    lists:umerge([evaluator(Rows -- [Row], Columns, Pieces ++ [{Row, Column}]) || Row <- Rows, lists:all(fun ({PRow, PCol}) -> abs(PRow - Row) /= abs(PCol - Column) end, Pieces)]).

%%%%%%%%%%%%%%%%%%%
%% PROFILE STUFF %%
%%%%%%%%%%%%%%%%%%%

profile(N, P) ->
    timer:tc(parallelQueensProblem, queensResolver, [N, P]).

test_queensProblem(N, P, PerfFile) ->
    {Time, _} = profile(N, P),
    file:write(PerfFile, io_lib:fwrite("~w\n", [Time])).

test_looper(_, _, 0, _) ->
    ok;
test_looper(N, P, Index, PerfFile) ->
    test_queensProblem(N, P, PerfFile),
    test_looper(N, P, Index-1, PerfFile).

granted_int(Num) when erlang:is_integer(Num) -> 
    Num;
granted_int(Num) when erlang:is_list(Num) -> 
    {NewNum, _} = string:to_integer(Num),
    NewNum;
granted_int(Num) when erlang:is_atom(Num) -> 
    granted_int(erlang:atom_to_list(Num)).

test_loop(N, P, Repetitions, PerfFile) ->
    {ok, PFile} = file:open(PerfFile, [write]),
    Result=test_looper(granted_int(N), granted_int(P), granted_int(Repetitions), PFile),
    file:close(PFile),
    Result.

test_loop([N, P, Repetitions, PerfFile]) ->
    test_loop(N, P, Repetitions, PerfFile).


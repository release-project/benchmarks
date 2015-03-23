-module(ant_master).
-export([run/7]).
-include("types.hrl").

-spec best_solution ({solution(), pid()}, {solution(),pid()}|none) -> {solution(), pid()}.

best_solution(Solution, none) -> Solution;
best_solution(S1 = {{Cost1, _}, _}, S2 = {{Cost2, _}, _}) ->
    if
	Cost1 =< Cost2 -> S1;
	true -> S2
    end.
% The stuff with the pids is just so that we can see who's produced the best solution
    

-spec collect_colonies(non_neg_integer(), {solution(), pid()} | none) -> {solution(), pid()}.

collect_colonies (0,{Best, Pid}) -> 
    {Best, Pid}; 
collect_colonies (Num_left, Current_Best) -> % or use lists:foldl.
    receive
	{colony_done, {New_Solution, Pid}} ->
%	    {Cost, _} = New_Solution,
%	    io:format("Solution ~p from ~p~n", [Cost, Pid]),
	    collect_colonies(Num_left-1, best_solution ({New_Solution, Pid}, Current_Best))
    end.


-spec loop(non_neg_integer(), non_neg_integer(), [pid()], {solution(), pid()} | none, inputs(), params()) -> solution().

loop (0, _Repl, Colonies, {Best_solution, _}, _Inputs, _Params) -> 
    lists:foreach (fun (Pid) -> Pid ! {self(), stop_ants} end, Colonies),
    Best_solution;

loop (N, Repl, Colonies, Best_solution, Inputs, Params) -> 
%    io:format ("Colonies -> ~p~n", [Colonies]),
    lists:foreach (fun(Pid) -> Pid ! {self(), run} end, Colonies),
    {{Cost, _} = New_Solution, Best_Pid} = collect_colonies (length(Colonies)*Repl, Best_solution),
 
% Colonies return result Repl times so that we can generate large numbers of messages for 
% benchmarking purposes.  We compare all solutions (even repeated ones) with best solution.
% This gives us times which are (approximately) similar to what we'd get if we increased
% the number of colonies bu a factor of Repl.

% Note that we're using the best solution from the previous generation.
% We could also try starting anew, relying on the pheromone matrix to guide 
% ants back to earlier best solutions if they can't do any better.

%    Improved_Solution = localsearch:vnd_loop(New_Solution, Inputs, Params),
% local search now done in colony, after every generation of ants.

    Improved_Solution = New_Solution,
    {Cost, _} = Improved_Solution,

    #params{verbose=Verbose} = Params,
    case Verbose of
	true -> io:format ("Iteration ~p: cost = ~p from ~p~n", [N, Cost, Best_Pid]);
	false -> ok
    end,

    lists:foreach (fun(Pid) -> 
			   if Pid =/= Best_Pid ->
				   Pid ! {self(), {update, Improved_Solution}};
			      true -> ok
			   end
		   end, Colonies),

    loop (N-1, Repl, Colonies, {Improved_Solution, Best_Pid}, Inputs, Params).
    

-spec spawn_colonies (list(), [atom()]) -> [pid()].
spawn_colonies (Args, Nodes) ->
    lists:map (fun(H) -> spawn (H, ant_colony, init, Args) end, Nodes).

-spec run (numjobs(), pos_integer(), pos_integer(), pos_integer(), inputs(), params(), list()) -> solution().
run(Num_Jobs, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes) ->
    #params{heuristic=Heuristic, repl=Repl} = Params,
    Colonies = case Heuristic of
		   mixed -> 
		       Half = length(Nodes) div 2,
		       {Nodes_mdd, Nodes_au} = lists:split (Half, Nodes),
		       Args_mdd = [Num_Ants, Iter_Local, Num_Jobs, Inputs, Params#params{heuristic=mdd}],
		       Args_au  = [Num_Ants, Iter_Local, Num_Jobs, Inputs, Params#params{heuristic=au}],
		       spawn_colonies(Args_mdd, Nodes_mdd) ++ spawn_colonies(Args_au, Nodes_au);
		   
		   _ -> Args = [Num_Ants, Iter_Local, Num_Jobs, Inputs, Params],
			spawn_colonies (Args, Nodes)
	       end,
    loop (Iter_Global, Repl, Colonies, none, Inputs, Params).
				 



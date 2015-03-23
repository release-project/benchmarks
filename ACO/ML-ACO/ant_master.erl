-module(ant_master).
-export([run/9]).
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

loop (_Duplicating, 0, Colonies, {Best_solution, _}, _Inputs, _Params) -> 
    lists:foreach (fun (Pid) -> Pid ! {self(), stop_ants} end, Colonies),
    lists:foreach (fun (ID) -> receive ok -> ID end end, lists:seq(1,length(Colonies))), %% wait to collect all the acknowledges
    Best_solution;

loop (Duplicating, N, Colonies, Best_solution, Inputs, Params) -> 
    lists:foreach (fun(Pid) -> Pid ! {self(), run} end, Colonies),
    {{Cost, _} = New_Solution, Best_Pid} = collect_colonies (length(Colonies)*Duplicating, Best_solution),

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
				   %Pid ! {self(), {update, Improved_Solution}};
				   Pid ! {Best_Pid, {update, Improved_Solution}};
			      true -> ok
			   end
		   end, Colonies),
    loop (Duplicating, N-1, Colonies, {Improved_Solution, Best_Pid}, Inputs, Params).

run(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes) ->
	case ant_submaster:find_max_level(length(Nodes),Num_Processes) of
	{_MaxLevels=0,_NodeLength=0} -> 
		io:format ("Number of nodes (~p) with ~p processes per node is not enough to have any submaster node~n", [length(Nodes),Num_Processes]),
		Colonies = lists:map (fun(H) -> spawn (H, ant_colony, init,
						   [Num_Ants, Duplicating, Iter_Local, Num_Jobs, 
							Inputs, Params]) end, Nodes),
		loop (Duplicating, Iter_Global, Colonies, none, Inputs, Params);
	{MaxLevels,NodeLength} -> 
		io:format ("We have ~p levels for ~p nodes and ~p processes per node (~p nodes will be used)  ~n", [MaxLevels,length(Nodes),Num_Processes,NodeLength]),
		{UsingNodes,_Rest}=lists:split(NodeLength, Nodes),
		[Head|_Tail]=UsingNodes,
		ChildProcesses=lists:map (fun(ProcessIndex) ->
		spawn(Head,ant_submaster,run,[Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params,UsingNodes, _CurrentLevel=1, MaxLevels, _NodeIndex=1, ProcessIndex])
		end, lists:seq(1,Num_Processes)),
		loop (1, Iter_Global, ChildProcesses, none, Inputs, Params)
	end.


%% Creates a multi-level tree of sub-master nodes to collect the results from Colonies. 

%% Author: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>
%% RELEASE project (http://www.release-project.eu/)

-module(ant_submaster).
-compile(export_all).
-include("types.hrl").


%% compares two solutions and returns the better one
%-spec best_solution ({solution(), pid()}, {solution(),pid()}|none) -> {solution(), pid()}.
best_solution(Solution, none) -> Solution;
best_solution(none, Solution) -> Solution;
best_solution(S1 = {{Cost1, _}, _}, S2 = {{Cost2, _}, _}) ->
    if
	Cost1 =< Cost2 -> S1;
	true -> S2
    end.
% The stuff with the pids is just so that we can see who's produced the best solution
    

%-spec collect_ants(non_neg_integer(), solution() | none) -> solution().

%% collecting the collonies results from child nodes
collect_childs (0,{Best, Pid}) -> 
    {Best, Pid}; 
collect_childs (Num_left, Current_Best) -> % or use lists:foldl.
    receive
	{colony_done, {New_Solution, Pid}} ->
		collect_childs(Num_left-1, best_solution ({New_Solution, Pid}, Current_Best))
    end.

%% processing and passing all the messages from parent to childs and vice versa
loop(ChildProcesses, Best_solution, Duplicating) ->
    receive 
	{Parent, run} -> 
		lists:foreach (fun(Pid) -> Pid ! {self(), run} end, ChildProcesses),
		{New_Solution, Best_Pid} = collect_childs (length(ChildProcesses)*Duplicating, Best_solution),
	    Parent ! {colony_done, {New_Solution, Best_Pid}},
	    loop(ChildProcesses, {New_Solution, Best_Pid}, Duplicating);

	{Best_Pid, {update, Global_Best_Solution}} -> 
	
		lists:foreach (fun(Pid) -> 
			if Pid =/= Best_Pid ->
				%Pid ! {self(), {update, Improved_Solution}};
				Pid ! {Best_Pid, {update, Global_Best_Solution}};
				true -> ok
			end
		end, ChildProcesses),

		%lists:foreach (fun(Pid) -> Pid ! {self(), {update, Global_Best_Solution}} end, ChildProcesses),
		loop(ChildProcesses, {Global_Best_Solution,Best_Pid},Duplicating);

	{Parent, stop_ants} ->  % called by master at end of main loop
	    lists:foreach (fun (Pid) -> Pid ! {self(), stop_ants} end, ChildProcesses),
	    lists:foreach (fun (ID) -> receive ok -> ID end end, lists:seq(1,length(ChildProcesses))),
	    Parent ! ok
    end.

%% creates appropriate processes on child nodes
run(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex,ProcessIndex) ->
    if 
		CurrentLevel==MaxLevel-1 ->
			ChildNodes=get_childs(Num_Processes,CurrentLevel,MaxLevel,Nodes,NodeIndex),
			QuotaOfEachProcess=round(length(ChildNodes)/Num_Processes),
			Start=(ProcessIndex-1)*QuotaOfEachProcess+1,
			MyChildNodes=lists:sublist(ChildNodes, Start, Num_Processes),
			Colonies = lists:map (fun(H) -> spawn (H, ant_colony, init,
							   [Num_Ants, Duplicating, Iter_Local, Num_Jobs, 
								Inputs, Params]) end, MyChildNodes),
			loop(Colonies,none,Duplicating);
		true ->
			ChildNodes=get_childs(Num_Processes,CurrentLevel,MaxLevel,Nodes,NodeIndex),
			ChildNode=lists:nth(ProcessIndex, ChildNodes),
			ChildProcesses = lists:map(
			fun(NextProcessIndex) -> 
			spawn(ChildNode,ant_submaster,run,[Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params,Nodes, CurrentLevel+1, MaxLevel,Num_Processes*(NodeIndex-1)+ProcessIndex,NextProcessIndex])
			end, 
			lists:seq(1,Num_Processes)),
			loop (ChildProcesses,none,1)
	end.

%% auxiliary functions to create a tree of submaster nodes

%% calculates the number of levels in the tree
%% each node has "Num_Processes" processes
%% each process supervise one node in lower level, except the last level that each process supervises "Num_Processes" number of nodes

%% calculates the tree level based on the total number of nodes and node degree (degree of vertices)
find_max_level(Num_Nodes,Num_Processes) ->
	S=speculate_level(Num_Nodes,Num_Processes),
	find_max_level(Num_Nodes,Num_Processes,S).

find_max_level(Num_Nodes,Num_Processes,Speculated_level) ->
	Result=calc_formula(Num_Processes,Speculated_level),
	if
		Result ==0 -> {_Level=0,_NumNodes=0};
		Result =< Num_Nodes -> {Speculated_level,Result};
		true -> find_max_level(Num_Nodes,Num_Processes,Speculated_level-1) 
	end.

%% finds the largest possible power for Num_Processes to be less than Num_Nodes
speculate_level(Num_Nodes,Num_Processes)->
	speculate_level(Num_Nodes,Num_Processes,0).

speculate_level(Num_Nodes,Num_Processes,Acc)->
	Result=math:pow(Num_Processes,Acc),
	if
		 Result<Num_Nodes ->
			 speculate_level(Num_Nodes,Num_Processes,Acc+1);
		true ->
			round(Acc-1)
	end.

%% calculates 1+P^1+P^2+...+P^(N-2)+P^N
calc_formula(Num_Processes,Level) when Level>=2 ->
	calc_formula(Num_Processes,Level,_Acc=0,_Current_level=0);

%% No submaster can be allocated
calc_formula(_Num_Processes,_Level) ->
	0.

calc_formula(Num_Processes,Last_Level,Acc,Current_level) when Current_level=<Last_Level ->
	Num_Nodes=math:pow(Num_Processes,Current_level),
	case Current_level+2 of
		Last_Level ->
			calc_formula(Num_Processes,Last_Level,Acc+Num_Nodes,Current_level+2);
		_-> 
			calc_formula(Num_Processes,Last_Level,Acc+Num_Nodes,Current_level+1)
	end;

calc_formula(_Num_Processes,_Last_Level,Acc,_Current_level) ->
	round(Acc).

%% returns number of nodes for a specific level
nodes_in_level(Num_Processes, Level) ->
	round(math:pow(Num_Processes,Level-1)).

%% returns all the child nodes of a specific node. Node is specified by its level and its index in the level
%% How to test: Nodes=lists:seq(1, 277). list_to_tuple(ant_submaster:get_childs(4,3,4,Nodes,1)).
get_childs(Num_Processes,CurrentLevel,MaxLevel,Nodes,IndexInLevel) -> %when CurrentLevel<MaxLevel ->
	Num_Node_in_Current_Level=nodes_in_level(Num_Processes,CurrentLevel),
	if
		Num_Node_in_Current_Level<IndexInLevel ->
			throw(index_in_level_is_more_than_num_nodes_in_level);
		true -> ok
	end,
	if
		CurrentLevel>=MaxLevel -> 
			Num_Node_in_Next_Level=0,
			throw(current_level_must_be_less_than_max_level);
		CurrentLevel==MaxLevel-1 ->
			Num_Node_in_Next_Level=nodes_in_level(Num_Processes,CurrentLevel+2);			
		true->
			Num_Node_in_Next_Level=nodes_in_level(Num_Processes,CurrentLevel+1)
	end,
	Childs_Per_Node=Num_Node_in_Next_Level/Num_Node_in_Current_Level,
	Index_For_Next_Line=Childs_Per_Node*(IndexInLevel-1)+1,
	After_Me_This_Level=Num_Node_in_Current_Level-IndexInLevel,
	Child_index=level_index(Num_Processes,CurrentLevel)+IndexInLevel+After_Me_This_Level+Index_For_Next_Line,lists:sublist(Nodes, round(Child_index), round(Childs_Per_Node)).

%% returns the index of the first node for a specific level
level_index(Num_Processes,Level) ->
	level_index(Num_Processes,Level,0,0).

level_index(Num_Processes,Level,Acc,CurrentLevel) when CurrentLevel<Level-1 ->
	R=math:pow(Num_Processes,CurrentLevel),
	level_index(Num_Processes,Level,Acc+R,CurrentLevel+1);

level_index(_Num_Processes,_Level,Acc,_CurrentLevel)->
Acc.



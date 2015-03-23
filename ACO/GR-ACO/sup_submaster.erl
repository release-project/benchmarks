%% Supervises a number of sub-master processes locally

%% Author: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>

%% RELEASE project (http://www.release-project.eu/)

-module(sup_submaster).
-include("types.hrl").
-compile(export_all).


run(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params,Nodes, CurrentLevel, MaxLevel,NodeIndex, ProcessIndex, Recovery, {ParentName, ParentPid}) ->
	process_flag(trap_exit, true),
	if 
		Recovery==false ->
			io:format("creating a sup_submaster process on node ~p ~n" , [node()]);
		true ->
			io:format("recovery a sup_submaster process on node ~p ~n" , [node()])
	end,
	ChildProcesses = lists:map(
	fun(NextProcessIndex) -> 
		ChildProcessName=ant_submaster:generate_submaster_name(CurrentLevel+1, Num_Processes*(NodeIndex-1)+ProcessIndex, NextProcessIndex),
		case whereis(ChildProcessName) of
		undefined -> 
			ChildPID=spawn_link(ant_submaster,run,[Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, NextProcessIndex, ChildProcessName, Recovery]),
			{ChildPID, ChildProcessName, NextProcessIndex};
		ChildPID -> link(ChildPID),
			{ChildPID, ChildProcessName, NextProcessIndex}
		end
	end,
	lists:seq(1, Num_Processes)),
	ReliablePid=get_Pid_reliably_sgroup(ParentName, ParentPid),
	ReliablePid!{sup_submaster,ChildProcesses},
	monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, ChildProcesses, _Terminating=false).

monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, ChildProcesses, Terminating) ->
	receive
		{ParentPid, stop_ants} ->
			monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, ChildProcesses, _Terminating=true);
		{after_recovery,NewParentPid} ->
			NewParentPid! {sup_submaster,ChildProcesses},
			monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, NewParentPid}, ChildProcesses, Terminating);
		{'EXIT', FailedPID, Reason} ->
			if
				Terminating==false ->
					io:format("Failure of a sub-master process with PID ~p and reason ~p ~n", [FailedPID, Reason]),
					case ant_master:get_process_name(FailedPID, ChildProcesses) of
						not_found ->
							io:format("No recovery for sub-master process ~p is needed on node ~p ~n", [FailedPID, node()]),
							monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, ChildProcesses, Terminating);
						{ChildProcessName, NextProcessIndex} ->
							NewPID=spawn_link(ant_submaster,run,[Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, NextProcessIndex, ChildProcessName, _Recovery=true]),
							NewChildProcesses=ant_master:update_process_PID(ChildProcessName, NewPID, ChildProcesses),
							io:format("recovery of a sub-master process ~p on node ~p by new length ~p and new PID ~p ~n", [FailedPID, node(), length(NewChildProcesses), NewPID]),
							ReliablePid=get_Pid_reliably_sgroup(ParentName, ParentPid),
							ReliablePid!{afterFailure,NewChildProcesses, FailedPID},
							monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, NewChildProcesses, Terminating) 
					end;
				true ->
					if
						Reason=='normal' ->
							monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, ChildProcesses, Terminating);
						true->
							ReliablePid=get_Pid_reliably_sgroup(ParentName, ParentPid),
							ReliablePid! {'EXIT', FailedPID, Reason},
							monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, ChildProcesses, Terminating)
					end
			end
	end.

get_Pid_reliably_sgroup(ProcessName, ProcessPid) when not is_integer(ProcessPid)->
	if
		is_pid(ProcessPid)==true ->
			ProcessPid;
		true->
			get_Pid_reliably_sgroup(ProcessName, 3)
	end;

get_Pid_reliably_sgroup(ProcessName, 0)->
	io:format("After 3 times attempts, ant_submaster process is not available in global group with name ~p ~n", [ProcessName]),
	undefined;

get_Pid_reliably_sgroup(ProcessName, Num)->
	case global:whereis_name(ProcessName) of
	undefined -> 
		timer:sleep(1),
		get_Pid_reliably_sgroup(ProcessName, Num-1);
	Pid -> 
		if
			is_pid(Pid)==true ->
				Pid;
			true->
				timer:sleep(1),
				get_Pid_reliably_sgroup(ProcessName, Num-1)
		end
	end.

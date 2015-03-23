%% Supervises a number of sub-master processes locally 

%% Author: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>
%% RELEASE project (http://www.release-project.eu/)

-module(sup_submaster).
-include("types.hrl").
-compile(export_all).


run(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, GroupName) ->
	process_flag(trap_exit, true),
	#params{printer=Printer} = Params,
	if 
		Printer==false->
			put(parent_printer, no_print);
		true->
			put(parent_printer, Params#params.parent_printer)
	end,
	case ant_submaster:exist_group_with_wait(GroupName) of
		true-> 
			ok;
		_->
			?Print(io_lib:format("sd_erlang: group name ~p is not available on node sup_submaster process ~p " , [GroupName, node()]))
	end,
	if 
		Recovery==false ->
			?Print(io_lib:format("creating a sup_submaster process on node ~p " , [node()]));
		true ->
			?Print(io_lib:format("recovery a sup_submaster process on node ~p " , [node()]))
	end,
	ChildProcesses = lists:map(
	fun(NextProcessIndex) -> 
		ChildProcessName=ant_submaster:generate_submaster_name(CurrentLevel, NodeIndex, NextProcessIndex),
		%case s_group:whereis_name(GroupName, ChildProcessName) of
		case whereis(ChildProcessName) of
		undefined -> 
			ChildPID=spawn_link(ant_submaster,run,[Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, NextProcessIndex, ChildProcessName, Recovery]),
			%s_group:register_name(GroupName, ChildProcessName, ChildPID),
			register(ChildProcessName, ChildPID),
			{ChildPID, ChildProcessName, NextProcessIndex};
		ChildPID -> link(ChildPID),
			{ChildPID, ChildProcessName, NextProcessIndex}
		end
	end,
	lists:seq(1, Num_Processes)),
	ReliablePid=get_Pid_reliably_sgroup(GroupName, ParentName, ParentPid),
	ReliablePid!{sup_submaster,ChildProcesses},
	%Parent!{sup_submaster,ChildProcesses},
	monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, GroupName, ChildProcesses, _Terminating=false).

monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, GroupName, ChildProcesses, Terminating) ->
	receive
		{ParentPid, stop_ants} ->
			monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, GroupName, ChildProcesses, _Terminating=true);
		{after_recovery,NewParentPid} ->
			NewParentPid! {sup_submaster,ChildProcesses},
			monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, NewParentPid}, GroupName, ChildProcesses, Terminating);
		{'EXIT', FailedPID, Reason} ->
			if
				Terminating==false ->
					?Print(io_lib:format("Failure of a sub-master process with PID ~p and reason ~p ", [FailedPID, Reason])),
					case ant_master:get_process_name(FailedPID, ChildProcesses) of
						not_found ->
							?Print(io_lib:format("No recovery for sub-master process ~p is needed on node ~p ", [FailedPID, node()])),
							monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, GroupName, ChildProcesses, Terminating);
						{ChildProcessName, NextProcessIndex} ->
							NewPID=spawn_link(ant_submaster,run,[Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, NextProcessIndex, ChildProcessName, _Recovery=true]),
							s_group:register_name(GroupName, ChildProcessName, NewPID),
							NewChildProcesses=ant_master:update_process_PID(ChildProcessName, NewPID, ChildProcesses),
							?Print(io_lib:format("recovery of a sub-master process ~p on node ~p by new length ~p and new PID ~p ", [FailedPID, node(), length(NewChildProcesses), NewPID])),
							ReliablePid=get_Pid_reliably_sgroup(GroupName, ParentName, ParentPid),
							ReliablePid!{afterFailure,NewChildProcesses, FailedPID},
							monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, GroupName, NewChildProcesses, Terminating) 
					end;
				true ->
					ReliablePid=get_Pid_reliably_sgroup(GroupName, ParentName, ParentPid),
					ReliablePid! {'EXIT', FailedPID, Reason},
					monitor(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex, Recovery, {ParentName, ParentPid}, GroupName, ChildProcesses, Terminating)
			end
	end.

get_Pid_reliably_sgroup(GroupName, ProcessName, ProcessPid) when not is_integer(ProcessPid)->
	if
		is_pid(ProcessPid)==true ->
			ProcessPid;
		true->
			get_Pid_reliably_sgroup(GroupName, ProcessName, 3)
	end;

get_Pid_reliably_sgroup(GroupName, ProcessName, 0)->
	?Print(io_lib:format("After 3 times attempts, ant_submaster process is not available in group ~p with name ~p", [GroupName, ProcessName])),
	undefined;
get_Pid_reliably_sgroup(GroupName, ProcessName, Num)->
	case s_group:whereis_name(GroupName, ProcessName) of
	undefined -> 
		timer:sleep(1),
		get_Pid_reliably_sgroup(GroupName, ProcessName, Num-1);
	Pid -> 
		if
			is_pid(Pid)==true ->
				Pid;
			true->
				timer:sleep(1),
				get_Pid_reliably_sgroup(GroupName, ProcessName, Num-1)
		end
	end.

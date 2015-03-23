%% A single ant colony: spawns a set of ants, loops round getting them to construct new solutions

-module(ant_colony).
-export([init/8]).

-include ("types.hrl").

% The value 'none' below is used to represent the state at the very start
% when we don't have a current best solution.

-spec best_solution (solution(), solution()|none) -> solution().

best_solution(Solution, none) -> Solution;
best_solution(none, Solution) -> Solution; % added just for test amir
best_solution(S1 = {Cost1, _}, S2 = {Cost2, _}) ->
    if
	Cost1 =< Cost2 -> S1;
	true -> S2
    end.
    

-spec collect_ants(non_neg_integer(), solution() | none, list(), colony_state(), list()) -> solution().

collect_ants (0,Best, Ants, _Colony_state, _ReceivedPIDs) -> 
{Ants, Best}; 
collect_ants (Num_left, Current_Best, Ants, Colony_state, ReceivedPIDs) -> % or use lists:foldl.
    receive
	{ant_done, PID, New_Solution} ->
	    collect_ants(Num_left-1, best_solution (New_Solution, Current_Best), Ants, Colony_state, [PID]++ReceivedPIDs);
	{'EXIT', FailedPID, Reason} -> 
		io:format ("Failure of an ant process with PID ~p and reason ~p ~n", [FailedPID, Reason]),
		case recover_childs(FailedPID, Colony_state, Ants) of
		{no_updated, Ants}->
			collect_ants(Num_left, Current_Best, Ants, Colony_state, ReceivedPIDs);
		{updated, NewAnts}->
			case lists:member(FailedPID, ReceivedPIDs) of
				true-> collect_ants(Num_left, Current_Best, NewAnts, Colony_state , ReceivedPIDs);
				_->    collect_ants(Num_left-1, Current_Best, NewAnts, Colony_state, ReceivedPIDs)
			end
		end
	after
		?TimeOut ->
			io:format ("Timeout has occurred on colony process ~p on node ~p ~n", [self(), node()]),
			collect_ants (Num_left, Current_Best, Ants, Colony_state, ReceivedPIDs)
    end.

%-spec aco_loop (pos_integer(), [pid()], solution() | none) -> solution().
aco_loop (0, _, _, _, Ants, Best_Solution, Colony_state) -> 
    {Ants, Best_Solution, Colony_state};
aco_loop (Iter_Local, Num_Jobs, Inputs, Params, Ants, Best_Solution, Colony_state) ->
    lists:foreach (fun (Pid) -> Pid ! {self(), find_solution} end, Ants),
    {New_Ants, New_Solution} = collect_ants(length(Ants), none, Ants, Colony_state, _ReceivedPIDs=[]),
%    io:format ("Colony ~p got new solution~n",[self()]),

    {Cost1, _} = New_Solution,
    Improved_Solution = localsearch:vnd_loop(New_Solution, Inputs, Params),

    {Cost, _} = Improved_Solution,

    #params{vverbose=Vverbose} = Params,
    case Vverbose of
	true -> io:format ("Colony ~p: cost = ~p -> ~p~n", [self(), Cost1, Cost]);
	false -> ok
    end,
    
    New_Best_Solution = best_solution (Improved_Solution, Best_Solution),

    ok = update:update_pheromones(Num_Jobs, New_Best_Solution, Params),
    aco_loop (Iter_Local-1, Num_Jobs, Inputs, Params, New_Ants, New_Best_Solution, Colony_state).

main (Duplicating, Iter_Local, Num_Jobs, Inputs, Params, Ants, Current_Best, Colony_state) ->
    receive 
	{Master, run} -> 
%	    io:format ("Colony ~p got run from ~p~n", [self(),From]),
	    {New_Ants, New_Solution, New_Colony_state} = aco_loop (Iter_Local, Num_Jobs, 
				      Inputs, Params,
				      Ants, Current_Best, Colony_state),
%	    io:format ("tau[1] = ~p~n", [ets:lookup(tau,1)]),
%	    io:format ("Colony ~p returning solution ~p~n", [self(), New_Solution]),

	    %Master ! {colony_done, {New_Solution, self()}},
	    lists:foreach (fun (_ID) -> Master ! {colony_done, {New_Solution, self()}} end, lists:seq(1, Duplicating)),
	    main (Duplicating, Iter_Local, Num_Jobs, Inputs, Params, New_Ants, New_Solution, New_Colony_state);

	{_Master, {update, Global_Best_Solution}} -> 
	    update:update_pheromones (Num_Jobs, Global_Best_Solution, Params),
	    main (Duplicating, Iter_Local, Num_Jobs, Inputs, Params, Ants, Current_Best, Colony_state);

	%% NOTE!!!! Having updated tau according to the global best solution, the
	%% colony carries on with its OWN current best solution.  We could also 
	%% carry on with the global best solution:  might this lead to stagnation?

	{Master, stop_ants} ->  % called by master at end of main loop
	    ok = lists:foreach (fun (Pid) -> Pid ! {self(), stop} end, Ants),
	    ets:delete(tau),
	    Master ! ok
	%Msg -> error ({"Unexpected message in ant_colony:main", Msg})
    end.



%% For each VM, set up the ETS table and spawn the required number of ants.
%% Then get each VM to perform (say) 50 iterations and report best solution.
%% Send overall best solution to all VMs (or maybe get the owner of the best 
%% solution to send it to everyone), and start a new round of iterations.
%% Stop after a certain number of global iterations.

-spec spawn_ants(integer(), list()) -> [pid()].
spawn_ants(Num_Ants,Args) -> lists:map (fun (_) -> spawn_link(ant, start, Args) end, lists:seq(1,Num_Ants)).

-spec init(integer(), integer(), integer(), numjobs(), inputs(), params(), list(), boolean) -> pid(). 
init (Num_Ants, Duplicating, Iter_Local, Num_Jobs, Inputs, Params, ProcessName, Recovery) ->
	process_flag(trap_exit, true),
	global:register_name(ProcessName,self()),
    % Set up ets table: visible to all ants
    case lists:member(tau, ets:all()) of
		true -> %ok;  % Already exists: maybe left over from interrupted run.
			ok;
		false -> ets:new(tau, [set, public, named_table])
	  end,
    #params{tau0=Tau0} = Params,
    Tau = lists:map(fun(I) -> {I, util:make_tuple(Num_Jobs, Tau0)} end, lists:seq(1, Num_Jobs)),
    ets:insert(tau, Tau),
    
%    io:format("~p~n", [ets:tab2list(tau)]),

	Colony_state=#colony_state{num_Jobs=Num_Jobs, inputs=Inputs, params=Params},
    Ants = spawn_ants(Num_Ants, [Num_Jobs, Inputs, Params]),

	#params{chaos=Chaos} = Params,
	if 
		Chaos==true andalso Recovery==false->
			Chaos_starter=whereis(chaos_starter),
			io:format("ant colony - sending ~p pids from node ~p to ~p ~n" , [length(Ants), node(), Chaos_starter]),
			Chaos_starter! {pids, Ants};
		true ->
			ok
	end,


    main(Duplicating, Iter_Local, Num_Jobs, Inputs, Params, Ants, none, Colony_state),
    self().

    % We could also put Num_Jobs, Durations, Weights, Deadlines, 
    % Alpha, Beta, Q0 (all read_only) in an ETS table and reduce 
    % the number of parameters in a lot of function calls.

%% Recovers a failed process
recover_childs(FailedPID, Colony_state, Ants) ->
	#colony_state{num_Jobs=Num_Jobs, inputs=Inputs, params=Params}=Colony_state,
	case lists:member(FailedPID, Ants) of
		true ->
			Index=ant_master:index_of(FailedPID, Ants),
			PID=spawn_link(ant, start, [Num_Jobs, Inputs, Params]),
			util:send_pid(PID),
			NewAnts=lists:sublist(Ants,1,Index-1)++[PID]++lists:sublist(Ants,Index+1,length(Ants)),
			io:format ("recovery of ant process ~p on node ~p by new length ~p and new PID ~p ~n", [FailedPID, node(), length(NewAnts), PID]),
			{updated, NewAnts};
		_->
			io:format ("No recovery for ant process ~p is needed on node ~p ~n", [FailedPID, node()]),
			{no_updated, Ants}
	end.

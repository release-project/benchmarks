%% A single ant colony: spawns a set of ants, loops round getting them to construct new solutions

-module(ant_colony).
-export([init/6]).

-include ("types.hrl").

% The value 'none' below is used to represent the state at the very start
% when we don't have a current best solution.

-spec best_solution (solution(), solution()|none) -> solution().

best_solution(Solution, none) -> Solution;
best_solution(S1 = {Cost1, _}, S2 = {Cost2, _}) ->
    if
	Cost1 =< Cost2 -> S1;
	true -> S2
    end.
    

-spec collect_ants(non_neg_integer(), solution() | none) -> solution().

collect_ants (0,Best) -> Best; 
collect_ants (Num_left, Current_Best) -> % or use lists:foldl.
    receive
	{ant_done, New_Solution} ->
%	    io:format ("collect_ants ~p: got solution ~p~n", [self(), Num_left]),
	    collect_ants(Num_left-1, best_solution (New_Solution, Current_Best))
    end.

%-spec aco_loop (pos_integer(), [pid()], solution() | none) -> solution().
aco_loop (0, _, _, _, _, Best_Solution) -> 
    Best_Solution;
aco_loop (Iter_Local, Num_Jobs, Inputs, Params, Ants, Best_Solution) ->
    lists:foreach (fun (Pid) -> Pid ! {self(), find_solution} end, Ants),
    New_Solution = collect_ants(length(Ants), none),
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
    aco_loop (Iter_Local-1, Num_Jobs, Inputs, Params, Ants, New_Best_Solution).
 
main (Duplicating, Iter_Local, Num_Jobs, Inputs, Params, Ants, Current_Best) ->
    receive 
	{Master, run} -> 
%	    io:format ("Colony ~p got run from ~p~n", [self(),From]),
	    New_Solution = aco_loop (Iter_Local, Num_Jobs, 
				      Inputs, Params,
				      Ants, Current_Best),
%	    io:format ("tau[1] = ~p~n", [ets:lookup(tau,1)]),
%	    io:format ("Colony ~p returning solution ~p~n", [self(), New_Solution]),

	    %Master ! {colony_done, {New_Solution, self()}},
	    lists:foreach (fun (_ID) -> Master ! {colony_done, {New_Solution, self()}} end, lists:seq(1, Duplicating)),
	    main (Duplicating, Iter_Local, Num_Jobs, Inputs, Params, Ants, New_Solution);

	{_Master, {update, Global_Best_Solution}} -> 
	    update:update_pheromones (Num_Jobs, Global_Best_Solution, Params),
	    main (Duplicating, Iter_Local, Num_Jobs, Inputs, Params, Ants, Current_Best);

	%% NOTE!!!! Having updated tau according to the global best solution, the
	%% colony carries on with its OWN current best solution.  We could also 
	%% carry on with the global best solution:  might this lead to stagnation?

	{Master, stop_ants} ->  % called by master at end of main loop
	    ok = lists:foreach (fun (Pid) -> Pid ! {self(), stop} end, Ants),
	    ets:delete(tau),
	    Master ! ok;
	Msg -> error ({"Unexpected message in ant_colony:main", Msg})
    end.



%% For each VM, set up the ETS table and spawn the required number of ants.
%% Then get each VM to perform (say) 50 iterations and report best solution.
%% Send overall best solution to all VMs (or maybe get the owner of the best 
%% solution to send it to everyone), and start a new round of iterations.
%% Stop after a certain number of global iterations.

-spec spawn_ants(integer(), list()) -> [pid()].
spawn_ants(Num_Ants,Args) -> lists:map (fun (_) -> spawn(ant, start, Args) end, lists:seq(1,Num_Ants)).

-spec init(integer(), integer(), integer(), numjobs(), inputs(), params()) -> pid(). 
init (Num_Ants, Duplicating, Iter_Local, Num_Jobs, Inputs, Params) ->

    % Set up ets table: visible to all ants
    case lists:member(tau, ets:all()) of
	      true -> ok;  % Already exists: maybe left over from interrupted run.
	      false -> ets:new(tau, [set, public, named_table])
	  end,
    #params{tau0=Tau0} = Params,
    Tau = lists:map(fun(I) -> {I, util:make_tuple(Num_Jobs, Tau0)} end, lists:seq(1, Num_Jobs)),
    ets:insert(tau, Tau),
    
%    io:format("~p~n", [ets:tab2list(tau)]),

    Ants = spawn_ants(Num_Ants, [Num_Jobs, Inputs, Params]),
    main(Duplicating, Iter_Local, Num_Jobs, Inputs, Params, Ants, none),
    self().

    % We could also put Num_Jobs, Durations, Weights, Deadlines, 
    % Alpha, Beta, Q0 (all read_only) in an ETS table and reduce 
    % the number of parameters in a lot of function calls.


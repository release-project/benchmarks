-module(aco).
-export([run/0, run/1]).

-include ("types.hrl").


%% --------------------------- Read problem specification from file ------------------------------ %%

-spec get_int (string()) -> integer().

get_int(S) ->
    {N, _} = string:to_integer(S),
    N.

-spec read_input(string()) -> {pos_integer(), inputs()}.

read_input(Fname) ->
    util:check_file(Fname),
    case file:read_file(Fname) of
        {ok,B} ->
	    [N|Rest] = string:tokens(binary:bin_to_list(B), " \n"),

	    % Split at space & newline. 

	    % Input format: 3N+1 integers.  N is the first item in the file, 
	    % then three sequences of N integers, containing durations, 
	    % weights and due dates respectively.

	    Num_Jobs = get_int(N),  
            Rest1 = lists:map(fun get_int/1, Rest),

            {Durations, Rest2} = lists:split(Num_Jobs, Rest1),
            {Weights, Rest3} = lists:split(Num_Jobs, Rest2),
            {Deadlines, _} = lists:split(Num_Jobs, Rest3),
            {Num_Jobs, {list_to_tuple(Durations), list_to_tuple(Weights), list_to_tuple(Deadlines)}};
        {error, Report} ->
            error (Report)
    end.


%% --------------------------- Run the colony ------------------------------ %%    

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


-spec aco_loop (pos_integer(), pos_integer(), 
		[pid()], numjobs(), inputs(), params(), solution() | none) -> solution().
aco_loop (0, _, _, _, _, _, Best_Solution) -> 
    Best_Solution;
aco_loop (Num_Iter, Num_Ants, Ants, Num_Jobs, Inputs, Params, Best_Solution) ->
    lists:foreach (fun (Pid) -> Pid ! {self(), find_solution} end, Ants),

    New_Solution = collect_ants(Num_Ants, Best_Solution),

    Improved_Solution = localsearch:vnd_loop(New_Solution, Inputs, Params),

    #params{verbose=Verbose} = Params,

    case Verbose of
	true -> 
	    {Cost, _} = Improved_Solution,
	    io:format ("Iteration ~p: cost = ~p~n", [Num_Iter, Cost]);
	_ -> ok
    end,
		 
    ok = update:update_pheromones(Num_Jobs, Improved_Solution, Params),
    aco_loop (Num_Iter-1, Num_Ants, Ants, Num_Jobs, Inputs, Params, Improved_Solution).
 

-spec aco(string(), pos_integer(), pos_integer(), params()) -> {pos_integer(), {numjobs(), cost(), schedule()}}.
aco(Fname, Num_Ants, Num_Iter, Params0) ->
    {Num_Jobs, Inputs} = read_input(Fname),
    
    % Set up ets table: visible to all ants
    case lists:member(tau, ets:all()) of
	      true -> ok;  % Already exists: maybe left over from interrupted run.
	      false -> ets:new(tau, [set, protected, named_table])
	  end,

    #params{tau0=T0} = Params0,
    Tau0 = case T0 of
	       undefined -> fitness:tau0 (Num_Jobs, Inputs, Num_Ants);
	       _ -> util:check_positive({tau0, T0}), T0
	   end,
    Params = Params0#params{tau0=Tau0},

    Tau = lists:map(fun(I) -> {I, util:make_tuple(Num_Jobs, Tau0)} end, lists:seq(1, Num_Jobs)),
    ets:insert(tau, Tau),

    Ants = lists:map (fun (_) -> 
			      spawn(ant, start, [Num_Jobs, Inputs, Params]) end, 
		      lists:seq(1,Num_Ants)),

    {Time, {Best_Cost, Best_Schedule}} = 
	timer:tc (
	  fun() -> aco_loop (Num_Iter, Num_Ants, Ants, Num_Jobs, Inputs, Params, none) end
	 ),
    ok = lists:foreach (fun (Pid) -> Pid ! {self(), stop} end, Ants),
    ets:delete(tau),
    {Time, {Num_Jobs, Best_Cost, Best_Schedule}}.

-spec output_result(pos_integer(), {numjobs(), cost(), schedule()}, pos_integer(), pos_integer()) -> ok.
output_result (Time, {Num_Jobs, Best_Cost, Best_Schedule}, Num_Ants, Num_Iter) ->
    case get(output_type) of 
	default -> 
	    io:format("Best cost = ~p~n", [Best_Cost]),
	    io:format("Time = ~p~n", [Time]);
	schedule ->
	    io:format("Best cost = ~p~n", [Best_Cost]),
	    io:format("Best schedule = ~p~n", [Best_Schedule]),
	    io:format("Time = ~p~n", [Time]);
	time_only ->
	    io:format("~p~n", [Time]);
	r_output -> 
	    io:format ("~10p ~10p ~10p ~10p ~10p~n", [Num_Jobs, Num_Ants, Num_Iter, Best_Cost, Time])
    end.
	
%% --------------------------- Parse arguments etc. ------------------------------ %%    

-spec usage() -> ok.
usage() -> 
    io:format ("Usage: aco [options] <input file> <num_ants> <num_iter>~n"),
    io:format (" Input file should contain the number of jobs on the first line,~n"),
    io:format (" then three lines containing durations, weights, and deadlines.~n"),
    io:format (" Node file contains an Erlang list giving nodes to run colonies on.~n"),
    io:format ("~n"),
    io:format (" The default output is the total weighted tardiness of the best solution~n"), %
    io:format (" found, together with the execution time of the main loop in microseconds.~n"),
    io:format (" Time taken for setup and data input is ignored.~n"),
    io:format ("~n"),
    io:format ("Options:~n"),
    io:format (" v: verbose~n"),
    io:format (" sched: print schedule, cost and time taken~n"),
    io:format (" time:  print time only (for benchmarking)~n"),
    io:format (" R: produce output for R~n"),
    io:format (" N: set random seeds using now(). Use this if you get a crypto link error~n"),
    io:format (" cyclic: use a cyclic \"random\" number generator of period 10 (for benchmarking)~n"),
    io:format (" ne: don't evaporate pheromone during update~n"),
    io:format (" a=<float>: set value of pheromone influence factor alpha (default = 1.0)~n"),
    io:format (" b=<float>: set value of heuristic influence factor beta  (default = 2.0)~n"),
    io:format (" rho=<float>: set value of pheromone evaporation rate rho (default = 0.1)~n"),
    io:format (" q0=<float>: set value of random exploration factor q0    (default = 0.9)~n"),
    io:format (" tau0=<float>: set initial pheromone strength (default calculated from mdd)~n"),
    io:format (" mdd: use MDD heuristic (this is the default)~n"),
    io:format (" edd: use EDD heuristic instead of MDD~n"),
    io:format (" au:  use AU heuristic instead of MDD~n"),

    io:format (" o1, o2, o12, o21: local search options (experimental)\n"),
    io:format ("   o1:  interchange search~n"),
    io:format ("   o2:  insertion search~n"),
    io:format ("   o12: interchange + insertion~n"),
    io:format ("   o21: insertion + interchange~n"),
    io:format ("~n").
    
-spec run2([string()], params()) -> ok.
run2(Args,Params) ->
    case Args of
	["v"|Rest] ->
	    run2(Rest, Params#params{verbose=true});
	["vv"|Rest] ->
	    run2(Rest, Params#params{verbose=true, vverbose=true});
	["R"|Rest] ->
	    put(output_type, r_output),
	    run2(Rest, Params);
	["sched"|Rest] ->
	    put(output_type, schedule),
	    run2(Rest, Params);
	["time"|Rest] ->
	    put(output_type, time_only),
	    run2(Rest, Params);
	["ne"|Rest] ->
	    run2(Rest, Params#params{evaporate=false});
	["N"|Rest] ->
	    run2(Rest, Params#params{rng_type=now});
	["cyclic"|Rest] ->
	    run2(Rest, Params#params{rng_type=cyclic});
	["o1"|Rest] ->
	    run2(Rest, Params#params{search=o1});
	["o2"|Rest] ->
	    run2(Rest, Params#params{search=o2});
	["o12"|Rest] ->
	    run2(Rest, Params#params{search=o12});
	["o21"|Rest] ->
	    run2(Rest, Params#params{search=o21});
	[ [$a, $=   |V] |Rest] -> 
	    Alpha = util:float_of_string(V), 
	    run2(Rest, Params#params{alpha=Alpha});
	[ [$b, $=   |V] |Rest] ->
	    Beta = util:float_of_string(V), 
	    run2(Rest, Params#params{beta=Beta});
	[ [$q,$0,$= |V] |Rest] ->
	    Q0 = util:float_of_string(V), 
	    run2(Rest, Params#params{q0=Q0});
	[ [$r, $h, $o, $=   |V] |Rest] ->
	    Rho = util:float_of_string(V), 
	    run2(Rest, Params#params{rho=Rho});
	[ [$t, $a, $u, $0, $=   |V] |Rest] ->
	    Tau0 = util:float_of_string(V), 
	    run2(Rest, Params#params{tau0=Tau0});
	["mdd"|Rest] ->
	    run2(Rest, Params#params{heuristic=mdd});
	["edd"|Rest] ->
	    run2(Rest, Params#params{heuristic=edd});
	["au"|Rest] ->
	    run2(Rest, Params#params{heuristic=au});
	["mixed"|_Rest] ->
	    util:quit ("Error: mixed heuristics not available in this version.~n", []);
	[Fname, Num_Ants0, Num_Iter0] ->
	    Num_Ants = util:int_of_string(Num_Ants0),
	    Num_Iter = util:int_of_string(Num_Iter0),
	    util:check_positive ({'Num_Ants', Num_Ants}),
	    util:check_positive ({'Num_Iter', Num_Iter}),
	    {Time, Result} = aco (Fname, Num_Ants, Num_Iter, Params),
	    output_result (Time, Result, Num_Ants, Num_Iter);
	["h"] -> usage();
	[F|_] -> io:format ("Unknown option ~p~n", [F]), usage();
	[] -> usage()
		     
    end.


-spec run([atom()]) -> ok.
run(Args) ->
    Params=#params{},  % program parameters - see types.hrl
    put(output_type, default),  % What to output at the end.
    run2 (lists:map (fun atom_to_list/1, Args), Params).
    
-spec run() -> ok.
run () -> usage().




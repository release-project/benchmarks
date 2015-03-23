-module(aco).
-export([run/0, run/1, start_aco/8]).

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


%% ---------------- Start the master ---------------- %%

start_aco (Fname, Vertex_degree, Duplicating, Num_Ants, Iter_Global, Iter_Local, NodeFile, Params0) ->
    {Num_Jobs, Inputs} = read_input(Fname),
    #params{tau0=T0} = Params0,
    Tau0 = case T0 of
	       undefined -> fitness:tau0 (Num_Jobs, Inputs, Num_Ants);
	       _ -> util:check_positive({tau0, T0}), T0
	   end,
    Params = Params0#params{tau0=Tau0},   
    % FIX: with the EDD value for tau0, it seems that the pheromones 
    % evaporate completely and then we have problems in ant.erl
    % because many of the tau entries are 0.

    util:check_file(NodeFile),
    {ok, [Nodes0]} = file:consult(NodeFile),
    case util:dups(Nodes0) of
	[] -> ok;
		D -> util:quit ("Error. Duplicate nodes in ~p: ~p.~n", [NodeFile,D])
    end,
    % This will print a message but not actually terminate the VM.

    Nodes = case get(num_nodes) of 
		undefined -> Nodes0;
		NN -> 
		    L = length(Nodes0), 
		    if NN > L -> 
			    util:quit ("~p nodes requested, but only ~p nodes in ~p~n",
				  [NN, L, NodeFile]);
		       true ->
			    lists:sublist(Nodes0, NN)  % Only use first NN nodes.
		    end
	    end,

    case (get(auto)) of
	true -> remote:start_nodes(Nodes);  % These will be stopped automatically when the main program ends.
	_ ->  lists:foreach (fun util:check_node/1, Nodes)
        % This will print a message but not actually terminate the VM if there's a failure.
    end,
    {Time, {Best_Cost, Best_Schedule}} =
	timer:tc (ant_master, run, [Num_Jobs, Vertex_degree, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes]),
    L1 = lists:sort(Best_Schedule),
    L2 = lists:seq(1,Num_Jobs),
    if L1 =:= L2-> ok;
       true -> io:format ("WARNING: bad solution~n")
    end,
    
    {Time, {Num_Jobs, Best_Cost, Best_Schedule}}.



% Shut down all the VMs specified in NodeFile
-spec stop_vms(string()) -> ok.
stop_vms (NodeFile) ->
    util:check_file(NodeFile),
    {ok, [Nodes]} = file:consult(NodeFile),
    remote:stop_nodes (Nodes).

% At the moment, the colonies shut down at the end of an ACO run,
% but the VMs stay alive (this makes launching multiple runs easier).
% The stop_vms function shuts the VMs down.  We could also do this by
% leaving the main loop of the colonies running and having a message to
% tell them to shut down the VM, but this might need a bit more tidying up 
% at the end of individual runs.

-spec output_result(pos_integer(), pos_integer(), {numjobs(), cost(), schedule()}, pos_integer(), pos_integer(), pos_integer()) -> ok.

output_result (Time1, Time2, {Num_Jobs, Best_Cost, Best_Schedule}, Num_Ants, Iter_Global, Iter_Local) ->
    % Time1 is total time including setup, Time2 is time taken for actual ant computations
    case get(output_type) of 
	default -> 
	    io:format("Best cost = ~p~n", [Best_Cost]),
	    io:format("Time1 = ~p~n", [Time1]),
	    io:format("Time2 = ~p~n", [Time2]);
	schedule ->
	    io:format("Best cost = ~p~n", [Best_Cost]),
	    io:format("Best schedule = ~p~n", [Best_Schedule]),
	    io:format("Time1 = ~p~n", [Time1]),
	    io:format("Time2 = ~p~n", [Time2]);
	time_only ->
	    io:format("~p ~p~n", [Time1, Time2]);
	r_output -> 
	    io:format ("~10p ~10p ~10p ~10p ~10p ~10p ~10p~n", [Num_Jobs, Num_Ants, Iter_Global, Iter_Local, Best_Cost, Time1, Time2])
    end.


%% -------------------- Argument parsing -------------------- %%

-spec usage() -> ok.
usage() -> 
    io:format ("Usage: aco [options] <input file> <vertex_degree> <dupcount> <num_ants> <global_iter> <local_iter> <node_file>~n"),
    io:format (" Input file should contain the number of jobs on the first line,~n"),
    io:format (" then three lines containing durations, weights, and deadlines.~n"),
    io:format (" Node file contains an Erlang list giving nodes to run colonies on.~n"),
    io:format ("~n"),
    io:format (" If the 'auto' option is specified then the program will attempt to start~n"),
    io:format (" and stop the nodes automatically (success may depend on OS settings:~n"),
    io:format (" see slave:start); otherwise, it is assumed that the nodes are already~n"),
    io:format (" running, and they will not be stopped when the program terminates.~n"),
    io:format ("~n"),
    io:format (" Messages from colonies are repeated 'dupcount' times (for scalability experiments).~n"),
    io:format ("~n"),
    io:format (" The default output is the total weighted tardiness ofthe best solution~n"), %
    io:format (" found, together with the execution time of the main loop in microseconds.~n"),
    io:format (" Time taken for setup and data input is ignored, but time taken to spawn~n"),
    io:format (" colonies on remote nodes is included.~n"), 
    io:format (" If the 'auto' option is specified then the program will attempt to start~n"),
    io:format (" and stop the nodes automatically (success may depend on OS settings:~n"),
    io:format (" see slave:start); otherwise, it is assumed that the nodes are already~n"),
    io:format (" running, and they will not be stopped when the program terminates.~n"),
    io:format ("~n"),
    io:format (" Messages from colonies are repeated 'dupcount' times (for scalability experiments).~n"),
    io:format ("~n"),
    io:format ("~n"),
    io:format ("Options:~n"),
    io:format (" auto: program will attempt to start and stop nodes automatically"),
    io:format (" v: verbose~n"),
    io:format (" n=<int>: use only the first n nodes named in the node file~n"),
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
    io:format ("~n"),
    io:format ("Also, aco stop <node_file> shuts down all the specified nodes.~n").
    
%-spec run2([string()]) -> ok.
run2(Args,Params) ->
    case Args of
	["v"|Rest] ->
	    run2(Rest, Params#params{verbose=true});
	["vv"|Rest] ->
	    run2(Rest, Params#params{verbose=true, vverbose=true});
	["auto"|Rest] ->
	    put(auto, true),
	    run2(Rest, Params);
	[ [$n, $= |V] |Rest] -> 
	    put(num_nodes, util:int_of_string(V)), 
	    run2(Rest, Params);
	[ [$r, $e, $p, $l, $= |V] |Rest] -> 
	    put(duplicating, V), 
	    run2(Rest, Params);
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
	["mixed"|Rest] ->
	    run2(Rest, Params#params{heuristic=mixed});
	[Fname,Vertex_degree0, Duplicating0, Num_Ants0, Iter_Global0, Iter_Local0, NodeFile] ->
	    Vertex_degree = util:int_of_string(Vertex_degree0),
	    Duplicating = util:int_of_string(Duplicating0),
	    Num_Ants = util:int_of_string(Num_Ants0),
	    Iter_Global = util:int_of_string(Iter_Global0),
	    Iter_Local  = util:int_of_string(Iter_Local0),
	    util:check_positive ({'Num_Ants', Num_Ants}),
	    util:check_positive ({'Iter_Global', Iter_Global}),
	    util:check_positive ({'Iter_Local', Iter_Local}),
	    {Time1, {Time2, Result}} =
		timer:tc (aco, start_aco, [Fname, Vertex_degree, Duplicating, Num_Ants, Iter_Global, Iter_Local, NodeFile, Params]),
	    output_result(Time1, Time2, Result, Num_Ants, Iter_Global, Iter_Local);
	[Fname,Vertex_degree0, Num_Ants0, Iter_Global0, Iter_Local0, NodeFile] ->
	    run2 ([Fname,Vertex_degree0, get(duplicating), Num_Ants0, Iter_Global0, Iter_Local0, NodeFile], Params);
	    % Allow alternative syntax for replication uisng "repl=100" (say) instead of positional parameter
	["stop", NodeFile] -> stop_vms (NodeFile);
	["h"] -> usage();
	[F|_] -> io:format ("Unknown option ~p~n", [F]), usage();
	[] -> usage()
		     
    end.


-spec run([atom()]) -> ok.
run(Args) ->
    Params=#params{},  % program parameters - see types.hrl
    put(output_type, default),  % What to output at the end.
    put(duplicating, "1"),        % Default message replication count
    run2 (lists:map (fun atom_to_list/1, Args), Params).
    
-spec run() -> ok.
run () -> usage().




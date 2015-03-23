-module(aco).
-export([run/0, run/1]).

-include ("types.hrl").


%% --------------------------- Read problem specification from file ------------------------------ %%

open_file(Fname) ->
    case file:open(Fname, [read, raw]) of
        {ok, Fd} ->
            Fd;
        {error, _Reason} ->
            io:format("Error: can't open ~p~n", [Fname]),
	    init:stop()  % Is there a better way to do this?
    end.  

slurp (Fd) -> slurp (Fd,[]).
slurp (Fd, L) ->
    case file:read_line (Fd) of
	{ok, Data} ->
	    slurp (Fd, L++Data);
	eof -> L;
	{error, Msg} ->
	    error(Msg)
    end.
    

-spec read_input(string()) -> {integer(), inputs()}.

read_input(Fname) ->
    util:check_file(Fname),
    Fd = open_file(Fname),
    case file:read_line(Fd) of
        {ok,Line1} ->
            {Num_Jobs, _} = string:to_integer(hd (string:tokens (Line1, " "))),  
	    Rest = slurp (Fd),
            Rest1 = lists:map(fun(X) -> {Int, _} = string:to_integer(X), Int end,
				string:tokens(Rest, [32,10])),
	    % Split at space & newline. Couldn't see how to do this with a literal string.
	    % We're trying to cope with two different input formats.  First line contains
	    % number of jobs (perhaps followed by another number, always 1 in the files we have),
	    % then either 3 lines containing durations, weights and then due dates, 
	    % or the whole lot on one line.
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


-spec uid() -> string().

uid() -> % Quick attempt at unique ID - fix this later.
    {A, B, C} = now(),
    lists:flatten(io_lib:format("tau-~p-~p-~p", [A,B,C])).


-spec aco_loop (pos_integer(), pos_integer(), 
		[pid()], numjobs(), inputs(), params(), tab_name(), solution() | none) -> solution().
aco_loop (0, _, _, _, _, _, _, Best_Solution) -> 
    Best_Solution;
aco_loop (Num_Iter, Num_Ants, Ants, Num_Jobs, Inputs, Params, Tau_dets, Best_Solution) ->
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
		 
    ok = update:update_pheromones(Num_Jobs, Improved_Solution, Params, Tau_dets),
    aco_loop (Num_Iter-1, Num_Ants, Ants, Num_Jobs, Inputs, Params, Tau_dets, Improved_Solution).
 

-spec aco(string(), pos_integer(), pos_integer(), params()) -> {numjobs(), cost(), schedule()}.
aco(Fname, Num_Ants, Num_Iter, Params0) ->
    {Num_Jobs, Inputs} = read_input(Fname),
    
    #params{tau0=T0} = Params0,
    Tau0 = case T0 of
	       undefined -> fitness:tau0 (Num_Jobs, Inputs, Num_Ants);
	       _ -> util:check_positive({tau0, T0}), T0
	   end,
    Params = Params0#params{tau0=Tau0},

    Tau_dets = uid(),  
    % We have to make the name unique in case we're trying to run 
    % several copies of the program on machines which share a disk.
    % Eg, if we're doing simultaneous experiments on machine on a 
    % Beowulf cluster, we don't want independent VMs to share the same pheromone info.
    {ok, _} = dets:open_file(Tau_dets, [{type, set}]),
    Tau = lists:map(fun(I) -> {I, util:make_tuple(Num_Jobs, Tau0)} end, lists:seq(1, Num_Jobs)),
    dets:insert(Tau_dets, Tau),

    Ants = lists:map (fun (_) -> 
			      spawn(ant, start, [Num_Jobs, Inputs, Params, Tau_dets]) end, 
		      lists:seq(1,Num_Ants)),

    {Best_Cost, Best_Schedule} = aco_loop (Num_Iter, Num_Ants, Ants, Num_Jobs, Inputs, Params, Tau_dets, none),
    ok = lists:foreach (fun (Pid) -> Pid ! {self(), stop} end, Ants),
    dets:close(Tau_dets),
    file:delete(Tau_dets),
    {Num_Jobs, Best_Cost, Best_Schedule}.


%% --------------------------- Parse arguments etc. ------------------------------ %%    

-spec usage() -> ok.
usage() -> 
    io:format ("Usage: aco [options] <input file> <num_ants> <num_iter>~n"),
    io:format (" Input file should contain the number of jobs on the first line,~n"),
    io:format (" then three lines containing durations, weights, and deadlines.~n"),
    io:format (" Node file contains an Erlang list giving nodes to run colonies on.~n"),
    io:format ("~n"),
    io:format ("Options:~n"),
    io:format (" v: verbose~n"),
    io:format (" s: print schedule at end~n"),
    io:format (" R: produce output for R~n"),
    io:format (" N: set random seeds using now(). Use this if you get a crypto link error.~n"),
    io:format (" ne: don't evaporate pheromone during update.~n"),
    io:format (" a=<float>: set value of pheromone influence factor alpha (default = 1.0)~n"),
    io:format (" b=<float>: set value of heuristic influence factor beta  (default = 2.0)~n"),
    io:format (" rho=<float>: set value of pheromone evaporation rate rho (default = 0.1)~n"),
    io:format (" q0=<float>: set value of random exploration factor q0    (default = 0.9)~n"),
    io:format (" tau0=<float>: set initial pheromone strength (default calculated from mdd)~n"),
    io:format (" mdd: use MDD heuristic (this is the default)~n"),
    io:format (" edd: use EDD heuristic instead of MDD~n"),
    io:format (" au:  use AU heuristic instead of MDD~n"),

    io:format (" o1, o2, o12, o21: local search options (experimental)\n"),
    io:format ("   o1: interchange search~n"),
    io:format ("   o2: insertion search~n"),
    io:format ("   o12: interchange + insertion~n"),
    io:format ("   o21: insertion + interchange~n").
    
%-spec run2([string()]) -> ok.
run2(Args,Params) ->
    case Args of
	["v"|Rest] ->
	    run2(Rest, Params#params{verbose=true});
	["vv"|Rest] ->
	    run2(Rest, Params#params{verbose=true, vverbose=true});
	["R"|Rest] ->
	    put(r_output, true),    
	    run2(Rest, Params);
	["s"|Rest] ->
	    run2(Rest, Params#params{schedule=true});
	["ne"|Rest] ->
	    run2(Rest, Params#params{evaporate=false});
	["N"|Rest] ->
	    run2(Rest, Params#params{seed_now=true});
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
	[Fname, Num_Ants0, Num_Iter0] ->
	    Num_Ants = util:int_of_string(Num_Ants0),
	    Num_Iter = util:int_of_string(Num_Iter0),
	    util:check_positive ({'Num_Ants', Num_Ants}),
	    util:check_positive ({'Num_Iter', Num_Iter}),
	    #params{schedule=Print_schedule} = Params,
	    {Time, {Num_Jobs, Best_Cost, Best_Schedule}} =
		timer:tc (fun() -> aco (Fname, Num_Ants, Num_Iter, Params) end),
	    case get(r_output) of 
		true -> 
		    io:format ("~10w ~10w ~10w ~10w ~10w~n", [Num_Jobs, Num_Ants, Num_Iter, Best_Cost, Time]);
		_ -> 
		    io:format("Best cost = ~w~n", [Best_Cost]),
		    case Print_schedule of
			true -> io:format("Best schedule = ~w~n", [Best_Schedule]);
			_ -> ok
		    end,
		    io:format("Time = ~w~n", [Time])
	    end;
	["h"] -> usage();
	[F|_] -> io:format ("Unknown option ~p~n", [F]), usage();
	[] -> usage()
		     
    end.


-spec run([atom()]) -> ok.
run(Args) ->
    Params=#params{},  % program parameters - see types.hrl
    run2 (lists:map (fun atom_to_list/1, Args), Params).
    
-spec run() -> ok.
run () -> usage().




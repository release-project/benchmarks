%% Individual ant: constructs new solution based on pheromone matrix.

-module(ant).
-include ("types.hrl").
-export([start/3]).

% M&M -> Merkle & Middendorf: An Ant Algorithm with a new 
% Pheromone Evaluation Rule for Total Tardiness Problems


%% ------------------------------- Random numbers --------------------------------- %%

-spec init_rng(params()) -> ok.
init_rng(Params) ->
    #params{rng_type=RNG_type} = Params,
    put (rng_type, RNG_type),
    case RNG_type of
	now ->
	    {A, B, C} = now(),
	    random:seed(A,B,C);
	crypto -> 
	    <<A:32,B:32,C:32>> = crypto:rand_bytes(12),
	    random:seed(A,B,C);
            % This seems to be safer, but the HW machines don't have the
	    % correct C crypto library.
	cyclic -> put (rng_index, 1)
    end.

-spec random () -> float().
random() ->
    case get(rng_type) of
	cyclic ->   % Fake RNG for benchmarking
	    Vals = {0.09, 0.19, 0.29, 0.39, 0.49, 0.59, 0.69, 0.79, 0.89, 0.99},
	    N = tuple_size(Vals),
	    I = get(rng_index),
	    I1 = if I <  N -> I+1;
		    I >= N -> 1
		 end,
	    put (rng_index, I1),
	    element(I, Vals);
	_ -> random:uniform()
    end.

-spec reset_cyclic_rng () -> ok.
reset_cyclic_rng() ->
    case get(rng_type) of 
	cyclic -> put (rng_index, 1); 
	_ -> ok 
    end.


%% ---------------------------------------------------------------- %%
% Strictly, P(i,j) is equal to Pij/Sigma; he're we're calculating both as we go along.

construct_pij([], _, _, _, Sigma, P) -> % Sigma is sum of terms over unscheduled jobs: see M&M
    {lists:reverse(P), Sigma};
construct_pij([{J,Eta_J}|TEta], Alpha, Beta, TauI, Sigma, P) ->
    Pij = math:pow (element(J, TauI), Alpha) * math:pow(Eta_J, Beta),
    construct_pij(TEta, Alpha, Beta, TauI, Sigma+Pij, [{J,Pij}|P]).

% Eta contains info only for unscheduled jobs, so the above function constructs info only for those jobs.



%% ---------------------------------------------------------------- %%
% Careful here: elements of Pij can be extremely small (10^-200 towards end).

-spec find_limit_index ([idxf()], index(), float(), float()) -> index().
find_limit_index(P, Idx, Total, Limit) ->
    if Total >= Limit -> Idx;
       true -> case P of 
		   [] -> error ("Fell off end of P in find_limit_index");
						% This could happen if Limit is 1 (or very close)
						% and rounding errors make the sum of pij slightly
						% less than 1.
		   [{K, Kval}|Rest] -> 
		       find_limit_index (Rest, K, Total+Kval, Limit)
	       end
    end.


-spec find_random_job([idxf(),...], float()) -> index().
find_random_job(P_i, SumPij) ->
    case P_i of
	[] -> error ("find_rand_pij: P_i is empty");
	[{J1, J1val}|Rest] ->
	    Limit = random() * SumPij,
	    find_limit_index(Rest, J1, J1val, Limit)
    end.


    

%% ---------------------------------------------------------------- %%

% Find index j which maximises P[i,j] (i is fixed).
% Note that we're usually working with a partial schedule, in which case many of the
% elements of Pij are zero.  We need to be sure that at least one element is greater
% than zero when calling this function.


-spec find_index_max_P(index(), float(), [idxf()]) -> index().

find_index_max_P(MaxIndex, _, []) -> MaxIndex;
find_index_max_P(MaxIndex, MaxVal, [{K,Kval}|Rest]) ->
    if Kval =< MaxVal -> find_index_max_P (MaxIndex, MaxVal, Rest);
       Kval >  MaxVal -> find_index_max_P (K,        Kval,   Rest)
    end.


-spec find_index_max_P([idxf(),...]) -> index().

find_index_max_P ([]) -> error ("find_index_max_P: P_i is empty");
find_index_max_P ([{J1,J1val}|Rest]) -> find_index_max_P (J1, J1val, Rest).


%% ---------------------------------------------------------------- %%

-spec find_solution (I::integer(),   
                     % We're trying to schedule a job at position I. We need I to index tau here.
		     Unscheduled_Jobs :: natlist(), 
		     Inputs::inputs(),
		     Alpha::number(), Beta::number(), Q0::number(),
		     Heuristic :: heuristic(),
		     Partial_Schedule::natlist(),
		     Scheduled_Time::integer()
		    ) -> schedule().


find_solution(I, Unscheduled_Jobs, Inputs, Alpha, Beta, Q0, Heuristic,
		 Partial_Schedule, Scheduled_Time) ->
    case Unscheduled_Jobs of
	[] -> error ("Ran out of unscheduled jobs in find_solution");
	[Last] -> lists:reverse(Partial_Schedule, [Last]);
	_ ->
	    Eta = fitness:make_eta (Unscheduled_Jobs, Heuristic, Inputs, Scheduled_Time),
%	    io:format("Eta=~p~n", [Eta]),
	    [{I, TauI}] = ets:lookup(tau, I),  % Since tau is a set, we should always get a one-entry list.

	    {P_i, SumPij} = construct_pij(Eta, Alpha, Beta, TauI, 0.0, []),

	    Q = random(),
	    New_Job = if Q<Q0 -> find_index_max_P (P_i);  
				 % Value of J which % maximises %
				 % {tau^alpha}{eta^beta}[J] % for %
				 % unscheduled % jobs
			 true -> find_random_job (P_i, SumPij)
				 % Choose a job randomly wrt to probability distribution
				 % given by P_i
		      end, 
%	    io:format ("New_Job = ~p~n", [New_Job]),
	    {Durations,_,_} = Inputs,
	    Current_Time = Scheduled_Time+element(New_Job, Durations),
	    Now_Unscheduled = lists:delete (New_Job, Unscheduled_Jobs),
	    find_solution(I+1, Now_Unscheduled, Inputs, Alpha, Beta, Q0, Heuristic,
			  [New_Job|Partial_Schedule], Current_Time)
    end.

-spec find_solution (numjobs(), inputs(), params()) -> solution().
find_solution(Num_Jobs, Inputs, Params) ->
    #params{alpha=Alpha, beta=Beta, q0=Q0, heuristic=Heuristic} = Params,
    Unscheduled_Jobs = lists:seq(1, Num_Jobs),
    Schedule = find_solution(1, Unscheduled_Jobs, Inputs, Alpha, Beta, Q0, Heuristic, [], 0),
    Tardiness = fitness:tardiness(Schedule, Inputs),
    {Tardiness, Schedule}.


%% ---------------------------------------------------------------- %%

-spec loop(numjobs(), inputs(), params()) -> ok.
loop(Num_Jobs, Inputs, Params) ->
    receive
	{From, find_solution} ->
	    reset_cyclic_rng(),  % Reset cyclic "RNG" (if we're using it) to get same behaviour for all generations.
	    Result = find_solution(Num_Jobs, Inputs, Params),
	    From ! {ant_done, Result},
	    loop(Num_Jobs, Inputs, Params);
	{_From, stop} -> ok;
        Msg ->	     error ({unexpected_message, Msg})
    end.
     

-spec start(numjobs(), inputs(), params()) -> ok.
start(Num_Jobs, Inputs, Params) -> 
    init_rng(Params),
    loop(Num_Jobs, Inputs, Params).


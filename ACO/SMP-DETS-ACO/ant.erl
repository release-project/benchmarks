%% Individual ant: constructs new solution based on pheromone matrix.

-module(ant).
-include ("types.hrl").
-export([start/4]).

% M&M -> Merkle & Middendorf: An Ant Algorithm with a new 
% Pheromone Evaluation Rule for Total Tardiness Problems

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

-spec find_limit_index ([idxf90], index(), float(), float()) -> index().
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


-spec find_random_job([idxf()], float()) -> index().
find_random_job(P_i, SumPij) ->
    case P_i of
	[] -> error ("find_rand_pij: P_i is empty");
	[{J1, J1val}|Rest] ->
	    Limit = random:uniform() * SumPij,
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


-spec find_index_max_P([idxf()]) -> index().

find_index_max_P ([]) -> error ("find_index_max_P: P_i is empty");
find_index_max_P ([{J1,J1val}|Rest]) -> find_index_max_P (J1, J1val, Rest).


%% ---------------------------------------------------------------- %%

-spec find_solution (I::integer(),   
                     % We're trying to schedule a job at position I. We need I to index tau here.
		     Unscheduled_Jobs :: intlist(), 
		     Inputs::inputs(),
		     Alpha::number(), Beta::number(), Q0::number(),
		     Heuristic :: heuristic(),
		     Partial_Schedule::intlist(),
		     Scheduled_Time::integer(),
		     Tau_dets::tab_name()
		    ) -> schedule().


find_solution(I, Unscheduled_Jobs, Inputs, Alpha, Beta, Q0, Heuristic,
		 Partial_Schedule, Scheduled_Time, Tau_dets) ->
    case Unscheduled_Jobs of
	[] -> error ("Ran out of unscheduled jobs in find_solution");
	[Last] -> lists:reverse(Partial_Schedule, [Last]);
	_ ->
	    Eta = fitness:make_eta (Unscheduled_Jobs, Heuristic, Inputs, Scheduled_Time),
%	    io:format("Eta=~p~n", [Eta]),
	    [{I, TauI}] = dets:lookup(Tau_dets, I),  % Since tau is a set, we should always get a one-entry list.

	    {P_i, SumPij} = construct_pij(Eta, Alpha, Beta, TauI, 0.0, []),

	    Q = random:uniform(),
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
			  [New_Job|Partial_Schedule], Current_Time, Tau_dets)
    end.

-spec find_solution (numjobs(), inputs(), params(), tab_name()) -> solution().
find_solution(Num_Jobs, Inputs, Params, Tau_dets) ->
    #params{alpha=Alpha, beta=Beta, q0=Q0, heuristic=Heuristic} = Params,
    Unscheduled_Jobs = lists:seq(1, Num_Jobs),
    Schedule = find_solution(1, Unscheduled_Jobs, Inputs, Alpha, Beta, Q0, Heuristic, [], 0, Tau_dets),
    Tardiness = fitness:tardiness(Schedule, Inputs),
    {Tardiness, Schedule}.


%% ---------------------------------------------------------------- %%

-spec loop(numjobs(), inputs(), params(), tab_name()) -> ok.
loop(Num_Jobs, Inputs, Params, Tau_dets) ->
    receive
	{From, find_solution} ->
	    Result = find_solution(Num_Jobs, Inputs, Params, Tau_dets),
	    From ! {ant_done, Result},
	    loop(Num_Jobs, Inputs, Params, Tau_dets);
	{_From, stop} -> ok;
        Msg ->	     error ({unexpected_message, Msg})
    end.
     

-spec start(numjobs(), inputs(), params(), tab_name()) -> ok.
start(Num_Jobs, Inputs, Params, Tau_dets) -> 
    #params{seed_now=Seed_now} = Params,
    if Seed_now ->
	    {A, B, C} = now();
       true -> 
	    <<A:32,B:32,C:32>> = crypto:rand_bytes(12)
            % This seems to be safer, but the HW machines don't have the
	    % correct C crypto library.
    end,
    random:seed(A,B,C),
    loop(Num_Jobs, Inputs, Params, Tau_dets).


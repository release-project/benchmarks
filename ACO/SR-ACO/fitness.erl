-module(fitness).
-include ("types.hrl").
-export([tau0/3, make_eta/4, tardiness/2]).

%% -------------------------------- Earliest Due Date -------------------------------- %%

% Earliest Due Date - schedule jobs in order of deadlines

-spec edd_schedule(numjobs(), inputs()) -> schedule().
edd_schedule (Num_Jobs, {_, _, Deadlines}) ->
    Jobs = lists:seq (1, Num_Jobs),
    L = lists:zip(Jobs, tuple_to_list(Deadlines)),
    L1 = lists:sort (fun ({_,T1},{_,T2}) -> T1 < T2 end, L),
    lists:map (fun({J,_}) -> J end, L1).


% Suggested initial value for pheromone trails - see Merkle & Middendorf 
% (An Algorithm with a new Pheromone Evaluation Rule ...), although they're
% dealing with the SMTTP.

-spec tau0(numjobs(), inputs(), pos_integer()) -> float().
tau0(Num_Jobs, Inputs, Num_Ants) -> 
    EDD_schedule = edd_schedule(Num_Jobs, Inputs),
    TWT = tardiness (EDD_schedule, Inputs),  
%    io:format ("EDD schedule -> ~p~nTWT=~p~n", [EDD_schedule, TWT]),
    if TWT == 0 -> 1.0;  % This will happen if EDD schedules all jobs before their deadlines.
       true -> 1.0/(Num_Ants * TWT)
    end.



-spec edd(natlist(),intvec(),[idxf()]) -> [idxf()].
edd([], _, Acc) ->
    lists:reverse(Acc);
edd([J|Rest], Deadlines, Acc) ->
    This_Deadline = element(J, Deadlines),
    edd(Rest, Deadlines, [{J,1.0/This_Deadline}|Acc]).
    

     
%% ------------------------------- Modified Due Date --------------------------------- %%

% Modified Due Date.  Brauer/Bullnheimer/Hartl:  "... jobs are iteratively scheduled.
% After a job is scheduled, all remaining non-scheduled jobs are again sorted in 
% ascending order, but according to modified due dates.  The job yielding the lowest
% modified due date is appended to the sub-sequence generated so far. The modified
% due dates are given by max{T+p_j, d_j}, where T is the total processing time
% of all jobs already scheduled."


-spec mdd(natlist(), intvec(), intvec(), integer(), [idxf()]) -> [idxf()].
     
mdd([], _, _, _ST, Acc) ->
    lists:reverse(Acc);
mdd([J|Rest], Durations, Deadlines, Scheduled_Time, Acc) ->
    This_Deadline = element(J,Deadlines),
    This_Duration = element(J,Durations),
    Finish_Time = Scheduled_Time + This_Duration,
    MDD = max(Finish_Time, This_Deadline),
    mdd(Rest, Durations, Deadlines, Scheduled_Time, [{J,1.0/MDD}|Acc]).



%% ------------------------------- Apparent Urgency --------------------------------- %%

-spec au(natlist(), intvec(), intvec(), intvec(), integer(), float(), [idxf()]) -> [idxf()].
au([], _, _, _, _, _KPbar, Acc) ->
    lists:reverse(Acc);
au([J|Rest], Durations, Weights, Deadlines, Scheduled_Time, KPbar, Acc) ->
    This_Duration = element(J,Durations),
    This_Weight   = element(J,Weights),
    This_Deadline = element(J,Deadlines),
    Finish_Time = Scheduled_Time + This_Duration,
    V = max(This_Deadline - Finish_Time, 0),
    AU = (This_Weight/This_Duration) * math:exp (-V/KPbar),
    au(Rest, Durations, Weights, Deadlines, Scheduled_Time, KPbar, [{J,AU}|Acc]).
    % Despite what many papers say, it seems that we require AU here, not 1.0/AU.

-spec au(natlist(), intvec(), intvec(), intvec(), integer()) -> [idxf()].
au(Unscheduled, Durations, Weights, Deadlines, Scheduled_Time) ->
    Remaining_Durations = [element(J,Durations) || J <- Unscheduled],
    Pbar = lists:sum(Remaining_Durations)/length(Remaining_Durations),
    K = 2,
    KPbar = K * Pbar,
    au(Unscheduled, Durations, Weights, Deadlines, Scheduled_Time, KPbar, []).

-spec make_eta(natlist(), heuristic(), inputs(), integer()) -> [idxf()].
make_eta (Unscheduled, Heuristic, {Durations, Weights, Deadlines}, Scheduled_Time) ->
    case Heuristic of 
	mdd -> mdd (Unscheduled, Durations, Deadlines, Scheduled_Time, []);
	edd -> edd (Unscheduled, Deadlines, []);
	au  -> au  (Unscheduled, Durations, Weights, Deadlines, Scheduled_Time);
	mixed -> error ("Unexpected mixed heuristic in fitness:make_eta")
    end.
	    

%% -------------------------------- Tardiness -------------------------------- %%


-spec tardiness(integer(), integer(), schedule(), intvec(), intvec(), intvec()) -> integer().

tardiness(Cost, _, [],_, _, _) ->
    Cost;
tardiness(Cost, Current_Time, [This_Job|Rest], Durations, Weights, Deadlines) ->
    This_Duration = element(This_Job, Durations),
    This_Deadline = element(This_Job, Deadlines),
    This_Weight   = element(This_Job, Weights),
    Finish_Time   = Current_Time+This_Duration,

    New_Cost = if Finish_Time > This_Deadline -> 
		       Cost + (Finish_Time-This_Deadline)*This_Weight;
		  true -> Cost
	       end,
    tardiness (New_Cost, Finish_Time, Rest, Durations, Weights, Deadlines).


% Total Weighted Tardiness of a schedule

-spec tardiness(schedule(), inputs()) -> integer().
tardiness (Schedule, {Durations, Weights, Deadlines}) ->
    tardiness (0,0,Schedule, Durations, Weights, Deadlines).


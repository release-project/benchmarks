-module(update).
-include("types.hrl").
-export([update_pheromones/4]).
-define(GetRow(I), hd(dets:lookup(Tau_dets,I))).   % hd because we get a one-element list for sets.

-type tau_row() :: {index(), floatvec()}.  % One row of the pheromone matrix: second entry is tuple of floats

% Pheromone update for SMTWTP is described in
%   Merkle & Middendorf: An Ant Algorithm with a new Evaluation Rule ...
%   den Besten & Stuetzle: Ant Colony Optimization for the Total Weighted Tardiness Problem
% They perform a local update of the pheromone metrix after each ant has 
% selected a new job for its current schedule.  We don't do this because it would involve
% many concurrent writes to the pheromone matrix.
% Dorigo's book says that the den Besten-Stuetzle version gets better results,
% due to the VND local search method whic they use to improve the best solution
% at the end of each iteration.


-spec evaporate_row(floatvec(), float(), numjobs(), index(), [float()]) -> floatvec().

evaporate_row (_, _, Num_Jobs, J, Acc) when J > Num_Jobs ->
    list_to_tuple(lists:reverse(Acc));
evaporate_row (TauI, R, Num_Jobs, J, Acc) ->
    X = element(J, TauI),
    New_X = R*X,
    evaporate_row (TauI, R, Num_Jobs, J+1, [New_X|Acc]).


-spec evaporate_row(tau_row(), float(), numjobs(), tab_name()) -> true.

evaporate_row({I, TauI}, R, Num_Jobs, Tau_dets) -> 
    New_TauI = evaporate_row
		 (TauI, R, Num_Jobs, 1, []), 
    dets:insert(Tau_dets, {I, New_TauI}).  % Old value is overwritten since tau is a set 


-spec evaporate(numjobs(), float(), index(), tab_name()) -> ok.

evaporate(Num_Jobs, _, I, _) when I > Num_Jobs -> ok;
evaporate(Num_Jobs, R, I, Tau_dets) ->
    evaporate_row(?GetRow(I), R, Num_Jobs,Tau_dets),
    evaporate(Num_Jobs, R, I+1, Tau_dets).

% All evaporate does is to multiply the entire matrix by R

%% ---------------------------------------------------------------- %%

-spec reinforce_row(float(), tau_row(), index(), tab_name()) -> true.

reinforce_row (D, {I, TauI}, J, Tau_dets) ->
    Tij = element(J, TauI),
    New_Tij = Tij + D, 
    New_TauI = setelement(J, TauI, New_Tij),
    dets:insert(Tau_dets, {I, New_TauI}).


-spec reinforce(float(), schedule(), index(), tab_name()) -> ok.

reinforce(_, [], _, _) -> ok;
reinforce(D, [J|Rest], I, Tau_dets) ->
    reinforce_row(D, ?GetRow(I), J, Tau_dets),
    reinforce(D, Rest, I+1, Tau_dets).

check_tau(N,I,Z,Tau_dets) ->
    if I > N -> ok;
       true -> R = ?GetRow(I),
	       if R == Z ->
		       io:format ("Warning: tau[~p] is zero~n", [I]);
		  true -> ok
	       end,
	       check_tau(N,I+1,Z,Tau_dets)
    end.

check_tau (N,Tau_dets) ->
    Z = util:make_tuple(N, 0.0),
    check_tau (N,1,Z,Tau_dets).

-spec update_pheromones(numjobs(), solution(), params(), tab_name()) -> ok.

update_pheromones(Num_Jobs,{Best_t,Best_res}, Params, Tau_dets) ->
    #params{rho=Rho, evaporate=Evaporate} = Params,
    Tmin = max (Best_t, 1),  % just in case: we're going to divide by tmin shortly

    ok = if Evaporate -> evaporate(Num_Jobs, 1.0-Rho, 1, Tau_dets);
	    true -> ok
	 end,
    #params{rho=Rho} = Params,
%    D = 1.0/Tmin,
    D = Rho/Tmin,
    reinforce(D, Best_res, 1, Tau_dets),
    check_tau (Num_Jobs, Tau_dets).

% Do we evaporate or not?  There seems to be some dispute.

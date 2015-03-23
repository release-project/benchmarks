-module(update).
-include("types.hrl").
-export([update_pheromones/3]).
-define(GetRow(I), hd(ets:lookup(tau,I))).   % hd because we get a one-element list for sets.

-type tau_row() :: {index(), floatvec()}.  % One row of the pheromone matrix: second entry is tuple of floats

% Pheromone update for SMTWTP is described in
%   Merkle & Middendorf: An Ant Algorithm with a new Evaluation Rule ...
%   den Besten & Stuetzle: Ant Colony Optimization for the Total Weighted Tardiness Problem
% They perform a local update of the pheromone metrix after each ant has 
% selected a new job for its current schedule.  We don't do this because it would involve
% many concurrent writes to the pheromone matrix.
% Dorigo's book says that the den Besten-Stuetzle version gets better results,
% due to the VND local search method which they use to improve the best solution
% at the end of each iteration.


%% ---------------------------------------------------------------- %%

-spec evaporate_row (index(), natlist(), float()) -> true.
evaporate_row (I, S, R) ->
    {I, TauI} = ?GetRow(I),
    New_TauI = list_to_tuple([R * element(J, TauI) || J <- S]),
    ets:insert(tau, {I, New_TauI}).

-spec evaporate(numjobs(), float()) -> ok.
evaporate(Num_Jobs, R) ->
    S = lists:seq(1,Num_Jobs),
    lists:foreach(fun(I) -> evaporate_row(I,S,R) end, S).


% All evaporate does is to multiply the entire matrix by R

%% ---------------------------------------------------------------- %%

-spec reinforce_row(float(), tau_row(), index()) -> true.

reinforce_row (D, {I, TauI}, J) ->
    Tij = element(J, TauI),
    New_Tij = Tij + D, 
    New_TauI = setelement(J, TauI, New_Tij),
%    ets:delete(tau, element(1,Tau)),
    ets:insert(tau, {I, New_TauI}).


-spec reinforce(float(), schedule(), index()) -> ok.

reinforce(_, [], _) -> ok;
reinforce(D, [J|Rest], I) ->
    reinforce_row(D, ?GetRow(I), J),
    reinforce(D, Rest, I+1).

%% ---------------------------------------------------------------- %%

check_tau(N,I,Z) ->
    if I > N -> ok;
       true -> R = ?GetRow(I),
	       if R == Z ->
		       io:format ("Warning: tau[~p] is zero~n", [I]);
		  true -> ok
	       end,
	       check_tau(N,I+1,Z)
    end.

check_tau (N) ->
    Z = util:make_tuple(N, 0.0),
    check_tau (N,1,Z).

%% ---------------------------------------------------------------- %%

-spec update_pheromones(numjobs(), solution(), params()) -> ok.

update_pheromones(Num_Jobs,{Best_t,Best_res}, Params) ->
    #params{rho=Rho, evaporate=Evaporate} = Params,

    ok = if Evaporate -> evaporate(Num_Jobs, 1.0-Rho);
	    true -> ok
	 end,

    Tmin = max (Best_t, 1),  % just in case: we're going to divide by tmin shortly
%   D = 1.0/Tmin,
    D = Rho/Tmin,
    reinforce(D, Best_res, 1),
    check_tau (Num_Jobs).

% Do we evaporate or not?  There seems to be some dispute.

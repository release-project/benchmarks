-module(localsearch).
-include("types.hrl").
-export([vnd_loop/3]).


%------------------------------ Insertion Search ----------------------------------

% Remove list element at position I and insert at position J

-spec insert (natlist(), index(), index()) -> natlist().

insert (L, I, J) when I=:=J -> L;
insert (L, I, J) when I < J ->
    {A, L3} = lists:split (J, L),
    {L1, [X|L2]} = lists:split (I-1, A),
    L1 ++ L2 ++ [X|L3];
insert (L, I, J) when I > J ->
    {A, [X|R]} = lists:split (I-1, L) ,
    {L1, L2} = lists:split (J-1, A),
    L1 ++ [X|L2] ++ R.
    

-spec insert_loop2 (index(), index(), numjobs(), inputs(), schedule(), solution()) -> solution().

insert_loop2 (I, J, N, Inputs, Schedule, Solution = {Best_Time, _}) ->
    if I =:= J ->
	    Solution;
       J > N ->
	    Solution;
       true -> 
	    New_Schedule = insert (Schedule, I, J),
	    New_Time = fitness:tardiness(New_Schedule, Inputs),
	    if New_Time < Best_Time ->
		    insert_loop2 (I, J+1, N, Inputs, Schedule, {New_Time, New_Schedule});
	       true ->
		    insert_loop2 (I, J+1, N, Inputs, Schedule, Solution)
	    end
    end.


-spec insert_loop1 (index(), numjobs(), inputs(), schedule(), solution()) -> solution().

insert_loop1 (I, N, Inputs, Schedule, Solution) ->
    if I > N ->
	    Solution;
       true -> 
	    New_Solution = 
		insert_loop2 (I, 1, N, Inputs, Schedule, Solution),
	    insert_loop1 (I+1, N, Inputs, Schedule, New_Solution)
    end.


-spec insert_search (solution(), numjobs(), inputs()) -> solution().

insert_search (Solution = {_, Schedule}, N, Inputs) ->
    insert_loop1(1, N, Inputs, Schedule, Solution).



% ------------------------------ Interchange search ----------------------------------

-spec split2(natlist(), index(), index()) -> {natlist(), integer(), natlist(), integer(), natlist()}.

split2 (_, I, J) when I > J  -> error ("split2: wrong order");
split2 (_, I, J) when I =:= J -> error ("split2: splitting at " ++ integer_to_list(I) ++ " twice");
split2 (L, I, J) ->
	    {A, [Y|L3]} = lists:split (J-1, L),
	    {L1, [X|L2]} = lists:split (I-1, A),
	    {L1, X, L2, Y, L3}.


-spec swap(natlist(), index(), index()) -> natlist().

swap (L, I, J) ->
    if J =:= I -> L;
       I > J -> swap (L, J, I);
       true  -> 
	    {L1, X, L2, Y, L3} = split2 (L, I, J),
	    L1 ++ [Y|L2] ++ [X|L3]
    end.



-spec swap_loop2 (index(), index(), numjobs(), inputs(), schedule(), solution()) -> solution().

swap_loop2 (I, J, N, Inputs, Schedule, Solution = {Best_Time, _}) ->
    if J > N -> Solution;
       true -> 
	    New_Schedule = swap (Schedule, I, J),
	    New_Time = fitness:tardiness(New_Schedule, Inputs),
	    if New_Time < Best_Time ->
		    swap_loop2 (I, J+1, N, Inputs, Schedule, {New_Time, New_Schedule});
	       true ->
		    swap_loop2 (I, J+1, N, Inputs, Schedule, Solution)
	    end
    end.


-spec swap_loop1 (index(), numjobs(), inputs(), schedule(), solution()) -> solution().

swap_loop1 (I, N, Inputs, Schedule, Solution) ->
    if I =:= N -> Solution;
       true -> 
	    New_Solution =
		swap_loop2 (I, I+1, N, Inputs, Schedule, Solution),
	    swap_loop1 (I+1, N, Inputs, Schedule, New_Solution)
    end.

-spec swap_search(solution(), numjobs(), inputs()) -> solution().       
swap_search (Solution = {_, Schedule}, N, Inputs) ->
    swap_loop1(1, N, Inputs, Schedule, Solution).


% Actually, we should repeat the search until nothing better is found.
% ... but if we do that, it'll make the time unpredictable.
% We could implement it for completeness, but that would require dynamic
% programming, as described by den Besten/Stuetzle (p5), based on Congram et al.


% ---------------------------- Variable neighbourhood descent ------------------------------------

-spec vnd(solution(), inputs(), #params{}) -> solution().

vnd (Solution, Inputs, Params) ->

    #params{search=Search} = Params,
    
    {Durations, _, _} = Inputs,
    Num_Jobs = tuple_size(Durations),

    case Search of
	none -> Solution;
	o1   -> swap_search   (Solution, Num_Jobs, Inputs);
	o2   -> insert_search (Solution, Num_Jobs, Inputs);
	o12  -> insert_search (swap_search   (Solution, Num_Jobs, Inputs), Num_Jobs, Inputs);
	o21  -> swap_search   (insert_search (Solution, Num_Jobs, Inputs), Num_Jobs, Inputs)
    end.

vnd_loop (Solution = {Cost, _}, Inputs, Params) ->
    Improved_Solution = vnd(Solution, Inputs, Params),
    {New_Cost, _} = Improved_Solution,
    if New_Cost =:= Cost ->
	    Solution;
       true -> vnd_loop(Improved_Solution, Inputs, Params)
    end.
    

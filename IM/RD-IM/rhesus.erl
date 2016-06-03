%%%-------------------------------------------------------------------
%%% @author Mario Moro Hernandez
%%% @copyright (C) 2015, Mario Moro Hernandez
%%% @doc
%%%     The rhesus module is a custom implementation of the Chaos Monkey
%%%     to kill IM processes at a fixed time interval. It injects
%%%     messages that are randomly sent to a router_supervisor or server
%%%     process. These processes will handle the kill request by killing
%%%     a random IM process.
%%%
%%%     The module is designed in such a way that the 25% of the processes
%%%     killed are router processes and the other 75% are server processes.
%%%     Server processes are killed with a probability of 1/6 except for
%%%     chat_session and client_monitor processes that are killed with
%%%     a probability of 1/12. These probabilities assume long runs of the
%%%     rhesus module.
%%%
%%% @end
%%% Created : 12 Aug 2015 by Mario Moro Hernandez
%%%-------------------------------------------------------------------
-module(rhesus).

-export([chaos_on/0, chaos_on/1, extract_pids/2]).

%%===============================================================================
%% CHAOS GENERATION LOGIC
%%===============================================================================
%%--------------------------------------------------------------------
%% @doc
%%     chaos/0 starts a rhesus process that kills a process at a random
%%     time between five seconds and one hour.
%%
%% @spec chaos_on() -> ok
%% @end
%%--------------------------------------------------------------------
chaos_on() ->
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    Timer = (random:uniform(3595) + 5) * 1000,
    io:format("A random process will be killed every ~p seconds.~n", [Timer div 1000]),
    chaos_on(Timer, undefined).

%%--------------------------------------------------------------------
%% @doc
%%     chaos_on/1 starts a rhesus process that kills a process at the
%%     time passed as parameter (in milliseconds).
%%
%% @spec chaos_on(Timer) -> ok
%% @end
%%--------------------------------------------------------------------
chaos_on(Timer) ->
    chaos_on(Timer, undefined).

%%--------------------------------------------------------------------
%% @doc
%%     chaos_on/2 starts the killing process sequence given that there
%%     is a router supervisor process, and this process is alive.
%%
%% @spec chaos_on(Timer, Router_Sup_Pid) -> ok
%% @end
%%--------------------------------------------------------------------
chaos_on(Timer, Router_Sup_Pid) ->
    case Router_Sup_Pid of
	undefined ->
	    rhesus_gets_nervous(Timer);
	Pid ->
	    case is_process_alive(Pid) of
		true ->
		    rhesus_attack(Timer, Pid);
		false ->
		    rhesus_gets_nervous(Timer)
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%%     rhesus_gets_nervous/1 fires the killing process sequence given
%%     that there is a router supervisor process, and this process is
%%     alive.
%%
%% @spec rhesus_gets_nervous(Timer) -> ok
%% @end
%%--------------------------------------------------------------------
rhesus_gets_nervous(Timer) ->
    case find_router_sup_pid() of
	not_found ->
	    io:format("Router supervisor process not found on this node.~n");
	R_Sup_Pid ->
	    rhesus_attack(Timer, R_Sup_Pid)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec rhesus_attack(Timer, Router_Sup_Pid)-> ok
%% @end
%%--------------------------------------------------------------------
rhesus_attack(Timer, Router_Sup_Pid)->
    timer:sleep(Timer),
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    case random:uniform(4) of
	1 ->
	    Router_Sup_Pid ! rhesus_solves_conflict_router,
	    chaos_on(Timer, Router_Sup_Pid);
	_Other ->
	    Router_Sup_Pid ! rhesus_solves_conflict_server,
	    chaos_on(Timer, Router_Sup_Pid)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     find_router_sup_pid/0 returns the pid of the server_supervisor
%%     process, if such a process exists.
%%
%% @spe find_router_sup_pid() -> pid() | not_found
%% @end
%%--------------------------------------------------------------------
find_router_sup_pid() ->
    find_router_sup_pid(erlang:processes()).

%%--------------------------------------------------------------------
%% @doc
%%     find_router_supervisor/1 finds pid of a router supervisor process
%%     from a list of pids, assuming that such process exists. If there
%%     is no server supervisor, the function returns an error.
%%
%% @spec find_router_sup_pid([]) -> pid() | not_found
%% @end
%%--------------------------------------------------------------------
find_router_sup_pid([]) ->
    not_found;
find_router_sup_pid([H|T]) ->
    {current_function, {_, F, _}} = erlang:process_info(H, current_function),
    case F of
	router_supervisor ->
	    H;
	_Other ->
	    find_router_sup_pid(T)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     extract_pids/2 returns a list of the router pids, given a list
%%     of tuples {S_Group_Name, Router_Name, Router_Pid}. 
%%     
%% @spec extract_pids(List_Pids, List_Routers) -> [pid()]
%% @end
%%--------------------------------------------------------------------
extract_pids(List_Pids, List_Routers) ->
    case List_Routers of
	[] ->
	    List_Pids;
	[H|T] ->
	    {_Name, Pid} = H,
	    extract_pids([Pid|List_Pids],T)
    end.

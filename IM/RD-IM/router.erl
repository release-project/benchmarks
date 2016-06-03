%%%-------------------------------------------------------------------
%%% ROUTER MODULE
%%%
%%% @author Mario Moro Hernandez upon a design by Natalia Chechina
%%% @copyright (C) 2014, RELEASE project
%%% @doc
%%%	Router module for the Distributed Erlang instant messenger (IM)
%%%	application developed as a real benchmark for the Scalable 
%%%	Distributed Erlang extension of the Erlang/OTP language.
%%%
%%%	This module implementes the functionality for router nodes in a
%%%	system similar to the system described in the Section 2 of the
%%%	document "Instant Messenger Architectures Design Proposal".
%%% @end
%%% Created : 25 Jul 2014 by Mario Moro Hernandez
%%%-------------------------------------------------------------------
-module(router).

-export([router_supervisor/6, router_process/1, finish_node/0]).

-import(launcher, [launch_router_processes/6, launch_server_supervisors/4]).

-import(server, [restart_server_supervisor/1]).

-import(rhesus, [extract_pids/2]).

%%===============================================================================
%% ROUTER PROCESSES CODE
%%===============================================================================

%%---------------------------------------------------------------------
%% @doc
%%     router_supervisor_monitor/1 is a process that monitors the router
%%     supervisor process, and re-starts it in the case of failure.
%%
%%     This process has three states. The initial state stablishes the
%%     link between router_supervisor and router_supervisor_monitor
%%     processes. The final state just listens for any change in the
%%     router supervisor information, and traps the router supervisor
%%     failure triggering the recovery strategy. Finally, the supervisor
%%     recovery state is in charge of the recovery of the router supervisor
%%     process.
%%
%% @spec router_supervisor_monitor/1.
%% @end
%%---------------------------------------------------------------------
router_supervisor_monitor({initial_state, R_Sup_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids}) ->
    erlang:monitor(process, R_Sup_Pid),
    router_supervisor_monitor({final_state, R_Sup_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids});

router_supervisor_monitor({final_state, R_Sup_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids}) ->
    process_flag(trap_exit, true),
    R_Sup_Mon_Pid = self(),
    receive
	%% ============== DEBUG ==============
	{request_parameters, From} ->
	    From ! {parameters, self(), R_Sup_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids},
	    router_supervisor_monitor({final_state, R_Sup_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids});
	{request_routers_info, From} ->
	    From ! {routers_info, Routers_Info},
	    router_supervisor_monitor({final_state, R_Sup_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids});
	{request_routers_list, From} ->
	    From ! {routers_list, Routers_List},
	    router_supervisor_monitor({final_state, R_Sup_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids});
	{request_servers_list, From} ->
	    From ! {servers_list, Server_List},
	    router_supervisor_monitor({final_state, R_Sup_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids});
	{request_monitored_routers, From} ->
	    From ! {monitored_routers, Monitored_Routers},
	    router_supervisor_monitor({final_state, R_Sup_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids});
	%% ===================================

	{'EXIT', normal} ->
	    R_Sup_Pid ! {'EXIT', normal},
	    process_flag(trap_exit, false),
	    timer:sleep(1000),
	    ok;
	{'DOWN', _Ref, process, Pid, Reason} ->
	    case Reason of
		killed ->
		    io:format("R_Sup_Mon received {'DOWN', _Ref, process, ~p, ~p}.~n" ++
			      "Spawning new router supervisor~n", [Pid, Reason]),
		    {New_R_S_Pid, _Ref2} = spawn_monitor(fun() -> router_supervisor(R_Sup_Mon_Pid,
										    Monitored_Routers,
										    Routers_List,
										    Server_List,
										    Routers_Info, 
										    Routers_DBs_Pids) end),
		    New_R_S_Pid ! {monitor_processes_after_recovery},
		    spawn(fun() -> notify_others_supervisor(Routers_List, New_R_S_Pid) end),
		    router_supervisor_monitor({final_state, New_R_S_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids});
		Other->
		    io:format("Reason = ~p~n", [Other])
	    end;
	{R_Sup_Pid, New_Monitored_Routers, New_Routers_List, New_Server_List, New_Routers_Info, New_Routers_DBs_Pids} ->
	    router_supervisor_monitor({final_state, R_Sup_Pid, New_Monitored_Routers, New_Routers_List, New_Server_List, New_Routers_Info, New_Routers_DBs_Pids});
	Other ->
	    io:format("R_Sup_Mon received: ~p~n", [Other]),
	    router_supervisor_monitor({final_state, R_Sup_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids})
    end.

%%--------------------------------------------------------------------
%% @doc
%%     router_supervisor/5 constitutes the router supervisor process.
%%     This process is in charge of spawning the router processes
%%     in the router node, during the deployment of the system. Once
%%     the system is deployed, the router supervisor process monitors
%%     the router processes and re-starts them if they fail.
%%
%% @spec router_supervisor(R_Sup_Mon_Pid, Monitored_Routers,
%%           Routers_List, Routers_Info, Routers_DBs_Pids) ->
%%                   router_process/1
%% @end
%%--------------------------------------------------------------------
router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids) ->
    process_flag(trap_exit, true),
    R_Sup_Pid = self(),
    receive
	%% ============== DEBUG ==============
	{request_parameters, From} ->
	    From ! {parameters, self(), R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids},
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids);
	{request_routers_info, From} ->
	    From ! {routers_info, Routers_Info},
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids);
	{request_routers_list, From} ->
	    From ! {routers_list, Routers_List},
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids);
	{request_servers_list, From} ->
	    From ! {servers_list, Server_List},
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids);
	{request_monitored_routers, From} ->
	    From ! {monitored_routers, Monitored_Routers},
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids);
	%% ===================================

	%% Setup actions
	{Server_Nodes, Servers_Router_Pr, Router_Processes, Num_Total_Servers, Routers_Listener_Pid, launch_router_processes} ->
	    io:format("Launching router processes.~n"),
	    launch_router_processes(self(), Server_Nodes, Servers_Router_Pr, Router_Processes, Num_Total_Servers, Routers_Listener_Pid),
	    {New_R_Sup_Mon_Pid, _Ref} = spawn_monitor(fun() -> router_supervisor_monitor({initial_state,
											  R_Sup_Pid,
											  Monitored_Routers,
											  Routers_List,
											  Server_List,
											  Routers_Info,
											  Routers_DBs_Pids}) end),
	    router_supervisor(New_R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids);
	{list_routers, Received_Routers_List, Received_Routers_DBs_Pids} ->
	    timer:sleep(250),
	    spawn(fun() -> feed_routers_info(R_Sup_Pid, Received_Routers_List, Server_List, Routers_Info) end),
	    monitor_routers(Monitored_Routers, Received_Routers_List),
	    R_Sup_Mon_Pid ! {R_Sup_Pid, Monitored_Routers, Received_Routers_List, Server_List, Routers_Info, Received_Routers_DBs_Pids},
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Received_Routers_List, Server_List, Routers_Info, Received_Routers_DBs_Pids);
	{new_routers_info, {New_Servers_List, New_Routers_Info}} ->
	    R_Sup_Mon_Pid ! {R_Sup_Pid, Monitored_Routers, Routers_List, New_Servers_List, New_Routers_Info, Routers_DBs_Pids},
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, New_Servers_List, New_Routers_Info, Routers_DBs_Pids);
	%% update routers information
	{monitor_processes_after_recovery} ->
	    monitor_processes(Monitored_Routers, Routers_List, R_Sup_Mon_Pid),
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids);
	{update_router, {Received_R_Name, Received_R_Pid}} ->
	    New_Routers_List = lists:keyreplace(Received_R_Name, 1, Routers_List, {Received_R_Name, Received_R_Pid}),
	    case lists:keyfind(Received_R_Name, 1, Routers_Info) of
		false ->
		    io:format("STOP!!! THIS IS WRONG!!!~n");
		{R_Name, _R_Pid, Mon_S, S_List, S_Nds} ->
		    New_Routers_Info = lists:keyreplace(Received_R_Name, 1, Routers_Info, {R_Name, Received_R_Pid, Mon_S, S_List, S_Nds}),
		    R_Sup_Mon_Pid ! {R_Sup_Pid, Monitored_Routers, New_Routers_List, Server_List, New_Routers_Info, Routers_DBs_Pids},
		    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, New_Routers_List, Server_List, New_Routers_Info, Routers_DBs_Pids)
	    end;
	{delete_router, {Received_R_Name, _Received_R_Pid}} ->
	    New_Routers_List = lists:keydelete(Received_R_Name, 1, Routers_List),
	    R_Sup_Mon_Pid ! {R_Sup_Pid, Monitored_Routers, New_Routers_List, Server_List, Routers_Info, Routers_DBs_Pids}, %% <=== CHECK Routers_Info !!!
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, New_Routers_List, Server_List, Routers_Info, Routers_DBs_Pids);
	{update_server, {Received_S_Name, Received_S_Pid}} ->
	    New_Server_List = lists:keystore(Received_S_Name, 1, Server_List, {Received_S_Name, Received_S_Pid}),
	    R_Sup_Mon_Pid ! {R_Sup_Pid, Monitored_Routers, Routers_List, New_Server_List, Routers_Info, Routers_DBs_Pids},
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, New_Server_List, Routers_Info, Routers_DBs_Pids);
	{delete_server, {Received_S_Name, _Received_S_Pid}} ->
	    New_Server_List = lists:keydelete(Received_S_Name, 1, Server_List),
	    R_Sup_Mon_Pid ! {R_Sup_Pid, Monitored_Routers, Routers_List, New_Server_List, Routers_Info, Routers_DBs_Pids},
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, New_Server_List, Routers_Info, Routers_DBs_Pids);
	%% Termination logic
	{'EXIT', normal} ->
	    process_flag(trap_exit, false),
	    spawn(fun() -> stop_routers(Routers_List)end),
	    ok;
	%% ===== Uncomment the following two lines for bencherl =====
	%% {'EXIT', _Pid, _Reason} ->
	%%    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids);
	%% Reliability control.
	{'DOWN', _Ref, process, Pid, noproc} ->
	    case rpc:pinfo(Pid, status) of
		undefined ->
		    case lists:keytake(Pid, 2, Routers_List) of
			{value, {Router_Str, Router_Pid}, New_List_Routers} ->
			    New_Monitored_Routers = Monitored_Routers -- [string_to_atom(Router_Str)],
			    spawn(fun() -> notify_others(delete_router, {Router_Str, Router_Pid}, New_List_Routers, [node()]) end),
			    spawn(fun() -> update_router_dbs(Routers_DBs_Pids, New_List_Routers) end),
			    R_Sup_Mon_Pid ! {R_Sup_Pid, New_Monitored_Routers, New_List_Routers, Routers_Info, Routers_DBs_Pids},
			    router_supervisor(R_Sup_Mon_Pid, New_Monitored_Routers, New_List_Routers, Server_List, Routers_Info, Routers_DBs_Pids);
			false ->
			    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids)
		    end;
		true ->
		    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids)
	    end;
	{'DOWN', _Ref, process, R_Sup_Mon_Pid, _Reason} ->
	    io:format("Router Supervisor Monitor with pid ~p is down.~n", [R_Sup_Mon_Pid]),
	    {New_R_Sup_Mon_Pid, _} = spawn_monitor(fun() -> router_supervisor_monitor({initial_state,
										       R_Sup_Pid,
										       Monitored_Routers,
										       Routers_List,
										       Server_List,
										       Routers_Info,
										       Routers_DBs_Pids}) end),
	    router_supervisor(New_R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids);
	{'DOWN', Ref, process, Pid, Reason} ->
	    case lists:keyfind(Pid, 2, Routers_List) of
		false ->
		    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids);
		{Router_Str, _Router_Pid} ->
		    demonitor(Ref, [flush]),
		    io:format("========================================================~n" ++
				  " Router ~s is down with reason ~p~n" ++
				  "========================================================~n",
			      [Router_Str, Reason]),
		    {_R_N,_R_P, List_Monitored_Servers, _List_Servers, Server_Nodes} = lists:keyfind(Router_Str, 1, Routers_Info),
		    {New_Router_Pid, _Ref_N_R} = spawn_monitor( 
						   fun() -> router_process({recovery_state,
									    R_Sup_Pid,
									    Router_Str,
									    Routers_List, 
									    List_Monitored_Servers,
									    Server_List,
									    Server_Nodes})
						   end),
		    %% global:register_name(string_to_atom(Router_Str), New_Router_Pid),%% <====== ADDED GLOBAL REGISTER NAME ROUTER
		    New_List_Routers = lists:keyreplace(Pid, 2, Routers_List, {Router_Str, New_Router_Pid}),
		    spawn(fun() -> notify_others(update_router, {Router_Str, New_Router_Pid}, New_List_Routers, [node()]) end), 
		    spawn(fun () -> update_router_dbs(Routers_DBs_Pids, New_List_Routers) end),
		    R_Sup_Mon_Pid ! {R_Sup_Pid, Monitored_Routers, New_List_Routers, Server_List, Routers_Info, Routers_DBs_Pids},
		    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, New_List_Routers, Server_List, Routers_Info, Routers_DBs_Pids)
	    end;
	%% Chaos Generation logic
	rhesus_solves_conflict_router ->
	    Router_Pids = extract_pids([self(), R_Sup_Mon_Pid], Routers_List),
	    check_random_seed(),
	    exit(lists:nth(random:uniform(length(Router_Pids)),Router_Pids),kill),
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids);
	rhesus_solves_conflict_server ->
	    check_random_seed(),
	    {_, Router_Pid} = lists:nth(random:uniform(length(Routers_List)),Routers_List),
	    Router_Pid ! kill_server_process,
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids);
	%% Trap for unexpected messages
	Other ->
	    io:format("router_supervisor received: ~p~n", [Other]),
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Server_List, Routers_Info, Routers_DBs_Pids)
    end.

%%---------------------------------------------------------------------
%% @doc
%%     router_process/1 has basically two missions: to spawn the server
%%     supervisor processes and forward clients messages.
%%
%%     The process has three states. During the initial state, the router
%%     process spawns the server supervisors and informs the router
%%     supervisor process. When the server supervisors are spawned and
%%     the information is passed to the router_supervisor, the router
%%     process changes of state.
%%     
%%     In the final state, the router process listens for client messages
%%     and handles the server supervisors failures.
%%
%%     There is a third state (recovery_state) that serves as the initial
%%     state when the recovery strategy has been triggered.
%%
%% @spec router_process/1 -> server_supervisor_loop/4 | ok
%% @end
%%---------------------------------------------------------------------
router_process({initial_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, Server_Nodes}) ->
    process_flag(trap_exit, true),
    receive
	%% Setup Actions
	{Router_Name, Received_Server_Nodes, Servers_Router_Pr, Num_Total_Servers, Routers_Listener_Pid, launch_server} ->
	    io:format("~p spawning servers.~nServer_Nodes:~p~n", [Router_Name, Received_Server_Nodes]),
	    launch_server_supervisors(Received_Server_Nodes, Servers_Router_Pr, Num_Total_Servers, Routers_Listener_Pid),
	    New_List_Monitored_Servers = monitored_servers(Received_Server_Nodes, List_Monitored_Servers),
	    router_process({initial_state, R_Sup_Pid, List_Routers, New_List_Monitored_Servers, Received_Server_Nodes});
	{list_routers, Received_List_Routers} ->
	    io:format("router_process({initial_state}) received list of routers: ~p~n", [List_Routers]),
	    router_process({initial_state, R_Sup_Pid, Received_List_Routers, List_Monitored_Servers, Server_Nodes});
	{list_servers, List_Servers}->
	    io:format("router_process({initial_state}) received list of servers: ~p~n", [List_Servers]),
	    monitor_servers(List_Monitored_Servers, List_Servers),
	    %% =========== ROUTER RANDOM SEED ===========
	    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	    random:seed({A,B,C}),
	    %% ==========================================
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	%% Trap for unexpected messages.
	Other ->
	    io:format("Something failed at router_process({initial_state}). It received: ~p~n", [Other]),
	    router_process({initial_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, Server_Nodes})
    end;

router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes}) ->
    receive
	%% ============== DEBUG ==============
	{request_parameters, From} ->
	    From ! {parameters, self(), R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes},
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	{request_routers_list, From} ->
	    From ! {routers_list, List_Routers},
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	{request_servers_list, From} ->
	    From ! {servers_list, List_Servers},
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	%% ===================================

	%% Client login logic
	{Client_Name, Client_Pid, login} ->
	    Server_Num = compression_function(length(List_Servers), Client_Name) + 1,
	    {_, Target_Server_Pid} = lists:keyfind("server_" ++ integer_to_list(Server_Num), 1, List_Servers),
	    Target_Server_Pid ! {Client_Name, Client_Pid, login},
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	%% Chat session request logic
	{Sender, Receiver, start_chat_session}  ->
	    {_, Target_Server_Pid} = lists:nth(random:uniform(length(List_Servers)),List_Servers),
	    Target_Server_Pid ! {Sender, Receiver, start_chat_session},
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	%% Chaos kill request forward
	kill_server_process ->
	    {_, Target_Server_Pid} = lists:nth(random:uniform(length(List_Servers)),List_Servers),
	    Target_Server_Pid ! kill_server_process,
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	%% Error Handling
	%% ==== Uncomment these two lines for bencherl ====
	%%{'EXIT', _Pid, _Reason} ->
	%%    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	%% monitored process finished normally
	{'DOWN', _Ref, process, Pid, normal} ->
	    case lists:keytake(Pid, 2, List_Servers) of
		{value, {Server_Str, Server_Pid}, New_List_Servers} ->
		    New_Monitored_Servers = List_Monitored_Servers -- [Server_Str],
		    spawn(fun() -> notify_others(delete_server, {Server_Str, Server_Pid}, List_Routers, [node()]) end),
		    R_Sup_Pid ! {delete_server, {Server_Str, Server_Pid}},
		    router_process({final_state, R_Sup_Pid, List_Routers, New_Monitored_Servers, New_List_Servers, Server_Nodes});
		false ->
		    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes})
	    end;
	%% monitored process finished abnormally.	
	{'DOWN', _Ref, process, Pid, Reason} ->
	    {Server_Str, _Server_Pid} = lists:keyfind(Pid, 2, List_Servers),
	    io:format("========================================================~n" ++ 
		      " Server ~p is down with reason ~p~n" ++ 
		      "========================================================~n",
		      [Server_Str, Reason]),
	    Node = node_name(Server_Str, Server_Nodes),
	    Server = string_to_atom(Server_Str),
	    case Reason of
		noproc ->
		    case rpc:pinfo(Pid, status) of
			undefined ->
			    case lists:keytake(Pid, 2, List_Servers) of
				{value, {Server_Str, Server_Pid}, New_List_Servers} ->
				    New_Monitored_Servers = List_Monitored_Servers -- [Server_Str],
				    spawn(fun() -> notify_others(delete_server, {Server_Str, Server_Pid}, New_List_Servers, [node()]) end),
				    R_Sup_Pid ! {delete_server, {Server_Str, Server_Pid}},
				    router_process({final_state, R_Sup_Pid, List_Routers, New_Monitored_Servers, New_List_Servers, Server_Nodes});
				false ->
				    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes})
			    end;
			true ->
			    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes})
		    end;
		noconnection ->
		    io:format("Fatal error. Node ~p is down, and thus server ~p cannot be restarted.~n", [Node, Server]),
		    case lists:keytake(Pid, 2, List_Servers) of
			{value, {Server_Str, Server_Pid}, New_List_Servers} ->
			    New_Monitored_Servers = List_Monitored_Servers -- [Server_Str],
			    spawn(fun() -> notify_others(delete_server, {Server_Str, Server_Pid}, List_Routers, [node()]) end),
			    R_Sup_Pid ! {delete_server, {Server_Str, Server_Pid}},
			    router_process({final_state, R_Sup_Pid, List_Routers, New_Monitored_Servers, New_List_Servers, Server_Nodes});
			false ->
			    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes})
		    end;
		_Other ->
		    Server_Sup_Pid = spawn_link(Node, fun() -> restart_server_supervisor({first_stage, Server, length(List_Servers)}) end),
		    erlang:monitor(process, Server_Sup_Pid),
		    unlink(Server_Sup_Pid),
		    New_List_Servers = lists:keyreplace(Pid, 2, List_Servers, {Server_Str, Server_Sup_Pid}),
		    spawn(fun() -> notify_others(update_server, {Server_Str, Server_Sup_Pid}, List_Routers, [node()]) end),
		    R_Sup_Pid ! {update_server, {Server_Str, Server_Sup_Pid}},
		    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, New_List_Servers, Server_Nodes})
	    end;
	%% Termination logic
	{'EXIT', normal} ->
	    io:format("router_process() terminated normally.~n");
	{request_router_info, Dest_Pid} ->
	    Dest_Pid ! {router_info, self(), List_Monitored_Servers, List_Servers, Server_Nodes},
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	%% update information
	{update_router_supervisor, New_R_Sup_Pid} ->
	    router_process({final_state, New_R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	{update_router, Notify_Supervisor, {R_Name, R_Pid}} ->
	    New_List_Routers = lists:keyreplace(R_Name, 1, List_Routers, {R_Name, R_Pid}),
	    if
		Notify_Supervisor == yes ->
		    R_Sup_Pid ! {update_router, {R_Name, R_Pid}},
		    router_process({final_state, R_Sup_Pid, New_List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
		true ->
		    router_process({final_state, R_Sup_Pid, New_List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes})
	    end;
	{delete_router, Notify_Supervisor, {R_Name, R_Pid}} ->
	    New_List_Routers = lists:keydelete(R_Name, 1, List_Routers),
	    if
		Notify_Supervisor == yes ->
		    R_Sup_Pid ! {delete_router, {R_Name, R_Pid}},
		    router_process({final_state, R_Sup_Pid, New_List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
		true ->
		    router_process({final_state, R_Sup_Pid, New_List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes})
	    end;
	{update_server, Notify_Supervisor, {S_Name, S_Pid}} ->
	    New_List_Servers = lists:keyreplace(S_Name, 1, List_Servers, {S_Name, S_Pid}),
	    if
		Notify_Supervisor == yes ->
		    R_Sup_Pid ! {update_server, {S_Name, S_Pid}},
		    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, New_List_Servers, Server_Nodes});
		true ->
		    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, New_List_Servers, Server_Nodes})
	    end;
	{delete_server, Notify_Supervisor, {S_Name, S_Pid}} ->
	    New_List_Servers = lists:keydelete(S_Name, 1, List_Servers),
	    if
		Notify_Supervisor == yes ->
		    R_Sup_Pid ! {delete_server, {S_Name, S_Pid}},
		    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, New_List_Servers, Server_Nodes});
		true ->
		    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, New_List_Servers, Server_Nodes})
	    end;
	{list_routers, Received_List_Routers} ->
	    router_process({final_state, R_Sup_Pid, Received_List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	Other ->
	    {Router_Str, _Router_Pid} = lists:keyfind(self(), 2, List_Routers),
	    io:format("~p router_Process received: ~p~n", [Router_Str, Other]),
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes})
    %% This time-out is to kill possible zombie router processes after five minutes.
    %% This only happens when it is killed more than one process at a time.
    %% after 300000 ->
    %% 	    ok
    end;

router_process({recovery_state, R_Sup_Pid, Router_Str, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes}) ->
    process_flag(trap_exit, true),
    New_List_Routers = lists:keyreplace(Router_Str, 1, List_Routers, {Router_Str, self()}),
    monitor_servers(List_Monitored_Servers, List_Servers),
    router_process({final_state, R_Sup_Pid, New_List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes}).

%%===============================================================================
%% AUXILIARY FUNCTIONS
%%===============================================================================
%%--------------------------------------------------------------------
%% @doc
%%     finish_node/0 fires the sequence that finishes the node.
%%
%% @spec finish_node() -> {'EXIT, normal}
%% @end
%%--------------------------------------------------------------------
finish_node() ->
     find_router_mon() ! {'EXIT', normal}.

%%--------------------------------------------------------------------
%% @doc
%%     stop_routers/1 stops the routers in the routers list passed as
%%     parameter. After finishing all routers it finishes the router
%%     node.
%%
%% @spec stop_routers(Routers_List) -> ok.
%% @end
%%--------------------------------------------------------------------
stop_routers(Routers_List) ->
    case Routers_List of
	[] ->
	    init:stop();
	[{_Name, Pid}|T] ->
	    exit(Pid, normal),
	    stop_routers(T)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     find_router_mon/0 returns the pid of a router monitor process
%%     if such a process exists.
%%
%% @spec find_router_mon() -> pid() | not_found
%% @end
%%--------------------------------------------------------------------
find_router_mon() ->
    find_router_mon(erlang:processes()).

%%--------------------------------------------------------------------
%% @doc
%%     find_router_mon/1 returns the pid of a router monitor process
%%     given a list of processes pids.
%%
%% @spec find_router_mon(Processes) -> pid() | not_found
%% @end
%%--------------------------------------------------------------------
find_router_mon(List) ->
    case List of
	[] ->
	    not_found;
	[H|T] ->
	    {current_function, {_, F, _}} = erlang:process_info(H, current_function),
	    case F of 
		router_supervisor_monitor ->
		    H;
		_Other ->
		    find_router_mon(T)
	    end
    end.

%%---------------------------------------------------------------------
%% @doc
%%     monitored_servers/2 builds a list of the servers monitored by one
%%     router after the list of server nodes received during deployment.
%%
%% @spec monitored_servers(Server_Nodes, Monitored_Servers) -> list()
%% @end
%%---------------------------------------------------------------------
monitored_servers(Server_Nodes, Monitored_Servers) ->
    case Server_Nodes of
	[] ->
	    Monitored_Servers;
	[Server|Tail_Servers] ->
	    S = atom_to_list(Server),
	    Server_Name = string:left(S, string:chr(S,$@) - 1),
	    New_Monitored_Servers = Monitored_Servers ++ [Server_Name],
	    monitored_servers(Tail_Servers, New_Monitored_Servers)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     monitor_processes/3 traverses a list of routers and establishes
%%     a monitor-monitored processes relationship between some router
%%     processes, a router_supervisor_monitor, and a router_supervisor
%%     process. This function is called after a router_supervisor 
%%     process has been recovered.
%%
%% @spec monitor_processes(list(), list(), pid()) -> ok.
%% @end
%%--------------------------------------------------------------------
monitor_processes([], _Routers_List, R_Sup_Mon_Pid) ->
    erlang:monitor(process, R_Sup_Mon_Pid),
    ok;
monitor_processes([H|T], Routers_List, R_Sup_Mon_Pid) ->
    {_R_Name, R_Pid} = lists:keyfind(atom_to_list(H), 1, Routers_List),
    erlang:monitor(process, R_Pid),
    monitor_processes(T, Routers_List, R_Sup_Mon_Pid).

%%---------------------------------------------------------------------
%% @doc
%%     monitor_servers/2 traverses a list of servers and establishes a
%%     monitor-monitored processes relationship between a router process
%%     and a server supervisor process.
%%
%% @spec monitor_servers(Monitored_Servers, Servers_List) -> ok
%% @end
%%---------------------------------------------------------------------
monitor_servers([], _Servers_List) ->
    ok;
monitor_servers([Server|Tail_Mon_Serv], Servers_List) ->
    case lists:keyfind(Server, 1, Servers_List) of
	false ->
	    monitor_servers(Tail_Mon_Serv, Servers_List);
	{_Serv, Server_Pid} ->
	    erlang:monitor(process, Server_Pid),
	    monitor_servers(Tail_Mon_Serv, Servers_List)	
    end.

%%---------------------------------------------------------------------
%% @doc
%%      monitor_routers/2 is similar to monitor_servers/2, but for the
%%      router_supervisor - router_process processes.
%%
%% @spec monitor_routers(Monitored_Routers, Routers_List -> ok
%% @end
%%---------------------------------------------------------------------
monitor_routers([], _Routers_List) ->
    ok;
monitor_routers([Router|Tail], Routers_List) ->
    case lists:keyfind(atom_to_list(Router), 1, Routers_List) of
	false ->
	    monitor_routers(Tail, Routers_List);
	{_Router, Router_Pid} ->
	    erlang:monitor(process, Router_Pid),
	    monitor_routers(Tail, Routers_List)
    end.

%%---------------------------------------------------------------------
%% @doc
%%     node_name/2 finds the server node corresponding to a server
%%     supervisor.
%%
%% @spec node_name(Server, Server_Nodes) -> node name | {error, Reason}
%% @end
%%---------------------------------------------------------------------
node_name(Server, Server_Nodes) ->
    case Server_Nodes of
	[] ->
	    io:format("Error. There is not server node corresponding to the server process.~n"),
	    {error, no_server_node_found};
	[Target_Node|Tail] ->
	    T = atom_to_list(Target_Node),
	    case Server ==  string:left(T, string:chr(T,$@) - 1) of
		true ->
		    Target_Node;
		false ->
		    node_name(Server, Tail)
	    end
    end.    

%%---------------------------------------------------------------------
%% @doc
%%     update_router_dbs/2 updates the information of the routers_db
%%     processes when a router process has changed.
%%
%% @spec update_router_dbs(Routers_DBs, List) -> ok.
%% @end
%%---------------------------------------------------------------------
update_router_dbs(Routers_DBs, List) ->
    case Routers_DBs of
	[] ->
	    ok;
	[R_DB_Pid|Tail] ->
	    R_DB_Pid ! {List, receive_router_list},
	    update_router_dbs(Tail, List)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     feed_routers_info/3 gathers the information about the different
%%     routers present at the architecture and feeds the router_supervisor
%%     process with it. This is done when the start of the RD-IM. 
%%     
%% @spec feed_routers_info(pid(), list(), list(), list()) -> ok.
%% @end
%%--------------------------------------------------------------------
feed_routers_info(R_Sup_Pid, [], Servers_List, Routers_Info) ->
    R_Sup_Pid ! {new_routers_info, {Servers_List, Routers_Info}},
    ok;
feed_routers_info(R_Sup_Pid, [{Router_Name, Router_Pid}|T], Servers_List, Routers_Info) ->			    
    Router_Pid ! {request_router_info, self()},
    receive
	{router_info, Router_Pid, List_Mon_S, List_S, S_Nds} ->
	    feed_routers_info(R_Sup_Pid, T, List_S, [{Router_Name, Router_Pid, List_Mon_S, List_S, S_Nds}|Routers_Info]);
	{router_info, Received_Router_Pid, _List_Mon_S, _List_S, _S_Nds} ->
	    io:format("Received_Router_Pid = ~p; Router_Pid = ~p~n", [Received_Router_Pid, Router_Pid]),
	    feed_routers_info(R_Sup_Pid, T, Servers_List, Routers_Info);
	Other ->
	    io:format("feed_routers_info/3 received:~n~p~n", [Other]),
	    feed_routers_info(R_Sup_Pid, T, Servers_List, Routers_Info)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     notify_others_supervisor/2 notifies the router processes the new
%%     pid() of their router_supervisor process, when it has been
%%     recovered.
%%
%% @spec notify_others_supervisor(list(), pid()) -> ok
%% @end
%%--------------------------------------------------------------------
notify_others_supervisor([], _R_Sup_Pid) ->
    ok;
notify_others_supervisor([{_R_Name,R_Pid}|T], R_Sup_Pid) ->
    if
	node(R_Pid) == node(self()) ->
	    R_Pid ! {update_router_supervisor, R_Sup_Pid},
	    notify_others_supervisor(T, R_Sup_Pid);
	true ->
	    notify_others_supervisor(T, R_Sup_Pid)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     notify_others/4 notifies the router processes the new information
%%     about a failed and recovered router or server_supervisor process.
%%     This way, the other router processes can keep fresh and updated
%%     information about the architecture of the RD-IM.
%% @spec
%% @end
%%--------------------------------------------------------------------
notify_others(_Action, _Router, [], _Notified_Nodes) ->
    ok;
notify_others(Action, {R_Name, R_Pid}, [{_Target_R_Name, Target_R_Pid}|T], Notified_Nodes) ->
    case lists:member(node(Target_R_Pid), Notified_Nodes) of
	true when Target_R_Pid =/= R_Pid->
	    Target_R_Pid ! {Action, no, {R_Name, R_Pid}},
	    notify_others(Action, {R_Name, R_Pid}, T, Notified_Nodes);
	false when Target_R_Pid =/= R_Pid ->
	    Target_R_Pid ! {Action, yes, {R_Name, R_Pid}},
	    notify_others(Action, {R_Name, R_Pid}, T, [node(Target_R_Pid)|Notified_Nodes]);
	true ->
	    notify_others(Action, {R_Name, R_Pid}, T, Notified_Nodes)
    end.

%% ------------------
%% Hashing functions.
%% ------------------

%%---------------------------------------------------------------------
%% @doc
%%     compression_function/2 returns the hash value within the interval
%%     [1,Number of Servers], for a client name.
%%     
%%     The compression function is:
%%     (((A * I) + B) rem P) rem Num_Servers
%%
%%     where:
%%          A is randomly generated parameter.
%%          B is randomly generated parameter.
%%          I is the hash code of the client name.
%%          P is a prime number greater than the number of buckets
%%            (servers).
%%     
%%     However, A and B are hard-coded random values to avoid
%%     inconsistencies.
%%
%% @spec compression_function(Num_Servers, Client_Name) -> integer()
%% @end
%%---------------------------------------------------------------------
compression_function(Num_Servers, Client_Name) ->
    case is_atom(Client_Name) of
	true ->
	    I = hash_code(atom_to_list(Client_Name), 0);
	false ->
	    I = hash_code(Client_Name, 0)
    end,
    P = 4294967291, %%highest 32-bit prime number
    (((33 * I) + 429496) rem P) rem Num_Servers.

%%---------------------------------------------------------------------
%% @doc
%%     hash_code/2 calculates a hash code for a given string.
%%
%% @spec hash_code(String, Hash_Code) -> integer()
%% @end
%%---------------------------------------------------------------------
hash_code(String, Hash_Code) ->
    case length(String) > 0 of
	true ->
	    Hash_Shift = Hash_Code bsl 5,%% or Hash_Code bsr 27,
	    [H|T] = String,
	    New_Hash_Code =  Hash_Shift + H,
	    hash_code(T, New_Hash_Code);
	false ->
	    Hash_Code
    end.

%% -------------------------
%% Other auxiliary functions
%% -------------------------

%%--------------------------------------------------------------------
%% @doc
%%     string_to_atom/1 takes a string and returns an atom, or an
%%     existing atom if that is the case.
%%
%% @spec string_to_atom(String) -> atom() | existing_atom()
%% @end
%%--------------------------------------------------------------------
string_to_atom(String) ->
    try list_to_existing_atom(String) of
	Val ->
	    Val
    catch
	error:_ ->
	    list_to_atom(String)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     check_random_seed/0 checks whether the router_supervisor process
%%     has a random seed set. It sets a random seed if no such a random
%%     seed has been set.
%%
%% @spec check_random_seed() -> ok
%% @end
%%--------------------------------------------------------------------
check_random_seed() ->
    case erlang:process_info(self(), dictionary) of
	{dictionary, []} ->
	    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	    random:seed({A,B,C});
	_Other ->
	    ok
    end.

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
-module(router_v2).
%%-export([router_supervisor/5, router_process/1, finish_node/0]).
-compile(export_all).

-import(launcher, [launch_router_processes/6, launch_server_supervisors/4]).

-import(server, [restart_server_supervisor/1]).

-import(rhesus, [extract_pids/2]).

-include("config.hrl").

%% -record(config, {pid, mon_rtrs, rtrs_list, rtrs_info, rtrs_dbs}).

-type config() :: #config{}.
%% -type router() :: #router{}.
%% -type server() :: #server{}.

-spec router_supervisor_monitor(Config) -> no_return() when
      Config :: config().

-spec router_supervisor({atom(), Config}) -> no_return() when
      Config :: config().

%% ===================================================================
%% ROUTER PROCESSES 
%% ===================================================================
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
%% @spec router_supervisor_monitor(config()).
%% @end
%%---------------------------------------------------------------------
router_supervisor_monitor(Config) ->
    process_flag(trap_exit, true),
    receive
	{monitor, Pid} ->
	    erlang:monitor(process, Pid),
	    router_supervisor_monitor(Config);
	{update, New_Config} ->
	    router_supervisor_monitor(New_Config);
	{show_config, Pid} ->
	    Pid ! {config, Config},
	    router_supervisor_monitor(Config);
	%% {'DOWN', _Ref, process, Pid, Reason} ->
	%%     io:format("================================================================================" ++
	%% 	      " Router Supervisor with pid: ~p is down with reason ~p~n" ++
	%% 	      " Spawning new Router Supervisor.~n" ++
	%% 	      "================================================================================", [Pid, Reason]),
	%%     New_Config = Config#config{pid = self()},
	%%     spawn_monitor(fun() -> router_supervisor({recovery_state, New_Config}) end),
	%%     router_supervisor_monitor(New_Config);
	Other->
	    io:format("router_supervisor_monitor/1 received: ~p~n", [Other]),
	    router_supervisor_monitor(Config)
    end .

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
router_supervisor({recovery_state, Config}) ->
    process_flag(trap_exit, true),
    #config{pid = Pid, mon_rtrs = Monitored_Routers, rtrs_list = List_Routers} = Config,
    erlang:monitor(process, Pid),
    io:format("Monitored_Routers = ~p~n" ++
	      "List_Routers = ~p~n", [Monitored_Routers, List_Routers]),
    lists:foreach(fun({Name, R_Pid}) ->
			  case lists:member(string_to_atom(Name), Monitored_Routers) of
			      true ->
				  erlang:monitor(process, R_Pid);
			      false ->
				  ok
			  end
		  end, List_Routers),
    router_supervisor({normal_state, Config});

router_supervisor({normal_state, Config}) ->
    process_flag(trap_exit, true),
    Self = self(),
    receive
	%% Setup actions
	{launch_router_processes, Server_Nodes, Servers_Router_Pr, Router_Processes, Num_Total_Servers, Routers_Listener_Pid} ->
	    {New_R_Sup_Mon_Pid, _Ref} = spawn_monitor(fun() -> 
							      router_supervisor_monitor(Config)
						      end),
   	    New_R_Sup_Mon_Pid ! {monitor, Self},
	    spawn(fun() -> launch_router_processes(Self,
						   Server_Nodes,
						   Servers_Router_Pr,
						   Router_Processes,
						   Num_Total_Servers,
						   Routers_Listener_Pid)
		  end),
	    router_supervisor({normal_state, Config#config{pid=New_R_Sup_Mon_Pid}});
	{list_routers, New_List_Routers, New_Routers_DBs} ->
	    lists:foreach(fun({Name, Pid}) -> case lists:member(string_to_atom(Name), Config#config.mon_rtrs) of
						  true ->
						      erlang:monitor(process, Pid);
						  false -> ok
					      end
			  end, New_List_Routers),
	    New_Config = Config#config{rtrs_list = New_List_Routers, rtrs_dbs = New_Routers_DBs},
	    Config#config.pid ! {update, New_Config},
	    router_supervisor({normal_state, New_Config});
	
	%% Update info
	{update_router_info, New_R_Info} ->
	    io:format("New_R_Info = ~p~n", [New_R_Info]),
	    router_supervisor({normal_state, Config});
	
	%% ---------------------
	%% fault tolerance logic
	%% ---------------------
	{'EXIT', _Pid, _Reason} ->
	    router_supervisor({normal_state, Config});
	{'DOWN', _Ref, process, _Pid, normal} ->
	    router_supervisor({normal_state, Config});
	{'DOWN', _Ref, process, Pid, Reason} when Pid == Config#config.pid ->
	    io:format("================================================================================" ++
		      " Router Supervisor Monitor is down with reason ~p~n" ++
		      " Spawning new Router Supervisor Monitor.~n" ++
		      "================================================================================", [Reason]),
	    {New_Pid, _New_Ref} = spawn_monitor(fun() ->
							router_supervisor_monitor(Config#config{pid = self()})
						end),
	    New_Pid ! {monitor, self()},
	    router_supervisor({normal_state, Config#config{pid=self()}});
	{'DOWN', _Ref, process, Pid, Reason} ->
	    {R_Name, Pid, New_Rtrs_List} = extract_router(Pid, Config),
	    io:format("================================================================================" ++
		      " Router ~p is down with reason ~p~n" ++
		      " Spawning new Router process. (Cought by pid = ~p)~n" ++
		      "================================================================================", [R_Name, Reason, self()]),
	    {New_Pid, _New_Ref} = spawn_monitor(fun() ->
							router_process({final_state, [Config#config.rtrs_info]})
						end),
	    io:format("YET TO BE IMPLEMENTED~n"),
	    New_Config = Config#config{rtrs_list=[{R_Name, New_Pid}|New_Rtrs_List]},
	    Config#config.pid!{update, New_Config},
	    router_supervisor({normal_state, New_Config});
	%% ============== DEBUG ==============
	{request_routers_info, From} ->
	    From ! {routers_info, Config#config.rtrs_info},
	    router_supervisor({normal_state, Config});
	{request_routers_list, From} ->
	    From ! {routers_list, Config#config.rtrs_list},
	    router_supervisor({normal_state, Config});
	{request_monitored_routers, From} ->
	    From ! {monitored_routers, Config#config.mon_rtrs},
	    router_supervisor({normal_state, Config});
	test ->
	    io:format("It worked! :-D ~n"),
	    router_supervisor({normal_state, Config});
	%% ===================================
	Other ->
	    io:format("router_supervisor/5 received: ~p~n.", [Other]),
	    router_supervisor({normal_state, Config})
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
%% @spec router_process/1 -> server_supervisor_loop/4
%% @end
%%---------------------------------------------------------------------
router_process({initial_state, R_Sup_Pid, Routers_List, Monitored_Servers, Server_Nodes}) ->
    process_flag(trap_exit, true),
    receive
	%% Setup actions
	{launch_server, Router_Name, Received_Server_Nodes, Servers_Router_Pr, Num_Total_Servers, Routers_Listener_Pid} ->
	    io:format("~p spawning servers.~n" ++
			  "Server_Nodes:~p~n", [Router_Name, Received_Server_Nodes]),
	    launch_server_supervisors(Received_Server_Nodes, Servers_Router_Pr, Num_Total_Servers, Routers_Listener_Pid),
	    New_Monitored_Servers = monitored_servers(Received_Server_Nodes, Monitored_Servers),
	    router_process({initial_state, R_Sup_Pid, Routers_List, New_Monitored_Servers, Server_Nodes});
	{list_routers_servers, List_Routers, List_Servers} ->
	    %% ============ RANDOM SEED HERE ============
	    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	    random:seed({A,B,C}),
	    %% ==========================================
	    Servers = monitor_server(List_Servers, Monitored_Servers, []),
	    Router_Name = router_name(List_Routers, self()),
	    R_Info = #router{name = Router_Name, srvrs = Servers, srvr_nds = Server_Nodes},
	    %% io:format("R_Info = ~p~n", [R_Info]),
	    router_process({final_state, R_Sup_Pid, R_Info});
	%% Trap for unexpected messages.
	Other ->
	    io:format("Something failed at router_process({initial_state}). It received: ~p~n", [Other]),
	    router_process({initial_state, R_Sup_Pid, Routers_List, Monitored_Servers, Server_Nodes})
    end;
router_process({final_state, R_Sup_Pid, R_Info}) ->
    receive
	%% Client logic request logic
	{Client_Name, Client_Pid, login} ->
	    List_Servers = R_Info#router.srvrs,
	    #server{pid=Target_Server_Pid} = lists:nth((compression_function(length(List_Servers), Client_Name) + 1), List_Servers),
	    Target_Server_Pid ! {Client_Name, Client_Pid, login},
	    router_process({final_state, R_Sup_Pid, R_Info});
	%% Chat session request logic
	{Sender, Receiver, start_chat_session} ->
	    List_Servers = R_Info#router.srvrs,
	    #server{pid=Target_Server_Pid} = lists:nth(random:uniform(length(List_Servers)),List_Servers),
	    Target_Server_Pid ! {Sender, Receiver, start_chat_session},
	    router_process({final_state, R_Sup_Pid, R_Info});
	test ->
	    router_process({final_state, R_Sup_Pid, R_Info});
	%% ---------------------
	%% fault tolerance logic
	%% ---------------------
	{'DOWN', _Ref, process, Pid, normal} ->
	    %% New_List_Srv = lists:keydelete(Pid, 2, List_Servers),
	    New_R_Info = R_Info#router{srvrs=lists:keydelete(Pid,2,R_Info#router.srvrs)},
	    R_Sup_Pid ! {update_router_info, New_R_Info},
	    router_process({final_state, R_Sup_Pid, New_R_Info});
	    %% router_process({final_state, New_List_Srv});
	{'DOWN', _Ref, process, Pid, Reason} ->
	    io:format("YET TO BE IMPLEMENTED~n"),
	    New_R_Info = R_Info#router{srvrs=lists:keydelete(Pid,2,R_Info#router.srvrs)},
	    %% New_List_Srv = lists:keydelete(Pid, 2, List_Servers),
	    router_process({final_state, R_Sup_Pid, New_R_Info});
	    %% router_process({final_state, New_List_Srv});
	Any ->
	    io:format("router_process({final_state}) received:~n~p~n", [Any]),
	    router_process({final_state, R_Sup_Pid, R_Info})
	    %% router_process({final_state, List_Servers})
    end.


finish_node() ->
    %% TO DO
    ok.

%% ===================================================================
%% UTILITY FUNCTIONS
%% ===================================================================
monitored_servers([], Mon_Srvs) ->
    Mon_Srvs;
monitored_servers([H|T], Mon_Srvs) ->
    S = atom_to_list(H),
    Srv_Name = string:left(S, string:chr(S,$@) - 1),
    monitored_servers(T, [Srv_Name|Mon_Srvs]).

monitor_server([], _Monitored_Servers, Servers) ->
    Servers;
monitor_server([{Name, Pid}|T], Monitored_Servers, Servers) ->
    case lists:member(Name, Monitored_Servers) of
	true ->
	    erlang:monitor(process, Pid),
	    Srv = #server{name=Name, pid=Pid, monitored=true},
	    monitor_server(T, Monitored_Servers, [Srv|Servers]);
	false ->
	    Srv = #server{name = Name, pid = Pid, monitored = false},
	    monitor_server(T, Monitored_Servers, [Srv|Servers])
    end.

extract_router(Pid, Config) ->
    case lists:keytake(Pid,2,Config#config.rtrs_list) of
	false ->
	    {undefined, Pid, Config#config.rtrs_list}; 
	{value, {R_Name, Pid}, Rtrs_List} ->
	    {R_Name, Pid, Rtrs_List}
    end.

router_name([], _R_Pid)->
    undefined;
router_name(Routers_List, R_Pid) ->
    case Routers_List of 
	[{Name, R_Pid}|_T] ->
	    Name;
	[_H|T] ->
	    router_name(T, R_Pid)
    end.
%% -----------------
%% Hashing functions
%% -----------------

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
%% -----------------------
%% Other utility functions
%% -----------------------

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

    
    

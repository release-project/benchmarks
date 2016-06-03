%%%--------------------------------------------------------------------
%%% SERVER MODULE
%%%
%%% @author: Mario Moro Hernandez upon a design by Natalia Chechina
%%% @copyright (C) 2014, RELEASE project
%%% @doc
%%%	Server module for the Reliable Distributed Erlang instant
%%%     messenger (RD-IM) application developed as a real benchmark for
%%%     the Scalable Distributed Erlang extension of the Erlang/OTP
%%%     language.
%%%
%%%	This module implementes the functionality for server nodes in a
%%%	system similar to the system described in the Section 2 of the
%%%	document "Instant Messenger Architectures Design Proposal".
%%% @end 
%%% Created: 1 Jul 2014 by Mario Moro Hernandez
%%%--------------------------------------------------------------------
-module(server).

-export([start_server_supervisor/3, stop_server_supervisor/1, restart_server_supervisor/1, finish_node/0]).

%%---------------------------------------------------------------------
%% @doc
%%     Starts the server by spawing a server supervisor process.
%%
%% @spec start_server_supervisor(Node, Server_Name, Num_Total_Servers) ->
%%          Pid | {error, Error}
%% @end
%%---------------------------------------------------------------------
start_server_supervisor(Node, Server_Name, Num_Total_Servers) ->
    process_flag(trap_exit, true),
    %% =============== RANDOM SEED ===============
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    %% ===========================================
    Mon_Process_Table = monitored_processes_1,
    case ets:info(Mon_Process_Table) of
	undefined ->
	    ets:new(Mon_Process_Table, [set, named_table]);
	_Other ->
	    ok
    end,
    DBs = [monitored_processes_2] ++ client_db_name(Server_Name, Num_Total_Servers) ++ chat_db_name(Server_Name, Num_Total_Servers), 
    Monitored_DBs = spawn_databases(Node, DBs, Mon_Process_Table, []),
    register_dbs(Monitored_DBs),
    recover_monitor_db(monitored_processes_2, monitored_processes_1),
    {Client_DBs, Chat_DBs} = server_dbs(Server_Name),
    Monitored_Processes = {monitored_processes_1, monitored_processes_2},
    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers).

%%---------------------------------------------------------------------
%% @doc
%%     This function triggers the recovery strategy for the server
%%     supervisor process. It runs in two stages. The first one sets up
%%     the monitored_db process if it is not alive, and the ets table
%%     that holds the information about the processes monitored by the
%%     server_supervisor process. Then, it triggers the recovery strategy
%%     for the monitored_db process.
%%     The second stage, feeds the information received into the ets
%%     table of the server_supervisor process.     
%%
%% @spec start_server_supervisor(Node, Server_Name, Num_Total_Servers) ->
%%          Pid | {error, Reason}
%% @end
%%---------------------------------------------------------------------
restart_server_supervisor({first_stage, Server_Name, Num_Total_Servers}) ->
    process_flag(trap_exit, true),
    case whereis(provisional_container) of
	undefined ->
	    register(provisional_container, spawn(fun() -> provisional_container([]) end));
	_Pid ->
	    ok
    end,
    Mon_Process_Table = monitored_processes_1,
    case whereis(monitored_processes_2) of
     	undefined ->
	    {New_Mon_Process_DB_Pid, _Ref} = spawn_monitor(fun() -> monitored_db:monitored_db(monitored_processes_2) end),
	    register(monitored_processes_2, New_Mon_Process_DB_Pid),
	    New_Mon_Process_DB_Pid ! {undefined, add, New_Mon_Process_DB_Pid, "Monitor_DB", monitored_processes_2},
     	    ok;
	Mon_Process_DB_Pid ->
	    case ets:info(Mon_Process_Table) of
		undefined ->
		    ets:new(Mon_Process_Table, [set, named_table]);
		_Other ->
		    ok
	    end,
	    Mon_Process_DB_Pid ! {self(), recover_server, Mon_Process_Table, monitored_processes_2}
    end,
    restart_server_supervisor({second_stage, undefined, Server_Name, {Mon_Process_Table, monitored_processes_2}, Num_Total_Servers});

restart_server_supervisor({second_stage, _Div_Pid, Server_Name, Monitored_Processes, Num_Total_Servers}) ->
    {Mon_Process_Table, _Mon_Process_DB} = Monitored_Processes,
    receive
	{monitored_db_binary, Binary} ->
	    {Client_DBs, Chat_DBs} = server_dbs(Server_Name),
	    Processes = binary_to_term(Binary),
	    mon_proc(Processes),
	    ets:insert(Mon_Process_Table, Processes),
	    whereis(provisional_container) ! {flush_buffer, self()},
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers);
    	Other ->
	    whereis(provisional_container) ! Other,
    	    restart_server_supervisor({second_stage, _Div_Pid, Server_Name, Monitored_Processes, Num_Total_Servers})
    after 1000 ->
	    io:format("Emergency server supervisor recovery.~n"),
	    Processes = [{Pid, Function} ||
			    {Pid, {_, {_, Function, _}}} <- [{Pid, process_info(Pid, current_function)}
							     || Pid  <-processes()]],
	    safe_restart(Mon_Process_Table, Processes),
	    {Client_DBs, Chat_DBs} = server_dbs(Server_Name),
	    whereis(provisional_container) ! {flush_buffer, self()},
	    case whereis(monitored_processes_2) of
		undefined ->
		    {New_Mon_Process_DB_Pid, _Ref} = spawn_monitor(fun() -> monitored_db:monitored_db(monitored_processes_2) end),
		    register(monitored_processes_2, New_Mon_Process_DB_Pid),
		    New_Mon_Process_DB_Pid ! {undefined, add, New_Mon_Process_DB_Pid, "Monitor_DB", monitored_processes_2},
		    ets:insert(Mon_Process_Table, {New_Mon_Process_DB_Pid, "Monitor_DB", monitored_processes_2}),
		    spawn(fun() -> recover_monitor_db(monitored_processes_2, Mon_Process_Table) end),
		    ok;
		_Mon_Process_DB_Pid ->
		    spawn(fun() -> recover_monitor_db(monitored_processes_2, Mon_Process_Table) end),
		    ok
	    end,
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers)
    end.

%%--------------------------------------------------------------------
%% @doc
%%      safe_restart/2 is a backup function to restart the server
%%      supervisor if the restart_server_supervisor/1 function fails.
%%
%% @spec safe_restart(_Mon_Process_Table, Processes) -> ok
%% @end
%%--------------------------------------------------------------------
safe_restart(_Mon_Process_Table, []) ->
    ok;
safe_restart(Mon_Process_Table, [{Pid, Function}|T_Processes]) ->
    case Function of
	monitored_db_loop ->
	    ets:insert(Mon_Process_Table, {Pid, "Monitor_DB", monitored_processes_2}),
	    erlang:monitor(process, Pid),
	    safe_restart(Mon_Process_Table, T_Processes);
	chat_db_loop ->
	    ets:insert(Mon_Process_Table, {Pid, "Chat_DB", unknown}),
	    erlang:monitor(process, Pid),
	    safe_restart(Mon_Process_Table, T_Processes);
	client_db_loop ->
	    ets:insert(Mon_Process_Table, {Pid, "Client_DB", unknown}),
	    erlang:monitor(process, Pid),
	    safe_restart(Mon_Process_Table, T_Processes);
	client_monitor ->
	    ets:insert(Mon_Process_Table, {Pid, "Client_Monitor", unknown}),
	    erlang:monitor(process, Pid),
	    safe_restart(Mon_Process_Table, T_Processes);
	chat_session ->
	    ets:insert(Mon_Process_Table, {Pid, "Chat_Session", unknown}),
	    erlang:monitor(process, Pid),
	    safe_restart(Mon_Process_Table, T_Processes);
	_Other ->
	    safe_restart(Mon_Process_Table, T_Processes)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     Stops the server with the same pid as the Pid parameter.
%%
%% @spec stop_server_supervisor(Pid) -> {'EXIT', normal}
%% @end
%%--------------------------------------------------------------------
stop_server_supervisor(Pid) ->
    Pid!{'EXIT', normal}.

%%--------------------------------------------------------------------
%% @doc
%%     This function constitutes the server supervisor process. It
%%     handles the requests made to the server, and also monitors all
%%     the server processes.
%%
%% @spec server_supervisor_loop(Client_DB_Name, Chat_DB_Name, Monitored_Dbs)
%% @end
%%--------------------------------------------------------------------
server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers) ->
    {Monitored_Proc_Table, Monitored_Proc_DB} = Monitored_Processes,
    receive
	%% Client login request.
	{Client_Name, Client_Pid, login} ->
	    Server_Supervisor_Pid = self(),
	    {Client_Monitor_Pid, _Cl_Mon_Ref} = spawn_monitor(fun() -> client_monitor(Client_Name,
										      Client_Pid,
										      [],
										      Client_DBs,
										      Server_Supervisor_Pid)
							      end),
	    Client_Monitor_Pid ! {start_client_monitor},
	    ets:insert(Monitored_Proc_Table, {Client_Monitor_Pid, "Client_Monitor", Client_Name}),
	    send_to_monitored_db(Monitored_Proc_DB, {undefined, add, Client_Monitor_Pid, "Client_Monitor", Client_Name}),
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers);
	%% Chat session request.
	{Sender, Receiver, start_chat_session} ->
	    {Chat_Session_Pid, _Ch_Ses_Ref} = spawn_monitor(fun() -> chat_session(Chat_DBs,
										  [{Sender, undefined},
										   {Receiver, undefined}],
										  Num_Total_Servers)
							    end),
	    Chat_Session_Pid ! {self(), Sender, Receiver, start_chat_session},
	    ets:insert(Monitored_Proc_Table, {Chat_Session_Pid, "Chat_Session", chat_session_name(Sender, Receiver)}),
	    send_to_monitored_db(Monitored_Proc_DB, {undefined, add, Chat_Session_Pid, "Chat_Session", chat_session_name(Sender, Receiver)}),
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers);
	%% Finish server_supervisor.
	{error, chat_session_already_started} ->
	    io:format("Session already exists: chat_session process aborted.~n"),
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers);
	%% Confirmation server supervisor is down without any problems.
       	{'EXIT', normal} ->
	    process_flag(trap_exit, false),
	    stop_dbs(Client_DBs, Chat_DBs);
	%% Confirmation client is logged in.
	{login_success, _Client_Monitor_Pid} ->
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers);
	%% Confirmation chat session started. (This is never executed).
	{session_added, ok} ->
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers);
	%% Reliability control.
	%% ==== Uncomment the following two lines for bencherl ====
	%%{'EXIT', _Pid, _Reason} ->
	%%    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers);
	%% monitored process finished normally
	{'DOWN', _Ref, process, Pid, normal} ->
	    ets:delete(Monitored_Proc_Table, Pid),
	    send_to_monitored_db(Monitored_Proc_DB, {undefined, remove, Pid}),
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers);
	%% Try to monitor processes that do not exist any longer.
	{'DOWN', _Ref, process, Pid, noproc} ->
	    ets:delete(Monitored_Proc_Table, Pid),
	    send_to_monitored_db(Monitored_Proc_DB, {undefined, remove, Pid}),
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers);
	%% Monitored process finished abnormally.
	{'DOWN', Ref, process, Pid, Reason} ->
	    io:format("Process with pid ~p and ref ~p is down with reason ~p~n", [Pid, Ref, Reason]),
	    [_Pid, Type, Name] = look_up_pid(Monitored_Proc_Table, Pid),
	    case Type of
		"Monitor_DB" ->
		    {New_Monitored_Proc_DB_Pid, _Ref} = spawn_monitor(fun() -> monitored_db:monitored_db(Monitored_Proc_DB) end),
		    register(Monitored_Proc_DB, New_Monitored_Proc_DB_Pid),
		    ets:delete(Monitored_Proc_Table, Pid),
		    ets:insert(Monitored_Proc_Table, {New_Monitored_Proc_DB_Pid, "Monitor_DB", Monitored_Proc_DB}),
		    spawn(fun() -> recover_monitor_db(Monitored_Proc_DB, Monitored_Proc_Table) end),
		    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers);
		"Client_DB" ->
		    Tokens = string:tokens(atom_to_list(Name), "_"),
	    	    {New_Client_DBs, New_Chat_DBs} = replicate_db(Pid, Tokens, Client_DBs, Chat_DBs, Monitored_Processes),
		    db_recovery(Tokens),
		    server_supervisor_loop(Server_Name, New_Client_DBs, New_Chat_DBs, Monitored_Processes, Num_Total_Servers);
		"Chat_DB" ->
		    Tokens = string:tokens(atom_to_list(Name), "_"),
	    	    {New_Client_DBs, New_Chat_DBs} = replicate_db(Pid, Tokens, Client_DBs, Chat_DBs, Monitored_Processes),
	    	    db_recovery(Tokens),
		    server_supervisor_loop(Server_Name, New_Client_DBs, New_Chat_DBs, Monitored_Processes, Num_Total_Servers);
		"Chat_Session" ->
		    chat_session_recovery({initial_state, Pid, Chat_DBs, Monitored_Processes}),
		    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers);
		"Client_Monitor" ->
		    client_monitor_recovery(Pid, Client_DBs, self(), Monitored_Processes),
		    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers)
	    end;
	{recovered, ok} ->
	    New_Client_DBs = db_names_swap(Client_DBs),
	    New_Chat_DBs = db_names_swap(Chat_DBs),
	    server_supervisor_loop(Server_Name, New_Client_DBs, New_Chat_DBs, Monitored_Processes, Num_Total_Servers);
	kill_server_process ->
	    case random:uniform(60) of
		Number when Number =< 12 ->
		    Pid_To_Kill = whereis(monitored_processes_2);
		Number when Number > 12, Number =< 24 ->
		    Processes_To_Kill = ets:select(Monitored_Proc_Table,
						   [{{'$0', '$1', '$2'},
						     [{'==', '$1', "Chat_DB"}],
						     ['$0']}]),
		    Pid_To_Kill = lists:nth(random:uniform(length(Processes_To_Kill)),Processes_To_Kill);
		Number when Number > 24, Number =< 36 ->
		    Processes_To_Kill = ets:select(Monitored_Proc_Table,
						   [{{'$0', '$1', '$2'},
						     [{'==', '$1', "Client_DB"}],
						     ['$0']}]),
		    Pid_To_Kill = lists:nth(random:uniform(length(Processes_To_Kill)),Processes_To_Kill);
		Number when Number > 36, Number =< 48 ->
		    Pid_To_Kill = self();
		Number when Number > 48, Number =< 54 ->
		    Processes_To_Kill = ets:select(Monitored_Proc_Table,
						   [{{'$0', '$1', '$2'},
						     [{'==', '$1', "Client_Monitor"}],
						     ['$0']}]),
		    Pid_To_Kill = lists:nth(random:uniform(length(Processes_To_Kill)),Processes_To_Kill);
		Number when Number > 54 ->
		    Processes_To_Kill = ets:select(Monitored_Proc_Table,
						   [{{'$0', '$1', '$2'},
						     [{'==', '$1', "Chat_Session"}],
						     ['$0']}]),
		    Pid_To_Kill = lists:nth(random:uniform(length(Processes_To_Kill)),Processes_To_Kill)
	    end,
	    if 
		is_pid(Pid_To_Kill) ->
		    exit(Pid_To_Kill, kill);
		true ->
		    ok
	    end,			      
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers);
	%% Trap for any other messages.
	Other ->
	    io:format("Server supervisor received: ~p~n", [Other]),
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes, Num_Total_Servers)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     This function constitutes the chat_session process. It forwards
%%     the messages received from Client A to the Client B. It is also
%%     responsible of handling the chat session termiation and notifying
%%     the clients when this happens.
%%
%% @spec chat_session(Chat_DBs, Clients_Info, Num_Total_Servers).
%% @end
%%--------------------------------------------------------------------
chat_session(Chat_DBs, Clients_Info, Num_Total_Servers) ->
    {Chat_DB_Name, Chat_DB_Replica} = Chat_DBs,
    [{Client_A, Client_A_Pid}, {Client_B, Client_B_Pid}] = Clients_Info,
    Session_Name = chat_session_name(Client_A, Client_B),
    receive
	%% Start Chat Session.
	{_, Sender, Receiver, start_chat_session} ->
	    Session_Name = chat_session_name(Sender, Receiver),
	    send_to_db(Chat_DB_Name, {self(), peak, Session_Name}),
	    chat_session(Chat_DBs, Clients_Info, Num_Total_Servers);
	%% Session setup.
	[] ->
	    {Client_A, New_Client_A_Pid} = {Client_A, find_client_pid(Client_A, Num_Total_Servers)},%%1)},
	    {Client_B, New_Client_B_Pid} = {Client_B, find_client_pid(Client_B, Num_Total_Servers)},%%1)},
	    case New_Client_B_Pid of
		client_not_found ->
		    New_Client_A_Pid ! {receiver_not_found, Client_B};
		_ ->
		    New_Clients_Info = [{Client_A, New_Client_A_Pid},{Client_B, New_Client_B_Pid}],
		    send_to_db(Chat_DB_Name, {self(), add, Session_Name, self(), Client_A, New_Client_A_Pid, Client_B, New_Client_B_Pid}),
		    send_to_db(Chat_DB_Replica, {undefined, add, Session_Name, self(), Client_A, New_Client_A_Pid, Client_B, New_Client_B_Pid}),
		    chat_session(Chat_DBs, New_Clients_Info, Num_Total_Servers)
	    end;
	%% Session already exists.
	[_,_,_,_,_,_] ->
	    chat_session(Chat_DBs, Clients_Info, Num_Total_Servers);
	{session_added, ok} ->	   
	    Client_A_Pid ! {chat_session_success_sender, Session_Name, self()},
	    Client_B_Pid ! {chat_session_success_receiver, Session_Name, self()},
	    chat_session(Chat_DBs, Clients_Info, Num_Total_Servers);
	%% Message delivery confirmation
	%% (Normal client)
	{From, Timestamp, message_delivered_ok} ->
	    Metadata = {Session_Name, Client_A, Client_B, Timestamp},
	    case From of
		Client_A ->
		    Client_A_Pid ! {Metadata, message_delivered_ok};
		Client_B ->
		    Client_B_Pid ! {Metadata, message_delivered_ok}
	    end,
	    chat_session(Chat_DBs, Clients_Info, Num_Total_Servers);
	%% Traffic generator (Toxic client).
	{From, Unique_ID, Timestamp, message_delivered_ok} ->
	    Metadata = {Unique_ID, Session_Name, Client_A, Client_B, Timestamp},
	    case Client_A of
		From ->
		    Client_A_Pid ! {Metadata, message_delivered_ok};
		_To ->
		    Client_B_Pid ! {Metadata, message_delivered_ok}
	    end,
	    chat_session(Chat_DBs, Clients_Info, Num_Total_Servers);
	%% Session termination logic and confirmation to clients.
	%% Normal client
	{_, _, _, finish_chat_session} ->
	    send_to_db(Chat_DB_Name, {self(), remove, Session_Name}),
	    send_to_db(Chat_DB_Replica, {undefined, remove, Session_Name}),
	    chat_session(Chat_DBs, Clients_Info, Num_Total_Servers);
	%% Traffic generator (Toxic client).
	{_, _, _, _, finish_chat_session} ->
	    send_to_db(Chat_DB_Name, {self(), remove, Session_Name}),
	    send_to_db(Chat_DB_Replica, {undefined, remove, Session_Name}), 
	    chat_session(Chat_DBs, Clients_Info, Num_Total_Servers);
	{session_removed, ok} ->
	    Client_A_Pid ! {Session_Name, {'EXIT', ok}},
	    Client_B_Pid ! {Session_Name, {'EXIT', ok}};
	%% Messages delivery action.
	%% Normal client.
	{From, To, Timestamp, Message} ->
	    if
		Client_A == To ->
		    Client_A_Pid ! {From, Message, self(), Timestamp, receive_message};
		true ->
		    Client_B_Pid ! {From, Message, self(), Timestamp, receive_message}
	    end,
	    chat_session(Chat_DBs, Clients_Info, Num_Total_Servers);
	%% Traffic generator (Toxic client).
	{Unique_ID, From, To, Timestamp, Message} ->
	    if
		Client_A == To ->
		    Client_A_Pid ! {Unique_ID, From, Message, self(), Timestamp, receive_message};
		true ->
		    Client_B_Pid ! {Unique_ID, From, Message, self(), Timestamp, receive_message}
	    end,
	    chat_session(Chat_DBs, Clients_Info, Num_Total_Servers);
	%% Trap for any other messages.
	Other ->
	    io:format("Something failed at chat_session({final_state, Chat_DBs, Clients_Info}).~n" ++
		      "Received: ~p~n", [Other]),
	    chat_session(Chat_DBs, Clients_Info, Num_Total_Servers)
    after 300000 ->
    	    send_to_db(Chat_DB_Name, {undefined, remove, Session_Name}),
    	    send_to_db(Chat_DB_Replica, {undefined, remove, Session_Name}),
	    if
		is_pid(Client_A_Pid) and is_pid(Client_B_Pid) ->
		    Client_A_Pid ! {Session_Name, {'EXIT', ok}},
		    Client_B_Pid ! {Session_Name, {'EXIT', ok}};
		is_pid(Client_A_Pid) and (Client_B_Pid == client_not_found) ->
		    Client_A_Pid ! {Session_Name, {'EXIT', ok}};
		is_pid(Client_B_Pid) and (Client_A_Pid == client_not_found) ->
		    Client_B_Pid ! {Session_Name, {'EXIT', ok}};
		true ->
		    ok
	    end,
	    exit(normal)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     This is the client monitor process. It is responsible for pre-
%%     venting a client from logging in twice. It also handles the
%%     client's logout logic and finishes all the pending chat sessions
%%     a client has opened when it terminates (either normally or
%%     abnormally).
%%
%% @spec client_monitor(Client_Name, Client_Pid,
%%                      Client_DB_Name, Server_Supervisor_Pid).
%% @end
%%--------------------------------------------------------------------
client_monitor(Client_Name, Client_Pid, Opened_Chat_Sessions, Client_DBs, Server_Supervisor_Pid) ->
    process_flag(trap_exit, true),
    {Client_DB_Name, Client_DB_Replica} = Client_DBs,
    receive
	%% Login logic.
	{start_client_monitor} ->
	    case send_to_db(Client_DB_Name, {self(), peak, Client_Name}) of
		{error, _} ->
		    send_to_db(Client_DB_Replica, {self(), peak, Client_Name});
		{ok, _} ->
		    ok
	    end,
	    receive
		[] ->
		    erlang:monitor(process, Client_Pid),
		    send_to_db(Client_DB_Name, {self(), add, Client_Name, Client_Pid, self()}),
		    send_to_db(Client_DB_Replica, {undefined, add, Client_Name, Client_Pid, self()}),
		    client_monitor(Client_Name, Client_Pid, Opened_Chat_Sessions, Client_DBs, Server_Supervisor_Pid);
		Response ->
		    Client_Pid ! {error, client_already_logged_in},
		    io:format("Client_DB response: ~p~n", [Response])
	    after 1000 ->
		    self() ! {start_client_monitor},
		    client_monitor(Client_Name, Client_Pid, Opened_Chat_Sessions, Client_DBs, Server_Supervisor_Pid)
	    end;			
	{client_added, ok} ->	    
	    Client_Pid ! {login_success, self()},
	    Server_Supervisor_Pid ! {login_success, self()},
	    client_monitor(Client_Name, Client_Pid, Opened_Chat_Sessions, Client_DBs, Server_Supervisor_Pid);
	%% chat sessions the client is involved.
	{add_chat_session, Chat_Session} ->
	    New_Chat_Sessions = [Chat_Session|Opened_Chat_Sessions],
	    client_monitor(Client_Name, Client_Pid, New_Chat_Sessions, Client_DBs, Server_Supervisor_Pid);
	{remove_chat_session, Chat_Session_Name} ->
	    New_Chat_Sessions = lists:keydelete(Chat_Session_Name, 1, Opened_Chat_Sessions),
	    client_monitor(Client_Name, Client_Pid, New_Chat_Sessions, Client_DBs, Server_Supervisor_Pid);
	{'EXIT', Pid, Reason} ->
	    {'EXIT', Pid, Reason};
	%% monitored client finished normally.
	{'DOWN', _Ref, process, _Pid, normal} ->
	    ok;
	%% monitored client finished abnormally (behave as if client logged out).	
	{'DOWN', _Ref, process, _Pid, _Reason} ->
	    case send_to_db(Client_DB_Name, {self(), peak_by_client_monitor, self()}) of
		{error, _} ->
		    send_to_db(Client_DB_Replica, {self(), peak_by_client_monitor, self()});
		{ok, _} ->
		    ok
	    end,
	    receive
		[Client_Name, Client_Pid, _Client_Monitor_Pid] ->
	    	    send_to_db(Client_DB_Name, {self(), remove, Client_Name}),
	    	    send_to_db(Client_DB_Replica, {undefined, remove, Client_Name}),
	    	    finish_session(Client_Name, Opened_Chat_Sessions);
	    	Other ->
	    	    io:format("Something failed while handling abnormal client termination.~n" ++
			      "Received ~p~n", [Other])
	    after 1000 ->
		    self() ! {'DOWN', _Ref, process, _Pid, _Reason},
		    client_monitor(Client_Name, Client_Pid, Opened_Chat_Sessions, Client_DBs, Server_Supervisor_Pid)
	    end;
	%% Logout logic.
	{Client_Name, Client_Pid, logout} ->
	    send_to_db(Client_DB_Name, {self(), remove, Client_Name}),
	    send_to_db(Client_DB_Replica, {undefined, remove, Client_Name}),
	    client_monitor(Client_Name, Client_Pid, Opened_Chat_Sessions, Client_DBs, Server_Supervisor_Pid);
	{client_removed, ok} ->
	    Client_Pid ! {'EXIT', ok},
	    finish_session(Client_Name, Opened_Chat_Sessions)
    end.

%%===============================================================================
%% AUXILIARY FUNCTIONS
%%===============================================================================

%% -------------------------
%% Names building functions.
%% -------------------------

%%--------------------------------------------------------------------
%% @doc
%%     Auxiliary function to build the name of chat sessions.
%%
%% @spec chat_session_name(Sender, Receiver) -> atom()
%% @end
%%--------------------------------------------------------------------
chat_session_name(Sender, Receiver) ->
    case is_atom(Sender) of
	true ->
	    S = atom_to_list(Sender);
	false ->
	    S = Sender
    end,
    case is_atom(Receiver) of
	true ->
	    R = atom_to_list(Receiver);
	false ->
	    R = Receiver
    end,
    case S < R of
	true ->
	    {Client_A, Client_B} = {S,R};
	false ->
	    {Client_A, Client_B} = {R,S}
    end,
    string:join(["chat_session", Client_A, Client_B], "_").

%%--------------------------------------------------------------------
%% @doc
%%     Auxiliary function to build the name of client database.
%%
%% @spec client_db_name(Server_Number) -> atom()
%% @end
%%--------------------------------------------------------------------
client_db_name(Server_Number) ->
    Client_DB_Name = string:join(["server", integer_to_list(Server_Number), "1","Client_DB"], "_"),
    Client_DB_Replica = string:join(["server", integer_to_list(Server_Number), "2", "Client_DB"], "_"),
    {string_to_atom(Client_DB_Name), string_to_atom(Client_DB_Replica)}.

%%--------------------------------------------------------------------
%% @doc
%%     Auxiliary function to build the name of client databases.
%%
%% @spec client_db_name(Server_Name, Num_Total_Servers) -> [atom(), atom()]
%% @end
%%--------------------------------------------------------------------
client_db_name(Server_Name, Num_Total_Servers) ->
    [_, Server_Num] = string:tokens(atom_to_list(Server_Name), "_"),
    N1 = "server_" ++ Server_Num ++ "_1_Client_DB",
    case list_to_integer(Server_Num) of
	Num_Total_Servers ->
	    N2 = "server_1_2_Client_DB";
	_Other ->
	    N2 = "server_" ++ integer_to_list(list_to_integer(Server_Num) + 1) ++ "_2_Client_DB"
    end,
    Client_DB_Name = string_to_atom(N1),
    Client_DB_Replica = string_to_atom(N2),
    [Client_DB_Name, Client_DB_Replica].

%%--------------------------------------------------------------------
%% @doc
%%     Auxiliary function to build the name of chat databases.
%%
%% @spec chat_db_name(Server_Name, Num_Total_Servers) -> [atom(), atom()]
%% @end
%%--------------------------------------------------------------------
chat_db_name(Server_Name, Num_Total_Servers) ->
    [_, Server_Num] = string:tokens(atom_to_list(Server_Name), "_"),
    N1 = "server_" ++ Server_Num ++ "_1_Chat_DB",
    case list_to_integer(Server_Num) == Num_Total_Servers of
	true ->
	    N2 = "server_1_2_Chat_DB";
	false ->
	    N2 = "server_" ++ integer_to_list(list_to_integer(Server_Num) + 1) ++ "_2_Chat_DB"
    end,
    Chat_DB_Name = string_to_atom(N1),
    Chat_DB_Replica = string_to_atom(N2),
    [Chat_DB_Name, Chat_DB_Replica].

%%--------------------------------------------------------------------
%% @doc
%%     Auxiliary functions to build the name of the dabases of a server.
%%
%% @spec server_dbs(Server_Name) -> {{atom(), atom()}, {atom(), atom()}}
%% @end
%%--------------------------------------------------------------------
server_dbs(Server_Name) ->
    [_, Server_Num] = string:tokens(atom_to_list(Server_Name), "_"),
    Client_DB_Main = "server_" ++ Server_Num ++ "_1_Client_DB",
    Client_DB_Repl = "server_" ++ Server_Num ++ "_2_Client_DB",
    Chat_DB_Main = "server_" ++ Server_Num ++ "_1_Chat_DB",
    Chat_DB_Repl = "server_" ++ Server_Num ++ "_2_Chat_DB",
    Client_DBs = {string_to_atom(Client_DB_Main), string_to_atom(Client_DB_Repl)},
    Chat_DBs = {string_to_atom(Chat_DB_Main), string_to_atom(Chat_DB_Repl)},
    {Client_DBs, Chat_DBs}.

%% ------------------------------------
%% Databases creation and registration.
%% ------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%      Auxiliary function to build up a list of the databases monitored
%%      by a server supervisor. It spawns the databases needed and then
%%      it returns a list containing the names of the DBs monitored by
%%      the server_supervisor process.
%%
%% @spec spawn_databases(Node, DBs, Mon_Process_Table, Monitored_DBs) ->
%%                  list()
%% @end
%%--------------------------------------------------------------------
spawn_databases(_Node, [], _Mon_Process_Table, Monitored_DBs) ->
    Monitored_DBs;
spawn_databases(Node, [DB|Tail_DBs], Mon_Process_Table, Monitored_DBs) ->
    case DB of
	monitored_processes_2 ->
	    Type = "Monitor_DB";
	_Other ->
	    [_,_,_,T,_] = string:tokens(atom_to_list(DB), "_"),
	    Type = T ++ "_DB"
    end,
    Spawned_DB = spawn_db(Node, DB, Type, Mon_Process_Table),
    New_Monitored_DBs = Monitored_DBs ++ Spawned_DB,
    spawn_databases(Node, Tail_DBs, Mon_Process_Table, New_Monitored_DBs).

%%--------------------------------------------------------------------
%% @doc
%%     Auxiliary function to build up a list of the databases monitored
%%     by a server supervisor. It spawns the databases needed and then
%%     it returns a list containing the names of the DBs monitored by
%%     the server_supervisor process.
%%
%%     There may be a race condition here between spawn_link and monitor
%%     operations.
%%
%% @spec replica_node(Node, Server_Name, Num_Total_Servers) -> list()
%% @end
%%--------------------------------------------------------------------
spawn_db(Node, DB, Type, Mon_Process_Table) ->
    case Type of
	"Client_DB" ->
	    DB_Pid = spawn_link(Node, fun() -> client_db:client_db(DB) end);
	"Chat_DB" ->
	    DB_Pid = spawn_link(Node, fun() -> chat_db:chat_db(DB) end);
	"Monitor_DB" ->
	    DB_Pid = spawn_link(Node, fun() -> monitored_db:monitored_db(monitored_processes_2) end)
    end,
    _DB_Ref = erlang:monitor(process, DB_Pid),
    unlink(DB_Pid),
    ets:insert(Mon_Process_Table, {DB_Pid, Type, DB}),
    [{DB, DB_Pid}].

%%--------------------------------------------------------------------
%% @doc
%%      Auxiliary function to register gobally the databases spawned by
%%      a server supervisor.
%%
%% @spec register_dbs(DBs) -> ok
%% @end
%%--------------------------------------------------------------------
register_dbs([]) ->
    ok;
register_dbs([{Name, Pid}|T_DBs]) ->
    case Name of
	monitored_processes_2 -> 
	    register(Name, Pid),
	    register_dbs(T_DBs);
	_Other ->
	    global:register_name(Name, Pid),
	    register_dbs(T_DBs)
    end.

%% -------------------------------------
%% Process recovery auxiliary functions.
%% -------------------------------------

%%---------------------------------------------------------------------
%% @doc
%%      Process Identifier function (Process Recovery).
%%
%% @spec look_up_pid(Table_Name, Process_Pid) ->
%%                  [Pid(), process_type, process_name] |
%%                  [undefined, undefined, undefined]
%% @end
%%---------------------------------------------------------------------
look_up_pid(Table_Name, Process_Pid) ->    
    L = ets:select(Table_Name, [{{'$0', '$1', '$2'},
				 [{'==', '$0', Process_Pid}],
				 [['$0', '$1', '$2']]}]),
    if
	L == [] ->
	    [undefined, undefined, undefined];
	true ->
	    lists:last(L)
    end.

%%--------------------------------------------------------------------
%% @doc
%%      Provisional container for messages received during the final
%%      stage of the server supervisor recovery. (Used during server
%%      supervisor recovery).
%%      
%% @spec provisional_container(Buffer) -> function().
%% @end
%%--------------------------------------------------------------------
provisional_container(Buffer) ->
    receive
	{flush_buffer, Serv_Sup_Pid} ->
	    flush(lists:reverse(Buffer), Serv_Sup_Pid);
	Other ->
	    New_Buffer = [Other|Buffer],
	    provisional_container(New_Buffer)
    after 300000 ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%%      Flushes the messages received during the server supervisor
%%      recovery to the server supervisor.
%%      
%% @spec provisional_container(Buffer) -> function().
%% @end
%%--------------------------------------------------------------------
flush([], _Serv_Sup_Pid) ->
    ok;
flush([H|T], Serv_Sup_Pid) ->
    Serv_Sup_Pid ! H,
    flush(T, Serv_Sup_Pid).

%%--------------------------------------------------------------------
%% @doc
%%     mon_proc/1 monitors all the processes in the server node during
%%     the server_supervisor recovery.
%%
%% @spec mon_proc(Monitored_Processes) -> ok
%% @end
%%--------------------------------------------------------------------
mon_proc([]) ->
    ok;
mon_proc([{Pid, _Type, _Name}|T]) ->
    erlang:monitor(process, Pid),
    mon_proc(T).

%% --------------------------------------
%% Monitored Processes database recovery.
%% --------------------------------------

%%---------------------------------------------------------------------
%% @doc
%% Replicates the monitored processes table specified in the Source
%% parameter, in a new table with name Table_Name. Source is the ets
%% table owned by the server_supervisor process.
%%
%% @spec recover_server_table(Destination, Source, Destination_Pid) ->
%%                  true | {error, Error}
%% @end
%%---------------------------------------------------------------------
recover_monitor_db(Monitored_Proc_DB, Monitored_Proc_Table) ->
    ets:safe_fixtable(Monitored_Proc_Table, true),
    send_to_monitored_db(Monitored_Proc_DB, {self(), recover, term_to_binary(ets:tab2list(Monitored_Proc_Table))}),
    ets:safe_fixtable(Monitored_Proc_Table, false).

%% -----------------------------
%% Databases recovery functions.
%% -----------------------------

%%--------------------------------------------------------------------
%% @doc
%%     replicate/5 spawns a new db process and swaps the replica and
%%      main databases.
%%
%% @spec replicate_db(Failed_Pid, Tokens, Client_DBs, Chat_DBs,
%%          Monitored_Processes) -> {{atom(), atom()},{atom(), atom()}}
%% @end
%%--------------------------------------------------------------------
replicate_db(Failed_Pid, Tokens, Client_DBs, Chat_DBs, Monitored_Processes) ->
    {Monitored_Proc_Table, Monitored_Proc_DB} = Monitored_Processes,
    [_, Server_Num, Replica_Flag, Type, _] = Tokens,
    case Replica_Flag == "1" of
	true ->
	    DB2_Name_Str = string:join(["server", Server_Num, "2", Type, "DB"], "_");
	false ->
	    DB2_Name_Str = string:join(["server", Server_Num, "1", Type, "DB"], "_")
    end,
    DB_Name = string_to_atom(string:join(Tokens, "_")),
    %% spawn, register and monitor a new Db
    case Type of
	"Client" ->
	    {New_DB_Pid, _Ref} = spawn_monitor(fun() -> client_db:client_db(DB_Name) end),
	    global:register_name(DB_Name, New_DB_Pid),
	    ets:insert(Monitored_Proc_Table, {New_DB_Pid, "Client_DB", DB_Name}),
	    send_to_monitored_db(Monitored_Proc_DB, {undefined, add, New_DB_Pid, "Client_DB", DB_Name}),
	    ets:delete(Monitored_Proc_Table, Failed_Pid),
	    send_to_monitored_db(Monitored_Proc_DB, {undefined, remove, Failed_Pid});
	"Chat" ->
	    {New_DB_Pid, _New_Ref} = spawn_monitor(fun() -> chat_db:chat_db(DB_Name) end),
	    global:register_name(DB_Name, New_DB_Pid),
	    ets:insert(Monitored_Proc_Table, {New_DB_Pid, "Chat_DB", DB_Name}),
	    send_to_monitored_db(Monitored_Proc_DB, {undefined, add, New_DB_Pid, "Chat_DB", DB_Name}),
	    ets:delete(Monitored_Proc_Table, Failed_Pid),
	    send_to_monitored_db(Monitored_Proc_DB, {undefined, remove, Failed_Pid})
    end,
    %% return data
    case Type of
	"Client" when (Replica_Flag == "1") ->
	    New_Client_DBs = {string_to_atom(DB2_Name_Str), DB_Name},
	    {New_Client_DBs, Chat_DBs};
	"Chat" when (Replica_Flag == "1") ->
	    New_Chat_DBs = {string_to_atom(DB2_Name_Str), DB_Name},
	    {Client_DBs, New_Chat_DBs};
	_Other ->
	    {Client_DBs, Chat_DBs}
    end.

%%--------------------------------------------------------------------
%% @doc
%%     db_recovery/1 fires up the data transfer between replica and
%%     main databases.
%%
%% @spec db_recovery(Tokens) -> New DB | {error, reason}
%% @end
%%--------------------------------------------------------------------
db_recovery(Tokens) ->
    [_, Server, Replica_Flag, Type, _] = Tokens,
    if
	Replica_Flag == "1" ->
	    Src = string:join(["server", Server, "2", Type, "DB"], "_");
	true ->
	    Src = string:join(["server", Server, "1", Type, "DB"], "_")
    end,
    Destination = string_to_atom(string:join(Tokens, "_")),
    Source = string_to_atom(Src),
    send_to_db(Source, {self(), recover, Destination, Source}).

%%---------------------------------------------------------------------
%% @doc
%%     db_names_swap/1 swaps main and replica db names to use replica
%%     as main while main is recovering.
%%
%% @spec db_names_swap(DBs) -> {atom(), atom()}
%% @end
%%---------------------------------------------------------------------
db_names_swap({DB_Repl, DB_Main}) ->
    [_, _, Rep_Flag, _, _] = string:tokens(atom_to_list(DB_Repl), "_"),
    if
	Rep_Flag == "2" ->
	    {DB_Main, DB_Repl};
	true ->
	    {DB_Repl, DB_Main}
    end.

%% -------------------------------
%% Chat Session recovery function.
%% -------------------------------

%%---------------------------------------------------------------------
%% @doc
%%     Signals the end of the failed chat session to clients and removes
%%     the session from Chat_DBs.
%%
%% @spec chat_session_recovery(Session_Pid, Chat_DBs,
%%          Monitored_Processes) -> ok.
%% @end
%%---------------------------------------------------------------------
chat_session_recovery({initial_state, Session_Pid, Chat_DBs, Monitored_Processes}) ->
    {Chat_DB_Name, Chat_DB_Replica} = Chat_DBs,
    case send_to_db(Chat_DB_Name, {self(), peak_by_pid, Session_Pid}) of
	{error, _} ->
	    case global:whereis_name(Chat_DB_Replica) of
    		undefined ->
    		    timer:sleep(500),
    		    chat_session_recovery({initial_state, Session_Pid, Chat_DBs, Monitored_Processes});
    		Replica_Pid ->
    		    Replica_Pid ! {self(), peak_by_pid, Session_Pid},
    		    chat_session_recovery({final_state, Session_Pid, Chat_DBs, Monitored_Processes})
    	    end;
	{ok, _} ->
	    chat_session_recovery({final_state, Session_Pid, Chat_DBs, Monitored_Processes})
    end;
chat_session_recovery({final_state, Session_Pid, Chat_DBs, Monitored_Processes}) ->
    {Monitored_Proc_Table, Monitored_Proc_DB} = Monitored_Processes,
    {Chat_DB_Name, Chat_DB_Replica} = Chat_DBs,
    receive
    	[] ->
    	    io:format("chat_session_recovery received []~n"),
    	    ok;
    	[Session_Name, Session_Pid, _Client_A, Client_A_Pid, _Client_B, Client_B_Pid] ->
	    case Client_A_Pid of
		client_not_found ->
		    ok;
		Client_A_Pid ->
		    Client_A_Pid ! {Session_Name, {'EXIT', ok}}
	    end,
	    Client_B_Pid ! {Session_Name, {'EXIT', ok}},
	    send_to_db(Chat_DB_Name, {undefined, remove, Session_Name}),
	    send_to_db(Chat_DB_Replica, {undefined, remove, Session_Name}),
	    ets:delete(Monitored_Proc_Table, Session_Pid),
	    send_to_monitored_db(Monitored_Proc_DB, {undefined, remove, Session_Pid})
    end.

%% ---------------------------------
%% Client Monitor recovery function.
%% ---------------------------------

%%---------------------------------------------------------------------
%% @doc
%%     Spawns a new client monitor, signals the new monitor pid to the
%%     client, and updates Client_DBs.
%%
%% @spec client_monitor_recovery(Client_Monitor_Pid, Client_DBs,
%%          Server_Supervisor_Pid, Monitored_Processes) ->
%%                  client_monitor/5 | ok
%% @end
%%---------------------------------------------------------------------
client_monitor_recovery(Client_Monitor_Pid, Client_DBs, Server_Supervisor_Pid, Monitored_Processes) ->
    {Monitored_Proc_Table, Monitored_Proc_DB} = Monitored_Processes,
    {Client_DB_Name, Client_DB_Replica} = Client_DBs,
    case send_to_db(Client_DB_Name, {self(), peak_by_client_monitor, Client_Monitor_Pid}) of
	{error, _} ->
	    send_to_db(Client_DB_Replica, {self(), peak_by_client_monitor, Client_Monitor_Pid});
	{ok, _} ->
	    ok
    end,
    receive
    	[] ->
    	    %% io:format("client_monitor_recovery/3 received []~n"),
    	    ok;
    	[Client_Name, Client_Pid, _Client_Monitor_Pid] ->
	    Client_Pid ! {opened_chat_sessions, self()},
	    receive
		{chat_sessions, Chat_Sessions} ->
		    %% io:format("Chat_Sessions = ~p~n", [Chat_Sessions]),
		    Chat_Sessions
	    end,
	    {New_Client_Monitor_Pid, _Ref} = spawn_monitor(fun() -> client_monitor(Client_Name,
										   Client_Pid,
										   Chat_Sessions,
										   Client_DBs,
										   Server_Supervisor_Pid)
							   end),
	    Client_Pid ! {new_monitor_pid, New_Client_Monitor_Pid},
	    send_to_db(Client_DB_Name, {undefined, update_monitor_pid, Client_Name, New_Client_Monitor_Pid}),
	    send_to_db(Client_DB_Replica, {undefined, update_monitor_pid, Client_Name, New_Client_Monitor_Pid}),
	    ets:insert(Monitored_Proc_Table, {New_Client_Monitor_Pid, "Client_Monitor", Client_Name}),
	    send_to_monitored_db(Monitored_Proc_DB, {undefined, add, New_Client_Monitor_Pid, "Client_Monitor", Client_Name}),
	    ets:delete(Monitored_Proc_Table, Client_Monitor_Pid),
	    send_to_monitored_db(Monitored_Proc_DB, {undefined, remove, Client_Monitor_Pid})
    after 1000 ->
	    client_monitor_recovery(Client_Monitor_Pid, Client_DBs, Server_Supervisor_Pid, Monitored_Processes)
    end.

%% ---------------------------
%% Other processing functions.
%% ---------------------------
%%--------------------------------------------------------------------
%% @doc
%%     finish_node/0 fires the sequence that finishes the node.
%%
%% @spec finish_node() -> {'EXIT, normal}
%% @end
%%--------------------------------------------------------------------
finish_node() ->
    find_server_sup() ! {'EXIT', normal},
    timer:sleep(1000),
    init:stop().

%%--------------------------------------------------------------------
%% @doc
%%     stop_dbs/3 stops the database processes gently.
%%
%% @spec stop_dbs(S_Group_Name, Client_DBs, Chat_DBs) -> true | ok
%% @end
%%--------------------------------------------------------------------
stop_dbs(Client_DBs, Chat_DBs) ->
    {Cl_DB1, Cl_DB2} = Client_DBs,
    {Ch_DB1, Ch_DB2} = Chat_DBs,
    exit(whereis(monitored_processes_2),normal),
    exit(global:whereis_name(Cl_DB1),normal),    
    exit(global:whereis_name(Ch_DB1),normal),
    case global:whereis_name(Cl_DB2) of
	undefined ->
	    ok;
	Pid1 ->
	    exit(Pid1,normal)
    end,
    case global:whereis_name(Ch_DB2) of
	undefined ->
	    ok;
	Pid2 ->
	    exit(Pid2,normal)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     find_server_sup/0 returns the pid of a server supervisor process
%%     if such a process exists.
%%
%% @spec find_server_sup() -> pid() | not_found
%% @end
%%--------------------------------------------------------------------
find_server_sup() ->
    find_server_sup(erlang:processes()).

%%--------------------------------------------------------------------
%% @doc
%%     find_server_sup/1 returns the pid of a server supervisor process
%%     given a list of processes pids.
%%
%% @spec find_router_mon(Processes) -> pid() | not_found
%% @end
%%--------------------------------------------------------------------
find_server_sup([]) ->
    not_found;
find_server_sup([H|T]) ->
    case erlang:process_info(H, current_function) of
	{current_function, {_, server_supervisor_loop, _}} ->
	    H;
	_Other ->
	    find_server_sup(T)
    end.

%%---------------------------------------------------------------------
%% @doc
%%     Auxiliary function to finish remaining opened sessions when
%%     client logs out.
%%
%% @spec finish_session(Client_Name, Sessions) -> {all_sessions_finished, ok}
%% @end
%%---------------------------------------------------------------------
finish_session(_Client_Name, []) ->
    {all_sessions_finished, ok};
finish_session(Client_Name, [{Session_Name, Session_Pid}|T_Sessions]) ->    
    {Client_A, Client_B} = extract_clients(Session_Name),
    Session_Pid ! {Client_A, Client_B, undefined, finish_chat_session},
    finish_session(Client_Name, T_Sessions).

%%---------------------------------------------------------------------
%% @doc
%%     Auxiliary function to find a client pid.
%%
%% @spec find_client_pid(Client_Name, Server_Number) -> pid() | client_not_found
%% @end
%%---------------------------------------------------------------------
%% find_client_pid(Client_Name, Server_Number) ->
%%     {Client_DB_Name, Client_DB_Replica} = client_db_name(Server_Number),
%%     Replica_Pid = global:whereis_name(Client_DB_Replica),
%%     case global:whereis_name(Client_DB_Name) of
%% 	undefined when is_pid(Replica_Pid) == true ->
%% 	    Replica_Pid ! {self(), client_pid, Client_Name},
%% 	    receive
%% 		[] ->
%% 		    find_client_pid(Client_Name, Server_Number + 1);
%% 		Client_Pid ->
%% 		    Client_Pid
%% 	    end;
%% 	Main_Pid when is_pid(Main_Pid) == true ->
%% 	    Main_Pid ! {self(), client_pid, Client_Name},
%% 	    receive
%% 		[] ->
%% 		    find_client_pid(Client_Name, Server_Number + 1);
%% 		Client_Pid ->
%% 		    Client_Pid
%% 	    end;
%% 	_Other ->
%% 	    client_not_found
%%     end.

%%---------------------------------------------------------------------
%% @doc
%%     Auxiliary function to find a client pid.
%%
%% @spec find_client_pid(S_Group_Name, Client_Name, Num_Total_Servers) ->
%%          pid() | client_not_found
%% @end
%%---------------------------------------------------------------------
find_client_pid(Client_Name, Num_Total_Servers) ->
    Server_Number = compression_function(Num_Total_Servers, Client_Name) + 1,
    case random:uniform(2) of
	1 ->
	    {Client_DB_Main, Client_DB_Replica} = client_db_name(Server_Number);
	2 ->
	    {Client_DB_Replica, Client_DB_Main} = client_db_name(Server_Number)
    end,
    case send_to_db(Client_DB_Main, {self(), client_pid, Client_Name}) of
	{error, db_is_down} ->
	    send_to_db(Client_DB_Replica, {self(), client_pid, Client_Name});
	{ok, _Message} ->
	    ok
    end,
    receive
	Pid when is_pid(Pid)->
	    Pid;
	_Other ->
	    client_not_found
    %% after
    %% 	5000 ->
    %% 	    client_not_found
    end.

%%---------------------------------------------------------------------
%% @doc
%%     Auxiliary function to extract client names given the name of a
%%     chat session.
%%
%% @spec extract_clients(Session_Name) -> {Client_A, Client_B}
%% @end
%%---------------------------------------------------------------------
extract_clients(Session_Name) ->
    Tokens = string:tokens(Session_Name, "_"),
    Client_A = lists:nth(3, Tokens),
    Client_B = lists:nth(4, Tokens),
    {Client_A, Client_B}.


%%---------------------------------------------------------------------
%% @doc
%%     Auxiliary function to forward a message to the database specified
%%     as parameter DB_Name.
%%
%% @spec extract_clients(DB_Name, Message) -> ok.
%% @end
%%---------------------------------------------------------------------
send_to_db(DB_Name, Message) ->
    case global:whereis_name(DB_Name) of
	undefined ->
	    {error, db_is_down};
	Pid ->
	    Pid ! Message,
	    {ok, Message}
    end.

%%---------------------------------------------------------------------
%% @doc
%%     Auxiliary function to forward a message to the local database
%%     that contains the processes monitored by the server supervisor.
%%     Parameter DB_Name is left for generality, but this is actually
%%     the atom 'monitored_processes_2'
%% @spec extract_clients(DB_Name, Message) -> ok.
%% @end
%%---------------------------------------------------------------------
send_to_monitored_db(DB_Name, Message) ->
    case whereis(DB_Name) of
	undefined ->
	    {error, db_is_down};
	Pid ->
	    Pid ! Message,
	    {ok, Message}
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

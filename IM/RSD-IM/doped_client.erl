%%%--------------------------------------------------------------------
%%% DOPED CLIENT MODULE
%%%
%%% @author: Mario Moro Hernandez upon a design by Natalia Chechina
%%% @copyright (C) 2015, RELEASE project
%%% @doc
%%%	Doped Client module for the Distributed Erlang instant messenger
%%%     (IM) application developed as a real benchmark for the Scalable 
%%%	Distributed Erlang extension of the Erlang/OTP language.
%%%
%%%	This module implementes a modified client used to load a system
%%%     similar to the system described in the Section 2 of the document
%%%     "Instant Messenger Architectures Design Proposal".
%%% @end 
%%% Created: 24 Jan 2015 by Mario Moro Hernandez
%%%--------------------------------------------------------------------
-module(doped_client).

-export([launch/2, launch/3, logout/1, stop_client/1, message/3]).

-import(client_db, [start_local/1, stop_local/1]).

%%%===================================================================
%%% CLIENT CODE
%%%===================================================================

%%---------------------------------------------------------------------
%% @doc
%%     Starts the client by spawing and registering a client process.
%%
%% @spec start(Client_Name) -> Pid | {error, Reason}
%% @end
%%---------------------------------------------------------------------
start_client(Client_Name) ->
    global:whereis_name(routers_db) ! {retrieve_routers, self()},
    receive
	{Routers_List, router_list} ->
	    Tokens = string:tokens(Client_Name, "_"),
	    {Num_Node, _} = string:to_integer(lists:nth(2, Tokens)),
	    Pid = spawn(fun() -> client({Client_Name, Routers_List}) end),
	    whereis(client_db_name(Num_Node)) ! {self(), update_pid, {Client_Name, Pid}},
	    receive
		true ->
		    Answer = true;
		false ->
		    Answer = false
	    end,
	    Pid ! Answer;
	Other ->
	    io:format("Unable to start the client. No routers list received.~n"),
	    io:format("start_client/1 received: ~p~n", [Other])
    end.

%%--------------------------------------------------------------------
%% @doc
%%     Stops the client specified as a parameter by sending an 
%%     {'EXIT', ok} message.
%%
%% @spec stop(Client_Name)
%% @end
%%--------------------------------------------------------------------
stop_client(Client_Name) ->
    whereis(Client_Name) ! {'EXIT', ok}.

%%--------------------------------------------------------------------
%% @doc
%%     This is the function that constitutes the client process. It has
%%     two different states. Initially, it is spawned and registered 
%%     using the client name, only. Once it has been registered, it
%%     waits until it receives the Pid of its monitor.
%%
%%     When the client_monitor Pid has been received, the login process
%%     is concluded.
%%
%% @spec client({Client_Name, Client_Monitor_Pid, Chat_Pids})
%% @end
%%--------------------------------------------------------------------
client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List}) ->
    receive
	%% Chat Logic
	{Sender, Receiver, Message, send_message} ->
	    Session_Name = chat_session_name(Sender, Receiver),
	    case retrieve(Session_Name, Chat_Pids) of
	    	[] ->
		    Router_Pid = pick_router(Routers_List),
		    case rpc:pinfo(Router_Pid) of
			undefined ->
			    global:whereis_name(routers_db) ! {retrieve_routers, self()},
			    self() ! {Sender, Receiver, Message, send_message},
			    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List});
			_Other ->
			    Router_Pid ! {Sender, Receiver, start_chat_session},
			    timer:send_after(random:uniform(20) * 1000, {Sender, Receiver, Message, send_message}),
			    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List})
		    end;		    
		Chat_Session_Pid ->
		    Unique_ID = integer_to_list(erlang:crc32(Session_Name)) ++ integer_to_list(sys_time(now())),
		    Chat_Session_Pid ! {Unique_ID, Sender, Receiver, now(), Message},
		    case Message == finish_chat_session of
			true ->
			    ok;
			false ->
			    Metadata = {Unique_ID, Session_Name, Sender, Receiver, now()},
			    notify_logger(s, Metadata, not_delivered)
		    end,
		    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List})
	    end;
	{New_Routers_List, router_list} ->
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, New_Routers_List});
	{chat_session_success_sender, Session_Name, Chat_Session_Pid} ->
	    New_Chat_Pids = [{Session_Name, Chat_Session_Pid}|Chat_Pids],
	    client({Client_Name, Client_Monitor_Pid, New_Chat_Pids, Routers_List});
	{receiver_not_found, Receiver} ->
	    io:format("Sorry, but ~p is not logged in.~n", [Receiver]),
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List});
	{chat_session_success_receiver, Session_Name, Chat_Session_Pid} ->
	    New_Chat_Pids = [{Session_Name, Chat_Session_Pid}|Chat_Pids],
	    Client_Monitor_Pid ! {add_chat_session, {Session_Name, Chat_Session_Pid}},
	    client({Client_Name, Client_Monitor_Pid, New_Chat_Pids, Routers_List});
	{Metadata, message_delivered_ok} ->
	    Timestamp_2 = now(),
	    {_, _, _, _, Timestamp_1} = Metadata,
	    Latency = timer:now_diff(Timestamp_2, Timestamp_1)/2,
	    notify_logger(d, Metadata, Latency),
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List});
	{Unique_ID, Sender, Message, Chat_Session_Pid, Timestamp, receive_message} ->
	    Chat_Session_Pid ! {Sender, Unique_ID, Timestamp, message_delivered_ok},
	    Metadata = {Unique_ID, undefined, Sender, Client_Name, now()},
	    notify_logger(r, Metadata, received),
	   {_Str, Interactions, TG_Pid} = Message,
	    case Interactions of
		0 ->
		    self() ! {Client_Name, Sender, finish_chat_session, send_message},
		    TG_Pid ! {finished_chat, chat_session_name(Sender, Client_Name)};
		_Other ->
		    String = [random:uniform(26) + 96 || _ <- lists:seq(1, random:uniform(75))],
		    timer:send_after(random:uniform(20) * 1000,
				     {Client_Name,
				      Sender,
				      {String, Interactions - 1, TG_Pid},
				      send_message})
	    end,
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List});
	{Chat_Session_Name, {'EXIT', ok}} ->
	    New_Chat_Pids = lists:keydelete(Chat_Session_Name, 1, Chat_Pids),	    
	    Client_Monitor_Pid ! {remove_chat_session, Chat_Session_Name},
	    client({Client_Name, Client_Monitor_Pid, New_Chat_Pids, Routers_List});
	{opened_chat_sessions, Client_Monitor_Recover_Pid} ->
	    Client_Monitor_Recover_Pid ! {chat_sessions, Chat_Pids},
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List});
	{new_monitor_pid, New_Client_Monitor_Pid} ->
	    client({Client_Name, New_Client_Monitor_Pid, Chat_Pids, Routers_List});
	%% Logout Logic
	{logout} ->
	    Client_Monitor_Pid ! {Client_Name, self(), logout},
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List});
	%% Client Termination Logic
	{error, client_already_logged_in} ->
	    io:format("You are logged in already.~n"),
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List});
	{'EXIT', ok} ->	    
	    io:format("~p is stopped now.~n", [Client_Name]);
	%% Trap for unexpected messages
	Other ->
	    io:format("Something is failing at client(/4).~n" ++
		      "Received: ~p~n", [Other]),
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List})
    end;

%%--------------------------------------------------------------------
%% @doc
%%     client/1 is the client process when the client is not logged in
%%     yet. When the client_monitor Pid has been received, it changes
%%     the state and changes to the full logged-in client; i.e., the
%%     client({Client_Name, Client_Pid, Client_Monitor_Pid}) function
%%     defined above.
%%
%% @spec client(Client_Name)
%% @end
%%--------------------------------------------------------------------
client({Client_Name, Routers_List}) ->
    receive
	true -> %% This allows to have a client up, but not logged-in
	    client({Client_Name, Routers_List});
	false ->
	    {error, client_already_logged_in};
	{login_success, Client_Monitor_Pid} ->
	    client({Client_Name, Client_Monitor_Pid, [], Routers_List});
	{error, client_already_logged_in} ->
	    io:format("You are logged in already.~n");
	Other ->
	    io:format("This is failing at client(/2).~n" ++
		      "Received: ~p~n", [Other]),
	    client({Client_Name, Routers_List})
    end.

%%---------------------------------------------------------------------
%% @doc
%%     login/1 function starts a client named after the string passed as
%%     parameter and registers it in the system. If the client is logged
%%     in already, then returns an error message.
%%
%% @spec login(Client_Name) -> yes | {error, Reason}
%% @end
%%---------------------------------------------------------------------
login(Client_Name) ->
    Client_Pid = client_pid(Client_Name),
    case Client_Pid of
	undefined->
	    start_client(Client_Name),
	    global:whereis_name(routers_db) ! {retrieve_routers, self()},
	    receive
		{Routers_List, router_list} ->
		    Router_Pid = pick_router(Routers_List),
		    Router_Pid ! {Client_Name, client_pid(Client_Name), login};
		Other ->
		    io:format("Login failed at retrieving routers list.~n" ++
			      "Received: ~p~n", [Other])
	    end,
	    yes;
	{error, client_does_not_exist} ->
	    io:format("Client ~p does not exist.~n", [Client_Name]);
	_ ->
	    io:format("You are logged in already.~n")
    end.

%%---------------------------------------------------------------------
%% @doc
%%     logout/1 unegisters the client from the system.
%%
%% @spec logout(Client_Name) -> ok | {error, Reason}
%% @end
%%---------------------------------------------------------------------
logout(Client_Name) ->
    Client_Pid = client_pid(Client_Name),
    case Client_Pid of
	undefined ->
	    io:format("Client ~p is not logged in.~n",[Client_Name]),
	    {error, client_not_logged_in};
	_ ->
	    Client_Pid ! {logout},
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%%     message/3 enables the communication between two clients, sending
%%     the message passed as the parameter Message to the client specified
%%     as the To parameter.
%%
%% @spec message(From, To, Message) -> yes | {error, Reason}
%% @end
%%--------------------------------------------------------------------
message(From, To, Message) ->
    client_pid(From) ! {From, To, Message, send_message},
    yes.

%%%====================================================================
%%% TRAFFIC GENERATION LOGIC
%%%====================================================================

%%---------------------------------------------------------------------
%% @doc
%%     launch/2 distributes a number of clients evenly among the client
%%     nodes in the list passed as the second argument. This function
%%     assumes that the list Nodes contains only the client nodes.
%%
%% @spec launch(Total_Num_Clients, Nodes) -> ok
%% @end
%%---------------------------------------------------------------------
launch(Total_Num_Clients, Nodes) ->
    case Nodes of
	[] ->
	    ok;
        [H|T] ->
	    Num_Clients_Node = Total_Num_Clients div length(Nodes),
	    New_Total_Num_Clients = Total_Num_Clients - Num_Clients_Node,
	    [Node_Name|_Rest] = string:tokens(atom_to_list(H),"@"),
	    case string:sub_word(Node_Name, 2, $_) of
		[] ->
		    Num_Node = list_to_integer(string:sub_word(Node_Name, 2, $t));
		Val ->
		    Num_Node = list_to_integer(Val)
	    end,		    
	    spawn(H, fun() -> launch_node(H, Num_Clients_Node, Num_Node) end),
	    launch(New_Total_Num_Clients, T)
    end.

%%---------------------------------------------------------------------
%% @doc
%%     launch/3 distributes a number of clients evenly among the number
%%     of client nodes, and triggers the clients login logic.
%%
%% @spec launch(Total_Num_Clients, Num_Nodes, Domain) -> ok
%% @end
%%---------------------------------------------------------------------
launch(Total_Num_Clients, Num_Nodes, Domain) ->
    case Num_Nodes == 0 of
	true ->
	    ok;
	false ->
	    Num_Clients_Node = Total_Num_Clients div Num_Nodes,
	    New_Total_Num_Clients = Total_Num_Clients - Num_Clients_Node,
	    Node = client_node_name(Num_Nodes, Domain),
	    spawn(Node, fun() -> launch_node(Node, Num_Clients_Node, Num_Nodes) end),
	    launch(New_Total_Num_Clients, Num_Nodes - 1, Domain)
    end.

%%---------------------------------------------------------------------
%% @doc
%%     launch_node/3 triggers the logic to set up the clients that will
%%     run on an specific node.
%%
%% @spec launch_node(_Node, Num_Clients, Num_Node) -> setup/2
%% @end
%%---------------------------------------------------------------------
launch_node(_Node, Num_Clients, Num_Node) ->
    io:format("Setting up clients and traffic generators.~n", []),
    setup(Num_Clients, Num_Node).

%%---------------------------------------------------------------------
%% @doc
%%     setup/2 sets up the clients database to store the names and the
%%     pids of the clients that will be available in the node passed as
%%     the second argument to the function. Then, it triggers the clients
%%     login logic for the clients in that node
%%
%% @spec setup(Num_Clients, Num_Node) -> setup_clients/2
%% @end
%%---------------------------------------------------------------------
setup(Num_Clients, Num_Node) ->
    launch_clients_db(Num_Node),
    setup_clients(Num_Clients, Num_Node).

%%---------------------------------------------------------------------
%% @doc
%%     setup_clients/2 logs in to the system as many clients as the number
%%     passed as the first argument, and stores the said clients in the
%%     local client database of clients available for traffic generation.
%%
%% @spec setup_clients(Num_Clients, Num_Node) -> Status messages.
%% @end
%%---------------------------------------------------------------------
setup_clients(Num_Clients, Num_Node)->
    case Num_Clients == 0 of
	true ->
	    io:format("Clients at node client_~p@domain are set up.~n", [Num_Node]);
	false ->
	    Client_Name = client_name(Num_Clients, Num_Node),
	    Clients_DB_Name = client_db_name(Num_Node),
	    whereis(Clients_DB_Name) ! {self(), add, {Client_Name, undefined, not_in_use}},
	    receive
		{client_added, ok} ->
		    login(Client_Name),
		    setup_clients(Num_Clients - 1, Num_Node);
		Other ->
		    io:format("~n~nError adding doped clients to the database.~n" ++ 
				  "setup_clients/2 received: ~p~n" ++
				  "Aborting.~n", [Other])
	    end		    
    end.

%%---------------------------------------------------------------------
%% @doc
%%     launch_clients_db/1 starts a client_db process and registers it
%%     locally to the node specified as the argument.
%%
%% @spec launch_clients_db(Num_Node) -> Status message | {error, Reason}
%% @end
%%---------------------------------------------------------------------
launch_clients_db(Num_Node) ->
    DB_Name = client_db_name(Num_Node),
    start_local(DB_Name).

%%%===================================================================
%%% AUXILIARY FUNCTIONS
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%     This function returns the pid of a router, from a routers list
%%     that is passed as parameter.
%%
%% @spec pick_router(Routers_List) -> Pid | {error, Reason}
%% @end
%%--------------------------------------------------------------------
pick_router(Routers_List) ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    Router = lists:nth(random:uniform(length(Routers_List)),Routers_List),
    {_, _, Router_Pid} = Router,
    Router_Pid.

%%--------------------------------------------------------------------
%% @doc
%%     chat_session_name/2 returns the name of a chat session, built 
%%     after the names of the clients passed as parameters.
%%
%% @spec chat_session_name(Sender, Receiver) -> string() | {error, Reason}
%% @end
%%--------------------------------------------------------------------
chat_session_name(Sender, Receiver) ->
    case Sender < Receiver of
	true ->
	    string:join(["chat_session", Sender, Receiver], "_");
	false ->
	    string:join(["chat_session", Receiver, Sender], "_")
    end.

%%---------------------------------------------------------------------
%% @doc
%%     client_name/2 is an auxiliary function to build client names.
%%
%% @spec client_name(Num_Client, Num_Node) -> string()
%% @end
%%---------------------------------------------------------------------
client_name(Num_Client, Num_Node) ->
    TC = string:concat("tc", integer_to_list(Num_Client)),
    string:join([TC, integer_to_list(Num_Node)], "_").

%%---------------------------------------------------------------------
%% @doc
%%     client_db_name/1 is an auxiliary function to build the clients_db
%%     name of the node passed as the argument.
%%
%% @spec client_db_name(Node_Number) -> atom() | existing_atom()
%% @end
%%---------------------------------------------------------------------
client_db_name(Node_Number) ->
    Target_DB = string:join(["client",
			     integer_to_list(Node_Number),
			     "Client_DB"], "_"),
    try list_to_existing_atom(Target_DB) of
	Val ->
	    Val
    catch
	error:_ ->
	    list_to_atom(Target_DB)
    end.

%%---------------------------------------------------------------------
%% @doc
%%     client_node_name/2 is an auxiliary function to build the name of
%%     a client node given a node number and a domain.
%%
%% @spec client_node_name(Node_Number, Domain) -> atom() | existing_atom()
%% @end
%%---------------------------------------------------------------------
client_node_name(Node_Number, Domain) ->
    case is_atom(Domain) of
	true ->
	    D = atom_to_list(Domain);
	false ->
	    D = Domain
    end,
    Str = "client_" ++ integer_to_list(Node_Number) ++ "@" ++ D,
    string_to_atom(Str).

%%---------------------------------------------------------------------
%% @doc
%%     retrieve/2 is an auxiliary function to retrieve a chat session
%%     pid from a client's sessions registry.
%%
%% @spec retrieve(Session_Name, Chat_Pids) -> Pid() | empty list.
%% @end
%%---------------------------------------------------------------------
retrieve(Session_Name, Chat_Pids) ->
    case lists:keyfind(Session_Name, 1, Chat_Pids) of
	false ->
	    [];
	{_Chat_Session_Name, Chat_Session_Pid} ->
	    Chat_Session_Pid
    end.

%%---------------------------------------------------------------------
%% @doc
%%     client_pid/1 is an auxiliary function to retrieve a client pid.
%%
%% @spec client_pid(Client_Name) -> Pid()
%% @end
%%---------------------------------------------------------------------
client_pid(Client_Name) ->
    Tokens = string:tokens(Client_Name, "_"),
    {Num_Node, _} = string:to_integer(lists:nth(2, Tokens)),
    Target_DB = whereis(client_db_name(Num_Node)),
    Target_DB ! {self(), peak, {Client_Name}},
    receive
	[] ->
	    {error, client_does_not_exist};
	[Client_Name, Client_Pid, not_in_use] ->
	    Client_Pid
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

%%---------------------------------------------------------------------
%% @doc
%%     sys_time/1 returns as an integer the time passed as a tuple
%%
%% @spec sys_time({A1, A2, A3}) -> integer()
%% @end
%%---------------------------------------------------------------------
sys_time({A1, A2, A3}) ->
    ((A1 * 1000000) + A2) * 1000000 + A3.


%%---------------------------------------------------------------------
%% @doc
%%     notify_logger/3 abstracts the action of notifying the logger when
%%     a message has been sent, received or aknowledged.
%%
%% @spec notify_logger(Action, Metadata, Status) ->
%%                  ok | {Action, Metadata, Status}
%% @end
%%---------------------------------------------------------------------
notify_logger(Action, Metadata, Status) ->    
    case whereis(throughput_logger) of
	undefined ->
	    ok;
	Pid_Throughput_Logger ->
	    Pid_Throughput_Logger ! {Action, Metadata, Status}
    end,
    case whereis(latency_logger) of
	undefined ->
	    ok;
	Pid_Latency_Logger ->
	    Pid_Latency_Logger ! {Action, Metadata, Status}
    end.

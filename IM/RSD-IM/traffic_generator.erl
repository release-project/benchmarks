%%%--------------------------------------------------------------------
%%% TRAFFIC GENERATOR MODULE
%%%
%%% @author: Mario Moro Hernandez upon a design by Natalia Chechina
%%% @copyright (C) 2015, RELEASE project
%%% @doc
%%%	Traffic Generator module for the Distributed Erlang instant
%%%     messenger (IM) application developed as a real benchmark for
%%%     the Scalable Distributed Erlang extension of the Erlang/OTP
%%%     language.
%%%
%%%	This module implementes the traffic generation logic to load a
%%%	system similar to the system described in the Section 2 of the
%%%	document "Instant Messenger Architectures Design Proposal".
%%% @end
%%% Created : 25 Jan 2015 by Mario Moro Hernandez
%%%-------------------------------------------------------------------
-module(traffic_generator).

-export([start/3]).

%%====================================================================
%% MAIN FUNCTIONS
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%     start/3 starts the traffic generation assumed that clients are
%%     logged in.
%%
%% @spec start(Num_Clients, Cl_Nodes, TG_Nodes) -> ok
%% @end
%%--------------------------------------------------------------------
start(Num_Clients, Cl_Nodes, TG_Nodes) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A, B, C),
    Self = self(),
    Client_DBs = get_client_DBs(Cl_Nodes), 
    Container_Pids = [spawn(TG_Node, fun() ->
					     container(Client_DBs, Self, 0)
				     end)
		      || TG_Node <- TG_Nodes],
    setup_dbs(TG_Nodes, Container_Pids, Client_DBs, Self),
    start(Num_Clients, Cl_Nodes, TG_Nodes, Client_DBs, 0).

%%--------------------------------------------------------------------
%% @doc
%%     start/5 starts the traffic generators once the client databases
%%     are cloned and split.
%%
%% @spec start(Num_Clients, Cl_Nodes, TG_Nodes, Client_DBs, Done) -> ok
%% @end
%%--------------------------------------------------------------------
start(Num_Clients, Cl_Nodes, TG_Nodes, Client_DBs, Done) ->
    receive
	{split_db_finished, ok} ->
	    case Done + 1 == length(TG_Nodes) of
		true ->
		    timer:sleep(1000), %% <== NASTY HACK FOR SYNCHRONISATION
		    destroy_client_dbs(Client_DBs),
		    start_traffic(Num_Clients, TG_Nodes);
		false ->
		    start(Num_Clients, Cl_Nodes, TG_Nodes, Client_DBs, Done + 1)
	    end;
	Other ->
	    io:format("start/4 received: ~p~n", [Other]),
	    start(Num_Clients, Cl_Nodes, TG_Nodes, Client_DBs, Done)
    end.

%%--------------------------------------------------------------------
%% Client information containers setup
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%     setup_dbs/4 initiates the process of fetching the information
%%     of the logged-in clients, and set up a number of ets tables in
%%     the traffic generator nodes, so the traffic generators can
%%     access the pids of the clients and flood the architecture with
%%     traffic.
%%     
%% @spec setup_dbs(TG_Nodes, Container_Pids, Client_DBs, Start_Pid) -> ok.
%% @end
%%--------------------------------------------------------------------
setup_dbs(TG_Nodes, Container_Pids, Client_DBs, Start_Pid) ->
    case TG_Nodes of
	[] ->
	    ok;
	[TG_Node|Tail_TG_Nds] ->
	    [Cnt_Pid | Tail_Cnt_Pids] = Container_Pids,
	    spawn(TG_Node, fun() ->
				   setup_dbs(Client_DBs, Cnt_Pid, Start_Pid)
			   end),
	    setup_dbs(Tail_TG_Nds, Tail_Cnt_Pids, Client_DBs, Start_Pid)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     setup_dbs/3 does the required operations on each of the traffic
%%     generator nodes.
%%     
%% @spec setup_dbs(Client_DBs, Container_Pids, Start_Pid) -> ok.
%% @end
%%--------------------------------------------------------------------
setup_dbs(Client_DBs, Container_Pid, Start_Pid) ->
    case Client_DBs of
    	[] ->
    	    ok;
    	[H|T] ->
    	    spawn(fun() -> split_db(H, Container_Pid) end),
    	    setup_dbs(T, Container_Pid, Start_Pid)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     split_db/2 splits a client database into four public ets tables,
%%     so the traffic generators can access the pids of the clients.
%%     
%% @spec split_db({Client_DB_Name, Pid, Size}, Container_Pid) -> ok.
%% @end
%%--------------------------------------------------------------------
split_db({Client_DB_Name, Pid, _Size}, Container_Pid) ->
    DB_List = fetch_db(Pid),
    Tables = split_ets(DB_List, Client_DB_Name, 4),
    give_away_tables(Tables, Container_Pid).

%%--------------------------------------------------------------------
%% @doc
%%     fetch_db/1 returns a list that contains the full client database
%%     corresponding to the pid passed as parameter.
%%     
%% @spec fetch_db(Client_DB_Pid) -> list() | {error, Reason}.
%% @end
%%--------------------------------------------------------------------
fetch_db(Client_DB_Pid) ->
    Client_DB_Pid ! {self(), pass_binary},
    receive
	Binary when is_binary(Binary) ->
	    binary_to_term(Binary);
	_Other ->
	    {error, no_binary_gotten}
    end.

%%--------------------------------------------------------------------
%% @doc
%%     split_ets/3 is an auxiliary function to split_db/2. It creates
%%     a number of ets tables and feeds them with the data contained
%%     in the list passed as parameter. This list is provided by the
%%     function fetch_db/1. It returns a list with the names of the
%%     created tables.
%%     
%% @spec split_ets(List, Table_Name, Num_Tables) -> list().
%% @end
%%--------------------------------------------------------------------
split_ets(List, Table_Name, Num_Tables) ->
    case length(List) rem Num_Tables of
	0 ->
	    Limit = length(List) div Num_Tables;
	_Other ->
	    Limit = (length(List) div Num_Tables) + 1
    end,
    Tables = [ets:new(string_to_atom(X),[set, named_table, public])
	      || X <- [atom_to_list(Table_Name) ++ "_" ++ integer_to_list(Y)
		       || Y <- lists:seq(1, Num_Tables)]],
    lists:foreach(fun(X) -> insert_elements(X, Tables, Limit) end, List),
    Tables.

%%--------------------------------------------------------------------
%% @doc
%%     insert_elements/3 is an auxiliary function to split_ets/3. Given
%%     an element, and a list of tables, this function inserts the said
%%     element into the corresponding ets table.
%%   
%% @spec insert_elements(Element, Tables, Limit) -> true | false.
%% @end
%%--------------------------------------------------------------------
insert_elements(Element, Tables, Limit) ->
    {Name, _Pid, _} = Element,
    Num = list_to_integer(string:substr(string:sub_word(Name, 1, $_),3)),
    case Num rem Limit of
	0 ->  
	    ets:insert(lists:nth(Num div Limit,Tables), Element);
	_Other ->
	    ets:insert(lists:nth(Num div Limit + 1, Tables), Element)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     container/0 is the process that owns the ets tables containing
%%     the information of the logged-in clients.
%%
%% @spec container() -> ok.
%% @end
%%--------------------------------------------------------------------
container() ->
    receive
	stop ->
	    ok;
	_Other ->
	    container()
    end.

%%--------------------------------------------------------------------
%% @doc
%%     container/3 is the first stage of the process that owns the ets
%%     tables containing the information of the logged-in clients.
%%
%% @spec container(Client_DBs, Done) -> container/0.
%% @end
%%--------------------------------------------------------------------
container(Client_DBs, Start_Pid, Done) ->		     
    receive
	{'ETS-TRANSFER', _Tab, _FromPid, _GiftData} ->
	    case Done + 1 == length(Client_DBs) of
		true ->
		    Start_Pid ! {split_db_finished, ok},
		    container();
		false ->
		    container(Client_DBs, Start_Pid, Done + 1)
	    end;
	Other ->
	    io:format("container/2 received: ~p~n", [Other]),
	    container(Client_DBs, Start_Pid, Done)	
    end.

%%--------------------------------------------------------------------
%% @doc
%%     destroy_client_dbs/1 destroys the client data bases in the client
%%     nodes, given a list with the pids of the said data bases.
%%
%% @spec destroy_client_dbs(Client_DBs) -> ok.
%% @end
%%--------------------------------------------------------------------
destroy_client_dbs(Client_DBs) ->
    case Client_DBs of
	[] ->
	    ok;
	[{_, Pid, _}| T] ->
	    Pid ! {undefined, stop},
	    destroy_client_dbs(T)
    end.

%%--------------------------------------------------------------------
%% Traffic Launch logic
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%%     start_traffic/2 determines the number of chat sessions and
%%     to launch the necessary traffic generators.
%%
%% @spec start_traffic(Num_Clients, TG_Nodes) -> ok
%% @end
%%--------------------------------------------------------------------
start_traffic(Num_Clients, TG_Nodes) ->
    case Num_Clients rem 2 of
    	0 -> 
    	    Num_Chat_Sessions = Num_Clients div 2;
    	_Other ->
    	    Num_Chat_Sessions = Num_Clients div 2 + 1
    end,
    launch_generators(Num_Chat_Sessions,
		      TG_Nodes,
		      length(TG_Nodes)).

%%--------------------------------------------------------------------
%% @doc
%%     launch_generators/3 distributes the number of chat sessions
%%     amongst the clients and spawns the processes that start the
%%     traffic generator processes.
%%
%% @spec launch_generators(Num_Chat_Sessions, TG_Nds, Num_TG_Nds) -> ok
%% @end
%%--------------------------------------------------------------------
%%=====================================================================
%% This function should be the one in use. It is not changed because of
%% the experiments run already. But in the future, this is the version
%% of the function that must prevail.
%%
%% The function currently in use generates conversations between more
%% than the half of the clients logged in.
%%=====================================================================
%% launch_generators(_Num_Chat_Sessions, [], _Num_TG_Nds) ->
%%     ok;
%% launch_generators(Num_Chat_Sessions, TG_Nds, Num_TG_Nds) ->
%%    Cht_Sns_Nd = Num_Chat_Sessions div length(TG_Nds),
%%     New_Num_Chat_Sns = Num_Chat_Sessions - Cht_Sns_Nd,
%%     [Target_Node|T] = TG_Nds,
%%     spawn(Target_Node, fun() ->
%% 			       start_generators({first_stage,
%% 						 Cht_Sns_Nd, 10})
%% 		       end),
%%     launch_generators(New_Num_Chat_Sns, T, Num_TG_Nds - 1).

%%====================================================================
%% This is the function currently in use. It should not be used any
%% longer.
%%====================================================================
launch_generators(Num_Chat_Sessions, TG_Nds, Num_TG_Nds) ->
    case Num_TG_Nds of
    	1 ->
    	    Cht_Sns_per_Nd = (Num_Chat_Sessions div length(TG_Nds)) +
    		(Num_Chat_Sessions rem length(TG_Nds)),
    	    spawn(hd(TG_Nds), fun()->
    				      start_generators({first_stage,
    							Cht_Sns_per_Nd, 10})
    			      end),
    	    ok;
    	_Other ->
    	    Cht_Sns_per_Nd = Num_Chat_Sessions div Num_TG_Nds,
    	    Target_Node = lists:nth(Num_TG_Nds, TG_Nds),
    	    spawn(Target_Node, fun () -> 
    				       start_generators({first_stage,
    							 Cht_Sns_per_Nd, 10})
    			       end),
    	    launch_generators(Num_Chat_Sessions, TG_Nds, Num_TG_Nds -1)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     start_generators/1 start the traffic generator processes. It 
%%     first determines the number of traffic generators that will be
%%     spawned (first stage) and spawns the traffic generators (second
%%     stage).
%%
%% @spec start_generators({first_stage, Num_Chat_Sessions, N_Ch_TG}) -> ok
%% @end
%%--------------------------------------------------------------------
start_generators({first_stage, Num_Chat_Sessions, N_Ch_TG}) ->
    case Num_Chat_Sessions rem N_Ch_TG of
	0 ->
	    Num_TGs = Num_Chat_Sessions div N_Ch_TG;
	_Other ->
	    Num_TGs = Num_Chat_Sessions div N_Ch_TG + 1
    end,
    Client_Tables = get_client_tables(),
    start_generators({second_stage, Num_TGs, Num_Chat_Sessions, Client_Tables, N_Ch_TG});
start_generators({second_stage, Num_TGs, Num_Chat_Sessions, Client_Tables, N_Ch_TG}) ->
    case Num_TGs of
	1 ->
	    spawn(fun() -> strt(Num_Chat_Sessions, Client_Tables, []) end),
	    ok;
	_Other ->
	    spawn(fun() -> strt(N_Ch_TG, Client_Tables, []) end),
	    start_generators({second_stage, Num_TGs - 1, Num_Chat_Sessions - N_Ch_TG, Client_Tables, N_Ch_TG})
    end.

%%====================================================================
%% TRAFFIC GENERATOR PROCESS
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%     strt/3 sets the random seed before starting the traffic generator
%%     process.
%%
%% @spec strt(Num_Chats, Client_Tables, Active_Chats) -> no_return()
%% @end
%%--------------------------------------------------------------------
strt(Num_Chats, Client_Tables, Active_Chats) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    traffic_generator(Num_Chats, Client_Tables, Active_Chats).

%%--------------------------------------------------------------------
%% @doc
%%     traffic_generator/3 is the first state of the traffic_generator
%%     process. It starts the conversations and moves to the second
%%     state of the traffic generator process.
%%
%% @spec traffic_generator(Num_Chats, Client_Tables, Active_Chats) -> no_return()
%% @end
%%--------------------------------------------------------------------
traffic_generator(Num_Chats, Client_Tables, Active_Chats) ->
    case Num_Chats of
	0 -> 
	    traffic_generator(Client_Tables, Active_Chats);
	_Other ->
	    Self = self(),
	    start_conversation(Client_Tables, Self),
	    traffic_generator(Num_Chats - 1, Client_Tables, Active_Chats)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     traffic_generator/2 constitutes the final state of the traffic
%%     generator processes. It starts a new conversation when it receives
%%     the confirmation that one of the active chats has finsihed.
%%
%% @spec traffic_generator(Client_Tables, Active_Chats) -> no_return()
%% @end
%%--------------------------------------------------------------------
traffic_generator(Client_Tables, Active_Chats) ->
    receive
	{finished_chat, Chat_Session}->
	    {value, {_Chat}, New_Active_Chats} = lists:keytake(Chat_Session, 1, Active_Chats),
	    Self = self(),
	    start_conversation(Client_Tables, Self),
	    traffic_generator(Client_Tables, New_Active_Chats);
	{New_Chat, new_chat} ->
	    traffic_generator(Client_Tables, [{New_Chat}|Active_Chats]);
	Other ->
	    io:format("traffic_generator/2 received ~p~n", [Other]),
	    traffic_generator(Client_Tables, Active_Chats)
    end.

%%====================================================================
%% AUXILIARY FUNCTIONS
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%     get_client_DBs/1 returns a list of tuples containing the name of
%%     the client_DB as the key and its pid as the value. It is a
%%     wrapper for get_client_DBs/2, which performs the operations.
%%
%% @spec get_client_DBs(Nodes) -> [{Client_1_DB, Pid()}, ... ] | []
%% @end
%%--------------------------------------------------------------------
get_client_DBs(Nodes) ->
    get_client_DBs(Nodes, []).

%%--------------------------------------------------------------------
%% @doc
%%     get_client_DBs/2 returns a list with all the client databases
%%     deployed. This list is a list of tuples with the format
%%     {Client_DB_Name, Pid, Size}.
%%
%% @spec get_client_DBs(Nodes, Client_DBs) -> [{Client_DB_Name, Pid, Size}]
%% @end
%%--------------------------------------------------------------------
get_client_DBs(Nodes, Client_DBs) ->
    case Nodes of
	[] ->
	    Client_DBs;
	[Node|Tail_Nodes] ->
	    {Client_DB_Name, Pid, Size} = retrieve_client_db_pid(Node),
	    get_client_DBs(Tail_Nodes, [{Client_DB_Name, Pid, Size}|Client_DBs])
    end.

%%--------------------------------------------------------------------
%% @doc
%%     get_client_tables/0 returns a list containing the names of all
%%     the client tables present in the IM architecture.
%%
%% @spec get_client_tables() -> [Client_Tables]
%% @end
%%--------------------------------------------------------------------
get_client_tables() ->
    get_tables(ets:all(), []).

%%--------------------------------------------------------------------
%% @doc
%%     get_client_tables/2 returns a list containing the names of all
%%     the client tables present in the IM provided a list of ets tables
%%     passed as parameter (Tables).
%%
%% @spec get_tables(Tables, Client_Tables) -> [Client_Table]
%% @end
%%--------------------------------------------------------------------
get_tables([], Client_Tables) ->
    Client_Tables;
get_tables([H|T], Client_Tables) ->
    case is_atom(H) of 
	true ->
	    case string:equal(string:sub_word(atom_to_list(H), 1, $_), "client") of
		true ->
		    get_tables(T, [H|Client_Tables]);
		false ->
		    get_tables(T, Client_Tables)
	    end;
	false->
	    get_tables(T, Client_Tables)
    end.

%%--------------------------------------------------------------------
%% @doc
%%    retrieve_client_db_pid/1 retrieves the pid of the Client_DB
%%    process at a target node specified as parameter.
%%
%% @spec retrieve_client_db_pid(Node) -> Pid | ok
%% @end
%%--------------------------------------------------------------------
retrieve_client_db_pid(Node) ->
    [Node_Name|_Domain] = string:tokens(atom_to_list(Node), "@"),
    case string:sub_word(Node_Name, 2, $_) of
	[] -> 
	    Client_DB_Name = "client_" ++
		list_to_integer(string:sub_word(Node_Name, 2, $t)) ++
		"_Client_DB";
	Val ->
	    Client_DB_Name = "client_" ++ Val ++ "_Client_DB"
    end,
    From = self(),
    spawn(Node, fun() -> lookup_client_db(From, string_to_atom(Client_DB_Name)) end),
    receive
	{DB_Name, Pid, Size} ->
	    {DB_Name, Pid, Size};
	Other ->
	    io:format("Something went wrong. retrieve_client_db_Pid/1" ++
			  " received: ~p~n", [Other])
    end.

%%--------------------------------------------------------------------
%% @doc
%%    lookup_client_db/2 is an auxiliary function to the function
%%    retrieve_client_db_pid/2. It is spawned at the target node and
%%    sends the pid of the required Client_DB process (given that 
%%    such process exists).
%%
%% @spec lookup_client_db(From, Client_DB_Name) ->
%%         {client_db_pid, Pid} | undefined
%% @end
%%--------------------------------------------------------------------
lookup_client_db(From, Client_DB_Name) ->
    case whereis(Client_DB_Name) of
    	undefined ->
    	    From ! undefined;
    	Pid ->
    	    From ! {Client_DB_Name, Pid, ets:info(Client_DB_Name, size)}
    end.

%%--------------------------------------------------------------------
%% @doc
%%     start_conversation/1 triggers the interaction between two random
%%     clients.
%%
%% @spec chat_session_name(Sender, Receiver) -> string() | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start_conversation(Client_Tables, TG_Pid) ->
    {{Snd_Name, Snd_Pid}, {Rvr_Name, _Rvr_Pid}} = pick_random_clients(Client_Tables),
    String = [random:uniform(26) + 96 || _ <- lists:seq(1, random:uniform(75))],
    Interactions = random:uniform(48) + 12,
    Snd_Pid ! {Snd_Name, Rvr_Name, {String, Interactions, TG_Pid}, send_message},
    case Snd_Name < Rvr_Name of
	true ->
	    New_Chat = string:join(["chat_session", Snd_Name, Rvr_Name], "_");
	false ->
	    New_Chat = string:join(["chat_session", Rvr_Name, Snd_Name], "_")
    end,
    TG_Pid ! {New_Chat, new_chat}.

%%--------------------------------------------------------------------
%% @doc
%%     pick_random_clients/2 returns a tuple containing the information
%%     of two clients randomly chosen from a clients list.
%%
%% @spec pick_random_clients(Num_Node, Total_Nodes, Total_Clients) ->
%%          {Client_A, Client_B} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
pick_random_clients(Client_Tables) ->
    Sender = pick_random_sender(Client_Tables),
    case pick_random_sender(Client_Tables) of
	Sender ->
	    pick_random_clients(Client_Tables);
	Receiver ->
	    {Sender, Receiver}
    end.

%%--------------------------------------------------------------------
%% @doc
%%     This function returns a tuple {Client_Name, Client_Pid} corres-
%%     ponding to a client selected from a clients list.
%%
%% @spec pick_random_sender(Num_Node, Total_Clients) ->
%%          {Client_Name, Client_Pid} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
pick_random_sender(Client_Tables) ->
    Table = lists:nth(random:uniform(length(Client_Tables)), Client_Tables),
    Table_Size = ets:info(Table, size),
    [_, Num_Node, _, _, Tbl_Num] = string:tokens(atom_to_list(Table), "_"),
    Num_Client = random:uniform(Table_Size) + (Table_Size * (list_to_integer(Tbl_Num) - 1)),
    Client_Name = client_name(Num_Client, Num_Node),
    case ets:lookup(Table, Client_Name) of
	[{Client_Name, Client_Pid, _}] ->
	    {Client_Name, Client_Pid};
	_Other ->
	    pick_random_sender(Client_Tables)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     client_name/2 is an auxiliary function to build client names.
%%
%% @spec client_name(Num_Client, Num_Node) -> string()
%% @end
%%--------------------------------------------------------------------
client_name(Num_Client, Num_Node) ->
    TC = string:concat("tc", integer_to_list(Num_Client)),
    string:join([TC, Num_Node], "_").

%%--------------------------------------------------------------------
%% @doc
%%     give_away_tables/2 transfers the ownership of the tables in the
%%     list passed as the first parameter to the container process with
%%     the pid specified in the second parameter.
%%
%% @spec give_away_tables(Tables, Container_Pid) -> ok
%% @end
%%--------------------------------------------------------------------
give_away_tables(Tables, Container_Pid) ->
    case Tables of
	[] ->
	    ok;
	[H|T] ->
	    ets:give_away(H, Container_Pid, []),
	    give_away_tables(T, Container_Pid)
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

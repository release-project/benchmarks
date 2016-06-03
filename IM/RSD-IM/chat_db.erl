%%%--------------------------------------------------------------------
%%% CHAT_DB MODULE
%%%
%%% @author: Mario Moro Hernandez upon a design by Natalia Chechina
%%% @copyright (C) 2014, RELEASE project
%%% @doc
%%%	Chat_DB module for the Distributed Erlang instant messenger (IM)
%%%	application developed as a real benchmark for the Scalable 
%%%	Distributed Erlang extension of the Erlang/OTP language.
%%%
%%%	This module implementes the functionality for the database that
%%%	stores the chat sessions happening at a given time in a system
%%%     similar to the system described in the Section 2 of the document
%%%     "Instant Messenger Architectures Design Proposal".
%%% @end 
%%% Created: 1 Jul 2014 by Mario Moro Hernandez
%%%--------------------------------------------------------------------
-module(chat_db).

-export([start/1, stop/1, chat_db/1]).

%%%====================================================================
%%% API
%%%====================================================================

%%---------------------------------------------------------------------
%% @doc
%% Starts the chat sessions database.
%%
%% @spec start(DB_Name)
%% @end
%%---------------------------------------------------------------------
start(DB_Name) ->
    global:register_name(DB_Name, spawn(fun() -> chat_db(DB_Name) end)).

%%--------------------------------------------------------------------
%% @doc
%% Stops the chat sessions database.
%%
%% @spec stop(DB_Name)
%% @end
%%--------------------------------------------------------------------
stop(DB_Name) ->
    destroy(DB_Name),
    global:unregister_name(DB_Name).

%%--------------------------------------------------------------------
%% @doc
%% chat_db is first stage of the database process. It creates an ets
%% table after the atom specified as the argument DB_Name.
%%
%% @spec chat_db(DB_Name)
%% @end
%%--------------------------------------------------------------------
chat_db(DB_Name) ->
    case ets:info(DB_Name) of
	undefined ->
	    create(DB_Name),
	    chat_db_loop(DB_Name);
	_ ->
	    chat_db_loop(DB_Name)
    end.

%%--------------------------------------------------------------------
%% @doc
%% chat_db_loop constitutes the database process, and offers an inter-
%% face to interact with the ets table, allowing the input or retrieval
%% of information concerning the chat sessions in the system. 
%%
%% @spec chat_db_local(DB_Name)
%% @end
%%--------------------------------------------------------------------
chat_db_loop(DB_Name) ->
    receive
	{From, add, {Session_Name, Session_Pid, C1, C1_Pid, C2, C2_Pid}} -> 
	    add_session(DB_Name, Session_Name, Session_Pid, C1, C1_Pid, C2, C2_Pid),
	    send_reply(From, {session_added, ok}),
	    chat_db_loop(DB_Name);
	{From, remove, {Session_Name}} ->
	    remove_session(DB_Name, Session_Name),
	    send_reply(From, {session_removed, ok}),
	    chat_db_loop(DB_Name);
	{From, full_db} ->
	    A = retrieve_db(DB_Name),
	    send_reply(From, A),
	    chat_db_loop(DB_Name);
	{From, peak, {Session_Name}} ->
	    S = peak_session(DB_Name, Session_Name),
	    send_reply(From, S),
	    chat_db_loop(DB_Name);
	{From, peak_by_pid, {Session_Pid}} ->
	    S = peak_session_pid(DB_Name, Session_Pid),
	    send_reply(From, S),
	    chat_db_loop(DB_Name);
	{From, session_pid, {Session_Name}} ->
	    Pid = session_pid(DB_Name, Session_Name),
	    send_reply(From, Pid),
	    chat_db_loop(DB_Name);
	{From, opened_sessions, {Client}} ->
	    S = client_sessions(DB_Name, Client),
	    send_reply(From, S),
	    chat_db_loop(DB_Name);
	{From, recover, {S_Group_Name, Target_DB_Name, Source_DB_Name}} ->
	    spawn(fun() -> recover_db(S_Group_Name, From, Target_DB_Name, Source_DB_Name) end),
	    chat_db_loop(DB_Name);
	{undefined, From, recovery_binary, Binary} ->
	    ets:insert(DB_Name, binary_to_term(Binary)),
	    if
		is_list(From) == true ->
		    send_reply(lists:last(From), {recovered, ok});
		true ->
		    send_reply(From, {recovered, ok})
	    end,
	    chat_db_loop(DB_Name);
	{From, pass_binary} ->
	    Binary = term_to_binary(ets:tab2list(DB_Name)),
	    send_reply(From, Binary),
	    chat_db_loop(DB_Name);
	{From, stop} ->
	    stop(DB_Name),
	    send_reply(From, {chat_session_db_destroyed, ok});
	Other ->
	    io:format("Something failed at Chat_DB. Received: ~p", [Other]),
	    chat_db_loop(DB_Name)
    end.

%%---------------------------------------------------------------------
%% @doc
%% Creates a new ets table -named Table_Name- to store the different
%% chat sessions active in the system.
%%
%% @spec create(Table_Name) -> Table_Name | {error, Error}
%% @end
%%---------------------------------------------------------------------
create(Table_Name) ->
    ets:new(Table_Name, [set, named_table]).

%%---------------------------------------------------------------------
%% @doc
%% Destroys ets table named Table_Name.
%%
%% @spec destroy(Table_Name) -> Table_Name | {error, Error}
%% @end
%%---------------------------------------------------------------------
destroy(Table_Name) ->
    ets:delete(Table_Name).

%%---------------------------------------------------------------------
%% @doc
%% Adds a chat session to the chat sessions database.
%%
%% @spec add_session(Table_Name, Chat_Session_Pid,
%%   	 	     Client_A, Client_B) -> true | {error, Error}
%% @end
%%---------------------------------------------------------------------
add_session(Table_Name, Chat_Session_Name, Chat_Session_Pid, Client_A, Client_A_Pid, Client_B, Client_B_Pid) ->
    ets:insert(Table_Name, {Chat_Session_Name, Chat_Session_Pid, Client_A, Client_A_Pid, Client_B, Client_B_Pid}).

%%---------------------------------------------------------------------
%% @doc
%% Removes a chat session from the chat sessions database.
%%
%% @spec remove_session(Table_Name, Chat_Session_Pid) -> true | {error, Error}
%% @end
%%---------------------------------------------------------------------
remove_session(Table_Name, Chat_Session_Name) ->
    ets:delete(Table_Name, Chat_Session_Name).

%%---------------------------------------------------------------------
%% @doc
%% Returns the contents of the whole database as a list of lists, where
%% each of the nested lists is one session. (Not in use, only for debug
%% purposes).
%%
%% @spec retrieve_db(Table_Name) ->
%%                  [[session_1], ..., [session_n]] | {error, Error}
%% @end
%%---------------------------------------------------------------------
retrieve_db(Table_Name) ->
    ets:match(Table_Name, {'$0', '$1', '$2', '$3', '$4', '$5'}).

%%---------------------------------------------------------------------
%% @doc
%% Returns a list containing the data of the session passed as argument.
%%
%% @spec peak_session(Table_Name, Session_Name) ->
%%         [Session_Name, Session_Pid,
%%          Client_A, Client_A_Pid, ClientB, Client_B_Pid] | {error, Error}
%% @end
%%---------------------------------------------------------------------
peak_session(Table_Name, Session_Name) ->
    L = ets:select(Table_Name, [{{'$0', '$1', '$2', '$3', '$4', '$5'},
					 [{'==', '$0', Session_Name}],
					 [['$0', '$1', '$2', '$3', '$4', '$5']]}]),
    case L == [] of
	true ->
	    L;
	false ->
	    lists:last(L)
    end.

%%---------------------------------------------------------------------
%% @doc
%% Returns a list containing the data of the session corresponding to
%% the pid passed as argument.
%%
%% @spec peak_session(Table_Name, Session_Pid) ->
%%          [Session_Name, Session_Pid,
%%          Client_A, Client_A_Pid, ClientB, Client_B_Pid] | {error, Error}
%% @end
%%---------------------------------------------------------------------
peak_session_pid(Table_Name, Session_Pid) ->
     L = ets:select(Table_Name, [{{'$0', '$1', '$2', '$3', '$4', '$5'},
					 [{'==', '$1', Session_Pid}],
					 [['$0', '$1', '$2', '$3', '$4', '$5']]}]),
    case L == [] of
	true ->
	    L;
	false ->
	    lists:last(L)
    end.

%%---------------------------------------------------------------------
%% @doc
%% Returns the Pid of a given session passed as argument.
%%
%% @spec session_pid(Table_Name, Session_Name) -> Session_Pid | []
%% @end
%%---------------------------------------------------------------------
session_pid(Table_Name, Session_Name) ->
    L = ets:select(Table_Name, [{{'$0', '$1', '$2', '$3', '$4', '$5'},
					 [{'==', '$0', Session_Name}],
					 [['$1']]}]),
    case L == [] of
	true ->
	    L;
	false ->
	    lists:last(lists:last(L))
    end.

%%---------------------------------------------------------------------
%% @doc
%% Returns a list containing all the sessions -and the information of
%% these sessions- in which the client passed as the argument is one of
%% participants. It returns an empty list if the client is not involved
%% in any chat at the time of invocation.
%%
%% @spec retrieve_server(Table_Name, Session_Pid) ->
%%                  [Session_1, ..., Session_k]  | []
%% @end
%%---------------------------------------------------------------------
client_sessions(Table_Name, Client) ->
    ets:safe_fixtable(Table_Name, true),
    Sessions = opened_sessions(Table_Name, Client, ets:first(Table_Name), []),
    ets:safe_fixtable(Table_Name, false),
    Sessions.

%%---------------------------------------------------------------------
%% @doc
%% Replicates the chat session table specified in the Source argument,
%% in a new table with name Table_Name.
%%
%% @spec retrieve_server(Table_Name, Session_Pid) -> true | {error, Error}
%% @end
%%---------------------------------------------------------------------
recover_db(S_Group_Name, From, Destination, Source) ->
    ets:safe_fixtable(Source, true),
    send_to_replica(S_Group_Name, Destination, {undefined, From, recovery_binary, term_to_binary(ets:tab2list(Source))}),
    ets:safe_fixtable(Source, false).

%%---------------------------------------------------------------------
%% @doc
%% Auxiliary function to the function client_sessions(Table_Name, Client).
%% This function traverses the ets table containing all the conversations,
%% and puts in a list all those in which the client is one of the
%% participants.
%%
%% @spec opened_sessions(Table_Name, Client, Cursor, Sessions) ->
%%                  [Session_1, ..., Session_n] | []
%% @end
%%---------------------------------------------------------------------
opened_sessions(Table_Name, Client, Cursor, Sessions) -> 
    case Cursor of
	'$end_of_table' ->
	    Sessions;
	Cursor ->
	    case (element(3, lists:last(ets:lookup(Table_Name, Cursor))) == Client) or
		 (element(4, lists:last(ets:lookup(Table_Name, Cursor))) == Client) of
		true ->
		    opened_sessions(Table_Name, Client, ets:next(Table_Name, Cursor), 
				    (Sessions ++ [lists:last(ets:lookup(Table_Name, Cursor))]));
		false ->
		    opened_sessions(Table_Name, Client, ets:next(Table_Name, Cursor), Sessions)
	    end
    end.

%%---------------------------------------------------------------------
%% @doc
%%     send_to_replica/3 implements a safe mechanism to send replies to
%%     remote databases.
%%
%% @spec send_to_replica(S_Group_Name, DB_Replica, Message) -> Message | ok
%% @end
%%---------------------------------------------------------------------
send_to_replica(S_Group_Name, DB_Replica, Message) ->
    case s_group:whereis_name(S_Group_Name, DB_Replica) of
	undefined ->
	    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	    random:seed({A,B,C}),
	    {Own_Group, _Rubbish} = hd(s_group:own_s_groups()),
	    Broadcast_Message = {S_Group_Name, DB_Replica, Message},
	    broadcast_message(Own_Group, Broadcast_Message);
	Pid ->
	    Pid ! Message
    end.

%%--------------------------------------------------------------------
%% @doc
%%     broadcast_message/2 sends the message to a random relay to broadcast
%%     the message to the target database.
%%
%% @spec broadcast_message(S_Group_Name, Message) -> Message | ok 
%% @end
%%--------------------------------------------------------------------
broadcast_message(S_Group_Name, Message) ->
    Relay = string_to_atom("relay_" ++ integer_to_list(random:uniform(50))), %% <=== Change to the actual number of relays (currently 50).
    case s_group:whereis_name(S_Group_Name, Relay) of
	undefined ->
	    ok;
	Pid ->
	    Pid ! {Message, broadcast_message}
    end.

%%--------------------------------------------------------------------
%% @doc
%%     send_reply/2 implements a safe mechanism to send replies to
%%     remote databases without using s_group:whereis/2. It uses
%%     a kind of next-hop routing instead, following the pids of the
%%     relay and router recorded in the From field of the request
%%     message. This ensures that no extra connections are established
%%     when sending back the replies from the databases.
%%
%% @spec send_reply(Target, Reply) -> {reply_back, Next_Hops, Reply} |
%%                                    Reply | ok.
%% @end
%%--------------------------------------------------------------------
send_reply(Target, Reply) ->
    case Target of
	undefined ->
	    ok;
	[Pid|Next_Hops] ->
	    case rpc:pinfo(Pid, status) of
		undefined ->
		    {S_Group_Name, _Rubbish} = hd(s_group:own_s_groups()),
		    Relay = string_to_atom("relay_" ++ integer_to_list(random:uniform(50))), %% <=== Change to the actual number of relays (currently 50).
		    s_group:whereis_name(S_Group_Name, Relay) ! {reply_back, Next_Hops, Reply};
		_Status ->		    
		    Pid ! {reply_back, Next_Hops, Reply},
		    ok
	    end;
	Other when is_pid(Other) ->
	    Target ! Reply;
	Other ->
	    io:format("send_reply/2 received: ~p~n", [Other]),
	    ok
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

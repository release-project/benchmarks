%%%--------------------------------------------------------------------
%%% MONITORED_DB MODULE
%%%
%%% @author: Mario Moro Hernandez upon a design by Natalia Chechina
%%% @copyright (C) 2014, RELEASE project
%%% @doc
%%%	Monitored_DB module is an auxiliary data structure to store the
%%%     pids of all the processes monitored by a server supervisor. It
%%%     is used to retrieve information about processes when they fail
%%%     and they need to be restarted again. This module is based on the
%%%     client_db module.
%%% @end 
%%% Created: 15 Aug 2014 by Mario Moro Hernandez
%%%--------------------------------------------------------------------
-module(monitored_db).

-export([start/1, stop/1, start_local/1, stop_local/1, monitored_db/1]).

%%%====================================================================
%%% API
%%%====================================================================

%%---------------------------------------------------------------------
%% Starts the monitored processes database.
%%
%% @spec start(DB_Name)
%% @end
%%---------------------------------------------------------------------
start(DB_Name) ->
    global:register_name(DB_Name, spawn_link(fun() -> monitored_db(DB_Name) end)).

%%--------------------------------------------------------------------
%% @doc
%% Stops the monitored processes database.
%%
%% @spec stop(DB_Name)
%% @end
%%--------------------------------------------------------------------
stop(DB_Name) ->
    destroy(DB_Name),
    global:unregister_name(DB_Name).

%%---------------------------------------------------------------------
%% Starts the monitored processes database, and registers it locally.
%%
%% @spec start_local(DB_Name)
%% @end
%%---------------------------------------------------------------------
start_local(DB_Name) ->
    Pid = spawn_link(fun() -> monitored_db(DB_Name) end),
    register(DB_Name, Pid).

%%--------------------------------------------------------------------
%% @doc
%% Stops a monitored processes database that has been started locally.
%%
%% @spec stop_local(DB_Name)
%% @end
%%--------------------------------------------------------------------
stop_local(DB_Name) ->
    destroy(DB_Name),
    unregister(DB_Name).

%%--------------------------------------------------------------------
%% @doc
%% monitored_db is first stage of the database process. It creates an 
%% ets table after the atom specified in the parameter DB_Name.
%%
%% @spec monitored_db(DB_Name)
%% @end
%%--------------------------------------------------------------------
monitored_db(DB_Name) ->
    %% process_flag(trap_exit, true),
    case ets:info(DB_Name) of
	undefined ->
	    create(DB_Name),
	    monitored_db_loop(DB_Name);
	_ ->
	    monitored_db_loop(DB_Name)
    end.

%%--------------------------------------------------------------------
%% @doc
%% monitored_db_loop constitutes the database process, and offers an
%% interface to interact with the ets table, allowing the input or
%% retrieval of information concerning the clients logged in the system. 
%%
%% @spec monitored_db_loop(DB_Name)
%% @end
%%--------------------------------------------------------------------
monitored_db_loop(DB_Name) ->
    receive 
	{From, add, Process_Pid, Process_Type, Process_Name} -> 
	    add_process(DB_Name, Process_Pid, Process_Type, Process_Name),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! {process_added, ok}
	    end,
	    monitored_db_loop(DB_Name);
	{From, remove, Process_Pid} ->
	    remove_process(DB_Name, Process_Pid),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! {process_removed, ok}
	    end,
	    monitored_db_loop(DB_Name);
	{From, peak, Process_Pid} ->
	    C = peak_process(DB_Name, Process_Pid),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! C
	    end,
	    monitored_db_loop(DB_Name);
	{From, retrieve, Process_Pid} ->
	    C = retrieve_process(DB_Name, Process_Pid),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! C
	    end,
	    monitored_db_loop(DB_Name);
	{From, recover, Binary} ->
	    ets:insert(DB_Name, binary_to_term(Binary)),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! {recovered, ok}
	    end,
	    monitored_db_loop(DB_Name);
	{From, recover_server, Target_DB_Name, Source_DB_Name} ->
	    spawn(fun() -> recover_server_table(Target_DB_Name, Source_DB_Name, From) end),
	    monitored_db_loop(DB_Name);
	{From, stop} ->
	    stop(DB_Name),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! {monitored_db_destroyed, ok}
	    end
    end.

%%---------------------------------------------------------------------
%% @doc
%% Creates a new ets table -named Table_Name- to store the different
%% clients active in the system.
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
%% Adds a process to the monitored processes database.
%%
%% @spec add_process(Table_Name, Process_Pid, Process_Type,
%%                  Process_Name) -> true | {error, Error}
%% @end
%%---------------------------------------------------------------------
add_process(Table_Name, Process_Pid, Process_Type, Process_Name) ->
    ets:insert(Table_Name, {Process_Pid, Process_Type, Process_Name}).

%%---------------------------------------------------------------------
%% @doc
%% Removes a process from the monitored_processes database.
%%
%% @spec remove_process(Table_Name, Process_Pid) -> true | {error, Error}
%% @end
%%---------------------------------------------------------------------
remove_process(Table_Name, Process_Pid) ->
    ets:delete(Table_Name, Process_Pid).

%%---------------------------------------------------------------------
%% @doc
%% Returns a list containing the data of the process passed as parameter.
%% It does not delete the process from the db.
%%
%% @spec peak_process(Table_Name, Process_Pid) ->
%%                       [Process_Pid, Process_Type, Process_Name] 
%%                       | {error, Error}
%% @end
%%---------------------------------------------------------------------
peak_process(Table_Name, Process_Pid) ->
    L = ets:select(Table_Name, [{{'$0', '$1', '$2'},
				 [{'==', '$0', Process_Pid}],
				 [['$0', '$1', '$2']]}]),
    case L == [] of
	true ->
	    L;
	false ->
	    lists:last(L)
    end.

%%---------------------------------------------------------------------
%% @doc
%% Deletes the process passed as parameter from the database and returns
%% a list containing the data of the said process.
%%
%% @spec retrieve_process(Table_Name, Process_Pid) ->
%%                       [Client_Monitor_Pid, Client] | {error, Error}
%% @end
%%---------------------------------------------------------------------
retrieve_process(Table_Name, Process_Pid) ->
    C = peak_process(Table_Name, Process_Pid),
    remove_process(Table_Name, Process_Pid),
    C.

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
recover_server_table(_Destination, Source, Destination_Pid) ->
    ets:safe_fixtable(Source, true),
    Destination_Pid ! {monitored_db_binary, term_to_binary(ets:tab2list(Source))},
    ets:safe_fixtable(Source, false).

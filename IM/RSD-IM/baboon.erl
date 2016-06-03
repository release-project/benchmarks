%%%-------------------------------------------------------------------
%%% BABOON MODULE
%%%
%%% @author Mario Moro Hernandez upon a design by Natalia Chechina
%%% @copyright (C) 2015, RELEASE project
%%% @doc
%%%     Baboon module is a quick implementation of the Chaos Monkey to
%%%     kill name-registered processes only. It must be executed in
%%%     the dashboard node.
%%%
%%%     Usage: baboon:start(Time_Interval).
%%%            where Time_Interval is time in milliseconds.
%%% @end
%%% Created : 22 Jul 2015 by Mario Moro Hernandez
%%%-------------------------------------------------------------------
-module(baboon).

-export([start/1]).

%%--------------------------------------------------------------------
%% @doc
%%     start/1 starts the baboon processes to kill name-registered 
%%     processes at the interval specified in the Timer parameter.
%%     Timer parameter is in milliseconds.
%%
%% @spec start(Timer) -> pid()
%% @end
%%--------------------------------------------------------------------
start(Timer) ->
    spawn(fun() -> chaos_on(Timer) end).

%%--------------------------------------------------------------------
%% @doc
%%     chaos_on/1 sets up the baboon processes to kill name-registered
%%     processes at the interval specified by Timer parameter.
%%
%% @spec chaos_on(Timer) -> no_return()
%% @end
%%--------------------------------------------------------------------
chaos_on(Timer) ->
    Router_Nodes = find_router_nodes(),
    Server_Groups = find_server_groups(Router_Nodes),
    Baboon_Pids = deploy_baboons(Server_Groups, []),
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    chaos_on(Timer, Baboon_Pids).

%%--------------------------------------------------------------------
%% @doc
%%     chaos_on/2 signs the baboon processes when to kill a process.
%% 
%% @spec chaos_on(Timer, Baboon_Pids) -> no_return()
%% @end
%%--------------------------------------------------------------------
chaos_on(Timer, Baboon_Pids) ->
    lists:nth(random:uniform(length(Baboon_Pids)), Baboon_Pids) ! kill_something,
    timer:sleep(Timer),
    chaos_on(Timer, Baboon_Pids).

%%--------------------------------------------------------------------
%% @doc
%%     baboon/1 constitutes the baboon process. At the reception of the
%%     kill signal it chooses a random (name-registered) process and
%%     kills it.
%%
%% @spec baboon({setup, Server_Group}) -> baboon({ready, Server_Group, Non_Relay_Processes}).
%%       baboon({ready, Server_Group, Processes}) -> no_return().
%% @end
%%--------------------------------------------------------------------
baboon({setup, Server_Group}) ->
    Non_Relay_Processes = non_relay_processes(s_group:registered_names({s_group, Server_Group}), []),
    baboon({ready, Server_Group, Non_Relay_Processes});
baboon({ready, Server_Group, Processes}) ->
    receive
	_Any ->
	    exit(s_group:whereis_name(Server_Group, lists:nth(random:uniform(length(Processes)), Processes)), kill),
	    baboon({ready, Server_Group, Processes})
    end.

%%====================================================================
%% AUXILIARY FUNCTIONS
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%     find_router_nodes/0 finds all the router nodes present in the
%%     IM architecture.
%%
%% @spec find_router_nodes() -> list() | {error, Reason}
%% @end
%%--------------------------------------------------------------------
find_router_nodes() ->
    S = self(),
    [Target_Node|_] = nodes(),
    spawn(Target_Node, fun() -> find_router_nodes(S) end),
    receive
	{router_nodes, Router_Nodes} when Router_Nodes =/= no_router_nodes_found->
	    Router_Nodes;
	_Other ->
	    io:format("Upppppssss... something went wrong. Aborting.~n"),
	    {error, no_router_nodes_found}
    end.

%%--------------------------------------------------------------------
%% @doc
%%     find_router_nodes/1 find the router nodes in an s_group and
%%     notifies find_router_nodes/0.
%%
%% @spec find_router_nodes(To) -> {router_nodes, list()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
find_router_nodes(To) ->
    S_Groups = s_group:own_s_groups(),
    To ! {router_nodes, get_router_nodes(S_Groups)}.

%%--------------------------------------------------------------------
%% @doc
%%     get_router_nodes/1 extract the router nodes present in an s_group.
%%
%% @spec get_router_nodes(list()) -> list() | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get_router_nodes([]) ->
    {error, no_router_nodes_found};
get_router_nodes([{S_Gr_Name, Nodes}|T]) ->
    case S_Gr_Name of
	routers_group ->
	    Nodes;
	_Other ->
	    get_router_nodes(T)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     find_server_groups/1 finds the server s_groups present in the
%%     IM architecture.
%%
%% @spec find_server_groups(Router_Nodes) -> fun()
%% @end
%%--------------------------------------------------------------------
find_server_groups(Router_Nodes) ->
    S = self(),
    lists:foreach(fun(Router_Node) ->
			  spawn(Router_Node,
				fun() ->
					get_server_groups(Router_Node, S)
				end)
		  end, Router_Nodes),
    receive_server_groups([], length(Router_Nodes)).

%%--------------------------------------------------------------------
%% @doc
%%     receive_server_groups/2 collects the names of the server s_groups
%%     present in the IM architecture.
%%
%% @spec receive_server_groups(S_Groups, Acc) -> list().
%% @end
%%--------------------------------------------------------------------
receive_server_groups(S_Groups, 0) ->
    S_Groups;
receive_server_groups(S_Groups, Received) ->
    receive
	{s_group_name, S_Grp_Name} ->
	    receive_server_groups([S_Grp_Name|S_Groups], Received - 1);
	_Other ->
	    receive_server_groups(S_Groups, Received)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     get_server_groups/2 notifies the server_s_groups found to the
%%     receive_server_groups/2 function.
%% @spec get_server_groups(Router_Node, pid()) -> {s_group_name, {atom(), list()}}
%% @end
%%--------------------------------------------------------------------
get_server_groups(Router_Node, To) ->
    S_Groups = s_group:own_s_groups(),
    Serv_Grps = server_groups(S_Groups, []),
    To ! {s_group_name, {Router_Node, Serv_Grps}}.

%%--------------------------------------------------------------------
%% @doc
%%     server_groups/2 returns a list with the server_s_grups found in
%%     the IM architecture.
%%
%% @spec server_groups([{Server_Group_Name, Nodes}]) -> list()
%% @end
%%--------------------------------------------------------------------
server_groups([], Srv_Grps) ->
    Srv_Grps;
server_groups([{Srv_Grp_Name,_Nodes}|T], Srv_Grps) ->
    case Srv_Grp_Name of
	routers_group ->
	    server_groups(T, Srv_Grps);
	_Other ->
	    server_groups(T, [Srv_Grp_Name|Srv_Grps])
    end.

%%--------------------------------------------------------------------
%% @doc
%%     deploy_baboons/2 spawns the baboon processes in the corresponding
%%     router node. Returns a list with the pids of all deployed baboon
%%     processes.
%%
%% @spec deploy_baboons([{Router_Node, Server_Groups}]), Baboon_Pids) -> [pid()]
%% @end
%%--------------------------------------------------------------------
deploy_baboons([], Baboon_Pids) ->
    Baboon_Pids;
deploy_baboons([{Router_Node, [Serv_Grp|_]}|T], Baboon_Pids) ->
    Pid = spawn(Router_Node, fun() -> baboon({setup, Serv_Grp}) end),
    deploy_baboons(T, [Pid|Baboon_Pids]).


%%--------------------------------------------------------------------
%% @doc
%%     non_relay_process/2 removes the relay processes from the list of
%%     name-registered processes that will be killed.
%%
%% @spec non_relay_processes(Processes, list()) -> list()
%% @end
%%--------------------------------------------------------------------
non_relay_processes([], Prcs) ->
    Prcs;
non_relay_processes([{_, H}|T], Prcs) ->
    case string:substr(atom_to_list(H), 1, 5) of
	"relay" ->
	    non_relay_processes(T, Prcs);
	_Other ->
	    non_relay_processes(T, [H|Prcs])
    end.

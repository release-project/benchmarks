-module(debug).

-compile(export_all).

%% =============== Functions to kill processes manually ===============
kill_router() ->
    Routers = extract(router),
    case length(Routers) of
	0 ->
	    io:format("No router processes found in this node.~n"),
	    ok;
	1 ->
	    exit(hd(Routers), kill);
	_Other ->
	    prompt_user(Routers)
    end.

kill_r_sup() ->
    R_Sup = extract(router_sup),
    case length(R_Sup) of
	0 ->
	    io:format("No router supervisor process found in this node.~n"),
	    ok;
	_Other ->
	    exit(hd(R_Sup),kill)
    end.

kill_r_sup_mon() ->
    R_Sup_Mon = extract(router_sup_mon),
    case length(R_Sup_Mon) of
	0 ->
	    io:format("No router supervisor monitor process found in this node.~n"),
	    ok;
	_Other ->
	    exit(hd(R_Sup_Mon),kill)
    end.

kill_servers() ->
    R_Nodes = find_router_nodes(),
    Server_Groups = find_server_groups(R_Nodes),
    _Killer_Pids = deploy_server_killer(Server_Groups, []).

deploy_server_killer([], Killer_Pids) ->
    Killer_Pids;
deploy_server_killer([{Router_Node, _}|T], Killer_Pids) ->
    Pid = spawn(Router_Node, fun() -> server_killer() end),
    deploy_server_killer(T, [Pid|Killer_Pids]).

server_killer() ->
    S_Nodes = extract_server_nodes(),
    lists:foreach(fun(S_Node) -> spawn(S_Node, fun() -> kill() end) end, S_Nodes).

kill() ->
    Pids = extract(server_sup),
    timer:sleep(random:uniform(500) + 500),
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids).


%% =============== Function to check if information in processes is outdated ===============
check_lists() ->
    case string:sub_word(atom_to_list(node()),1,$_) of
	"router" ->
	    R_Nodes = extract_router_nodes(nodes(),[node()]);
	_Other ->
	    R_Nodes = extract_router_nodes()
    end,
    Self = self(),
    lists:foreach(fun(Node) -> spawn(Node, fun() -> request_lists(Self) end) end, R_Nodes),
    check_lists(length(R_Nodes), 0, []).

check_lists(Total_Nodes, Total_Nodes, Lists) ->
    lists_checker(lists:flatten(Lists),[]);
check_lists(Total_Nodes, Received, Lists) ->
    receive
	{lists, Received_Lists} ->
	    check_lists(Total_Nodes, Received + 1, [Received_Lists|Lists]);
	Other ->
	    io:format("check_lists received: ~n~p~n", [Other]),
	    ok
    end.

%% =============== Controlled Rhesus ===============
chaos_on(Kill_Time) ->
    chaos_on(60000, Kill_Time).

chaos_on(Run_Time, Kill_Time) ->
    case string:sub_word(atom_to_list(node()),1,$_) of
	"router" ->
	    R_Nodes = extract_router_nodes(nodes(),[node()]);
	_Other ->
	    R_Nodes = extract_router_nodes()
    end,
    Rhesus_Pid = spawn(hd(R_Nodes), fun()->rhesus:chaos_on(Kill_Time)end), 
    spawn(fun() -> timer:sleep(Run_Time),
		   exit(Rhesus_Pid, kill),
		   check_lists()
	  end),
    ok.

%% =============== Utility functions ===============
%% --------------- List Checker utility functions ---------------
request_lists(Master_Pid) ->
    Pids = extract(router),
    Pids2 = Pids ++ extract(router_sup),
    Pids3 = Pids2 ++ extract(router_sup_mon),
    lists:foreach(fun(Pid) -> Pid ! {request_parameters, self()} end, Pids3),
    request_lists(Master_Pid, length(Pids3), 0, []).

request_lists(Master_Pid, Num_Processes, Num_Processes, Lists) ->
    Master_Pid ! {lists, Lists};
request_lists(Master_Pid, Num_Processes, Received, Lists) ->
    receive
	{parameters, Pid, SG,  _Useless_Pid, _Monitored_Routers, _Monitored_Relays, Routers_List, Server_List, _Relay_List,  _Routers_Info, _Routers_DBs_Pids} ->
	    request_lists(Master_Pid, Num_Processes, Received + 1, [{Pid, {SG, Routers_List, Server_List}}|Lists]);
	{parameters, Pid, SG, _R_Sup_Pid, Routers_List, _List_Monitored_Servers, Server_List, _Server_Nodes} ->
	    request_lists(Master_Pid, Num_Processes, Received + 1, [{Pid, {SG, Routers_List, Server_List}}|Lists]);
	Other ->
	    io:format("request_lists/4 received:~n~p~n", [Other]),
	    ok
    end.

lists_checker([],[]) ->
    io:format("Everything seems ok.~n");
lists_checker([], Outdated) ->
    Outdated;
lists_checker([{Pid, {SG_List, R_List, S_List}}|T], Outdated) ->
    if T == [] ->
	    lists_checker(T, Outdated);
       true ->
	    {_T_Pid, {_T_SG_List, T_R_List, T_S_List}} = hd(T),
	    if
		{T_R_List, T_S_List} == {R_List, S_List} ->
		    lists_checker(T, Outdated);
		{T_R_List, T_S_List} =/= {R_List, S_List} ->
		    lists_checker(T, [{Pid, {SG_List, R_List, S_List}}|Outdated]);
		T_R_List =/= R_List and (T_S_List == S_List) ->
		    lists_checker(T, [{SG_List, Pid, R_List}|Outdated]);
		T_S_List =/= S_List and (T_R_List == R_List) ->
		    lists_checker(T, [{SG_List, Pid, S_List}|Outdated]);
		true ->
		    lists_checker(T, Outdated)
	    end
    end.

%% --------------- S_Groups extraction (to kill servers without creating extra connections) ---------------
find_router_nodes() ->
    S = self(),
    Target_Node = target(nodes()),
    spawn(Target_Node, fun() -> find_router_nodes(S) end),
    receive
	{router_nodes, Router_Nodes} when Router_Nodes =/= no_router_nodes_found->
	    Router_Nodes;
	_Other ->
	    io:format("Upppppssss... something went wrong. Aborting.~n"),
	    {error, no_router_nodes_found}
    end.

find_router_nodes(To) ->
    S_Groups = s_group:own_s_groups(),
    To ! {router_nodes, get_router_nodes(S_Groups)}.

get_router_nodes([]) ->
    {error, no_router_nodes_found};
get_router_nodes([{S_Gr_Name, Nodes}|T]) ->
    case S_Gr_Name of
	routers_group ->
	    Nodes;
	_Other ->
	    get_router_nodes(T)
    end.

find_server_groups(Router_Nodes) ->
    S = self(),
    lists:foreach(fun(Router_Node) ->
			  spawn(Router_Node,
				fun() ->
					get_server_groups(Router_Node, S)
				end)
		  end, Router_Nodes),
    receive_server_groups([], length(Router_Nodes)).

receive_server_groups(S_Groups, 0) ->
    S_Groups;
receive_server_groups(S_Groups, Received) ->
    receive
	{s_group_name, S_Grp_Name} ->
	    receive_server_groups([S_Grp_Name|S_Groups], Received - 1);
	_Other ->
	    receive_server_groups(S_Groups, Received)
    end.

get_server_groups(Router_Node, To) ->
    S_Groups = s_group:own_s_groups(),
    Serv_Grps = server_groups(S_Groups, []),
    To ! {s_group_name, {Router_Node, Serv_Grps}}.

server_groups([], Srv_Grps) ->
    Srv_Grps;
server_groups([{Srv_Grp_Name,_Nodes}|T], Srv_Grps) ->
    case Srv_Grp_Name of
	routers_group ->
	    server_groups(T, Srv_Grps);
	_Other ->
	    server_groups(T, [Srv_Grp_Name|Srv_Grps])
    end.

%% --------------- Pids and Node Names extractors ---------------
extract(Process_Type) ->
    case Process_Type of
	router -> 
	    extract(router_process, processes(), []);
	router_sup ->
	    extract(router_supervisor, processes(), []);
	router_sup_mon ->
	    extract(router_supervisor_monitor, processes(), []);
	server_sup ->
	    extract(server_supervisor_loop, processes(), []);
	_Other ->
	    []
    end.

extract(_Process_Type, [], Return) ->
    Return;
extract(Process_Type, [H|T], Return) ->
    {current_function, {_,F,_}} = erlang:process_info(H, current_function),
    if
	F == Process_Type ->
	    extract(Process_Type, T, [H|Return]);
	true ->
	    extract(Process_Type, T, Return)
    end.

extract_router_nodes() ->
    extract_router_nodes(nodes(),[]).

extract_router_nodes([], R_Nds)->
    R_Nds;
extract_router_nodes([H|T], R_Nds) ->
    case string:sub_word(atom_to_list(H),1,$_) of
	"router" ->
	    extract_router_nodes(T,[H|R_Nds]);
	_Other ->
	    extract_router_nodes(T,R_Nds)
    end.

extract_server_nodes() ->
    extract_server_nodes(nodes(connected),[]).

extract_server_nodes([], S_Nds) ->
    S_Nds;
extract_server_nodes([H|T], S_Nds) ->
    case string:sub_word(atom_to_list(H), 1, $_) of
	"server" ->
	    extract_server_nodes(T, [H|S_Nds]);
	_Other ->
	    extract_server_nodes(T, S_Nds)
    end.

%% --------------- Menus ---------------
prompt_user(Routers) ->
    {Routers_List, Routers_String} = routers_list(Routers),
    Killall = length(Routers) + 1,
    Menu_String = "\n===========================\n" ++
	          "Multiple routers to kill.\n" ++
	          "Please choose an option:\n" ++
	          lists:flatten(Routers_String) ++

	          integer_to_list(Killall) ++ ".- Kill all routers\n" ++
	          "===========================\n",
    {ok, [Input]} = io:fread(Menu_String ++
			   "Write an option (1-" ++ 
			   integer_to_list(Killall) ++"): ", "~d"),
    case Input of
	Killall ->
	    lists:foreach(fun(X) -> exit(X,kill) end, Routers);
	_Other when Input < Killall->
	    Target_Router = string:sub_word(lists:nth(Input, Routers_String),2) -- "\n",
	    {_, _, Pid} = lists:keyfind(Target_Router, 2, Routers_List),
	    exit(Pid, kill);
	_Other ->
	    prompt_user(Routers)
    end.

routers_list() ->
    routers_list(extract(router)).

routers_list(Routers) ->
    R_Sup = hd(extract(router_sup)),
    R_Sup ! {request_routers_list, self()},
    receive
	{routers_list, Routers_List} ->
	    Num_Rts = length(Routers),
	    T = lists:sort([X ++ "\n" || {_, X, _} <- [lists:keyfind(Y, 3, Routers_List) || Y <- Routers]]),
	    R = [integer_to_list(A) ++ ".- " ++ lists:nth(A,T) || A <- lists:seq(1,Num_Rts)],
	    {lists:keysort(2,Routers_List), R};
	_Other ->
	    []
    end.

%% ----------- silly function needed only to use the server killer at any node -----------
target([]) ->
    {error, no_router_nodes_found};
target([H|T]) ->
    case [lists:nth(X, atom_to_list(H)) || X<-lists:seq(1,3)] of
	"rou" -> H;
	_Other -> target(T)
    end.

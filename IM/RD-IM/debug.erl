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
    S_Nodes = extract_server_nodes(),
    Killers_Pids = [spawn(Node, fun() -> server_killer() end)|| Node <- S_Nodes].

server_killer() ->
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
	_Other ->
	    check_lists(Total_Nodes, Received, Lists)
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
    Pids = extract(router) ++ extract(router_sup) ++ extract(router_sup_mon),
    lists:foreach(fun(Pid) -> Pid ! {request_parameters, self()} end, Pids),
    request_lists(Master_Pid, length(Pids), 0, []).

request_lists(Master_Pid, Num_Processes, Num_Processes, Lists) ->
    Master_Pid ! {lists, Lists};
request_lists(Master_Pid, Num_Processes, Received, Lists) ->
    receive
	{parameters, Pid, _Useless_Pid, _Monitored_Routers, Routers_List, Server_List, _Routers_Info, _Routers_DBs_Pids} ->
	    request_lists(Master_Pid, Num_Processes, Received + 1, [{Pid, {Routers_List, Server_List}}|Lists]);
	{parameters, Pid, _R_Sup_Pid, Routers_List, _List_Monitored_Servers, Server_List, _Server_Nodes} ->
	    request_lists(Master_Pid, Num_Processes, Received + 1, [{Pid, {Routers_List, Server_List}}|Lists]);
	_Other ->
	    request_lists(Master_Pid, Num_Processes, Received, Lists)
    end.

lists_checker([],[]) ->
    io:format("Everything seems ok.~n");
lists_checker([], Outdated) ->
    Outdated;
lists_checker([{Pid, {R_List, S_List}}|T], Outdated) ->
    if T == [] ->
	    lists_checker(T, Outdated);
       true ->
	    {_T_Pid, {T_R_List, T_S_List}} = hd(T),
	    if
		{T_R_List, T_S_List} == {R_List, S_List} ->
		    lists_checker(T, Outdated);
		{T_R_List, T_S_List} =/= {R_List, S_List} ->
		    lists_checker(T, [{Pid, {R_List, S_List}}|Outdated]);
		T_R_List =/= R_List and (T_S_List == S_List) ->
		    lists_checker(T, [{Pid, R_List}|Outdated]);
		T_S_List =/= S_List and (T_R_List == R_List) ->
		    lists_checker(T, [{Pid, S_List}|Outdated]);
		true ->
		    lists_checker(T, Outdated)
	    end
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
    extract_server_nodes(nodes(),[]).

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
	    {_, Pid} = lists:keyfind(Target_Router, 1, Routers_List),
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
	    T = lists:sort([X ++ "\n" || {X, _} <- [lists:keyfind(Y, 2, Routers_List) || Y <- Routers]]),
	    R = [integer_to_list(A) ++ ".- " ++ lists:nth(A,T) || A <- lists:seq(1,Num_Rts)],
	    {lists:keysort(2,Routers_List), R};
	_Other ->
	    []
    end.    

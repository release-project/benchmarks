%% starts the benchmark
%%
%% Author: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>
%%
%% RELEASE project (http://www.release-project.eu/)
%%

-module(init_bench).

-export([main/0,main/1]).

main() ->
    G=fun bench:g12345/1, 
    N= 1000000, %% calculates Orbit for 0..N
    P= 24, %% Naumber of worker processes on each node
    NumGateways= 60, %% Naumber of gateway processes on each sub-master node
    G_size=8, %% Number of nodes in each s_group
    Nodes=config:get_key(nodes), %% Loads list of node names from config file
    NumGroups=length(Nodes) div G_size,
    Start = now(),
    if 
		NumGroups>0 ->
			NumberOfGroups=NumGroups,
			Group_size=G_size;
		true ->
			NumberOfGroups=1, %% when number of nodes is less than group size
			Group_size=length(Nodes)
	end,    
	bench:dist(G,N,NumGateways,P,Nodes,NumberOfGroups),
    LapsedUs = timer:now_diff(now(), Start),
    io:format("N:~p  ---- Num gateway processes: ~p --- Num worker processes: ~p --- Num Nodes: ~p  ---- Group size: ~p \n",[N, NumGateways, P, length(Nodes), Group_size]),
    io:format("Elapsed time in total (microseconds): ~p \n",[LapsedUs]). %microseconds

main(Nodes) ->
    G=fun bench:g12345/1, 
    N= 1000000, %% calculates Orbit for 0..N
    P= 40, %% Naumber of worker processes on each node
    NumGateways= 40, %% Naumber of gateway processes on each sub-master node
    G_size=6, %% Number of nodes in each s_group
    NumGroups=length(Nodes) div G_size,
    Start = now(),
    if 
		NumGroups>0 ->
			NumberOfGroups=NumGroups,
			Group_size=G_size;
		true ->
			NumberOfGroups=1, %% when number of nodes is less than group size
			Group_size=length(Nodes)
	end,    
	bench:dist(G,N,NumGateways,P,Nodes,NumberOfGroups),
    LapsedUs = timer:now_diff(now(), Start),
    io:format("N:~p  ---- Num gateway processes: ~p --- Num worker processes: ~p --- Num Nodes: ~p  ---- Group size: ~p \n",[N, NumGateways, P, length(Nodes), Group_size]),
    io:format("Elapsed time in total (microseconds): ~p \n",[LapsedUs]). %microseconds

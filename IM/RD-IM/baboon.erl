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
%%% Created : 22 Jul 2015 by mario <mario@ishigaki>
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
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    chaos_on(Timer, get_db_processes()).

%%--------------------------------------------------------------------
%% @doc
%%     chaos_on/2 kills a random name-registered process at the time
%%     interval passed as the parameter Timer (in milliseconds).
%% 
%% @spec chaos_on(Timer, Baboon_Pids) -> no_return()
%% @end
%%--------------------------------------------------------------------
chaos_on(Timer, Processes) ->
    exit(global:whereis_name(lists:nth(random:uniform(length(Processes)),Processes)),kill),
    timer:sleep(Timer),
    chaos_on(Timer, Processes).

%%--------------------------------------------------------------------
%% @doc
%%     get_db_processes/1 returns a list of the name-registered processes
%%     at the server nodes (i.e., database processes).
%%
%% @spec get_db_processes() -> Registered_Processes
%% @end
%%--------------------------------------------------------------------
get_db_processes() ->
    get_db_processes(global:registered_names(), []).

%%--------------------------------------------------------------------
%% @doc
%%     get_db_processes/2 returns a list of the name-registered processes
%%     at the server nodes given a list of name-registered processes.
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
get_db_processes([], DBs) ->
    DBs;
get_db_processes([H|T], DBs) ->
    case string:substr(atom_to_list(H),1,6) of
	"server" ->
	    get_db_processes(T, [H|DBs]);
	_Other ->
	    get_db_processes(T, DBs)
    end.

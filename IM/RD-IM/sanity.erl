-module(sanity).
-export([info/0, record/1]).

client_monitors() ->
    client_monitors(erlang:processes(), 0).

client_monitors([], Num_Mon) ->
    io:format("Number of client_monitor processes = ~p~n", [Num_Mon]),
    Num_Mon;

client_monitors([H|T], Num_Mon) ->
    {current_function, {_,F,_}} = erlang:process_info(H, current_function),
    case F of
	client_monitor ->
	    client_monitors(T, Num_Mon + 1);
	_Other ->
	    client_monitors(T, Num_Mon)
    end.

chat_sessions() ->
    chat_sessions(erlang:processes(),0).

chat_sessions([], Num_Chat) ->
    io:format("Number of chat_session processes = ~p~n", [Num_Chat]),
    Num_Chat;
chat_sessions([H|T], Num_Chat) ->
    {current_function, {_,F,_}} = erlang:process_info(H, current_function),
    case F of
	chat_session ->
	    chat_sessions(T, Num_Chat + 1);
	_Other ->
	    chat_sessions(T, Num_Chat)
    end.

total_processes() ->
    IM_P = client_monitors() + chat_sessions() + 5,
    S_P = length(erlang:processes()),
    io:format("Total IM processes = ~p~n" ++
	      "Total processes = ~p~n",
	      [IM_P, S_P]),
    ok.

info() ->
    io:format("=============================================================~n" ++
	      " IM processes running in node ~p~n" ++ 
	      "=============================================================~n", [node()]),
    total_processes(),
    io:format("=============================================================~n"),
    ok.
    
record(FileName) ->
    case file:open(FileName, [read,write]) of
	{ok, Fd} ->
	    io:format("Fd = ~p~n", [Fd]),
	    io:fwrite(Fd, "~p", ["IM Processes, Total Processes~n"]),
	    spawn(fun() -> write_num_processes(Fd) end);
	{error, Reason} ->
	    io:format("Error: ~p~n", [Reason])
    end.

write_num_processes(Fd) ->
    IM_P = client_monitors() + chat_sessions() + 5,
    S_P = length(erlang:processes()),
    io:fwrite(Fd, "~p, ~p~n", [IM_P, S_P]),
    timer:sleep(1000),
    write_num_processes(Fd).

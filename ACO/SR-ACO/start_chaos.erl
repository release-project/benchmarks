-module(start_chaos).
-export([run/3]).
-include("types.hrl").

run (Parent_printer, Chaos, Print) -> 
	register(chaos_starter,self()),
	put(parent_printer,  Parent_printer),
	if
		Chaos==true ->
			?Print(io_lib:format("chaos starter on node ~p" , [node()])),
			application:start(pman),
			application:start(chaos_monkey),
			chaos_monkey:on();
		true ->
			ok
	end,
	receive_and_forward_messages(Print, Chaos).

receive_and_forward_messages(Print, Chaos)->
	receive
		{pids,NewPIDs} -> 
			if Chaos==true ->
				Chaos_monkey=util:get_local_name(chaos_monkey),
				Chaos_monkey! {pids, NewPIDs};
			true ->
				ok
			end,
			receive_and_forward_messages(Print, Chaos);
		{run_the_chaos} ->
			if Chaos==true ->
				Chaos_monkey=util:get_local_name(chaos_monkey),
				Chaos_monkey! {run_the_chaos, get(starter)};
			true ->
				ok
			end,
			receive_and_forward_messages(Print, Chaos);
		{message, Message} ->
			if Print==true ->
				Parent_printer=get(parent_printer),
				case is_pid(Parent_printer) of
					false -> 
						io:format ("~p ~n", [lists:flatten(Message)]);
					_-> 
						Parent_printer! {message, Message}
				end;
			true ->
				ok %% no print as Print is false
			end,
			receive_and_forward_messages(Print, Chaos)
	end.

	

	

-module(start_chaos).
-export([run/0]).

run () -> 
	io:format("chaos starter on node ~p ~n" , [node()]),
	register(chaos_starter,self()),
	application:start(pman),
	application:start(chaos_monkey),
	chaos_monkey:on(),
	receive_and_forward_messages().

receive_and_forward_messages()->
	io:format("chaos starter before wait on node ~p ~n" , [node()]),
	timer:sleep(20), %% letting Chaos Monkey get started
	io:format("chaos starter after wait on node ~p ~n" , [node()]),
	receive
		{pids,NewPIDs} -> 
			util:show_message(lists:flatten(io_lib:format("chaos starter received something on node ~p ~n" , [node()]))),
			Chaos_monkey=util:get_local_name(chaos_monkey),
			io:format("chaos starter after received something on node ~p ~n" , [node()]),
			Chaos_monkey! {pids, NewPIDs},
			receive_and_forward_messages();
		{run_the_chaos} ->
			io:format("chaos starter - before getting {run_the_chaos} on node ~p ~n" , [node()]),
			Chaos_monkey=util:get_local_name(chaos_monkey),
			io:format("chaos starter - sending {run_the_chaos} from node ~p to ~p ~n" , [node(), Chaos_monkey]),
			Chaos_monkey! {run_the_chaos}
	end.

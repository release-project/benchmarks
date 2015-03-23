-module(util).
-compile(export_all).

-spec quit(string(),list()) -> ok.
quit(Fmt,Args) ->
    io:format(Fmt,Args),
    init:stop().
% This isn't ideal.  I think it only stops the current process, and there may
% be other processes running.  Specifically, if you put a non-existent node
% in the nodes file, we get an error message but the start_aco function carries
% on and attempts to start a process on that node anyway.

-spec int_of_string(string()) -> integer().
int_of_string(S) ->
   try
	list_to_integer(S)
    catch 
	error: _ ->
	    quit ("Error: bad integer ~p~n", [S])
    end.

-spec float_of_string(string()) -> float().
float_of_string(S) ->
    try 
	case string:chr (S, $.) of
	    0 -> list_to_integer(S);
	    _ -> list_to_float([$0|S])  % list_to_float doesn't like ".9"
	end
    catch
	error: _ -> quit ("Error: bad float ~p~n", [S])
    end.
		 
-spec check_positive({atom(), number()}) -> ok.
check_positive({S,N}) ->
    if N > 0 ->
	    ok;
       true -> quit ("Error: require ~p > 0, but found ~p.~n", [S,N])
    end.

-include_lib ("kernel/include/file.hrl"). % For file_info type

-spec check_file (string()) -> ok.
check_file(Fname) ->
    case file:read_file_info(Fname) of
	{ok, Info} -> 
	    #file_info{access=Access, type=Type} = Info,
	    if Access =:= read orelse Access =:= read_write
	       -> case Type of 
		      regular -> ok;
		      _ -> quit ("Error reading ~p: found ~p~n", [Fname, Type])
		  end;
	       true -> quit ("Error: file ~p not readable~n", [Fname])
	    end;
	{error, Reason} -> 
	    case Reason of
		enoent -> quit ("Error: can't open file ~p~n", [Fname]);
		_ -> quit ("Error opening file ~p: ~p~n", [Fname, Reason])
	    end
    end.

-spec check_node (atom()) -> ok.
check_node(N) ->
    case net_adm:ping(N) of
	pong -> ok;
	pang -> quit ("Error: can't find node ~p~n", [N])
    end.
	     


-spec make_tuple(non_neg_integer(), any()) -> tuple().
make_tuple(N,T) -> list_to_tuple (lists:duplicate(N,T)).


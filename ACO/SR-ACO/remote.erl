-module (remote).
-export ([start_nodes/1, stop_nodes/1]).

%% ----------------------- Starting nodes automatically ----------------------- %%
%
% We want to use slave:start to start (possibly remote) nodes automatically.
% * Use the -rsh ssh falg if you want to start nodes on remote machines.
% * This may be problematic because you need to have 'erl' in your path
%   and ssh doesn't read .profile or .bash_profile when it's executing a remote
%   command (this may be system dependent).  I got round this by adjusting the
%   path in .bashrc, although that's bad practice (.bashrc may be sourced multiple
%   times if you invoke recursive shells).
% * Your .beam files also need to be in Erlang's path.  The code below will do this
%   by reading the -pa argument and passing it to the remote machines, so you need to
%   start the program with 
%
%     erl -rsh ssh -pa <path to code> ...
%
%   It may also be possible to do this by setting appropriate shell variables, 
%   although then you again have to make sure that ssh is reading the right files.  
%
%   There shouldn't be any problem if you're starting nodes on the same machine as the 
%   main aco program.

-spec split_node_name (atom()) -> {atom(), atom()}.
split_node_name (S) ->
    case string:tokens(atom_to_list(S),"@") of
	[Node,Host] ->{list_to_atom(Host), list_to_atom(Node)};
	_  -> util:quit ("Error: bad node name ~p~n", [S])
    end.

-spec flatten_path([[string()]]) -> string().

flatten_path (P) -> string:join (lists:append(P), " ").
%% If you say '-pa aa bb -pa cc', you'll get [["aa", "bb"], ["cc"]]
%% lists:flatten does a deep flattening, so use append instead.

-spec start_nodes([atom()]) -> ok.
start_nodes (Nodes) ->
    Path = case init:get_argument(pa) of
	       {ok, Pa} -> "-pa " ++ flatten_path(Pa);
	       _ -> []
	   end,

    lists:foreach (fun (N) ->
			  {Host, Node} = split_node_name(N),
			   case slave:start(Host, Node, Path) of
			       {ok, N} -> ok;
			       {error, Msg} -> util:quit ("Error while trying to start node ~p: ~p~n", [N,Msg]);
			       E -> util:quit ("Error while trying to start node ~p: ~p~n", [N,E])
			   end
		   end, Nodes).


-spec stop_nodes ([atom()]) -> ok.
stop_nodes (Nodes) ->
    lists:foreach (fun(Node) -> rpc:call(Node, init, stop, []) end, Nodes).

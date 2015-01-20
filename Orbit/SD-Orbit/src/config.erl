%% provides access to the configuration file
%%
%% Author: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>
%%
%% RELEASE project (http://www.release-project.eu/)
%%

-module(config).

-export([get_key/1]).

%% ===================================================================
%% Public API
%% ===================================================================

%% return the value of a key
get_key(Key) ->
	List=load(['bench.config']),
	get_value(Key,List).
	
get_value(_Key,[]) ->
	not_found;
get_value(Key,[{Key, Value} | _Rest]) ->
	Value;

get_value(Key,[{_OtherKey, _Value} | Rest]) ->
	get_value(Key,Rest).

%% loads a config file
load(Files) ->
    TermsList =
        [ case file:consult(File) of
              {ok, Terms} ->
                  Terms;
              {error, Reason} ->
				  io:format("Failed to parse config file ~s: ~p\n", [File, Reason])
          end || File <- Files ],
		  lists:append(TermsList).


%% creates dynamically s_gropus
%%
%% Author: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>
%%

-module(grouping).

-export([initiate/2,create_group/3, create_group_list/3]).


%% makes s_groups by dividing all the nodes into a number of s_groups
initiate(Nodes, NumGroup) when NumGroup>0, is_list(Nodes) ->
        L= length(Nodes),
        Size=L div NumGroup,
        if
                Size==0 ->
                  group_size_is_zero;
                true->
                        do_grouping(Nodes, Size, NumGroup,[])
        end.

do_grouping(Nodes, _Size, 1, Acc) ->
        {ok, [make_group(Nodes, length(Acc)+1)|Acc]};

do_grouping(Nodes, Size, NumGroup, Acc) ->
        Group=lists:sublist(Nodes, Size),
        Remain=lists:subtract(Nodes, Group),
        NewGroup = make_group(Group, length(Acc)+1),
        do_grouping(Remain, Size, NumGroup-1, [NewGroup|Acc]).

%% creates a s_group on Submaster and includes all Workers in it
make_group([Submaster|Workers], Counter) ->
        spawn(Submaster, grouping, create_group, [self(), [Submaster|Workers], Counter]),
        receive GroupName ->
                {Submaster, GroupName}
        end.

%% create a s_group on the current node and return the name of s_group back to the master
create_group(Master, Nodes, Counter) ->
        FixName=group, %% prefix for creating s_group name
        GroupName=list_to_atom(atom_to_list(FixName) ++ integer_to_list(Counter)),
        try 
                {ok, GroupName, _Nodes}=s_group:new_s_group(GroupName,Nodes), %% creates group and add all nodes to it
                Master! GroupName %% Sends the group name to the submaster node
        catch
                 _:_ -> io:format("exception: SD Erlang is not installed at: ~p \n",[code:root_dir()]),
                        sderlang_is_not_installed
        end.  

%% divids the Orbit space among submasters
create_group_list(Sub_masters, N, NumberOfGroups) ->
        Submaster_nodes=[node()]++[HostName || {HostName, _Group} <- Sub_masters],
        {ok,master_group,_Submaster_nodes}=s_group:new_s_group(master_group, Submaster_nodes), %% creates a group and add master and all submasters to it
        List_of_groups=[{Host, GroupName, N div NumberOfGroups} || {Host, GroupName} <- Sub_masters], %% calculate the size that each group is responsible for
        R=N-((N div NumberOfGroups)*NumberOfGroups), %% when N is not divisible by NumberOfGroups -> R>0
        NewList_of_groups=sub_master:change_list(List_of_groups,length(List_of_groups),length(List_of_groups),3,(N div NumberOfGroups)+R),
        io:format("Number of groups: ~p\n", [length(NewList_of_groups)]),
        io:format("[{Host, GroupName, TableSize}] ~p\n", [NewList_of_groups]),
        NewList_of_groups.

#/bin/bash -x

#===============================================================================
# File  : start.sh
# Author: Mario Moro Hernandez
# Date  : 28th July 2014
# Description:
#         Bash script to launch the distributed instant messenger architecture
#         locally in one host.
#
#         It is configured to be used with Mate terminal. Uncomment the lines 
#         63, 66, and 116 to use it with Gnome terminal. Assumes that path to
#         "erl" is set. Otherwise it is also necessary to write the full path
#         in the following lines: 63, 64, 66, 67, 115 and 116.
# 
# Usage :
#       ./start.sh host servers_R_Nd servers_R_PR servers_total clients traffic qualified_names domain
#
# Parameters:
#	host: (int) number of host	
#       servers_R_Nd: (int) number of children server nodes per router node
#       servers_R_PR: (int) number of children server nodes per router process
#       servers_total: (int) number of total server nodes
#       clients: (int) number of client nodes
#       traffic: (int) number of traffic generator nodes
#       qualified_names: (y|n) qualified domain name or not
#       domain: domain
#
# Example:
#	./start.sh 1 2 1 2 4 10 y 'bo720-4-01.gla.ac.uk'
#	./start.sh 1 2 1 2 4 10 n vaquilla14a
#===============================================================================

function quit {
    exit
}

function node_generator {
    node_type=$1
    domain=$2
    qualified_names=$3
    total_routers=$4
    nodes_per_router=$5
    host=$6
    hidden=$7

    counter1=$(($total_routers*$(($host))))
    counter2=$nodes_per_router
    
    to_return=()
    i=0

    while [ $counter1 -gt $(($total_routers*$(($host-1)))) ]; do
	if [ $nodes_per_router -eq 0 ]; then
		to_return[i]="'${node_type}_${counter1}@${domain}'"
		l=${#to_return[i]}
		if [ "$qualified_names" = "y" ]; then
			name="-name"
		else
			name="-sname"
		fi
		if [ "$hidden" = "y" ]; then
		    #gnome-terminal --title="${to_return[i]:1:($l-2)}" -e "erl ${name} ${to_return[i]} -setcookie cookie -hidden" &
		    mate-terminal --title="${to_return[i]:1:($l-2)}" -e "erl ${name} ${to_return[i]} -setcookie cookie -hidden" &
		else               
		    #gnome-terminal --title="${to_return[i]:1:($l-2)}" -e "erl ${name} ${to_return[i]} -setcookie cookie" &
		    mate-terminal --title="${to_return[i]:1:($l-2)}" -e "erl ${name} ${to_return[i]} -setcookie cookie" &
		fi
		echo "${to_return[i]:1:($l-2)}"
	else
		while [ $counter2 -gt 0 ]; do
			to_return[i]="'${node_type}_${counter1}_${counter2}@${domain}'"
			echo "${to_return[i]:1:(-1)}"
			let counter2=counter2-1
			let i=i+1
		done
		counter2=$nodes_per_router
		let i=i-1
	fi
	let counter1=counter1-1
	let i=i+1
    done
}

function start {
    host=$1
    servers_per_router_node=$2
    servers_per_router_process=$3
    servers_total=$4
    clients=$5
    traffic=$6
    #qualified_names=$7
    domain=$8

    routers=$(($servers_total/$servers_per_router_node))
    echo "Routers = ${routers}"
    if [ $(($servers_total%$servers_per_router_node)) -gt 0 ]; then
		let routers=routers+1
    fi
    node_generator traffic $domain $7 $traffic 0 $host y
    node_generator client $domain $7 $clients 0 $host y
    node_generator router $domain $7 $routers 0 $host n
    node_generator server $domain $7 $servers_total 0 $host n #$servers_per_router_process
}

#fun="\"timer:sleep(1000), router:start(${2}, ${3}, ${4}, '${7}')"
start $1 $2 $3 $4 $5 $6 $7 $8
if [ "$6" = "y" ]; then
    name="-name"
else
    name="-sname"
fi

if [ $1 -eq 1 ]; then
    # gnome-terminal --title="Dashboard@$8" -e "erl ${name} dashboard@$8 -setcookie cookie" & # -eval $fun.\"" &
    mate-terminal --title="Dashboard@$8" -e "erl ${name} dashboard@$8 -setcookie cookie" & # -eval $fun.\"" &
fi
quit

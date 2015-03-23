#!/bin/bash 

# Author: Amir Ghaffari

# @RELEASE project (http://www.release-project.eu/)

pin=false; # Pin VMs to cores
vertex_degree=2
duplicating=2
Num_Ants=40
Iter_Global=20
Iter_Local=10
#Instances_FileName="../inputs/geiger/wt_1000_1.txt"
Instances_FileName="../inputs/geiger/wt_100_prime_1.txt"
#Instances_FileName="../inputs/orlib/wt40/wt40.001"
Duration=5;
BwlfCluster=true # Specifies whether use Heriot-Watt University's beowulf cluster or use Uppsala University's kalkyl cluster
Root_path=`pwd`;

# Specifies how many experiment is needed
for experiment in  3
do
	
	cd $Root_path;
	if $BwlfCluster ; then
		Max_node=4; # available node at Heriot-Watt's beowulf cluster  
	else
		Max_node=6; # available node at Uppsala's kalkyl cluster  
	fi

	for Number_of_Erlang_Nodes in 8
	do     
		cd $Root_path;
		let Number_of_VMs_per_Nodes=$Number_of_Erlang_Nodes/$Max_node;

		let temp=$Max_node*$Number_of_VMs_per_Nodes;

		if [ $Number_of_VMs_per_Nodes -eq 0 ]
		then
			Number_of_VMs_per_Nodes=1;
		else
			if [ $Number_of_Erlang_Nodes -ne $temp ] ; then
				let Number_of_VMs_per_Nodes=$Number_of_VMs_per_Nodes+1
			fi
		fi

		if [ $Number_of_Erlang_Nodes -le $Max_node ]
		then
			Total_nodes=$Number_of_Erlang_Nodes
		else
			Total_nodes=$Max_node
		fi

		if $BwlfCluster ; then
			Base_directory=bwlf_Nodes_${Total_nodes}_VMs_${Number_of_VMs_per_Nodes}_Exp_${experiment}
		else
			Base_directory=uppsala_Nodes_${Total_nodes}_VMs_${Number_of_VMs_per_Nodes}_Exp_${experiment}
		fi

		if [ -d "$Base_directory" ]; then
			rm -rf $Base_directory;
		fi 
		mkdir $Base_directory;

		cp -r *.erl *.hrl nodes.txt compile experiment.sh ${Base_directory};
		cd $Base_directory;

		if $BwlfCluster ; then
			chmod 755 experiment.sh
			./experiment.sh $BwlfCluster $experiment $Total_nodes $Number_of_VMs_per_Nodes $vertex_degree $duplicating $Num_Ants $Iter_Global $Iter_Local $Instances_FileName $Number_of_Erlang_Nodes $pin;
			cd ..
			rm -rf bwlf_Nodes_*
		else
			# calculate how long this benchmark will take
			let Total_number_of_Erlang_Nodes=$Total_nodes*$Number_of_VMs_per_Nodes;
			let Duration_sec=$Duration*60;

			let Total_benchmark_time_seconds=$Duration_sec+$Total_number_of_Erlang_Nodes;
			convertsecs() {
			 ((h=${1}/3600))
			 ((m=(${1}%3600)/60))
			 ((s=${1}%60))
			 printf "%02d:%02d:%02d\n" $h $m $s
			}

			Bench_time=$(convertsecs $Total_benchmark_time_seconds)
			########## end of time calculation
			let Total_cores=Total_nodes*16;
			String_format_sbatch="SBATCH -p node -N ${Total_nodes} -n ${Total_cores}";
			Experiment_name=experiment_${Total_nodes}_vms_${Number_of_VMs_per_Nodes}_expriment_${experiment};
			sed "s/SBATCH -p node -N 0 -n 0/$String_format_sbatch/g" experiment.sh>$Experiment_name;
			sed -i "s/00:00:00/$Bench_time/g" $Experiment_name;
			chmod 755 $Experiment_name;
			sbatch $Experiment_name $BwlfCluster $experiment $Total_nodes $Number_of_VMs_per_Nodes $vertex_degree $duplicating $Num_Ants $Iter_Global $Iter_Local $Instances_FileName $Number_of_Erlang_Nodes $pin;
		fi

	done
done

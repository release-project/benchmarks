#!/bin/bash -l
#SBATCH -A p2012172
#SBATCH -p node -N 0 -n 0
#SBATCH -t 00:00:00 

# Author: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>

# @RELEASE project (http://www.release-project.eu/)


BwlfCluster=$1;
experiment=$2; 
Total_Nodes=$3; 
Number_of_VMs=$4
vertex_degree=$5
duplicating=$6
Num_Ants=$7
Iter_Global=$8
Iter_Local=$9
Instances_FileName=${10}
Number_of_Erlang_Nodes=${11}
pin=${12}
Separate_Master_and_Starter=${13}
Run_chaos=${14}

Start_Node=1;

Base_directory=`pwd`;

Original_Config_File="${Base_directory}/nodes.txt";

Base_Result_directory="${Base_directory}/../results";

if [ ! -d "$Base_Result_directory" ]; then
	mkdir -p $Base_Result_directory;
fi

Erlang_path="/home/ag275/sderlang/bin";

if [ ! -d "$Base_directory" ]; then
	echo "Base Directory does not exist: $Base_directory"
	exit;
fi

cd $Base_directory;

if $BwlfCluster ; then
	let To_Node=$Start_Node+$Total_Nodes-1;
	Killing_nodes=$Total_Nodes;
	Node_Counter=0;
	for index in `seq $Start_Node $To_Node`; do 
			let Node_Counter=$Node_Counter+1;
			if [ "$index" -lt 10 ]
			then
				Hostnames[$Node_Counter]="bwlf0${index}.macs.hw.ac.uk";
				tempip=`ssh -q ${Hostnames[$Node_Counter]} "hostname -i;"`;
				IPaddresses[$Node_Counter]=$tempip;
			else
				Hostnames[$Node_Counter]="bwlf${index}.macs.hw.ac.uk";
				tempip=`ssh -q ${Hostnames[$Node_Counter]} "hostname -i;"`;
				IPaddresses[$Node_Counter]=$tempip;
			fi
	done
else
	for index in `seq 1 $Total_Nodes`; do 
		let zero_index=$index-1;
		Killing_nodes=0;
		tempip=`srun -r $zero_index  -N 1 -n 1 bash -c "hostname -i"`; 
		while [ -z "$tempip" ]; do
			sleep 1;
			tempip=`srun -r $zero_index  -N 1 -n 1 bash -c "hostname -i"`;
		done
		temphostname=`srun -r  $zero_index -N 1 -n 1 bash -c hostname`;
		while [ -z "$temphostname" ]; do
			sleep 1;
			temphostname=`srun -r  $zero_index -N 1 -n 1 bash -c hostname`;
		done
		IPaddresses[$index]=$tempip;
		Hostnames[$index]=$temphostname;

	done
fi

Number_of_Nodes=$Total_Nodes;
let Total_number_of_Erlang_Nodes=$Number_of_Nodes*$Number_of_VMs;

Config_file="${Base_Result_directory}/output_nodes_${Number_of_Nodes}_VMs_${Number_of_VMs}_Exp_${experiment}.txt";

for index in `seq 1 $Number_of_VMs`; do 
	VMs[$index]="node${index}@"
done

Output_file_name="${Base_Result_directory}/output_nodes_${Number_of_Nodes}_VMs_${Number_of_VMs}_Exp_${experiment}";
echo "Start at time :">$Output_file_name;

date +'%T'>>$Output_file_name;
echo "vertex_degree=$vertex_degree">>$Output_file_name;
echo "duplicating=$duplicating">>$Output_file_name;
echo "Num_Ants=$Num_Ants">>$Output_file_name;
echo "Iter_Global=$Iter_Global">>$Output_file_name;
echo "Iter_Local=$Iter_Local">>$Output_file_name;
echo "Instances_FileName=$Instances_FileName">>$Output_file_name;
echo "Number_of_Erlang_Nodes=$Number_of_Erlang_Nodes">>$Output_file_name;
echo "Pin=$pin">>$Output_file_name;
echo "Separate_Master_and_Starter=$Separate_Master_and_Starter">>$Output_file_name;
echo "Run_chaos=$Run_chaos">>$Output_file_name;

for index in `seq 1 $Number_of_Nodes`; do 
	echo "IP is ${IPaddresses[$index]} and name is ${Hostnames[$index]} for index $index">>$Output_file_name;
done
echo "========================================================">>$Output_file_name;

Qoute_mark="'";
Comma_mark=",";
String_format_addresses="";
break_the_loop=0
for index in `seq 1 $Number_of_Nodes`; do
	for counter in `seq 1 $Number_of_VMs`; do
		VMname=${VMs[$counter]}
		if [ $index -eq 1 -a $counter -eq 1 ]
		then
			String_format_addresses=${String_format_addresses}${Qoute_mark}${VMname}${Hostnames[$index]}${Qoute_mark}
		else
			String_format_addresses=${String_format_addresses}${Comma_mark}${Qoute_mark}${VMname}${Hostnames[$index]}${Qoute_mark}
		fi
		let total_counter=($index-1)*$Number_of_VMs+$counter;
		if [ "$total_counter" -ge "$Number_of_Erlang_Nodes" ]
		then
			break_the_loop=1
			break
		fi
	done
	if [ ${break_the_loop} -eq 1 ]
	then
		break
		break_the_loop=0
	fi
done

sed "s/Here_put_VMs_names/$String_format_addresses/g" $Original_Config_File>$Config_file;
echo "Name of VM nodes are: $String_format_addresses">>$Output_file_name;
echo "Number of Nodes in file: $total_counter">>$Output_file_name;
echo "Number_of_Nodes=$Number_of_Nodes      Number_of_VMs=$Number_of_VMs">>$Output_file_name;

for index in `seq 1 $Killing_nodes`; do 
	ssh -q ${IPaddresses[$index]} "
		echo '========================= killing (index=$index) ==================';
		pwd;
		hostname -i;
		hostname;
		date +'%T';
		echo 'befor kill=====';
		top -b -n 1 | grep beam.smp;
		pkill -u ag275 beam.smp;
		pkill -u ag275 beam;
		pkill -u ag275 epmd;
		echo 'after kill=====';
		top -b -n 1 | grep beam.smp;
		echo 'time:';
		date +'%T';
		echo '===========================================';
	";
done
echo "==========================================After killing VMs">>$Output_file_name;
date +'%T'>>$Output_file_name;

PATH=$Erlang_path:$PATH;
export PATH;
./compile

if $Run_chaos; then
	cd chaos;
	./rebar compile
	cd ..
fi

index=$Number_of_Nodes;
while [ $index -gt 0 ]; do
	ssh -q ${IPaddresses[$index]} "
	echo '===========================================';
	PATH=$Erlang_path:$PATH;
	export PATH;
	echo 'Running Erlang VM on hostname and path at time:';
	pwd;
	hostname -i;
	hostname;
	date +'%T';
	counter=$Number_of_VMs
	while [ \${counter} -gt 0 ]; do
		cd ${Base_directory}/ebin;
		echo '===============';
		VMname=\"node\${counter}@\";
		if [ ${index} -eq 1 -a \${counter} -eq 1 ]
		then
			if $Separate_Master_and_Starter; then
				if $pin; then
					(taskset -c $counter erl -detached -name \${VMname}${Hostnames[$index]})&
				else
					if $Run_chaos; then
						cd ..
						(erl -detached -name \${VMname}${Hostnames[$index]} -pa chaos/ebin -pa ebin)&
					else
						(erl -detached -name \${VMname}${Hostnames[$index]})&
					fi
				fi
			else
				cd ..
				if $pin; then
					taskset -c $counter erl -name \${VMname}${Hostnames[$index]} -noinput -s aco run $Instances_FileName $vertex_degree $duplicating $Num_Ants $Iter_Global $Iter_Local $Config_file -s init stop -pa ebin>>$Output_file_name
				else
					erl -name \${VMname}${Hostnames[$index]} -noinput -s aco run $Instances_FileName $vertex_degree $duplicating $Num_Ants $Iter_Global $Iter_Local $Config_file -s init stop -pa ebin>>$Output_file_name
				fi
			fi
		else
			if $pin; then
				(taskset -c $counter erl -detached -name \${VMname}${Hostnames[$index]})&
			else
				if $Run_chaos; then
					cd ..
					(erl -detached -name \${VMname}${Hostnames[$index]} -pa chaos/ebin -pa ebin)&
				else
					(erl -detached -name \${VMname}${Hostnames[$index]})&
				fi
			fi
		fi
		let counter=\${counter}-1
	done

	top -b -n 1 | grep beam.smp;

	echo '===========================================';
	"
	let index=$index-1
done

if $Separate_Master_and_Starter; then
	echo -n 'Starter is located on node: '>>$Output_file_name;
	hostname>>$Output_file_name;
	PATH=$Erlang_path:$PATH;
	export PATH;
	cd ${Base_directory}
	VMname="starter_node@";
	if $Run_chaos; then
		if $pin; then
			taskset -c $counter erl -name ${VMname}${Hostnames[1]} -noinput -s aco run chaos $Instances_FileName $vertex_degree $duplicating $Num_Ants $Iter_Global $Iter_Local $Config_file -s init stop -pa ebin -pa chaos/ebin>>$Output_file_name
		else
			erl -name ${VMname}${Hostnames[1]} -noinput -s aco run chaos $Instances_FileName $vertex_degree $duplicating $Num_Ants $Iter_Global $Iter_Local $Config_file -s init stop -pa ebin -pa chaos/ebin>>$Output_file_name
		fi
	else
		if $pin; then
			taskset -c $counter erl -name ${VMname}${Hostnames[1]} -noinput -s aco run $Instances_FileName $vertex_degree $duplicating $Num_Ants $Iter_Global $Iter_Local $Config_file -s init stop -pa ebin >>$Output_file_name
		else
			erl -name ${VMname}${Hostnames[1]} -noinput -s aco run $Instances_FileName $vertex_degree $duplicating $Num_Ants $Iter_Global $Iter_Local $Config_file -s init stop -pa ebin >>$Output_file_name
		fi
	fi
	# clean up the created VM
	pkill -u ag275 beam.smp;
	pkill -u ag275 beam;
	pkill -u ag275 epmd;
fi

for index in `seq 1 $Killing_nodes`; do 
	ssh -q ${IPaddresses[$index]} "
		echo '========================= (index=$index) ==================';
		hostname;
		date +'%T';
		pkill -u ag275 beam.smp;
		pkill -u ag275 beam;
		pkill -u ag275 epmd;
		echo '===========================================';
	"
done

echo 'Time after the benchmark'>>$Output_file_name;
date +'%T'>>$Output_file_name;
rm $Config_file

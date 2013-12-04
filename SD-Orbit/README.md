SD Erlang Orbit
-----------

Introduction
------------

SD Erlang Orbit uses s_group to reduce the number of connections on nodes. Please see `description.pdf` from `description` directory for more details.

How to build and run Orbit
----------------------------------------

There are two ways to run the benchmark. .

* Run on the local node (`nonode@nohost`):

		$ ./run

The config file for a local run is `bench.config`. After finishing the benchmark, the results are shown on screen. If you get `SD Erlang is not installed` exception, you need to install SD Erlang and `which erl` should point to it.

* To run the benchmark on a cluster, you need to specify the cluster information (such as number of nodes, nodes name, number of experiments,and path where Erlang has been installed) in both files: `run.sh` and `experiment.sh`

		$ ./run.sh

The config file for clusters is `template.config`. After finishing the benchmark, the results are saved in `results` directory.


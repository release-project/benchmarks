Multi-Level ACO (ML-ACO)
----------------------------
In Two-Level ACO (TL-ACO) with a large number of colony nodes the master process, which is responsible of collecting and processing the result form colonies, can become overloaded and a bottleneck for scalability. We develop a Multi-Level version of distributed ACO (ML-ACO), in which, in addition to the master node, there can be multiple levels of sub-master nodes to help the master through sharing the burden of collecting and processing the results from colony nodes. The number of sub-master nodes is adjustable based on the number colony nodes in a way that each sub-master node handles reasonable amount of loads. Then, run it as below:

		$ ./run.sh

To run the version on a cluster of Erlang nodes, please update the files `run.sh` and `experiment.sh` based on your cluster specification.

For more details please refer to `description\description.pdf`.

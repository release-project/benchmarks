Scalable Reliable ACO (SR-ACO)
----------------------------
* We employ SD Erlang to improve the scalability of Reliable ACO (R-ACO). We develop the Scalable Reliable ACO (SR-ACO) by reducing the cost of name registration and mesh connectivity.
To run the version on a cluster of Erlang nodes, please update the files `run.sh` and `experiment.sh` based on your cluster specification. You also need to install the Scalable Distributed Erlang (SD Erlang). Then, run it as below:

		$ ./run.sh

For more details please refer to `description\description.pdf`.

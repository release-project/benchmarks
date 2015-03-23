Ant Colony Optimisation (ACO)
----------------------------
Ant Colony Optimization (ACO) is a metaheuristic that has proved to be successful a variety of combinatorial optimization problems such as scheduling problems, routing problems, and allocation problems.

The Single Machine TotalWeighted Tardiness Problem (SMTWTP) is a well-known scheduling problem which aims to find out the starting times for a given set of jobs on a single processor to minimize the weighted tardiness of the jobs with respect to given due dates. Different heuristic methods have been developed to solve benchmark instances for the SMTWTP successfully. Ant colony optimization is one of the approach that has been applied for the single machine total tardiness problem.

There are four versions of distributed Erlang ACO available as follow:
* Tow-Level ACO (TL-ACO): a distributed version of ACO developed by Kenneth Mackenzie. For more info please refer to `TL-ACO\report\aco-erlang.pdf`.
* Multi-Level ACO (ML-ACO): In TL-ACO with a large number of colony nodes the master process, which is responsible of collecting and processing the result form colonies, can become overloaded and a bottleneck for scalability. As a solution to this problem, we propose a Multi-Level design for distributed ACO (ML-ACO), in which, in addition to the master node, there can be multiple levels of sub-master nodes to help the master through sharing the burden of collecting and processing the results from colony nodes. The number of sub-master nodes is adjustable based on the number colony nodes in a way that each sub-master node handles reasonable amount of loads. 
* GR-ACO is a reliable version of distributed ACO with reliability implemented using Erlang's `global` module.  The reliability of GR-ACO has been evaluated by employing Chaos Monkey.
* We observed that reliability limits the scalability of GR-ACO because of the use of global name registration. We improve the scalability of GR-ACO and develop a Scalable Reliable ACO (SR-ACO) by employing techniques that SD Erlang offers for reducing the cost of name registration and mesh connectivity.

For more details please refer to `description\description.pdf`.

Find more about [`Chaos Monkey`](https://github.com/dLuna/chaos_monkey)

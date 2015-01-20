Orbit
==========

Orbit is a symbolic computing kernel and a generalization of a transitive closure computation. Orbit aims to calculate subset Q1 from set Q that is a set of natural numbers from 0 to N < infinity. The program has one supervisor process, a distributed hash table, and initially one working process. The hash table is initially empty and is distributed between all participating nodes. The supervisor process is responsible for spawning the rst worker process and for the program termination after all possible numbers are explored.

* D-Orbit is a distributed Erlang Orbit implementation.

* SD-Orbit is an SD Erlang Orbit implementation that uses s_groups to reduce the number of connections between the nodes.

* Orbit-Description.pdf provides an overview and a descruiption of the two implementations.
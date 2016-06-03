Instant Messenger Benchmark
-----------

Introduction
------------

These files contain the source to build the Reliable Distributed Erlang (D-Erlang) Instant Messenger benchmark.

To build the files just compile them:
  elrc *.erl

Execution of the system
-----------------------
To launch the application, just execute the bash script start.sh:

./start.sh Server\_Nodes\_per\_Router\_Node Server\_Supervisors\_per\_Router\_Process Total\_Server\_Nodes Total\_Client\_Nodes Qualified\_Names Domain

E.g.: 

      ./start.sh 1 2 1 2 4 n host
      ./start.sh 1 2 1 2 4 y 'domain'

The parameters are as follows:
  - Number\_of\_the\_host
  - Server\_Nodes\_per\_Router\_Node (int): maximum number of server nodes that are children of a router node.
  - Server\_Supervisors\_per\_Router\_Process (int): maximum number of server supervisor processes supervised by one router process.
  - Total\_Server\_Nodes (int): total number of server nodes in the architecture.
  - Total\_Client\_Nodes (int): total number of client nodes.
  - Qualified\_Names (char): type 'y' to use fully qualified domain names or 'n' to use short names.
  - Domain: name of the host or domain of the network.
   
Thus, the first of the previous lines would launch one router node router\_1@host with two router processes, and two server nodes server\_1@host, and server\_2@host; in turn, the second would launch the same configuration but now nodes are: router\_1@domain, server\_1@domain, and server\_2@domain. In both cases, the script creates four nodes to lauch clients.

Notice that start.sh will launch the system using detached virtual machines (i.e., /$> erl -detached).

Alternatively,

  ./start\_gui.sh Server\_Nodes\_per\_Router\_Node Server\_Supervisors\_per\_Router\_Process Total\_Server\_Nodes Qualified\_Names Domain
  
  will launch one new linux terminal per each node.
  
  The start_gui.sh script promts the user what GUI is the host OS running and, and then launches the system. Apart from the terminals that host the specified nodes, one more node is opened: dashboard@domain. This node allows the user to launch the system and also serves as a log for the operations that are taking place.
  
  GUIs supported by the script include: Gnome, Mate, KDE and XCFE. It also offers the ability to launch the system detached, just by calling the ./start.sh script.

Use of the system
-----------------
  - Once the environment is launched, to start the application type launcher:start(num\_of\_router\_nodes, num\_of\_server\nodes\_per\_router\_process, total\_num\_of\_servers, [host\_n, host\_n-1, ..., host\_1], num\_total\_of_hosts). E.g.: given the environment started in the previous example, the system is started just typing
      (dashboard@host)1> launcher:start(2,1,2,4,[host],1).
  - To log a client in to the system type client:login(name\_of\_client) in the client shell. The client will be executed in that shell.
  - To log a client out from the system type client:logout(name\_of\_client) in the shell where the client is executing.
  - To send a message type client:message(Sender\_Name, Receiver\_Name, message). This needs to be executed in the shell where the sender client is running.
  - To run a throughput benchmark, the user must follow this sequence:
    - logger:launch(Technology, Routers, Servers, Clients, Num\_Client\_Nodes, Trials, Timer, Threshold, Domain).
    - toxic\_client:launch(Num\_Clients, Num\_Client\_Nodes, Domain).
    - toxic\_client:launch\_traffic(Num\_Clients, Num\_Client\_Nodes, Domain).

  - To run a latency benchmark, the user must follow this sequence:
    - logger:start\_latency(Technology, Condition, Num\_of\_trials).
    - toxic\_client:launch(Num\_Clients, Num\_Client\_Nodes, Domain).
    - toxic\_client:launch\_traffic(Num\_Clients, Num\_Client\_Nodes, Domain).
 
See @doc comments within the code for further details.

Limitations
-----------
  - This still work in progress.
  - It is reliable, yet not fully tested.
  - Benchmarks are still not fully automated.
  - It is pure erlang messages passing (does not use sockets).

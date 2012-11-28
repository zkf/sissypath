# Method #

## Test scenarios ##

### Pre-defined graph ###

(figure) The pre-defined graph

The simple pre-defined graph, shown in the figure, has seven nodes.
Node seven is defined as the exit, while nodes 4--6 are thought of as a hallway connecting the rooms represented by nodes 1--3.

When this graph is used in simulations, it is prepared as follows.
Every node is assigned a hazard with a probability of 0.5, and a random start position is uniformly picked among the nodes of the graph.

## Random graph ###

This graph is automatically generated, given the number of nodes and the number of edges in the graph.


## Multi-armed bandit approach to path finding ##

We place a multi-armed bandit at every node, the arms representing the edges emanating from the node.


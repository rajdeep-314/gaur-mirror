module DepGraphV = String

module DepGraph = Graph.Persistent.Digraph.Concrete(DepGraphV)

module DepGraphJohnson = Graph.Cycles.Johnson(DepGraph)

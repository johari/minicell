import networkx as nx

from networkx.algorithms import isomorphism

print "start"
G1 = nx.path_graph(4)
G2 = nx.path_graph(2)
GM = isomorphism.GraphMatcher(G1,G2)
print "end"

print GM.is_isomorphic()


for iso in GM.subgraph_isomorphisms_iter():
	print iso

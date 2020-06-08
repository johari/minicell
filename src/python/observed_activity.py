import random
import networkx as nx

G = nx.Graph()
# G.add_edge(0,1,weight=2)
# G.add_edge(1,0)
# G.add_edge(2,2,weight=3)
# G.add_edge(2,2)

# S = nx.to_scipy_sparse_matrix(G, nodelist=[0,1,2])
# print(find(S))


# 1000 people
# 700 houses
# 10 different items (fertilizer, gasoline, honda, truck, ..)
# 15 factories
# phone calls

print("Creating people")
id = 0
for i in range(5):
    id += 1
    label = "Person %d" % id
    if i == 0:
        label = "Bill"
    elif i == 1:
        label = "Ted"
    G.add_node(id, PersonNode=True, label=label)

people = list(nx.get_node_attributes(G, 'PersonNode'))
print(people )

print("Creating houses")
for i in range(3):
    id += 1
    label = "House %d" % id
    if i == 0:
        label = "123 Main St"
    G.add_node(id, HouseNode=True, label=label)

houses = list(nx.get_node_attributes(G, 'HouseNode'))
print(houses)

print("Creating items")
for item in ["truck", "fertilizer", "car"]:
    id += 1
    G.add_node(id, ItemNode=True, label=item)

items = list(nx.get_node_attributes(G, 'ItemNode'))
print(items)

print("Creating factories")
for i in range(5):
    id += 1
    label = "Factory %d" % id
    if i == 0:
        label = "Acme, Inc."
    G.add_node(id, FactoryNode=True, label=label)

factories = list(nx.get_node_attributes(G, 'FactoryNode'))
print(factories)

print("Establishing phone calls between people")
for person1 in people:
    for person2 in people:
        if random.random() > 0.9:
            G.add_edge(person1, person2, PhoneEdge=True)

G.add_edge(people[0], factories[0], ObserveEdge=True)
G.add_edge(people[1], factories[0], ObserveEdge=True)

G.add_edge(people[0], houses[0], ResideEdge=True)
G.add_edge(people[1], houses[0], ResideEdge=True)

G.add_edge(people[0], items[0], RentEdge=True)
G.add_edge(people[1], items[1], BuyEdge=True)

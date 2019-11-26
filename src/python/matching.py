import networkx as nx

from pprint import pprint

import json

from networkx.algorithms import isomorphism

from flask import Flask, request
import flask

app = Flask(__name__, static_url_path="")

def my_node_match(x, y):
    if x["label"][0] == "*" or y["label"][0] == "*":
        return True
    else:
        return x == y


def my_edge_match(x, y):
    if x["weight"] == 0 or y["weight"] == 0:
        return True
    else:
        return x == y

def parsePayload(payload):
    nodesQ = eval(payload[0])
    nodesQ = [(u, {"label": v}) for (u, v) in nodesQ]

    edgesQ = eval(payload[1])
    edgesQ = [(u, v, {"weight": w}) for (u, v, w) in edgesQ]

    nodesB = eval(payload[2])
    nodesB = [(u, {"label": v}) for (u, v) in nodesB]

    edgesB = eval(payload[3])
    edgesB = [(u, v, {"weight": w}) for (u, v, w) in edgesB]

    return nodesQ, edgesQ, nodesB, edgesB

@app.route('/sm', methods=["POST"])
def sm():
    payload = request.get_json()

    nodesQ, edgesQ, nodesB, edgesB = parsePayload(payload)

    GQ = nx.Graph()

    GQ.add_nodes_from(nodesQ)
    GQ.add_edges_from(edgesQ)

    GB = nx.Graph()
    GB.add_nodes_from(nodesB)
    GB.add_edges_from(edgesB)


    GM = isomorphism.GraphMatcher(GB,GQ, node_match=my_node_match, edge_match=my_edge_match)

    res = []
    for iso in GM.subgraph_isomorphisms_iter():
        res += [iso]

    return flask.jsonify([r.keys() for r in res])

@app.route('/sm-di', methods=["POST"])
def smDi():
    payload = request.get_json()

    nodesQ, edgesQ, nodesB, edgesB = parsePayload(payload)

    GQ = nx.DiGraph()

    GQ.add_nodes_from(nodesQ)
    GQ.add_edges_from(edgesQ)

    GB = nx.DiGraph()
    GB.add_nodes_from(nodesB)
    GB.add_edges_from(edgesB)


    GM = isomorphism.DiGraphMatcher(GB,GQ, node_match=my_node_match, edge_match=my_edge_match)

    res = []
    for iso in GM.subgraph_isomorphisms_iter():
        res += [iso]

    return flask.jsonify([r.keys() for r in res])

if __name__ == '__main__':
    app.run(port=9999)

import networkx as nx

from pprint import pprint

import json

from networkx.algorithms import isomorphism

from flask import Flask, request
import flask

import observed_activity
from string import digits

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

def typed_node_match(x, y):
    if y["label"][0] == "*":
        r = str.maketrans('', '', digits)
        ytype = y["label"][1:].capitalize().translate(r)
        # print(x, y, ytype)
        return ("%sNode" % ytype) in x
    else:
        print(x["label"], y["label"])
        return x["label"] == y["label"]

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

    print(nx.to_scipy_sparse_matrix(GQ))

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

@app.route('/sm-live-demo', methods=["POST"])
def smLiveDemo():
    payload = request.get_json()

    payload = [eval(u) for u in payload]
    queryGraph = nx.Graph()
    queryGraph.add_nodes_from([(u, {"label": v}) for (u, v) in payload[0]])
    queryGraph.add_edges_from([(u, v) for (u, v, w) in payload[1]])

    baseGraph = observed_activity.G

    GM = isomorphism.GraphMatcher(baseGraph, queryGraph, node_match=typed_node_match)
    res = []

    headers = set()
    for iso in GM.subgraph_monomorphisms_iter():
        # base graph -> query graph
        # we need to find the subgraph of basegraph corresponding to the query graph
        # and extract all the metadata information from it
        #
        # there are two types of wildcard:
        #   wildcard for nodes
        #       retrieve "data" from the node
        #   wildcard for edges
        ans = {}
        for k, v in iso.items():
            qdata = queryGraph.nodes[v]["label"]
            if qdata[0] == "*":
                ans[qdata] = baseGraph.nodes[k]["label"]
                headers.add(qdata)
        res.append(ans)

    headers = list(headers)
    dataFrame = [headers]
    for result in res:
        row = []
        for elem in headers:
            row.append(result[elem])
        dataFrame.append(row)

    # nodesQ, edgesQ, graphPtr = parsePayload(payload)

    # print nodesQ
    # print edgesQ
    # print graphPtr

    print(dataFrame)
    return flask.jsonify(dataFrame)

if __name__ == '__main__':
    app.run(port=5000)

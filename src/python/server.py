### sample python interface - pagerank

from scipy.sparse import *
from scipy import *

from ctypes import *
import json

import os

from flask import Flask, request
import flask

app = Flask(__name__, static_url_path="")
root_dir = "/home/nima/johari/excellent/"

"""
        ["assets", "spreadsheet.js"] -> do

            res $ responseFile status200 [(hContentType, "text/javascript")] ("../build/spreadsheet.js") Nothing

        ("assets":pathToAsset) -> do
            let fullPath = intercalate "/" (T.unpack <$> pathToAsset)
            let myMime = (defaultMimeLookup $ fromString fullPath)
            print myMime
            res $ responseFile status200 [(hContentType, myMime)] ("../static/" <> fullPath) Nothing
"""

worksheet = [{"value": "a", "cometKey": "A1", "valueType": "ESLit"}]

@app.route('/minicell/all.json')
def endpoint_show_all():
    return flask.jsonify(worksheet)

@app.route('/assets/spreadsheet.js')
def minicell_elm_frontend():
    return flask.send_from_directory(os.path.join(root_dir, 'build'), "spreadsheet.js")

@app.route('/minicell/<cometKey>/write.json', methods=["GET", "POST"])
def write(cometKey):
    global worksheet
    print(request)
    newVal = {"value": request.form["user"], "cometKey": cometKey, "valueType": "ESLit"}
    if [val for val in worksheet if val["cometKey"] == cometKey]:
        worksheet = [newVal if val["cometKey"] == cometKey else val for val in worksheet]
    else:
        worksheet += [newVal]
    return "hello world!"

# [ ] A background graph (loaded from the memory)
# [ ] A query graph (wrangled from the sheet)
    # [ ] A list of results

@app.route('/assets/<path:path>')
def serve_js(path):
    print(path)
    return flask.send_from_directory(os.path.join(root_dir, 'static'), path)

@app.route('/')
def hello_world():
    return flask.send_from_directory(os.path.join(root_dir, 'static'), "main.html")

if __name__ == '__main__':
    app.run()

exit()
### load gunrock shared library - libgunrock
gunrock = cdll.LoadLibrary('/home/nima/code/gunrock/build/lib/libgunrock.so')

"""
double pagerank(
    const int  num_nodes,     // Input graph number of nodes
    const int  num_edges,     // Input graph number of edges
    const int* row_offsets,   // Input graph row_offsets
    const int* col_indices,   // Input graph col_indices
    bool       normalize,   // normalized pagerank flag
    int*       node_ids,
    float*     ranks);
"""

### read in input CSR arrays from files
row_list = [int(x.strip()) for x in open('toy_graph/row.txt')]
col_list = [int(x.strip()) for x in open('toy_graph/col.txt')]
print 'set pointers'
### convert CSR graph inputs for gunrock input
row = pointer((c_int * len(row_list))(*row_list))
col = pointer((c_int * len(col_list))(*col_list))
nodes = len(row_list) - 1
edges = len(col_list)

### output array
node = pointer((c_int * nodes)())
rank = pointer((c_float * nodes)())

normalize = 1
print 'run gunrock'
### call gunrock function on device
elapsed = gunrock.pagerank(nodes, edges, row, col, normalize, node, rank)


# # # # # # # # # # #
# subgraph matching #
# # # # # # # # # # #

qrow_list = row_list[:]
qcol_list = col_list[:]

print 'set pointers'
### convert CSR graph inputs for gunrock input
# row = pointer((c_int * len(row_list))(*row_list))
# col = pointer((c_int * len(col_list))(*col_list))

qrow = pointer((c_int * len(qrow_list))(*qrow_list))
qcol = pointer((c_int * len(qcol_list))(*qcol_list))

nodes = len(row_list) - 1
edges = len(col_list)

qnodes = len(qrow_list) - 1
qedges = len(qcol_list)

"""
double sm(
    const int            num_nodes,
    const int            num_edges,
    const int           *row_offsets,
    const int           *col_indices,
    const int            num_query_nodes,
    const int            num_query_edges,
    const int           *query_row_offsets,
    const int           *query_col_indices,
    const int            num_runs,
          int            *subgraphs);
"""

print "run sm"
elapsed = gunrock.sm(nodes, edges, row, col, qnodes, qedges, qrow, qcol, 1, node)


### sample results
print 'elapsed: ' + str(elapsed)
print node

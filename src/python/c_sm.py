import ctypes
import networkx as nx
import struct

lib = ctypes.cdll.LoadLibrary('/Users/nima/johari/gunrock-sm/libgunrock.so')

def match(base_graph, query_graph):

    sp_base = nx.to_scipy_sparse_matrix(base_graph)
    sp_query = nx.to_scipy_sparse_matrix(query_graph)

    row_list = sp_base.indptr
    col_list = sp_base.indices

    nodes = len(row_list) - 1
    edges = len(col_list)

    qrow_list = sp_query.indptr
    qcol_list = sp_query.indices

    qnodes = len(qrow_list) - 1
    qedges = len(qcol_list)

    row = ctypes.pointer((ctypes.c_int * len(row_list))(*row_list))
    col = ctypes.pointer((ctypes.c_int * len(col_list))(*col_list))

    qrow = ctypes.pointer((ctypes.c_int * len(qrow_list))(*qrow_list))
    qcol = ctypes.pointer((ctypes.c_int * len(qcol_list))(*qcol_list))


    ################

    def NodeConditionT(node, cond, operand):
        # This is how NodeConditionT is represented as a struct in C

        # struct NodeConditionT {
        #   VertexId node;  // VertexId in the query graph
        #   StrConditionT cond;
        #   const char *operand; //In fact, it must be `char operand[32];` otherwise you get SEGFAULT
        # };

        # First element of the struct is a 32-bit integer
        # Second element of the struct is an enum
        # Third element is an array of char with maximum 32 elements.
        return struct.pack("i i 32s", node, cond, operand)

    def node_eql_to_davis(nodeId):
        # return struct.pack(nodeId, 1, "davis")
        return NodeConditionT(nodeId, 1, "davis")

    def node_neq_to_berkeley(nodeId):
        # return struct.pack(nodeId, 2, "berkeley")
        return NodeConditionT(nodeId, 2, "berkeley")

    # struct EdgeLabelT {
    #   VertexId src;
    #   VertexId dest;
    #   int label;
    # };

    def edgeLabel(src, dest, label):
        return struct.pack("iii", src, dest, label)

    # struct EdgeConditionT {
    #   VertexId src;
    #   VertexId dest;
    #   IntConditionT cond;
    #   int operand;
    # };

    def edge_gt_n(src, dest, n):
        return struct.pack("iiii", src, dest, 3, n) # edge between src and dst > n. enum for ">" is 3.

    # Attach node labels in the base graph
    # The order of the array corresponds to `node_id`s in Gunrock, ordered from 0..(n-1)
    base_nodeLabels = ["davis", "berkeley"]
    c_base_nodeLabels = (ctypes.c_wchar_p * len(base_nodeLabels))()
    c_base_nodeLabels[:] = base_nodeLabels

    # Declare conditions on the edges of the base graph

    # for the query graph, set the following conditions
    #     * on node with id 42,
    #     * on node with id 45
    query_node_conditions = [] # [node_eql_to_davis(42), node_neq_to_berkeley(45)]

    # pack an array of structs into one binary chunk
    c_query_node_conditions = "".join(query_node_conditions)

    # Labels on edges in the base graph:

    # Base graph may have integer as node label
    base_edgeLabels = [edgeLabel(0, 1, 30), edgeLabel(3, 4, 45)]
    # pack an array of structs into one binary chunk
    c_base_edgeLabels = "" # "".join(base_edgeLabels)

    query_edgeConditions = [edge_gt_n(0, 1, 42)]
    # pack an array of structs into one binary chunk
    c_query_edgeConditions = "" # "".join(query_edgeConditions)

    #################





    subgraphs_count    = ctypes.c_int()
    subgraphs_mappings = ctypes.POINTER(ctypes.c_int)()

    # elapsed = lib.sm_cpp(nodes, edges, row, col, qnodes, qedges, qrow, qcol, 1, c_base_nodeLabels,
    #                     c_query_node_conditions, c_base_edgeLabels, c_query_edgeConditions, node)

    elapsed = lib.sm_cpp(nodes, edges, row, col, qnodes, qedges, qrow, qcol, 1, c_base_nodeLabels,
                        c_query_node_conditions, c_base_edgeLabels, c_query_edgeConditions,
                        ctypes.byref(subgraphs_count), ctypes.byref(subgraphs_mappings))

    res = []

    print("********")
    print(subgraphs_count.value)
    print("********")

    for res_number in range(subgraphs_count.value):
        iso = {}
        start = res_number*qnodes*2
        print("start", res_number, start)
        # print(res_number, qnodes, start)
        for i in range(start, start+qnodes*2, 2):
            print(i, "****")
            iso[subgraphs_mappings[i]] = subgraphs_mappings[i+1]
        print(iso)
        res.append(iso)

    return res
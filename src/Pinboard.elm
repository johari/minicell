module Pinboard exposing (Bookmark, decoder, dressUp, getBookmarks, iWantToWearShoes, paperOutline, pinboardGraph, viewBookmarkTable, viewSingleBookmark)

import Dict
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Graph.DOT as DOT exposing (..)
import Html exposing (Html, b, br, button, code, div, h1, hr, input, li, ol, strong, text, ul)
import Html.Attributes exposing (href)
import Http
import Json.Decode as D
import List exposing (concat, map)
import List.Extra exposing (unique, zip)
import Maybe exposing (withDefault)
import String exposing (join, split)
import Stylize exposing (..)
import Tuple exposing (first, second)



-- node labels are strings, edge labels are empty
-- dressUp : Graph String ()


type alias Bookmark =
    { href : String
    , description : String
    , tags : List String
    , time : String
    }


decoder : D.Decoder Bookmark
decoder =
    D.map4 Bookmark
        (D.field "href" D.string)
        (D.field "description" D.string)
        (D.field "tags" (D.list D.string))
        (D.field "time" D.string)


getBookmarks : String -> Cmd (Result Http.Error (List Bookmark))
getBookmarks query =
    Http.send (\x -> x) (Http.get ("http://localhost:4567/api/" ++ query) (D.list decoder))


pinboardGraph : List Bookmark -> Graph String ()
pinboardGraph bookmarks =
    let
        hostnamesWithTags =
            map (\x -> ( x.href, x.tags )) bookmarks

        allTags =
            map second hostnamesWithTags |> concat |> unique

        tagDict =
            Dict.fromList (zip allTags (List.range 0 100))

        allHostnames =
            map first hostnamesWithTags |> map extractHostName |> unique

        hnDict =
            Dict.fromList (zip allHostnames (List.range 200 300))

        edges =
            map
                (\x ->
                    let
                        hnKey =
                            Dict.get (extractHostName x.href) hnDict |> withDefault 0

                        tagKeys =
                            map (\tag -> Dict.get tag tagDict) x.tags |> map (withDefault 0)
                    in
                    zip (List.repeat (List.length tagKeys) hnKey) tagKeys
                )
                bookmarks

        allNodes =
            map (\( y, x ) -> Node x y) (Dict.toList tagDict ++ Dict.toList hnDict)
    in
    Graph.fromNodesAndEdges allNodes (concat edges |> map (\( x, y ) -> Edge y x ()))


viewBookmarkTable urls description_query =
    Html.table [] (List.map (viewSingleBookmark description_query) urls)


viewSingleBookmark description_query b =
    Html.tr []
        [ text ""

        -- , Html.td [] [ text b.href ]
        , Html.td []
            [ Html.a [ href b.href ]
                [ stylizeHostname b.href
                ]
            ]
        , Html.td [] [ vgrep b.description description_query "label_2" ]
        , Html.td []
            [ Html.b [] [ vgrep (join " " b.tags) "tasks" "label_1" ]
            ]

        -- , Html.td [] [ text (extractHostName b.href) ]
        , Html.td [] [ vgrep b.time "2018-07-" "label_time" ]
        ]


dressUp =
    let
        nodes =
            [ Node 0 "Socks"
            , Node 1 "XYZ"
            , Node 2 "Pants"
            , Node 3 "Shoes"
            , Node 4 "XYZ"
            , Node 5 "Shirt"
            , Node 6 "Belt"
            , Node 7 "Tie"
            , Node 8 "XYZ"
            , Node 9 "iohk.io"
            , Node 10 "iohk"
            , Node 11 "jobs"
            ]

        e from to =
            Edge from to ()

        edges =
            [ e 0 3 -- socks before shoes
            , e 1 2 -- undershorts before pants
            , e 1 3 -- undershorts before shoes
            , e 2 3 -- pants before shoes
            , e 2 6 -- pants before belt
            , e 5 6 -- shirt before belt
            , e 5 7 -- shirt before tie
            , e 6 8 -- belt before jacket
            , e 7 8 -- tie before jacket
            , e 9 10
            , e 9 11
            ]
    in
    Graph.fromNodesAndEdges nodes edges



-- iWantToWearShoes : List String


iWantToWearShoes model =
    Graph.guidedDfs
        Graph.alongIncomingEdges
        -- which edges to follow
        (Graph.onDiscovery
            (\ctx list ->
                -- append node labels on discovery
                ctx.node.label :: list
            )
        )
        [ 3

        {- "Shoes" NodeId -}
        ]
        -- start with the node labelled "Shoes"
        []
        -- accumulate starting with the empty list
        dressUp
        -- traverse our dressUp graph from above
        |> Tuple.first



-- ignores the untraversed rest of the graph
-- iWantToWearShoes == ["Pants", "Undershorts", "Socks", "Shoes"]
-- CAUSE OF DEATH                 .
-- toGiphyUrl : String -> String
-- toGiphyUrl topic =
--     Url.crossOrigin "https://api.giphy.com"
--         [ "v1", "gifs", "random" ]
--         [ Url.string "api_key" "dc6zaTOxFJmzC"
--         , Url.string "tag" topic
--         ]


paperOutline =
    """
digraph G {

  {
    abstract
    introduction
    motivating_examples
    formulation
  }
  paper -> {outline};
  key_idea -> many_underlying_graphs -> no_interaction_model_to_pick_one -> example_1;

outline -> {abstract, introduction, motivating_examples, formulation, discussion, related_work, future_work};
introduction -> background -> {spreadsheets, graphs};
introduction -> running_example;
introduction -> key_idea;
introduction -> main_contributions;

main_contributions -> {graph_processing_capabilities_to_widely_used_interfaces, novel_end_user_friendly_interaction_model_for_important_class_of_programmatic_tasks_without_code};

formulation -> main_components_as_z_said;

motivating_examples -> {example_1, example_2, something_with_source_and_sink};
example_1 -> tagcloud;
example_2 -> something_with_numbers;
tagcloud -> {visualization, many_underlying_graphs};

something_with_numbers -> operations_research -> distribution_and_inventory;

main_components_as_z_said -> {first_part, second_part, third_part};
first_part -> graph_construction -> {thin_air, existing};
thin_air -> wrangling -> extract_and_impose_model;
extract_and_impose_model -> {vertices, edges, style_attributes};
style_attributes -> {label_of_nodes, size_of_nodes, color_of_nodes, position_of_nodes_x_y};
thin_air -> "example_with_numbers (needed)";
existing -> algebraic_graphs -> {combine_two, filter_nodes, filter_edges}
second_part -> graph_visualization;
third_part -> graph_computation -> interaction_model -> {easy, difficult};
easy -> "=SHORTEST_PATH(G1, davis, berkeley)"
difficult -> graph_program_by_visual_demonstration -> graph_visualization;

graph_computation -> algorithms -> optimization -> {max_flow, shortest_path};
algorithms -> traversal;

  graphs -> {"(powerful) modeling tools"} -> "model real life problems" -> {social_networks, shortest_paths_on_road_networks, max_flow_and_other_optimization};

  //graphs -> representation -> {linked_list, "matrices (or tables)"};

  spreadsheets -> most_powerful_environment_for_semistructured_data -> tables;

  //"semistructured_data" -> {xml, tables, wiki_pages, markdown};
  //wiki_pages -> {tables, metadata, infobox};

  future_work -> bidirectional_graph_environment -> {bidirectional, environment};
  environment -> {visualization, editing, processing};
  processing -> algorithms;
  editing -> bidirectional;

  algorithms -> {shortest_path, max_flow, pagerank};

  // contributions -> spreadsheet_contributions -> {pagerank_in_spreadsheets, graph_algs_in_spreadsheets};

  discussion -> {use_cases, limitations, evaluation};
  evaluation -> user_study;

  related_work -> program_synthesis -> sumit -> {flash_relate, flash_fill, flash_extract};
  program_synthesis -> ravi -> sketch_and_sketch -> {sketch_and_sketch_2016, sketch_and_sketch_2018};

  related_work -> graphs_in_spreadsheet -> NodeXL;

  sketch_and_sketch -> bidirectional;
  sketch_and_sketch_2016 -> svg;
  sketch_and_sketch_2018 -> tables;

  flash_fill -> string_transformation;
  flash_relate -> {data_modeling_tabular, data_wrangling};
  data_modeling_tabular -> tables;

  flash_extract -> data_wrangling;

  data_wrangling -> semistructured_data;

  flash_extract -> input_output_demonstration -> {highlighting, table_of_input_output};
  table_of_input_output -> {spreadsheets, tables};

  flash_fill -> spreadsheets;
}
"""

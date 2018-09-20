module Examples.Outline exposing (paperOutline)


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

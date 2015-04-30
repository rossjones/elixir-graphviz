defmodule GraphViz do

  @author "Oren Ben-Kiki <oren@ben-kiki.com>"
  @moduledoc """
  Generate GraphViz diagrams from Elixir code.

  ## Restrictions

  This module has the following restrictions:

  * Everything has to be properly declared: GraphViz provides a lot of
    mechanisms for setting up and using default values, but this module doesn't
    provide access to any of them. This is hopefully less of an issue for a
    code-generated diagram.

  * Little control over order: `dot `is notoriously preverse when it comes to
    ordering nodes or clusters of the same rank. One way to (try to) force this
    order is to tightly control the order they appear in the file, but this
    module isn't very friendly in this regard. It basically emits elements in
    the reverse of the order in which they were added. Arguably `dot` should
    finally provide a better way to control this issue, but there seems little
    chance of that ever hapenning.

  * No validation: GraphViz provides a large but finite set of attributes and
    values, but this module just allows you to emit anything you want without
    any regard to whether GraphViz will accept them or not.

  * No formatting: Attributes are expected to be printable (that is, allowed
    inside `\#{...}`. There is no automatic `"` added around attribute values
    so it is the caller's responsibility to do any form of quoting needed
    (e.g., if a label contains spaces, the caller needs to use `inspect("Foo
    Bar")` as the label instead of simply `"Foo Bar"`. Likewise, if a style
    should be `"round,filled"` then the caller needs to use
    `inspect("round,filled")` instead of, say, a nice `[ :round, :filled ]`.
    The only upside to this is that setting a label to a simple `<...html...>`
    "just works". That, and the fact that all attributes are printed exactly
    the same without any complex special confusing rules. Again, this is
    hopefully less of an issue for a code-generated diagram.

  * Addition only: You can add elements to the graph but you can't take them
    out. Mercifully it is at least possible to update elements after they have
    been added.

  * The node and edge records are defined in a way that caters to directed
    graphs. Edges therefore have `source` and `target` nodes, and nodes have
    separate `incoming` and `outgoing` edge lists.

  * The output isn't formatted in the prettiest or the most compact possible
    way. On the bright side it is very regular, so you can get away with a
    simplified parser if you want to post-process it.

  Otherwise this is a fairly complete way to generate GraphViz diagrams.

  ## How to use this

  First, create a new graph:

      graph = %GraphViz.Graph{ name: "name", is_strict: true, is_directed: true, attributes: [ label: "A label", rankdir: :LR ] }

  Then, add stuff into the graph:

      graph = graph
           |> GraphViz.add(%GraphViz.SubGraph{ id: id_of_cluster, is_cluster: true, attributes: [ label: "Cluster" ] })
           |> GraphViz.add(%GraphViz.Node{ id: id_of_source, parent: id_of_cluster, attributes: [ label: "Source" ] })
           |> GraphViz.add(%GraphViz.Node{ id: id_of_target, attributes: [ label: "Target" ] })
           |> GraphViz.add(%GraphViz.Edge{ id: id_of_edge, source: id_of_source, target: id_of_target, attributes: [ label: "Edge" ] })

  You will need to use some sort of a unique identifier for each element
  (including edges!). Using references (`make_ref`) is OK, as is using anything
  else which is unique.

  You can lookup existing elements by their identifier:

      element = GraphViz.lookup(graph, identifier)

  You can give any attribute you want, with any value you want. GraphViz will
  silently ignore unknown attributes. As long as you keep the identifiers
  unmolested, you can update an element after creating it:

      graph = GraphViz.update(graph, new_version_of_some_element)

  Finally, you can print the graph:

      GraphViz.print(:stdio, graph)

  """

  # A unique identifier for the graph elements. The simplest choice is to use
  # references, but this has the ugly side-effect that the output becomes
  # unpredictable. Tests use manually specified atoms, which is obviously not
  # scalable. Whatever the choice is, this module doesn't care what is used as
  # long as identifiers are unique.
  @type id :: any

  defmodule Graph do
    @moduledoc """
    A graph contains the top-level elements (nodes or sub-graphs) and edges
    connecting the nodes.
    """
    defstruct name: nil, is_directed: nil, is_strict: nil, elements: [], by_id: %{}, attributes: [], data: nil
  end

  @type tgraph :: %Graph {
      # The name of the graph.
      name: String.t,
      # Whether this is a directed graph.
      is_directed: boolean,
      # Whether this is a strict graph (only one edge between two nodes).
      is_strict: boolean,
      # A list of all the root elements.
      elements: [GraphViz.id],
      # Map unique identifiers to the matching object. This covers all the
      # elements regardless of where they are.
      by_id: Dict.t,
      # Optional additional graph attributes.
      attributes: [Keyword.t],
      # Optional additional arbitrary data. This doesn't impact the generated
      # diagram in any way.
      data: any
  }


  defmodule SubGraph do
    @moduledoc """
    A sub-graph groups together multiple elements. This scopes attributes etc.
    If it is also a cluster, then `dot` will lay it in a box.
    """
    defstruct id: nil, parent: nil, is_cluster: nil, attributes: [], elements: [], data: nil
  end

  @type tsubgraph :: %SubGraph {
    # The sub-graph's unique identifier.
    id: GraphViz.id,
    # The sub-graph's parent identifier. A `nil` indicates this is
    # directly under the graph.
    parent: GraphViz.id | nil,
    # Whether this should be considered a cluster for `dot`. If it is, its
    # name in the generated dot file will be prefixed with `cluster_`.
    is_cluster: boolean,
    # Optional additional cluster attributes.
    attributes: [ Keyword.t ],
    # The elements contained in the cluster.
    elements: [ GraphViz.id ],
    # Optional additional arbitrary data. This doesn't impact the generated
    # diagram in any way.
    data: any
  }

  defmodule Node do
    @moduledoc """
    A node represents an atomic shape in the diagram. Actually, GraphViz
    recognizes sub-structure inside nodes (especially if they are record or
    HTML table nodes). But the node parts are all addressed using attributes so
    we ignore this.
    """
    defstruct id: nil, parent: nil, attributes: [], outgoing: [], incoming: [], data: nil
  end

  @type tnode :: %Node {
    # The node's unique identifier.
    id: GraphViz.id,
    # The node's parent identifier. A `nil` indicates this is directly under
    # the graph. the complete graph.
    parent: GraphViz.id | nil,
    # Optional additional node attributes.
    attributes: [ Keyword.t ],
    # List of edges originating at this node.
    outgoing: [ GraphViz.id ],
    # List of edges terminating at this node.
    incoming: [ GraphViz.id ],
    # Optional additional arbitrary data. This doesn't impact the generated
    # diagram in any way.
    data: any
  }

  defmodule Edge do
    @moduledoc """
    An edge connects two nodes. In directed graphs, this causes `dot` to impose
    a rank constraint between the source and target nodes.
    """
    defstruct id: nil, source: nil, target: nil, attributes: [], data: nil
  end

  @type t :: %Edge {
    # The edge's unique identifier.
    id: GraphViz.id,
    # The edge's source node.
    source: GraphViz.id,
    # The edge's target node.
    target: GraphViz.id,
    # Optional additional edge attributes.
    attributes: [ Keyword.t ],
    # Optional additional arbitrary data. This doesn't impact the generated
    # diagram in any way.
    data: any
  }

  # The types of elements a graph may contain.
  @type element :: SubGraph.t | Node.t | Edge.t

  @doc """
  Lookup an element in the graph by its identifier.
  """
  @spec lookup(Graph.t, id) :: element

  def lookup(%Graph{} = graph, id) do
    Dict.get(graph.by_id, id)
  end

  @doc """
  Add a new element to the graph. This will update the overall graph's
  dictionary to include the new element and connect it to all the related
  elements. Otherwise, the caller is responsible for filling all the
  non-default fields.
  """
  @spec add(Graph.t, element) :: Graph.t

  def add(graph, element) do
    graph |> update(element) |> connect(element)
  end

  @doc """
  Update an element in the graph dictionary. This trusts the caller not to mess
  with connectivity fields (sub-elements, source/target nodes,
  incoming/outgoing edge lists).
  """
  @spec update(Graph.t, element) :: Graph.t

  def update(%Graph{} = graph, %SubGraph{} = sub_graph) do
    %{graph | by_id: Dict.put(graph.by_id, sub_graph.id, sub_graph)}
  end

  def update(  %Graph{} = graph, %Node{} = node) do
    %{graph | by_id: Dict.put(graph.by_id, node.id, node)}
  end

  def update(%Graph{} = graph, %Edge{} = edge) do
    %{graph | by_id: Dict.put(graph.by_id, edge.id, edge)}
  end

  # Connect an element to all the other related elements in the graph.
  @spec connect(Graph.t, element) :: Graph.t

  defp connect(%Graph{} = graph, %SubGraph{} = sub_graph) do
    case sub_graph.parent do
      nil ->
        %{ graph | elements: [sub_graph.id | graph.elements]}
      parent ->
        graph |> update(add_sub_element(lookup(graph, parent), sub_graph.id))
    end
  end

  defp connect(%Graph{} = graph, %Node{} = node) do
    case node.parent do
      nil ->
        %{ graph | elements: [node.id | graph.elements]}
      parent ->
        graph |> update(add_sub_element(lookup(graph, parent), node.id))
    end
  end

  defp connect(%Graph{} = graph, %Edge{} = edge) do
    graph |> update(add_outgoing_edge(lookup(graph, edge.source), edge.id))
          |> update(add_incoming_edge(lookup(graph, edge.target), edge.id))
  end

  # Add a sub-element to a parent element.
  @spec add_sub_element(SubGraph.t, id) :: Graph.t | SubGraph.t

  defp add_sub_element(%SubGraph{} = sub_graph, id) do
    %{sub_graph | elements: [id | sub_graph.elements]}
  end

  # Add an incoming edge to a node.
  @spec add_incoming_edge(Node.t, id) :: Node.t

  def add_incoming_edge(%Node{} = node, id) do
    %{node | incoming: [id | node.incoming ]}
  end

  # Add an outgoing edge to a node.
  @spec add_outgoing_edge(Node.t, id) :: Node.t

  def add_outgoing_edge(%Node{} = node, id) do
    %{node | outgoing: [id | node.outgoing]}
    #node.update_outgoing([ id | &1 ])
  end

  @doc """
  Print the graph in the `dot` language.
  """
  @spec print(:io.device, Graph.t) :: :ok

  def print(device, %Graph{} = graph) do
    if graph.is_strict do
      IO.write(device, "strict ")
    end
    if graph.is_directed do
      IO.write(device, "digraph ")
    else
      IO.write(device, "graph ")
    end
    IO.write(device, inspect(graph.name))
    IO.puts(device, " {")
    graph.elements |> Enum.each fn x -> print_id(device, graph, x) end
    edge_separator = if graph.is_directed do
                       "->"
                     else
                       "--"
                     end
    graph.by_id |> Dict.to_list |> Enum.each fn x -> print_edge(device, edge_separator, x) end

    graph.attributes |> Enum.each fn x -> print_attribute(device, x, ";") end
    IO.puts(device, "}")
    :ok
  end

  # Print a nested graph element by its id.
  @spec print_id(:io.device, Graph.t, id) :: :ok

  defp print_id(device, %Graph{} = graph, id) do
    print_element(device, graph, lookup(graph, id))
  end

  # Print a nested graph element.
  @spec print_element(:io.device, graph :: %Graph{}, Graph.t | Node.t) :: :ok

  defp print_element(device, %Graph{} = graph, %SubGraph{} = sub_graph) do
    if sub_graph.is_cluster do
      IO.puts(device, "subgraph \"cluster_#{inspect(sub_graph.id)}\" {")
    else
      IO.puts(device, "subgraph \"#{inspect(sub_graph.id)}\" {")
    end
    sub_graph.elements |> Enum.each fn x -> print_id(device, graph, x) end
    sub_graph.attributes |> Enum.each fn x -> print_attribute(device, x, ";") end
    IO.puts(device, "}")
    :ok
  end

  defp print_element(device, _graph, %Node{} = node) do
    IO.puts(device, "\"#{inspect(node.id)}\" [")
    node.attributes |> Enum.each fn x-> print_attribute(device, x, ",") end
    IO.puts(device, "];") # HACK: This allows us to print each of the real attributes with a final ",".
    :ok
  end

  # Print edge elements. It is safest to print these at the very end so if we
  # somehow messed up the identifiers, the implicit nodes will not appear to
  # belong to any of the sub-graphs.
  @spec print_edge(:io.device, separator :: String.t, { id, element }) :: :ok

  defp print_edge(device, separator, { _id, %Edge{} = edge }) do
    IO.puts(device, "\"#{inspect(edge.source)}\" #{separator} \"#{inspect(edge.target)}\" [")
    edge.attributes |> Enum.each fn x -> print_attribute(device, x, ",") end
    IO.puts(device, "];")
    :ok
  end

  defp print_edge(_device, _separator, _non_edge) do
    :ok
  end

  # Print a graph attribute. We terminate graph and edge/node attributes
  # differently. It is safest to print the attributes at the end of each
  # (sub)graph so they will not be interpreted as defaults for the elements.
  @spec print_attribute(:io.device, { name :: atom, value :: any }, terminator :: String.t) :: :ok

  defp print_attribute(device, { name, value }, terminator) do
    IO.puts(device, "#{name} = #{value}#{terminator}")
    :ok
  end

end

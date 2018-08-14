// Copyright (c) 2018 Round Inc.. All rights reserved.

package swissbog

object Graph {
  type Vertex   = Currency
  type Edges    = Map[(Vertex, Vertex), Double]
  type Distance = Map[Vertex, Double]
  type Parent   = Map[Vertex, Vertex]

  final case class Graph(vertice: Set[Vertex], edges: Edges, distance: Distance, parent: Parent)

  /** The implementation follows
    * [[http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.86.1981&rep=rep1&type=pdf]]
    * which is an enhanced version of Bellman-Ford based algorithm
    * to find a negative cycle in a weighted directed graph.
    *
    * The orginal algorithm can find a negative cycle reachable from
    * an arbitrary source vertex. It can't find negative cycles that
    * may not be reached from the source vertex seletected.
    * The enhancement is to add an additional vertex S to the graph
    * whereby S has an edge, weights 0, to every other vertex. By using
    * the orginal algorithm on the newly created graph, we can find a
    * negative cycle (if exists) reachable from S. The cycle found cannot
    * contain S as there is no directed edge coming from the original
    * graph to S.
    *
    * The complexity of building the graph is O(E + 2V), detecting the
    * existence of negative cycle takes O(E), plus collecting cyclic
    * vertice complexity of O(V^2/2). All are superseded by O(EV^2) of
    * the Bellman-Ford run which is O(EV^2). An important note is that
    * such O(EV^2) can be reduced to O(EV) by using array data structure.
    */
  def findNegativeCycle(edges: Edges): Option[Vector[Vertex]] = {
    val sGraph: Graph = buildGraph(edges)
    val g: Graph      = bellmanford(sGraph)
    g.edges.collectFirst {
      case ((u, v), weight) if g.distance(v) > g.distance(u) + weight =>
        collectCycleVertex(v, Vector(v), g.parent)
    }.flatten
  }

  /** Iterates over E edges for (V - 1) times. For each iteration,
    * it will take O(V) time to update `distance` and `parent` maps,
    * so the total running time is O(EV^2).
    * This can be improved to O(EV) by using an array to keep `distance`
    * and `parent` maps.
    */
  def bellmanford(graph: Graph): Graph =
    (1 until graph.vertice.size).foldLeft(graph) { (gv, _) =>
      graph.edges.foldLeft(gv) { (g, r) =>
        val ((u, v), weight) = r
        val d: Double        = g.distance(u) + weight
        if (g.distance(v) > d) {
          val dv: Distance = g.distance - v + (v -> weight)
          val pv: Parent   = g.parent - v + (v -> u)
          Graph(g.vertice, g.edges, dv, pv)
        } else {
          g
        }
      }
    }

  /** Needs O(E) steps to collect the V vertice from the edges, where E is the
    * number of edges. Then it takes additionally O(V) to generate the
    * distance map, and O(V) to traverse the set of original vertice to create
    * the additional edges from S. In total,`buildGraph` function needs
    * O(E + 2V) to complete running.
    */
  def buildGraph(edges: Edges): Graph = {
    val source: Vertex       = Currency(0.toString)
    val vertice: Set[Vertex] = edges.keys.flatMap(k => Set(k._1, k._2)).toSet
    val sEdges: Edges        = edges ++ vertice.map(source -> _ -> 0d)
    val distance: Distance = (vertice + source).map {
      case `source` => source -> 0d
      case x        => x -> Double.PositiveInfinity
    }.toMap
    Graph(vertice + source, sEdges, distance, Map.empty)
  }

  /** Finding the parent vertex from `parent` map can take up to O(V) since
    * do not revisit a vertex hence the longest path length is V.
    * In the recursive call, the finding happens again on the same `parent`
    * map. However we will not traverse the whole map every time in the worst
    * search space of V nodes we need V^2/2 amortized time.
    * We can improve the complexity by using an array to keep track of the
    * vertice parent.
    */
  def collectCycleVertex(current: Vertex, ls: Vector[Vertex], parent: Parent): Option[Vector[Vertex]] =
    parent.get(current).flatMap { next: Vertex =>
      if (ls.contains(next)) {
        Some(next +: ls.takeWhile(_ != next) :+ next)
      } else {
        collectCycleVertex(next, next +: ls, parent - current)
      }
    }
}

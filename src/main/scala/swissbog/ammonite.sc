import $ivy.`org.typelevel::cats-core:1.2.0`
import $ivy.`org.typelevel::cats-effect:0.10.1`
import $ivy.`io.circe::circe-generic:0.9.3`
import $ivy.`io.circe::circe-literal:0.9.3`
import $ivy.`io.circe::circe-parser:0.9.3`
import $ivy.`org.http4s::http4s-blaze-client:0.18.15`
import $ivy.`org.http4s::http4s-circe:0.18.15`
import $ivy.`org.http4s::http4s-dsl:0.18.15`

import cats.effect.IO
import cats.implicits._
import io.circe.KeyDecoder
import org.http4s.EntityDecoder
import org.http4s.circe._
import org.http4s.client.blaze._

final case class Currency(name: String) extends AnyVal {
  override def toString: String = name
}

final case class Trade(from: Currency, to: Currency, rate: Double)

final case class Arbitrage(exchanges: Vector[Trade]) extends AnyVal {

  def profit: Double = exchanges.map(_.rate).reduce(_ * _)

  override def toString: String = exchanges.mkString("", " -> ", s" -> $profit")
}

type Rates = Map[(Currency, Currency), Double]

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

object Algebra {

  /**
    * An implementation of:
    * https://math.stackexchange.com/questions/94414/an-algorithm-for-arbitrage-in-currency-exchange
    *  and
    * http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.86.1981&rep=rep1&type=pdf
    *
    * Finding a trading loop has time complexity of O(EV^2) where V is the number of currencies
    * and E is the number of rate entries.
    * Then for the loop, O(EV) is needed to transform it into an `Arbitrage`. Therefore, the total
    * complexity of arbitrage finding is O(EV^2).
    */
  def findArbitrages(rates: Rates): Option[Arbitrage] =
    Graph.findNegativeCycle(rates.mapValues(-math.log(_))).map { loop =>
      val pairs = loop
        .sliding(2)
        .map(s => s.head -> s.tail.head)
      val trades = pairs.foldLeft(Vector.empty[Trade]) { (trades, pair) =>
        trades ++ rates
          .get(pair)
          .map(Trade(pair._1, pair._2, _))
      }
      Arbitrage(trades)
    }

  implicit val currencyDecoder: KeyDecoder[(Currency, Currency)] = new KeyDecoder[(Currency, Currency)] {
    override def apply(key: String): Option[(Currency, Currency)] = {
      val Array(from, to) = key.split("_")
      Some(Currency(from) -> Currency(to))
    }
  }

  implicit val ratesDecoder: EntityDecoder[IO, Rates] = jsonOf

  def getRates: IO[Rates] =
    for {
      client <- Http1Client[IO]()
      rates  <- client.expect[Rates]("https://fx.priceonomics.com/v1/rates/")
    } yield {
      client.shutdownNow()
      rates.filterKeys(k => k._1 != k._2)
    }
}

import Algebra._

getRates
  .attempt
  .unsafeRunSync()
  .leftMap(e => "Failed with error: " + e.getMessage)
  .flatMap {
    findArbitrages(_).toRight("No arbitrage")
  }
  .leftMap(println)
  .foreach(println)

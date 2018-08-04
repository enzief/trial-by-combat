// Copyright (c) 2018 Round Inc.. All rights reserved.

package swissbog

import org.jgrapht.Graph
import org.jgrapht.alg.cycle.JohnsonSimpleCycles
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.SimpleDirectedGraph

import scala.collection.JavaConverters._

final case class Currency(name: String) extends AnyVal

object Currency {
  val USD = Currency("USD")
  val CHF = Currency("CHF")
  val GBP = Currency("GBP")
}

final case class Trade(from: Currency, to: Currency, rate: Float)

final case class Arbitrage(exchanges: Vector[Trade]) extends AnyVal {

  def profit: Float = exchanges.map(_.rate).reduce(_ * _)
}

object Main {
  import Currency._

  type Rates = Map[(Currency, Currency), Float]

  def main(args: Array[String]): Unit = {
    val rates: Rates = Map(
      (USD, CHF) -> 1f,
      (CHF, USD) -> 1.1f,
      (USD, GBP) -> 0.7f,
      (GBP, USD) -> 1.25f,
      (GBP, CHF) -> 1.22f,
      (CHF, GBP) -> 0.8f
    )
    val graph: Graph[Currency, DefaultEdge] = new SimpleDirectedGraph(classOf[DefaultEdge])
    graph.addVertex(USD)
    graph.addVertex(CHF)
    graph.addVertex(GBP)
    graph.addEdge(USD, CHF)
    graph.addEdge(CHF, USD)
    graph.addEdge(USD, GBP)
    graph.addEdge(GBP, USD)
    graph.addEdge(GBP, CHF)
    graph.addEdge(CHF, GBP)

    findArbitrages(graph, rates)
      .foreach(println)
  }

  /**
    * According to
    * D. B. Johnson, Finding all the elementary circuits of a directed graph, SIAM J. Comput., 4 (1975), pp. 77-84.,
    * finding the trading loops for a `Graph[Currency, DefaultEdge]` has time complexity of
    * O((V + E) * C) where V is the number of currencies and E is the number of rate entries.
    *
    * For each loop found, whose size is maximum V, we need O(V) to rotate as described in `rotate` function.
    * Then for each rotation, O(V * E) is needed to transform it into an `Arbitrage`. Therefore, the total
    * complexity of one loop transforming is O(V^2 * E).
    *
    * The total number of loops is, unfortunately, possibly exponential. And hence, it may
    * require exponential time to find all arbitrage loops from an input set of data.
    */
  def findArbitrages(graph: Graph[Currency, DefaultEdge], rates: Rates): List[Arbitrage] =
    new JohnsonSimpleCycles(graph)
      .findSimpleCycles()
      .asScala
      .toList
      .flatMap { simpleLoop =>
        rotate(simpleLoop.asScala.toVector)
          .map { loop =>
            val pairs = (loop :+ loop.head)
              .sliding(2)
              .map(s => s.head -> s.tail.head)
            val trades = pairs.foldLeft(Vector.empty[Trade]) { (trades, pair) =>
              trades ++ rates.get(pair).map(Trade(pair._1, pair._2, _))
            }
            Arbitrage(trades)
          }
          .filter(_.profit > 1)
      }

  /**
    * Rotating a loop has time complexity of O(V) where V is length of the input list
    * since appending to a vector and prepending a list is trivial.
    */
  def rotate[A](list: Vector[A]): List[Vector[A]] =
    list.foldLeft(List.empty[Vector[A]]) { (ll, _) =>
      val head = ll.headOption.getOrElse(list)
      (head.tail :+ head.head) :: ll
    }
}

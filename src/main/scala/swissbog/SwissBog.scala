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

final case class Arbitrage(exchanges: Vector[Trade]) {

  val profit: Float = exchanges.map(_.rate).reduce(_ * _)
}

object Main {
  import Currency._

  def main(args: Array[String]): Unit = {
    val rates: Map[(Currency, Currency), Float] = Map(
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

    val arbitrades: List[Arbitrage] = new JohnsonSimpleCycles(graph)
      .findSimpleCycles()
      .asScala
      .toList
      .flatMap { simpleLoop =>
        rotate(simpleLoop.asScala.toList)
          .map { loop =>
            val pairs = (loop :+ loop.head)
              .sliding(2)
              .map(s => s.head -> s.tail.head)
            val trades = pairs.foldLeft(Vector.empty[Trade]) { (arbs, pair) =>
              arbs ++ rates.get(pair).map(Trade(pair._1, pair._2, _))
            }
            Arbitrage(trades)
          }
          .filter(_.profit > 1)
      }

    arbitrades.foreach(println)
  }

  def rotate[A](list: List[A]): List[List[A]] =
    list.foldLeft(List.empty[List[A]]) { (ll, _) =>
      val head = ll.headOption.getOrElse(list)
      (head.tail :+ head.head) :: ll
    }
}

// Copyright (c) 2018 Round Inc.. All rights reserved.

package swissbog

import cats.effect.IO
import io.circe.KeyDecoder
import org.http4s.EntityDecoder
import org.http4s.circe._
import org.http4s.client.blaze._
import org.jgrapht.Graph
import org.jgrapht.alg.cycle.JohnsonSimpleCycles
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.SimpleDirectedGraph

import scala.collection.JavaConverters._

final case class Currency(name: String) extends AnyVal
final case class Trade(from: Currency, to: Currency, rate: Float)

final case class Arbitrage(exchanges: Vector[Trade]) extends AnyVal {

  def profit: Float = exchanges.map(_.rate).reduce(_ * _)
}

object Main {

  type Rates = Map[(Currency, Currency), Float]

  def main(args: Array[String]): Unit = {
    val rates: Rates = getRates.unsafeRunSync
    println(rates)
    findArbitrages(buildGraph(rates), rates)
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

  def buildGraph(rates: Rates): Graph[Currency, DefaultEdge] = {
    val currencies: Set[Currency]           = rates.keys.flatMap(k => Set(k._1, k._2)).toSet
    val graph: Graph[Currency, DefaultEdge] = new SimpleDirectedGraph(classOf[DefaultEdge])
    currencies.foreach(graph.addVertex)
    rates.keys.foreach(k => graph.addEdge(k._1, k._2))
    graph
  }
}

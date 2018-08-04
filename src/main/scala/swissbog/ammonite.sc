import $ivy.`org.jgrapht:jgrapht-core:1.2.0`
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
import org.jgrapht.Graph
import org.jgrapht.alg.cycle.JohnsonSimpleCycles
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.SimpleDirectedGraph

import scala.collection.JavaConverters._

final case class Currency(name: String) extends AnyVal {
  override def toString: String = name
}

final case class Trade(from: Currency, to: Currency, rate: Float)

final case class Arbitrage(exchanges: Vector[Trade]) extends AnyVal {

  def profit: Float = exchanges.map(_.rate).reduce(_ * _)

  override def toString: String = exchanges.mkString("", " -> ", s" -> $profit")
}

type Rates = Map[(Currency, Currency), Float]

object Algebra {

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
  def findArbitrages(graph: Graph[Currency, DefaultEdge], rates: Rates): IO[List[Arbitrage]] =
    for {
      cycles <- IO(
                 new JohnsonSimpleCycles(graph)
                   .findSimpleCycles()
                   .asScala
                   .toList
               )
    } yield {
      cycles.flatMap { simpleLoop =>
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

  def buildGraph(rates: Rates): IO[Graph[Currency, DefaultEdge]] =
    IO {
      val currencies: Set[Currency]           = rates.keys.flatMap(k => Set(k._1, k._2)).toSet
      val graph: Graph[Currency, DefaultEdge] = new SimpleDirectedGraph(classOf[DefaultEdge])
      currencies.foreach(graph.addVertex)
      rates.keys.foreach(k => graph.addEdge(k._1, k._2))
      graph
    }
}

import Algebra._

val arbitrages =
  getRates.attempt.unsafeRunSync().flatMap { rates =>
    buildGraph(rates).attempt.unsafeRunSync().flatMap { graph =>
      findArbitrages(graph, rates).attempt.unsafeRunSync()
    }
  }

arbitrages.leftMap(e => println("Failed with: " + e.getMessage))
arbitrages.foreach(_.foreach(println))

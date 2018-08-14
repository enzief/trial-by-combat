// Copyright (c) 2018 Round Inc.. All rights reserved.

package swissbog

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

object Main {

  type Rates = Map[(Currency, Currency), Double]

  def main(args: Array[String]): Unit =
    getRates
      .map(findArbitrages)
      .attempt
      .unsafeRunSync()
      .leftMap(e => "Failed with error: " + e.getMessage)
      .flatMap(_.toRight("No arbitrage"))
      .leftMap(println)
      .foreach(println)

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

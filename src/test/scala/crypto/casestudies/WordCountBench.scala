package crypto.dsl

import org.scalameter.api._
import scala.io.Source
import scala.concurrent._
import scala.concurrent.duration._

import scalaz._
import scalaz.syntax.traverse._

import crypto._
import crypto.cipher._
import crypto.remote._
import crypto.dsl.Implicits._

object WordCountBench extends CustomPerformanceTest {
  def wordCountPlain(input: String): List[(String,Int)] =
    wordCountPlain_(IList.fromList(input.split("""\s+""").toList))

  def wordCountPlain_(input: IList[String]): List[(String,Int)] =
    input.groupBy(identity)(Order.fromScalaOrdering).map(_.length).toList

  def wordCountText(input: EncString): CryptoM[List[(OpeString,Int)]] =
    input.split("""\W+""") >>= wordCount_

  def wordCount_(input: IList[EncString]): Crypto[List[(OpeString,Int)]] =
    input.traverse(toOpeStr).map(_.groupBy(x => x).map(_.length).toList)

  val ns = List(200,400,600,800,1000,1200,1400,1600,1800,2000,2200,2400,2600,2800,3000,3200)
  val limits = Gen.enumeration("chars")(ns: _*)

  lazy val keyRing = KeyRing.create
  implicit lazy val decoder = EncInt.decode(keyRing)

  lazy val preparedEncrypted = ns.map { charLimit =>
    val content: String = Source.fromFile("ghci-debugger.txt").mkString.take(charLimit)
    val encrypted = Common.encryptStrOpe(keyRing)(content)
    (charLimit,content -> encrypted)
  }.toMap

  performance of "Word-Count" in {
    measure method "plain" in {
      using (limits) in { charLimit =>
        wordCountPlain(preparedEncrypted(charLimit)._1).sortBy(_._2)
      }
    }

    measure method "local" in {
      lazy val interpret = LocalInterpreter(keyRing)
      def finish[A](x: A): A = x

      using (limits) in { charLimit =>
        finish(interpret(wordCountText(preparedEncrypted(charLimit)._2))).sortBy(_._2)
      }
    }

    measure method "remoteOpt" in {
      lazy val interpret = {
        lazy val cryptoService = new CryptoServiceImpl(keyRing)(CustomExecutionContext(5))
        lazy val pubKeys = Await.result(cryptoService.publicKeys, 10.seconds)
        new RemoteInterpreterOpt(cryptoService, pubKeys)(ExecutionContext.Implicits.global)
      }
      def finish[A](x: Future[A]): A = Await.result(x, 42.minutes)

      using (limits) in { charLimit =>
        finish(interpret(wordCountText(preparedEncrypted(charLimit)._2))).sortBy(_._2)
      }
    }
  }
}

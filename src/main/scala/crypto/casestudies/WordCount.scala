package crypto.casestudies

import scala.io.Source
import scala.concurrent._
import scala.concurrent.forkjoin._

import scalaz._
import scalaz.syntax.traverse._

import crypto._
import crypto.cipher._
import crypto.dsl._
import crypto.dsl.Implicits._


import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import argonaut._
import Argonaut._

object WordCount {
  def encryptContent(k: KeyRing)(input: String): EncString =
    Common.encryptStrOpe(k)(input)

  def writeEncryptedFile(content: EncString, fileName: String) =
    Files.write(
      Paths.get(fileName),
      content.asJson.nospaces.getBytes(StandardCharsets.UTF_8))

  def readEncryptedFile(fileName: String): EncString =
    Parse.decodeOption[EncString](Source.fromFile(fileName).mkString).get
}

object WordCountCaseStudy extends App {
  import WordCount._

  // Split the text into words and perform word count on it
  def wordCountText(input: EncString): CryptoM[List[(OpeString,Int)]] =
    input.split("""\W+""") >>= wordCount_

  // Just do it as concisely as possible
  def wordCount_(input: IList[EncString]): Crypto[List[(OpeString,Int)]] =
    input.traverse(toOpeStr).map(_.groupBy(x => x).map(_.length).toList)



  sealed trait CharLimit
  case class FixedLimit(i: Int) extends CharLimit
  case object NoLimit extends CharLimit

  val limit: CharLimit = args.lift(0).map(FixedLimit.compose(_.toInt)).getOrElse(NoLimit)
  println(s"Limit used: $limit")

  val keyRing = KeyRing.create

  // import scala.concurrent.duration._
  // import crypto.remote._
  // val interpret = {
  //   val cryptoService = new CryptoServiceImpl(keyRing)(CustomExecutionContext(5))
  //   val pubKeys = Await.result(cryptoService.publicKeys, 10.seconds)
  //     new RemoteInterpreterOpt(cryptoService, pubKeys)(ExecutionContext.Implicits.global)
  // }
  // def finish[A](x: Future[A]): A = Await.result(x, 42.minutes)

  val interpret = LocalInterpreter(keyRing)
  def finish[A](x: A): A = x

  implicit val decoder = EncInt.decode(keyRing)

  val content: String = {
    val c = Source.fromFile("ghci-debugger.txt").mkString
    println(s"Content length: ${c.length}")
    limit match {
      case FixedLimit(i) => c.take(i)
      case NoLimit => c
    }
  }

  val encrypted = encryptContent(keyRing)(content)
  writeEncryptedFile(encrypted, "ghci-debugger.json")
  println("Text encrypted")

  val startTime = System.currentTimeMillis
  val encryptedFromFile: EncString = readEncryptedFile("ghci-debugger.json")
  val counts: List[(OpeString, Int)] =
    finish(interpret(wordCountText(encryptedFromFile))).sortBy(_._2)

  Files.write(
    Paths.get("ghci-debugger-wordcount.json"),
    counts.asJson.nospaces.getBytes(StandardCharsets.UTF_8))
  val totalTime = System.currentTimeMillis - startTime

  val countsFromFile = Parse.decodeOption[List[(OpeString, Int)]](
    Source.fromFile("ghci-debugger-wordcount.json").mkString).get
  val decrypted = countsFromFile.map { case (w,c) => (Common.decryptStr(keyRing)(w), c) }

  // println(decrypted.mkString("[","\n","]"))
  println(s"Time for word count only: $totalTime")
}

object CustomExecutionContext {
  def apply(threads: Int): ExecutionContext = ExecutionContext.fromExecutorService(
    new ForkJoinPool(threads, ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, true))
}

object WordCountCaseStudyPlain extends App {
  def wordCountPlain(input: String): List[(String,Int)] =
    wordCountPlain_(IList.fromList(input.split("""\s+""").toList))

  def wordCountPlain_(input: IList[String]): List[(String,Int)] =
    input.groupBy(identity)(Order.fromScalaOrdering).map(_.length).toList

  sealed trait CharLimit
  case class FixedLimit(i: Int) extends CharLimit
  case object NoLimit extends CharLimit

  val limit: CharLimit = args.lift(0).map(FixedLimit.compose(_.toInt)).getOrElse(NoLimit)
  println(s"Limit used: $limit")

  val keyRing = KeyRing.create

  // import scala.concurrent.duration._
  // import crypto.remote._
  // val interpret = {
  //   val cryptoService = new CryptoServiceImpl(keyRing)(CustomExecutionContext(5))
  //   val pubKeys = Await.result(cryptoService.publicKeys, 10.seconds)
  //     new RemoteInterpreterOpt(cryptoService, pubKeys)(ExecutionContext.Implicits.global)
  // }
  // def finish[A](x: Future[A]): A = Await.result(x, 42.minutes)

  val interpret = LocalInterpreter(keyRing)
  def finish[A](x: A): A = x

  implicit val decoder = EncInt.decode(keyRing)

  val content: String = {
    val c = Source.fromFile("ghci-debugger.txt").mkString
    println(s"Content length: ${c.length}")
    limit match {
      case FixedLimit(i) => c.take(i)
      case NoLimit => c
    }
  }

  val startTime = System.currentTimeMillis
  val counts: List[(String, Int)] = wordCountPlain(content).sortBy(_._2)
  val totalTime = System.currentTimeMillis - startTime

  // println(decrypted.mkString("[","\n","]"))
  println(s"Time for word count only: $totalTime")
}

object WordCountPlain {
 }

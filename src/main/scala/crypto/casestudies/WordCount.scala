package crypto.casestudies

import scala.io.Source

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
    // Split the text into words and perform word count on it
  def wordCountText(input: EncString): CryptoM[List[(OpeString,Int)]] =
    input.split("""\W+""") >>= wordCount_

  // Just do it as concisely as possible
  def wordCount_(input: IList[EncString]): Crypto[List[(OpeString,Int)]] =
    input.traverse(toOpeStr).map(_.groupBy(x => x).map(_.length).toList)

  def encryptFileContent(k: KeyRing)(input: String): EncString =
    Common.encryptStrOpe(k)(Source.fromFile(input).mkString)

  def writeEncryptedFile(content: EncString, fileName: String) =
    Files.write(
      Paths.get(fileName),
      content.asJson.nospaces.getBytes(StandardCharsets.UTF_8))

  def readEncryptedFile(fileName: String): EncString =
    Parse.decodeOption[EncString](Source.fromFile(fileName).mkString).get
}

object WordCountCaseStudy extends App {
  import WordCount._

  val k = KeyRing.create
  val locally = LocalInterpreter(k)
  implicit val decoder = EncInt.decode(k)

  val encrypted = encryptFileContent(k)("ghci-debugger.txt")
  writeEncryptedFile(encrypted, "ghci-debugger.json")

  val encryptedFromFile: EncString = readEncryptedFile("ghci-debugger.json")
  val counts: List[(OpeString, Int)] =
    locally(wordCountText(encryptedFromFile)).sortBy(_._2)

  Files.write(
    Paths.get("ghci-debugger-wordcount.json"),
    counts.asJson.nospaces.getBytes(StandardCharsets.UTF_8))

  val countsFromFile = Parse.decodeOption[List[(OpeString, Int)]](
    Source.fromFile("ghci-debugger-wordcount.json").mkString).get
  val decrypted = countsFromFile.map { case (w,c) => (Common.decryptStr(k)(w), c) }

  println(decrypted.mkString("[","\n","]"))
}

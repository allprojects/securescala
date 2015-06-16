package crypto.casestudies

import scala.io.Source

import scalaz.std.list._

import crypto._
import crypto.dsl._
import crypto.dsl.Implicits._
import crypto.cipher._

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import argonaut._
import Argonaut._

case class Person(firstName: String, lastName: String, age: Int, income: Int)
case class EncPerson(firstName: EncString, lastName: EncString, age: EncInt, income: EncInt)

object EncPerson {
  implicit def encode: EncodeJson[EncPerson] =
    jencode4L((p:EncPerson)=>EncPerson.unapply(p).get)(
      "first_name","last_name","age","income")

  implicit def decode(implicit d: DecodeJson[EncInt]): DecodeJson[EncPerson] =
    jdecode4L(EncPerson.apply)("first_name","last_name","age","income")
}

object Person {
  implicit def encode: EncodeJson[Person] =
    jencode4L((p:Person)=>Person.unapply(p).get)(
      "first_name","last_name","age","income")

  implicit def decode(implicit d: DecodeJson[EncInt]): DecodeJson[Person] =
    jdecode4L(Person.apply)("first_name","last_name","age","income")
}

object PersonUtils {
  def encrypt(k: KeyRing): Person => EncPerson = _ match {
    case Person(f,l,a,i) =>
      val ef = Common.encryptStrOpe(k)(f)
      val el = Common.encryptStrOpe(k)(l)
      val ea = Common.encrypt(Comparable, k)(a)
      val ei = Common.encrypt(Comparable, k)(i)
      EncPerson(ef,el,ea,ei)
  }

  def decrypt(k: KeyRing): EncPerson => Person = _ match {
    case EncPerson(ef,el,ea,ei) =>
      val f = Common.decryptStr(k)(ef)
      val l = Common.decryptStr(k)(el)
      val a = Common.decrypt(k)(ea)
      val i = Common.decrypt(k)(ei)
      Person(f,l,a.toInt,i.toInt)
  }

  val persons = List[Person](
    Person("Gerold", "Wootton", 34, 66274),
    Person("Misty", "Baum", 56,  62194),
    Person("Columbine", "Piper", 33, 52098),
    Person("August", "Jaeger", 51,  47411),
    Person("Wenonah", "Lewis", 40, 58928),
    Person("Maddox", "Short", 28, 55283),
    Person("Randolph", "Head", 40, 58061),
    Person("Rochus", "Robertson", 35, 49700),
    Person("Berry", "Honeycutt", 40, 49401),
    Person("Baldur", "Moore", 37, 69401)
  )

  def epersons(k: KeyRing): List[EncPerson] = persons.map(encrypt(k))

  def writePersonsFile(k: PubKeys)(ps: List[EncPerson]) = {
    Files.write(
      Paths.get("persons.json"),
      ps.asJson.nospaces.getBytes(StandardCharsets.UTF_8))
  }

  def readPersonsFile(k: PubKeys) = {
    implicit val decoder = EncInt.decode(k)
    Parse.decodeOption[List[EncPerson]](Source.fromFile("persons.json").mkString).get
  }
}

object PersonsCaseStudy extends App {
  val k = KeyRing.create
  val locally = LocalInterpreter(k)
  implicit val decoder = EncInt.decode(k)

  PersonUtils.writePersonsFile(k)(PersonUtils.epersons(k))
  val ps = PersonUtils.readPersonsFile(k)

  def whoEarnsMost(ps: List[EncPerson]): Crypto[EncPerson] = {
    sortBy(ps)(_.income).map(_.last)
  }

  def averageAge(ps: List[EncPerson]): CryptoM[EncInt] = {
    average(Common.zero(k))(ps.map(_.age))
  }
  val avg = Common.decrypt(k)(locally(averageAge(ps)))
  println(s"Average age (rounded) is: $avg")

  val result = locally(whoEarnsMost(ps))
  println(s"The person earning the most money is: ${PersonUtils.decrypt(k)(result)}")
}

// https://github.com/samuelgruetter/rx-playground
// https://github.com/ReactiveX/RxScala/blob/4857c7727d0fb2192a95740e545a7b69785f08b4/examples/src/test/scala/examples/Olympics.scala
// https://github.com/ReactiveX/RxJava/wiki/Additional-Reading
package crypto.casestudies

import scala.language.postfixOps
import scala.concurrent.duration._
import rx.lang.scala.Observable

object Hello {
  def hello(names: String*): Unit = {
    val _ = Observable.from(names) subscribe { n =>
      println(s"Hello $n!")
    }
  }
  def main(args: Array[String]) = {
    // Hello.hello("Markus")
List(
      Observable.interval(200 millis).map(_ => 1).take(5),
      Observable.interval(200 millis).map(_ => 2).take(5),
      Observable.interval(200 millis).map(_ => 3).take(5),
      Observable.interval(200 millis).map(_ => 4).take(5)
    ).toObservable.flatten(2).toBlocking.foreach(println(_))
  }
}

package crypto.casestudies

object WordCountSpark {
  import org.apache.spark.rdd.RDD
  import org.apache.spark.SparkContext
  import org.apache.spark.SparkConf

  import crypto._
  import crypto.cipher._

  import scala.io._
  import scalax.file.Path

  val conf = new SparkConf().setAppName("Word Count on Spark")
  val sc = new SparkContext(conf)

  object WordCount {
    def encryptContent(k: KeyRing)(input: String): EncString =
      Common.encryptStrOpe(k)(input)

    // If I need to write data to a file, they must be collected data
    def writeEncryptedFile(content: EncString, fileName: String) =
      Path(fileName).write(content.toString)

    // RDD requires strict invariant
    // It handles all dirty works of fromString() parsing the file
    def readEncryptedFile(fileName: String): RDD[OpeString] = 
      sc.textFile(fileName).map(line => line.replaceAll("List|OpeString|[()]", "").split(", ")).map(x => x.map(BigInt(_)).toList).map(OpeString(_))

  }

  def main(args: Array[String]) = {

    val keyring = KeyRing.create

    val plain = Source.fromFile(args(0)).mkString
    WordCount.writeEncryptedFile(WordCount.encryptContent(keyring)(plain), "encrypted_test.txt")

    val content = WordCount.readEncryptedFile("encrypted_test.txt")
    content.map(Common.decryptStr(keyring.priv)).flatMap(word => word.split("""\W+""")).map(x => (x, 1)).reduceByKey((l, r) => l + r).collect.foreach(println)

  }
}

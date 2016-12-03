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
  implicit val sc = new SparkContext(conf)

  object EncryptedFileOperations {
    def encryptContent(k: KeyRing)(input: String): EncString =
      Common.encryptStrOpe(k)(input)

    // If I need to write data to a file, they must be collected data
    def writeEncryptedFile(content: EncString, fileName: String) =
      Path(fileName).write(content.toString)

    // RDD requires strict invariant
    // It handles all dirty works of fromString() parsing the file
    def readEncryptedFile(fileName: String)(implicit sc: SparkContext): RDD[OpeString] = 
      sc.textFile(fileName).map(line => line.replaceAll("List|OpeString|[()]", "").split(", ")).map(x => x.map(BigInt(_)).toList).map(OpeString(_))

  }

  def wordCount(content: RDD[String]): Array[(String, Int)] = content.flatMap(word => word.split("""\W+""")).map(x => (x, 1)).reduceByKey((l, r) => l + r).collect

  def main(args: Array[String]) = {
    import EncryptedFileOperations._

    val keyring = KeyRing.create

    val plain = Source.fromFile(args(0)).mkString
    writeEncryptedFile(encryptContent(keyring)(plain), "encrypted_test.txt")

    val content = readEncryptedFile("encrypted_test.txt")
    val result = wordCount(content.map(Common.decryptStr(keyring.priv)))
    result.foreach(println)
  }
}

object WordCountSparkBench {
  import WordCountSpark._

  import crypto._
  import crypto.cipher._

  import scala.io._
  import scalax.file.Path

  def main(args: Array[String]) = {
    import EncryptedFileOperations._

    val keyring = KeyRing.create

    val plain = Source.fromFile(args(0)).mkString.take(args(1).toInt)
    writeEncryptedFile(encryptContent(keyring)(plain), "encrypted_test.txt")

    val content = readEncryptedFile("encrypted_test.txt")
    wordCount(content.map(Common.decryptStr(keyring.priv)))
    ()
  }
}

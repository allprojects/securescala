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

  def wordCount(content: RDD[String]) = content.flatMap(word => word.split("""\W+""")).map(x => (x, 1)).reduceByKey((l, r) => l + r)

  def main(args: Array[String]) = {
    import EncryptedFileOperations._

    val keyring = KeyRing.create

    val plain = Source.fromFile(args(0)).mkString
    writeEncryptedFile(encryptContent(keyring)(plain), "encrypted_test.txt")

    val content = readEncryptedFile("encrypted_test.txt")
    wordCount(content.map(Common.decryptStr(keyring.priv)))
    ()
  }
}

object WordCountSparkBench {
  import WordCountSpark._

  val ns = List(200,400,600,800,1000,1200,1400,1600,1800,2000,2200,2400,2600,2800,3000,3200)

  def main(args: Array[String]) = {
    import EncryptedFileOperations._

    import crypto._
    import crypto.cipher._

    import scala.io._
    import scalax.file.Path

    val startTime = System.nanoTime
    println(s"Start benchmarking on encrypted text at: ${startTime}\n")
    val keyring = KeyRing.create

    val plain = Source.fromFile(args(0)).mkString
    val benchSamples = ns.map(n => (plain.take(n), n))
    benchSamples.foreach { case (str, limit) =>
      System.gc
      val encryptStartTime = System.nanoTime
      val encrypted = encryptContent(keyring)(str)
      val encryptTime = System.nanoTime - encryptStartTime

      writeEncryptedFile(encrypted, s"encrypted_test_${limit}.txt")

      val encryptedContent = readEncryptedFile(s"encrypted_test_${limit}.txt")

      val decryptStartTime = System.nanoTime
      val plain = encryptedContent.map(Common.decryptStr(keyring))
      val decryptTime = System.nanoTime - decryptStartTime

      val mapReduceStartTime = System.nanoTime
      wordCount(plain)
      val mapReduceTime = System.nanoTime - mapReduceStartTime

      println(s"Sample for ${limit} characters")
      println(s"Time elapsed for encryption: ${encryptTime} ns")
      println(s"Time elapsed for decryption: ${decryptTime} ns")
      println(s"Time elapsed for map reduce on plain data: ${mapReduceTime} ns")
      println(s"In total: ${mapReduceTime + encryptTime + decryptTime} ns\n")
    }

    println(s"Total time elapsed: ${System.nanoTime - startTime} ns\n\n")
  }
}

object WordCountPlainSparkBench {
  import WordCountSpark._
  import WordCountSparkBench.ns

  def main(args: Array[String]) = {
    println("Start benchmarking spark on plain text\n")
    val contents = sc.textFile(args(0))
    val dataList = ns.map { n => (sc.parallelize(contents.take(n)), n) }

    dataList.foreach { case (data, limit) =>
      System.gc
      val startTime = System.nanoTime
      wordCount(data)
      val elapsedTime = System.nanoTime - startTime

      println(s"Sample on plain text with ${limit} characters")
      println(s"Elapsed time: ${elapsedTime} ns\n")
    }
  }
}

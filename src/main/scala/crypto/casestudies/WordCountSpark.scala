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
    val result = wordCount(content.map(Common.decryptStr(keyring.priv)))
    result.saveAsTextFile("result.txt")
    ()
  }
}

object WordCountSparkBench {
  import WordCountSpark._

  def main(args: Array[String]) = {
    import EncryptedFileOperations._

    import crypto._
    import crypto.cipher._

    import scala.io._
    import scalax.file.Path

    val startTime = System.nanoTime
    println(s"Start at: ${startTime}")
    val keyring = KeyRing.create
    val ns = List(200,400,600,800,1000,1200,1400,1600,1800,2000,2200,2400,2600,2800,3000,3200)

    val plain = Source.fromFile(args(0)).mkString
    val benchExamples = ns.map(n => (plain.take(n), n))
    val encryptTimeList = benchExamples.map { case (str, limit) =>
        val encryptStartTime = System.nanoTime
        writeEncryptedFile(encryptContent(keyring)(str), s"encrypted_test_${limit}.txt")
        System.nanoTime - encryptStartTime
    }
    val mapReduceTimeList = ns.map(n => readEncryptedFile(s"encrypted_test_${n}.txt")).map{_.map{Common.decryptStr(keyring.priv)(_)}}.map { c =>
        val mapReduceStartTime = System.nanoTime
        wordCount(c)
        System.nanoTime - mapReduceStartTime
      }
    val timeList = ns zip encryptTimeList zip mapReduceTimeList map {case ((x, y), z) => (x, y, z)}

    timeList.foreach{
      case (limit, encryptTime, mapReduceTime) =>
        println(s"Example for ${limit} characters")
        println(s"Time elapsed for encryption: ${encryptTime} ns")
        println(s"Time elapsed for map reduce on encrypted data: ${mapReduceTime} ns")
        println(s"In total: ${mapReduceTime + encryptTime} ns\n")
    }
    println(s"Total time elapsed: ${System.nanoTime - startTime} ns\n")
    
    ()
  }
}

### Why securescala?
Cloud computing offers an attractive and cost-efficient computing platform and hence it has been widely adopted by the industry and the government. At the same time, cloud computing poses a serious security challenge because sensitive data must often be outsourced to third party entities that can access the data and perform computations on them. Partial homomorphic encryption (PHE) is promising for secure computation, since it allows programs to be executed over encrypted data. Despite advances in cryptographic techniques have improved the expressivity of such programs, integration with mainstream languages has seen little progress.

### What is securescala?
SecureScala is a domain-specific language in Scala that allows expressing secure programs without requiring any cryptographic knowledge. SecureScala is based on a novel combination of free monads and free applicative functors and supports parallel execution and static analyzability.

### Execution model

Figure shows an execution model of a program utilizing a third party cloud service. 
* The user submits a program for execution through the client interface. 
* The client deploys the program to the (untrusted) cloud service (along with any required encrypted data) which executes the program over encrypted data. 
* The cloud service can utilize a trusted service to overcome limitations of PHE, by re-encrypting data as necessary, i.e., converting between encryption schemes. 
* Upon program completion, the encrypted results are returned to the client, decrypted and returned to the user in plain text.

### Functionality

```scala
def wordCountText(e: EncStr): CryptoM[List[(OpeStr,Int)]] = e.split("""\s+""") >>= wordCount_

def wordCount_(es: IList[EncStr]): Crypto[List[(OpeStr,Int)]] = es.traverse(toOpeStr).map(
                                                                  _.groupBy(x => x).map(_.length).toList)
 ```

### Academic publications
* Markus Hauck, Savvas Savvides, Patrick Eugster, Mira Mezini and Guido Salvaneschi _SecureScala: Scala Embedding of Secure Computations_ 2016 7th ACM SIGPLAN Symposium on Scala [[pdf]](http://www.guidosalvaneschi.com/attachments/papers/2016_SecureScala-Scala-Embedding-of-Secure-Computations_pdf.pdf)
* Markus Hauck _A Platform for Secure Distributed Programming_ Master Thesis, TU Darmstadt, 2015

### About
Securescala is a project at the Software Technology Group - Technical University of Darmstadt, Germany. Many people contributed to the project:

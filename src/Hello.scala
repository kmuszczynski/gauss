import java.io.{File, PrintWriter}

import scala.io.Source

object Hello extends App {
  println("Hello there!")
  val q1: Wymierna = Wymierna(2,3)
  val q2: Wymierna = Wymierna(1,4)
  val q3: Wymierna = Wymierna(12,48)
  val q4: Wymierna = Numeric[Wymierna].plus(q1,q2)

  println(q3)
//  println(q3 - q2)
//  println(q1 * q2)
//  println(q1 / q2)
//
//  val t = new MojaMacierz[Wymierna](3)

  /*
  val test = new MojaMacierz[Double](5)
//  test.generateRandomNumbersFile()

  val fileObject = new File("wyniki.txt")
  val printWriter = new PrintWriter(fileObject)

  for (i <- 1 to 10) {
    val rozmiar = i * 100
    val TD = new MojaMacierz[Double](rozmiar)
    val TF = new MojaMacierz[Float](rozmiar)
//    println("Double")
//    println(TD.testuj())
//    println("Float")
//    println(TF.testuj())
//    println("#" * 60)
    val linia = "Double\n" + TD.testuj() + "Float\n" + TF.testuj() + ("#" * 60) + "\n"
    printWriter.write(linia)
  }
  printWriter.close()
   */
  println("General Kenobi")
}

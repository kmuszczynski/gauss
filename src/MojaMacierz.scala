import java.io.{File, PrintWriter}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.Random

//noinspection SpellCheckingInspection
class MojaMacierz[@specialized(Double, Float) T:ClassTag](val rozmiar: Int)(implicit num: Numeric[T]) {
  import num._
  var tab: Array[Array[T]] = Array.ofDim[T](rozmiar, rozmiar+1)
  var wektor: Array[T] = Array.ofDim[T](rozmiar)
  var wektorRóżnic: Array[T] = Array.ofDim[T](rozmiar)
//  type Zmiana = List[Object]
//  def Zmiana = List[String,Int,Int]
  val historiaZmian: mutable.Stack[List[Any]] = new mutable.Stack[List[Any]]
  val kolejkaZmian: mutable.Queue[List[Any]] = new mutable.Queue[List[Any]]
  //Dlaczego new Stack jest potrzebne? (W przeciwnym razie rzuca "MojaMacierz must implement abstract historiaZmian or ...")
  //Dlaczego nie da się new List, a new Stack już tak?

  //FAKTYCZNY ALGORYTM GAUSSA I SKLADNIKI

  //PIERWSZY ETAP GAUSSA
  def pierwszyEtapGaussaBezWyboru (): Unit = {
    for {
      i <- 0 until rozmiar
    } {
      if(tab(i)(i) == 0) {
        znajdźNajwiększyWKolumnie(i,i)
      }
      jedenKrokGaussa(i,i)
    }
  }

  def pierwszyEtapGaussaZWyboremCzęściowym (): Unit = {
    for {
      i <- 0 until rozmiar
    } {
      znajdźNajwiększyWKolumnie(i,i)
      jedenKrokGaussa(i,i)
    }
  }

  def pierwszyEtapGaussaZWyboremPełnym(): Unit = {
    for {
      i <- 0 until rozmiar
    } {
      znajdźNajwiększyWMinorze(i,i)
      jedenKrokGaussa(i,i)
    }
  }
  //END PIERWSZY ETAP

  //ETAP DRUGI
  def etapDrugi (): Unit = {
    for(i <- 1 until rozmiar) {
      odejmijOdWszystkichPowyżej(rozmiar-i,rozmiar-i)

    }
  }
  def odejmijOdWszystkichPowyżej (row: Int, col: Int): Unit = {
    for(i <- 0 until row) {
      tab(row-i-1)(rozmiar) -= tab(row)(rozmiar) * tab(row-i-1)(col)
    }
  }
  //END ETAP DRUGI

  def gauss (wybór: String): Unit = {
    wybór match {
      case "częściowy" =>
        pierwszyEtapGaussaZWyboremCzęściowym()
        etapDrugi()
      case "pełny" =>
        pierwszyEtapGaussaZWyboremPełnym()
        etapDrugi()
//        unswapAll()
        swapWektorToMatchMatrix()
      case _ =>
        pierwszyEtapGaussaBezWyboru()
        etapDrugi()
    }
  }

  //WYBORY ROZNE
  def znajdźNajwiększyWKolumnie (row: Int, col: Int): Unit = {
    if(row < rozmiar-1) {
      var listBuffer = new ListBuffer[T]()
      for (i <- row + 1 until rozmiar) {
        listBuffer += abs(tab(i)(col))
      }
      val lista = listBuffer.toList
      swapRows(lista.indexOf(lista.max) + row + 1, row)
    }
  }

  def znajdźNajwiększyWKolumnieIOddajIndex (row: Int, col: Int): Int = {
      var listBuffer = new ListBuffer[T]()
      for (i <- row + 1 until rozmiar) {
        listBuffer += abs(tab(i)(col))
      }
      val lista = listBuffer.toList
      lista.indexOf(lista.max)+row+1
  }

  def znajdźNajwiększyWMinorze (row: Int, col: Int): Unit = {
    if(row < rozmiar-1) {
      var listBufferIndexów = new ListBuffer[Int]()
      var listBufferWartości = new ListBuffer[T]()
      for (i <- col until rozmiar) {
        val index = znajdźNajwiększyWKolumnieIOddajIndex(row,i)
        listBufferIndexów += index
        listBufferWartości += tab(index)(i)
      }
      val listaIndexów = listBufferIndexów.toList
      val listaWartości = listBufferWartości.toList
      val targetCol = listaWartości.indexOf(listaWartości.max)
      val targetRow = listaIndexów.apply(targetCol)

      swapRows(row,targetRow)
      swapCols(col,targetCol + col)
    }
  }
  //END WYBORY

  //JEDEN KROK Z GAUSSA
  def jedenKrokGaussa (row: Int, col: Int): Unit = {
    uzyskajJedynkęNaPrzekątnej(row,col)
    for (i <- row+1 until rozmiar) {
      subtractRow(i,row,tab(i)(col))
    }
  }

  def uzyskajJedynkęNaPrzekątnej (i: Int, j: Int): Unit = {
    val mnożnik: T = odwrotność(tab(i)(j))
    for (iter <- j to rozmiar) {
      tab(i)(iter) *= mnożnik
    }
  }

  def zero(): T = {
    tab(0)(0) match {
      case d: Double =>
        0.asInstanceOf[Double].asInstanceOf[T]
      case _ =>
        0.asInstanceOf[Float].asInstanceOf[T]
    }
  }

  //Tę wersję wygenerował IntelliJ, bo mu się nie podobały ify, mądrala
  def odwrotność(t: T): T = {
    t match {
      case d: Double =>
        (1.asInstanceOf[Double] / d).asInstanceOf[T]
      case _ =>
        (1.asInstanceOf[Float] / t.asInstanceOf[Float]).asInstanceOf[T]
    }
  }

//  def odwrotność(t: T): T = {
//    if(t.isInstanceOf[Double]) {
//      (1.asInstanceOf[Double] / t.asInstanceOf[Double]).asInstanceOf[T]
//    } else {
//      (1.asInstanceOf[Float ] / t.asInstanceOf[Float ]).asInstanceOf[T]
//    }
//  }
  //END JEDEN KROK GAUSSA

  //MNOŻENIE WIERSZY
  def multiplyRow (index: Int, factor: T): Unit = {
    for(i <- 0 to rozmiar) {
      tab(index)(i) *= factor
    }
  }

  //ODEJMOWANIE WIERSZY
  def subtractRow (zmniejszTen: Int, oTyle:Int, mnożnik:T = 1.asInstanceOf[T]): Unit = {
    for(i <- 0 to rozmiar) {
      tab(zmniejszTen)(i) -= tab(oTyle)(i) * mnożnik
    }
  }

  //ZAMIANY I ODMIANY
  def swapRows (a: Int, b:Int, unswap:Boolean = false): Unit = {
    val temp: Array[T] = tab(a)
    tab(a) = tab(b)
    tab(b) = temp

    if(!unswap) {
      historiaZmian.push(List("row", a, b))
      kolejkaZmian.enqueue(List("row", a, b))
    }
  }

  def swapCols (a: Int, b:Int, unswap:Boolean = false): Unit = {
    for (i <- 0 until rozmiar) {
      swapItemsInCols(i,a,b)
    }
    if (!unswap) {
      historiaZmian.push(List("col",a,b))
      kolejkaZmian.enqueue(List("col",a,b))
    }
  }

  def swapItemsInCols (row: Int, a: Int, b:Int): Unit = {
    val temp: T = tab(row)(a)
    tab(row)(a) = tab(row)(b)
    tab(row)(b) = temp
  }

  def unswapAll (): Unit = {
    while(historiaZmian.nonEmpty) {
      historiaZmian.pop() match {
        case List("row",a: Int,b: Int) => swapRows(a,b,unswap = true)
        case List("col",a: Int,b: Int) => swapCols(a,b,unswap = true)
      }
    }
  }

  def swapWektorToMatchMatrix (): Unit = {
    while(kolejkaZmian.nonEmpty) kolejkaZmian.dequeue() match {
      case List("col",a: Int,b: Int) =>
        val temp: T = wektor(a)
        wektor(a) = wektor(b)
        wektor(b) = temp
      case _ =>
    }
  }
  //END ZAMIANY

  //SHENANIGANY Z GENROWANIEM WEKTORA SZUKANEGO
  def generujWektorSzukany (): Unit = {
    for (i <- 0 until rozmiar) {
      wektor(i) = zero()
    }

    for {
      i <- 0 until rozmiar
      j <- 0 until rozmiar
    } {
      wektor(i) += tab(i)(j) * tab(j)(rozmiar)
    }

    for (i <- 0 until rozmiar) {
      val temp = wektor(i)
      wektor(i) = tab(i)(rozmiar)
      tab(i)(rozmiar) = temp
    }
  }

  def printWektor (): Unit = {
    for (i <- 0 until rozmiar) {
      println(wektor(i))
    }
  }

  def norma (): T = {
    for (i <-0 until rozmiar) {
      wektorRóżnic(i) = abs( tab(i)(rozmiar) - wektor(i) )
    }
    wektorRóżnic.max
  }

  def printNorma (): Unit = {
    println(norma())
  }

  def printRóżnice (): Unit = {
    for (i <-0 until rozmiar) {
      val różnica: T = abs( tab(i)(rozmiar) - wektor(i) )
      println(różnica)
    }
  }
  //END SHENANIGANY

  //END GAUSS

  //ROZNE TAKIE POMOCNICZE

  //String -> T, potrzebne, żeby czytać dane z pliku do T[][]
  def genericCastString (s: String): T = {
    if (tab(0)(0).isInstanceOf[Double]) {
      s.toDouble.asInstanceOf[T]
    } else {
      s.toFloat.asInstanceOf[T]
    }
  }

  //Generowanie pliku z zadaną matrycą losowych liczb
  def generateRandomNumbersFile (): Unit = {
    val fileObject = new File("cyferki.txt")
    val printWriter = new PrintWriter(fileObject)

    for(_ <- 0 until rozmiar) {
      for(_ <- 0 to rozmiar) {
        printWriter.write(s"${random()} ")
      }
      printWriter.write("\n")
    }

    printWriter.close()
  }

  //Wczytywanie z pliku do MojaMacierz.tab
  def loadArrayFromFile (): Unit = {
    val source = Source.fromFile("cyferki.txt")
    try{
      val lines = source.getLines
      for {
        i <- 0 until rozmiar
      } {
        tab(i) = lines.next().split(" ").map(genericCastString).take(rozmiar+1)
      }
    }
    finally source.close()
    //Podobno dobrym zwyczajem jest umieszczać close() w środku finally {}
  }

  //Wypisywanie tablicy do 'debugowania'
  def printTab (): Unit = {
    for (row <- tab) {
      for  (item <- row) {
        //print(s"$item ")
        print("%+2.2f ".format(item))
      }
      println()
    }
    println()
  }

  //Losowa liczba zgodnie z zadanym wzorem
  def random (): Double = {
    ( ( Random.nextDouble() * 131072 ) - 65536 ) / 65536
    //Random.nextDouble()
  }

  //END POMOCNICZE

  def testuj(): String = {


    loadArrayFromFile()
    generujWektorSzukany()
    val czasPrzedBez = System.nanoTime()
    gauss("bez")
    val czasPoBez = System.nanoTime()
    val czasWykonaniaBez = czasPoBez - czasPrzedBez
    val normaBez = norma()

    loadArrayFromFile()
    generujWektorSzukany()
    val czasPrzedCzęściowy = System.nanoTime()
    gauss("częściowy")
    val czasPoCzęściowy = System.nanoTime()
    val czasWykonaniaCzęściowy = czasPoCzęściowy - czasPrzedCzęściowy
    val normaCzęściowy = norma()

    loadArrayFromFile()
    generujWektorSzukany()
    val czasPrzedPełny = System.nanoTime()
    gauss("pełny")
    val czasPoPełny = System.nanoTime()
    val czasWykonaniaPełny = czasPoPełny - czasPrzedPełny
    val normaPełny = norma()

//    println(s"n: $rozmiar")
//    println(s"G  [[błąd: \t$normaBez ${" " * (30 - normaBez.toString.length)}czas: $czasWykonaniaBez]]")
//    println(s"PG [[błąd: \t$normaCzęściowy ${" " * (30 - normaCzęściowy.toString.length)}czas: $czasWykonaniaCzęściowy]]")
//    println(s"FG [[błąd: \t$normaPełny ${" " * (30 - normaPełny.toString.length)}czas: $czasWykonaniaPełny]]")
    s"n: $rozmiar\n"+
      s"G  [[błąd: \t$normaBez ${" " * (30 - normaBez.toString.length)}czas: $czasWykonaniaBez]]\n"+
      s"PG [[błąd: \t$normaCzęściowy ${" " * (30 - normaCzęściowy.toString.length)}czas: $czasWykonaniaCzęściowy]]\n"+
      s"FG [[błąd: \t$normaPełny ${" " * (30 - normaPełny.toString.length)}czas: $czasWykonaniaPełny]]\n"

  }

}

class Wymierna(l: Int, m: Int) extends Numeric[Wymierna]{
  private val nwd: Int = nwd(l,m)
  val licznik: Int = l / nwd
  val mianownik: Int = m / nwd

  @scala.annotation.tailrec
  private def nwd(a: Int, b: Int): Int = {
    if(b == 0) a
    else nwd(b, a%b)
  }

  override def toString: String = s"$licznik/$mianownik"
  override def equals(that: Any): Boolean = that match {
    case v: Wymierna => v.licznik == licznik && v.mianownik == mianownik
    case _ => false
  }

//  def *(q: Wymierna): Wymierna = Wymierna(licznik * q.licznik, mianownik * q.mianownik)
//  def +(q: Wymierna): Wymierna = Wymierna(licznik * q.mianownik + q.licznik * mianownik, mianownik * q.mianownik)
//  def -(q: Wymierna): Wymierna = Wymierna(licznik * q.mianownik - q.licznik * mianownik, mianownik * q.mianownik)
//  def /(q: Wymierna): Wymierna = Wymierna(licznik * q.mianownik, mianownik * q.licznik)

  override def plus(x: Wymierna, y: Wymierna): Wymierna = {
    Wymierna(x.licznik * y.mianownik + y.licznik * x.mianownik, x.mianownik * y.mianownik)
  }

  override def minus(x: Wymierna, y: Wymierna): Wymierna = {
    Wymierna(x.licznik * y.mianownik - y.licznik * x.mianownik, x.mianownik * y.mianownik)
  }

  override def times(x: Wymierna, y: Wymierna): Wymierna = {
    Wymierna(x.licznik * y.licznik, x.mianownik * y.mianownik)
  }

  override def negate(x: Wymierna): Wymierna = {
    Wymierna(-x.licznik,x.mianownik)
  }

  override def fromInt(x: Int): Wymierna = {
    Wymierna(x,1)
  }

  override def parseString(str: String): Option[Wymierna] = str match {
    case s"$a/$b" => Some(Wymierna(Integer.parseInt(a),Integer.parseInt(b)))
    case _ => None
  }

  override def toInt(x: Wymierna): Int = {
    x.licznik / x.mianownik
  }

  override def toLong(x: Wymierna): Long = {
    (x.licznik / x.mianownik).asInstanceOf[Long]
  }

  override def toFloat(x: Wymierna): Float = {
    x.licznik.asInstanceOf[Float] / x.mianownik.asInstanceOf[Float]
  }

  override def toDouble(x: Wymierna): Double = {
    x.licznik.asInstanceOf[Double] / x.mianownik.asInstanceOf[Double]
  }

  override def compare(x: Wymierna, y: Wymierna): Int = {
    val a: Int = x.licznik * y.mianownik
    val b: Int = y.licznik * x.mianownik
    Integer.compare(a, b)
  }
}

implicit object Wymierna {
  def apply(l: Int,m: Int): Wymierna = new Wymierna(l,m)
}

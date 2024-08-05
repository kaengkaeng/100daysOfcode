class Rational(x:Int, y: Int) :
  require(y>0,s"denominator must be positive, was$x/$y")
  def this(x:Int) = this(x,1)

  private def gcd(a:Int, b:Int):Int =
    if b ==0 then a else gcd(b,a%b)

  def numer = x / gcd(x,y)
  def denom = y / gcd(x,y)

  def add(r: Rational, s: Rational): Rational = {
    Rational(r.numer * s.denom + s.numer * r.denom, r.denom * s.denom)
  }


  def makeString(r:Rational):String=
    s"${r.numer}/${r.denom}"

  def mul(r:Rational) =
    Rational(numer * r.numer,denom * r.denom)

  def neg = Rational(-numer, denom)

  def less(that:Rational):Boolean =
    this.numer * that.denom < that.numer * this.denom

  def max(that:Rational) : Rational =
    if this.less(that) then that else this

  override def toString = s"$numer/$denom"

end Rational //End Markers

def removeAt[T](n:Int, xs:List[T]):List[T] =
  xs match {
    case Nil => Nil
    case y :: ys if n == 0 => ys
    case y :: ys => y :: removeAt(n - 1, ys)
  }
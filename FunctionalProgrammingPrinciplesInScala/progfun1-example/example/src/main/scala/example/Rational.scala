package example

class Rational(x: Int, y: Int):

  /**
   * Preconditions
   * */
  require(y > 0, s"denominator must be positive, was $x/$y")
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)
  private val g = gcd(x.abs,y)
  def numer = x
  def denom = y

  def add(r: Rational) =
    Rational(numer * r.denom + r.numer * denom, denom * r.denom)

  def mul(r: Rational) =
    Rational(numer * r.numer, denom * r.denom)

  def neg = Rational(-numer, denom)

  def sub(r: Rational) = add(r.neg)

  def less(that: Rational): Boolean =
    numer * that.denom < that.numer * denom

  def max(that: Rational): Rational =
    if this.less(that) then that else this

  override def toString: String = s"${numer / g}/${denom / g}"

end Rational

val x = Rational(1,3)
val y = Rational(5,7)
val z = Rational(3,2)

@main def testRational = println(x.add(y).mul(z))
@main def testRational2 = println(x.sub(y).sub(z))


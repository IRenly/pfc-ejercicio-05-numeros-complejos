package taller

/** Punto 3. Un número racional x/y guardado en su forma normalizada:
  * numerador y denominador sin factores comunes, el signo en el numerador y
  * el denominador siempre positivo. El cero se guarda como 0/1.
  *
  * Tal como está compila y las pruebas quedan en rojo.
  */
class Racional(x: Int, y: Int) {
  // Precondición del constructor: el denominador no es cero.
  require(y != 0, "el denominador es invalido" );

  // El máximo común divisor de dos enteros no negativos.
  private def mcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else (mcd(b, a % b))
  }

  // Numerador y denominador ya normalizados.
  private val divisor = mcd(x.abs, y.abs)

  def numer: Int = {
    if (y < 0) -1*(x/divisor)
    else x/divisor

  }

  def denom: Int = {
    y.abs/divisor
  }

  def +(r: Racional): Racional = {
    new Racional(numer * r.denom + r.numer * denom, denom * r.denom)
  }

  def -(r: Racional): Racional = {
    new Racional(numer * r.denom - r.numer * denom, denom * r.denom)
  }

  def *(r: Racional): Racional = {
    new Racional(numer * r.numer, denom * r.denom)
  }

  def /(r: Racional): Racional = {
    new Racional(numer * r.denom, denom * r.numer)
    }

  // Si los dos racionales representan el mismo número.
  def ==(r: Racional): Boolean = {
    numer == r.numer && denom == r.denom
  }

  def <(r: Racional): Boolean = {
    numer * r.denom < denom * r.numer
  }
  // El mayor de los dos.
  def max(r: Racional): Racional = {
    if( this < r ) r
    else this
  } // Completar

  // "n/d", o solo "n" cuando el denominador es 1.
  override def toString: String = {
    if (denom == 1) s"${numer}"
    else s"$numer/$denom"
    }
  }

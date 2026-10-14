package taller

/** Punto 4. Un número complejo r + i·i, con r la parte real e i la
  * imaginaria. Los objetos no cambian: cada operación devuelve uno nuevo.
  *
  * Tal como está compila y las pruebas quedan en rojo.
  */
class Complejos(val r: Double, val i: Double) {

  def +(otro: Complejos): Complejos = {
      new Complejos((r + otro.r ), (i + otro.i))
  }

  def -(otro: Complejos): Complejos = {
      new Complejos((r - otro.r), (i - otro.i))
  }

  def *(otro: Complejos): Complejos = {
      new Complejos(((r * otro.r) - (i * otro.i)),((r * otro.i) + (otro.r * i)))
  }
  def /(otro: Complejos): Complejos = {
      new Complejos(((r * otro.r) + (i * otro.i))/(math.pow(otro.r, 2) + math.pow(otro.i, 2)),(((otro.r * i) - (r * otro.i))/(math.pow(otro.r, 2) + math.pow(otro.i, 2))))
  }


  // "a + bi" con las dos partes redondeadas a tres decimales; si la parte
  // imaginaria es negativa, "a - bi".
  override def toString: String = {
    val rRed = Math.round(r * 1000.0) / 1000.0
    val iRed = Math.round(i.abs * 1000.0) / 1000.0
    if (i < 0) s"$rRed - ${iRed}i"
    else s"$rRed + ${iRed}i"
  }
}

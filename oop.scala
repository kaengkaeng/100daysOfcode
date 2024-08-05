import scala.util.Random

class BBaum[A] :

  private class Node(var elem: A, var left: Option[Node], var right: Option[Node])
  private var root: Option[Node] = None

  def insert(x: A): Unit =
    def step(curr: Option[Node]): Option[Node] = {
      curr match {
        case None => Some(new Node(x, None, None))
        case Some(node) =>
          if (Random.nextBoolean()) {
            node.left = step(node.left)
          } else {
            node.right = step(node.right)
          }
          Some(node)
      }
    }
    root = step(root)



class Studi(private val name: String, private val matr:Int ):
  private val studiengang: String = "BIO*Informatiks"
  private var ects: Int = 0  // var로 변경하여 값을 수정할 수 있도록 합니다.

  def study(topic: String): Unit = {
    println(this.name + " reads a book about " + topic)  // 문자열 결합 시 공백 추가
    this.ects = this.ects + 1
}

  def getMatr: Int = this.matr
  def getName: String = this.name


object Studi:
  private var matr_counter: Int = 100

  def resetMatrCounter(new_counter: Int): Unit =
    matr_counter = new_counter







/*
class Lamp(private val power:Int):
  require (power >= 0 )
  println("Hier beginnt der Primaerkonstruktor.")

  private var on: Boolean = false
  println("Hier endet der Primaerkonstruktor.")

  def turnOn():Unit =
    this.on = true

  def turnOff():Unit =
    this.on = false

  def isOn:Boolean = this.on
  def getPower:Int = this.Power
  def change(power:Int):Unit =
    if power >= 0 && this.!on then this.power = power
    */


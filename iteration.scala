
//fibonacchi formel
//f_0 = 0 , f_1 =1
//f_n = f(n-1)+ f(n-2)

//1) Rekursion
def fib(n:Int):Int =
  n match
    case 0 => 0
    case 1 => 1
    case _ => fib(n-1) + fib(n-2)

def fib_tail(n:Int): Int = {
  def step(acc1:Int, acc2:Int, n:Int) :Int =
    n match
      case 0 => acc2
      case 1 => acc1
      case _ => step(acc1+acc2, acc1, n-1)
  step(1,0,n)
}

// zahlen sind umgedrehte zahlen liefert.
// Int -> String (toString) ->Int (toInt)
def reverseZahl(n: Int): Int =
  def step(acc: String, n: Int): String =
    n match
      case 0 if acc.isEmpty => "0" // n이 처음부터 0인 경우 처리 // (1)Anker
      case 0 => acc // (2)Anker
      case _ => step(acc + (n % 10).toString, n / 10) //Rekursionsschritte

  step("", n).toInt

// a: =1

/*
def  LoveVsFriendship(s: String): Int = {
  val alphabet = "abcdefghijklmnopqrstuvwxyz"
  var value = 0

  for (i <- s) {
    value += alphabet.indexOf(i) + 1
  }

  value
}
*/

// 1. replace bekommt eine Zahl n und zwei Ziffern a und b und ersetzt alle Vorkommen von a in n durch b.
// 1) tailrekursion
def replace[A](list:List[A], a:A, b:A):List[A] = {
  def help(acc:List[A], a:A, b:A, list:List[A]):List[A] =
    list match {
      case Nil => acc
      case x :: xs if x == a => help (acc::: List(b), a,b,xs) //  a== x, dann a ersetzt durch b
      case x :: xs => help(acc:::List(x), a,b,xs) // a != x dann x hinzufuegen.
    }
  help(Nil, a,b, list)
}

//2) map
def map_replace[A](a:A, b:A, list:List[A]):List[A]=
  list.map(x => if (x == a) b else x )

// List
def solve(lst: List[Int]): List[Int] = {
  def step(acc: List[Int], lst: List[Int]): List[Int] =
    lst match {
      case Nil => acc
      case x :: Nil => x :: acc
      case x :: y :: xs if x == y => step(acc, xs)
      case x :: y :: xs => step(x :: acc, xs)
    }

  step(Nil, lst).reverse
}

// 입력받은 두 수의 합의 이진수 출력하는 함수
def addBinary(a: Int, b: Int): String = {
  def step(acc: String, sum: Int): String = {
    sum match {
      case 0 => acc
      case _ => step(((sum % 2).toString) + acc, sum / 2)
    }
  }
  step("", a + b)
}

def takeNumbers(f: Int => Int,n : Int, list : List[Int], i : Int):List[Int]=
  // Funktion bestimmt ob das Ergebnis einer Funktion einen wert i überschreitet
  // und gibt eine Liste der ersten n Werte die dies tun zurück
  list match
    case Nil => Nil
    case x::xs if n == 0 => Nil
    case x::xs if f(x) > i => x :: takeNumbers(f,n-1,xs,i) // Wenn f(x) größer dem Wert i wird x der Ausgabeliste hinzugefügt
    case x::xs => takeNumbers(f,n,xs,i)//// Wenn f(x) kleiner gleich dem Wert i wird x der Ausgabeliste nicht hinzugefügt

def quersumme( i : Int): Int =
//Funktion berechnet die Quersumme einer Zahl
  def help(acc : Int, i: Int): Int =
    i match
      case 0 => acc
      case _ => help(acc + i%10,i/10)
  help(0,i)


// sum_if( _ <=5, 234516)
// 5보다 작은 값은 addieren
def sum_if(p: Int => Boolean, num: Int): Int = {
  def loop(n: Int): Int =
    n match {
      case 0 => 0
      case x if p(x % 10) => (x % 10) + loop(x / 10)
      case _ => loop(n / 10)
  }

  loop(math.abs(num))
}




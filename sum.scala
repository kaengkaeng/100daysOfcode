
def twoSum(p: Int => Boolean, numbers: List[Int], target: Int): (Int, Int) = {
  def loop(numbers: List[Int]): (Int, Int) =
    numbers match {
      case Nil => throw new NoSuchElementException("No such pair found")
      case x :: Nil => throw new NoSuchElementException("No such pair found")
      case x :: y :: xs if p(x + y) => (x, y) // 조건을 만족하는 숫자 쌍을 찾았을 때 해당 숫자 쌍을 반환합니다.
      case _ :: y :: xs => loop(y :: xs) // 조건을 만족하지 않는 숫자 쌍이면 다음 숫자 쌍을 검사합니다.
    }

  loop(numbers)
}



// 기본적으로 List ist immutable
def partition[A](p:A => Boolean, list:List[A]):(List[A], List[A])=
  def step(p:A=> Boolean, acc1:List[A], acc2:List[A], list:List[A]):(List[A], List[A])=
    list match
      case Nil => (acc1, acc2)
      case x :: xs if p(x) => step(p, acc1 ::: List(x), acc2, xs)
      case x :: xs => step(p, acc1, acc2 ::: List(x), xs)
  step(p, Nil, Nil, list)


//리스트 합치기
def lconcat[A](list:List[List[A]]):List[A]=
    list match
      case Nil => Nil
      case x :: xs => x ::: lconcat(xs)


// 리스트 안에 원소에 f 함수 식을 적용했을때 최대값이 나오는 원소 출력
def argmax(xsl: List[Int], f: Int => Int): Int = {
  def help(list: List[Int], f: Int => Int, max: Int): Int =
    list match {
      case Nil => max
      case x :: xs if f(x) > f(max) => help(xs, f, x)
      case _ :: xs => help(xs, f, max)
    }
  help(xsl, f, Int.MinValue) // 초기 max 값을 Int의 최솟값으로 설정합니다.
}

/*
// 주어진 숫자 n을 확장된 형태로 반환하는 함수 : (input "12" -> output "10+2")
def expandedForm(n: Long): String = {
  def step(n: Long, acc1: Long, acc2): String =
    n match {
      case 0 => acc.toString
      case _ => step(n / 10, acc + (n % 10), acc2*10)
    }
  step(n, 0).reverse // 숫자를 뒤집어야 올바른 형태로 출력됩니다.
}
*/

//scala
//def auswahl(arr: Array[Int]): Int =
  //arr.foldLeft(Int.MaxValue)((min, x) => if (x < min) x else min)

def aufwahl(arr: Array[Int]): Int = {
  if (arr.isEmpty) throw new IllegalArgumentException("Array cannot be empty")
  var minimum = arr(0)
  for (i <- 0 until arr.length) {
    if (arr(i) < minimum) {
      minimum = arr(i)
    }
  }
  minimum
}



def countDigit(n: Int): Int = {
  var count = 0
  var remaining = n  // 입력값을 임시 변수에 저장

  while (remaining > 0) {
    val digit = remaining % 10  // 현재 자릿수를 계산
    if (digit >= 0 && digit < 10) // digit이 0에서 9 사이에 있는지 확인
      count += 1
    remaining = remaining / 10  // 다음 자릿수로 이동
  }

  count
}

def countDigitPm(n: Int): Int =
  def step(acc: Int, n: Int): Int =
    n match {
      case 0 => acc
      case _ if (n % 10) < 10 => step(acc + 1, n / 10)
      case _ => step(acc, n / 10)
  }
  step(0, n)

def quersumme(n: Int): Int = {
  var summe = 0
  var rest = n
  while (rest > 0) {
    val digit = rest % 10
    if (digit < 10) // digit이 0에서 9 사이에 있는지 확인
      summe += digit
    rest = rest / 10
  }
  summe // while 루프 밖에서 반환
}

def quersummeP(n:Int):Int =
  def step(acc:Int, n:Int):Int =
    n match
      case 0 => acc
      case _ if (n%10) < 10 => step(acc+(n%10), n/10)
      case _ => step(acc, n/10)
  step(0,n)

//faltung
def qs(n: List[Int]): Int =
  n.foldLeft(0)((acc, n) => if (n % 10 < 10) acc + (n % 10) else acc)


def spanne(n: Int): (Int, Int) = {
  def step(gro: Int, kle: Int, n: Int): (Int, Int) =
    n match {
      case 0 => (gro, kle)
      case _ => step(math.max(gro, n % 10), math.min(kle, n % 10), n / 10)
    }
  if (n == 0) (0, 0)
  else step(Int.MinValue, Int.MaxValue, math.abs(n))
}


def xors(list1: List[Int], list2: List[Int]): List[Int] = {
  var erg: List[Int] = List()
  for (i <- 0 until list1.length) {
    if (!list2.contains(list1(i))) erg = list1(i) :: erg
  }
  for (j <- 0 until list2.length) {
    if (!list1.contains(list2(j))) erg = list2(j) :: erg
  }
  erg
}

enum Shape:
  case Rectangle(base: Double, height: Double) // Rectangle로 수정
  case Circle(radius: Double)

def area(sh: Shape): Double =
  import Shape.*
  sh match
    case Rectangle(b, h) => b * h
    case Circle(r) => math.Pi * r * r

def divide(a:Double, b:Double): Option[Double] =
  if b == 0.0 then None
  else Some(a/b)


def universe(num: Int): Option[Boolean] = {
  val numStr = num.abs.toString  // 음수일 경우 절대값을 취하고 문자열로 변환
  if (numStr.contains("42")) Some(true)
  else Some(false)
}

def ensureQuestion(s: String): String =
  s match {
    case "" => "?"                   // 빈 문자열일 경우 "?" 반환
    case _ if s.contains("?") => s   // 문자열에 "?"이 포함되어 있으면 원래 문자열 반환
    case _ => s + "?"                // 그 외의 경우 문자열 끝에 "?" 추가
  }



/*
  Precondition: All Elements of List must be comparable, their Datatype must be in TypeClass Ordering
Effect: None
Result: True or False is delivered, True in case of sorted List, False in case of non-sorted List.
*/


def isSorted[T: Ordering](list: List[T]) : Boolean =
  def step(list: List[T]) : Boolean =
    val ord = summon[Ordering[T]]
    import ord.mkOrderingOps //Import für Vergleichsoperation
    list match
      case Nil => true //Wenn leer, dann sortiert
      case x :: Nil => true //Wenn nur 1 Element, dann sortiert
      case x::y::xs if x<=y => step(y::xs) //Wenn 1. Element kleiner als 2., dann wird hilfsfunktion rekursiv mit übrigbleibener Liste aufgerufen, da bis hier sortiert
      case _ => false //x>=y --> Liste ist nicht sortiert
  step(list) //Hilfsfunktionsaufruf

def pp[T:Ordering](xs:List[T]):Option[Boolean] =
  val ord = summon[Ordering[T]]
  import ord.mkOrderingOps //Import für Vergleichsoperation
  xs match
    case Nil => Some(true)
    case x :: y::xs if (x <= y) => Some(true)
    case _ => Some(false)

def operation(f:(Int,Int)=>Int):Unit

val add = (x:Int, y:Int) => {x+y}

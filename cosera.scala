def square(x:Double) = x*x
def abs(x:Double) = if x >0 then x else -x

def sqrt(x:Double) = {
  def sqrtIter(guess: Double, x: Double): Double = {
    if isGoodEnough(guess, x) then guess
    else sqrtIter(improve(guess, x), x)
    // guess가 충분히 정확할 때까지 반복
  }

  def improve(guess: Double, x: Double) = {
    (guess + x / guess) / 2
    // 추정값 개선
  }

  def isGoodEnough(guess: Double, x: Double) = {
    abs(square(guess) - x) < 0.001
    // 추정값의 정확성을 평가하고, 그렇지 않으면 정확할때까지 반복
  }

  sqrtIter(1.0,x) // sqrt 함수가 호출되면 이 함수를 호출하여 제곱근 계산 시작
  }

@main def test = println(sqrt(2))

def factorial(n:Int):Int =
  if n == 0 then 1 else n*factorial(n-1)
  // anker = 0, 종료조건

//higer order functions

def sumInts(a:Int, b:Int):Int =
  if a > b then 0 else a + sumInts( a+1, b)

def cube(x:Int):Int = x*x*x

def sumCubes(a:Int, b:Int):Int =
  if a > b then 0 else cube(a) +sumCubes(a+1, b)
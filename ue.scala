
//xs =

def kmcm(xs: List[Int]): List[Boolean] = {
  xs.map(n => Math.pow(2, -n) <= 1) // 2^-2 표현하는 방법
}


//2) map
def map_replace[A](a:A, b:A, list:List[A]):List[A]=
  list.map(x => if (x == a) b else x )



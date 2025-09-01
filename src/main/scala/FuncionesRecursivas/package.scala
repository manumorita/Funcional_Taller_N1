package object FuncionesRecursivas {
  
  def maxLin(l: List[Int]): Int = {
    if (l.tail.isEmpty)
      l.head
    else {
      val maxTail = maxLin(l.tail)
      if (l.head > maxTail)
        l.head
      else
        maxTail
    }
  }

  def maxIt(l: List[Int]): Int = {
    def iterator(tailList: List[Int], maxSoFar: Int): Int = {
      if (tailList.isEmpty)
        maxSoFar
      else {
        val nuevoMax = if (tailList.head > maxSoFar) tailList.head else maxSoFar
        iterator(tailList.tail, nuevoMax)
      }
    }

    iterator(l.tail, l.head)
  }

  def movsTorresHanoi(n: Int): BigInt = {
    return 0
  }

  def torresHanoi(n: Int, t1: Int, t2: Int, t3: Int): List[(Int, Int)] = {
    return 0

  }


}

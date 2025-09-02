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

  def movsTorresHanoi (n: Int):BigInt = {
    if (n == 1)
      n
    else {
      val cantMov = (2 * movsTorresHanoi(n - 1)) + 1
      cantMov
    }
  }

  def torresHanoi (n:Int, t1:Int, t2:Int, t3:Int): List[(Int, Int)] = {
    if (n == 1)
      List((t1, t3))
    else {
      val mov1 = torresHanoi(n-1, t1, t3, t2)
      val moveBigDisk = List((t1, t3))
      val mov2 = torresHanoi(n-1, t2, t1, t3)

      val movements = mov1 ++ moveBigDisk ++ mov2

      movements
    }
  }


}

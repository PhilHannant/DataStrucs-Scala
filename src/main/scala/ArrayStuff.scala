import java.util
import java.util.HashMap

import scala.collection.mutable

object ArrayStuff extends App {

  println(largestZeroSum(Array(15, -2, 2, -8, 1, 7, 10, 23)))



  def largestZeroSum(arr: Array[Int]) = {
    var sum = 0
    var max = 0
    var hm: mutable.HashMap[Int, Int] = new mutable.HashMap()
    for(i <- 0 to arr.length-1) {
      println(sum)
      sum += arr(i)
      if (!hm.contains(sum)) {
        hm.put(sum, i)

      } else {
        hm.get(sum) match {
          case Some(value) => max = Math.max(i - value, max)
          case None => max
        }
      }
    }
    max
  }
}

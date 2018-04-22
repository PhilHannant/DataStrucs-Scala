import java.util
import java.util.HashMap

import scala.collection.mutable

object ArrayStuff extends App {

  println(largestZeroSum(Array(15, -2, 2, -8, 1, 7, 10, 23)))
  println(maxSumSubArray(Array(-2, -3, 4, -1, -2, 1, 5, -3)))



  def largestZeroSum(arr: Array[Int]) = {
    var sum = 0
    var max = 0
    var hm: mutable.HashMap[Int, Int] = new mutable.HashMap()
    for(i <- 0 to arr.length-1) {
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

  def maxSumSubArray(arr: Array[Int]): Int = {
    var currenMax: Int = arr(0)
    var maxHere: Int = arr(0)

    for(i <- 1 to arr.length-1){
      maxHere+= arr(i)
      maxHere = Math.max(arr(i), maxHere)
      currenMax = Math.max(currenMax, maxHere)
    }

    currenMax
  }
}

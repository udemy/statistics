package com.udemy

import org.apache.commons.math3.stat.StatUtils
import scala.Numeric.Implicits._

package object statistics {

  def sumOfSquaredDeviations[T: Numeric](data: Seq[T]): Double = {
    val doubles = data.map(_.toDouble)
    val dataMean = StatUtils.mean(doubles.toArray)
    doubles.fold(0D) {
      (b, x) => b + math.pow(x - dataMean, 2)
    }
  }
}

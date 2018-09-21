package com.udemy.statistics

package object distribution {
  case class Interval(lower: Double, upper: Double)

  case class Percentile(value: Double) {
    require(value >= 0 && value <= 1, "percentile must be a value between 0 and 1")

    def bounds: Interval = {
      val lower = (1 - value) / 2
      val upper = value + lower
      Interval(lower, upper)
    }
  }

  object CredibleInterval extends Enumeration {
    type CredibleInterval = Value
    val HighestDensityInterval: CredibleInterval = Value
  }
}

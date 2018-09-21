package com.udemy.statistics.distribution

import com.udemy.statistics.distribution.CredibleInterval.CredibleInterval
import org.apache.commons.math3.distribution.NormalDistribution

class Normal(μ: Double, σ: Double) {
  def credibleInterval(percentile: Percentile, intervalType: CredibleInterval = CredibleInterval.HighestDensityInterval): Interval = {
    if (σ <= 0) Interval(Double.NaN, Double.NaN)
    else {
      Interval(new NormalDistribution(μ, σ).inverseCumulativeProbability(percentile.bounds.lower),
        new NormalDistribution(μ, σ).inverseCumulativeProbability(percentile.bounds.upper))
    }
  }
}

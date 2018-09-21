/*
Copyright 2018 Udemy, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    https://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package com.udemy.statistics.distribution

import com.udemy.statistics.distribution.CredibleInterval.CredibleInterval
import org.apache.commons.math3.distribution.NormalDistribution

case class Normal(μ: Double, σ: Double) {

  def credibleInterval(percentile: Percentile, intervalType: CredibleInterval = CredibleInterval.HighestDensityInterval): Interval = {
    if (σ <= 0) Interval(Double.NaN, Double.NaN)
    else {
      Interval(new NormalDistribution(μ, σ).inverseCumulativeProbability(percentile.bounds.lower),
        new NormalDistribution(μ, σ).inverseCumulativeProbability(percentile.bounds.upper))
    }
  }
}

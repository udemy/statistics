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

import org.apache.commons.math3.distribution.NormalDistribution
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class NormalSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "normal Highest Density Interval" should "be the same as the central interval in a normal or NaN for invalid inputs" in {

    forAll { (mean: Double, standardDeviation: Double, percentile: Double) =>
      whenever(percentile >= 0 && percentile <= 1) {
        val normal = Normal(mean, standardDeviation)
        val interval = normal.credibleInterval(Percentile(percentile))

        if (standardDeviation < 0) {
          interval.lower.isNaN shouldBe true
          interval.upper.isNaN shouldBe true
        } else {
          val normalLower = new NormalDistribution(mean, standardDeviation).inverseCumulativeProbability((1 - percentile) / 2)
          val normalUpper = new NormalDistribution(mean, standardDeviation).inverseCumulativeProbability(percentile + (1 - percentile) / 2)
          interval.lower == normalLower &&
            interval.upper == normalUpper
        }
      }
    }
  }

}

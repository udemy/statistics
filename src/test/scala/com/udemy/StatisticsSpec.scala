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

package com.udemy

import org.apache.commons.math3.stat.StatUtils
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class StatisticsSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "sumOfSquaredDeviations" should "return the sum of squared deviations" in {

    forAll((Gen.listOf[Double](Arbitrary.arbDouble.arbitrary), "doubles")) {
      doubles: List[Double] =>
        val result = statistics.sumOfSquaredDeviations(doubles)

        if (doubles.isEmpty) {
          result.isNaN shouldBe true
        } else {
          val dataMean = StatUtils.mean(doubles.toArray)
          val sumOfSquares = doubles.fold(0D) {
            (b, x) => b + math.pow(x - dataMean, 2)
          }
          if (sumOfSquares.isNaN) result.isNaN shouldBe true
          else result shouldEqual sumOfSquares
        }
    }
  }
}

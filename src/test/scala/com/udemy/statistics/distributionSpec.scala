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

package com.udemy.statistics

import com.udemy.statistics.distribution.Percentile
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class distributionSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "Percentile" should "return Percentile or throw exception when value is not between 0 and 1" in {
    forAll {
      percentile: Double =>
        if (percentile < 0 || percentile > 1)
          assertThrows[IllegalArgumentException](Percentile(percentile))
        else
          Percentile(percentile).value == percentile
    }
  }

  "Percentile bounds" should "return appropriate bounds for percentile" in {
    forAll {
      percentile: Double =>
        if (percentile < 0 || percentile > 1)
          assertThrows[IllegalArgumentException](Percentile(percentile))
        else {
          val p = Percentile(percentile)
          p.bounds.lower shouldEqual (1 - percentile) / 2D
          p.bounds.upper shouldEqual percentile + ((1 - percentile) / 2D)
        }
    }
  }
}

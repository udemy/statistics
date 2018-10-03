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

package object distribution {
  case class Interval(lower: Double, upper: Double)

  case class Percentile(value: Double) {
    require(value >= 0 && value <= 1, "percentile must be a value between 0 and 1")

    def bounds: Interval = {
      val lower = (1 - value) / 2D
      val upper = value + lower
      Interval(lower, upper)
    }
  }

  object CredibleInterval extends Enumeration {
    type CredibleInterval = Value
    val HighestDensityInterval: CredibleInterval = Value
  }
}

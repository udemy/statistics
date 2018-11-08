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

import scala.Numeric.Implicits._

package object statistics {

  case class Probability(value: Double) {
    require(value >= 0 && value <= 1 || value.isNaN, "Probability must be between 0 and 1 inclusive.")
  }

  def mean[T: Numeric](vec: Seq[T]): Double = vec.sum.toDouble / vec.length.toDouble

  def sumOfSquaredDeviations[T: Numeric](data: Seq[T]): Double = {
    if (data.isEmpty) Double.NaN
    else {
      val dataMean = mean(data)
      data.map(_.toDouble).fold(0D) {
        (b, x) => b + math.pow(x - dataMean, 2)
      }
    }
  }
}

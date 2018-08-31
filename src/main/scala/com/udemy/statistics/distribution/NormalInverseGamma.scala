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

import com.udemy.statistics._
import org.apache.commons.math3.stat.StatUtils
import scala.Numeric.Implicits._

/**
  * When using this for as a conjugate prior the values should be as follows
  * @param μ mean was estimated from ν observations with sample mean μ
  * @param ν ν observations from which sample mean was estimated
  * @param α 1/2 of sample size from which variance was estimated
  * @param β 1/2 sum of squared deviations
  */
case class NormalInverseGamma(μ: Double, ν: Double, α: Double, β: Double) {

  def μMean: Double = μ
  def μMode: Double = μ
  def σSquaredMean: Double = if (α > .5) β / (α - .5) else Double.PositiveInfinity
  def σSquaredMode: Double = β / (α + 1.5)

  def posterior(likelihood: Seq[Double]): NormalInverseGamma = {
    val array = likelihood.toArray
    posterior(
    NormalInverseGamma(
      StatUtils.mean(array),
      likelihood.length,
      likelihood.length / 2D,
      sumOfSquaredDeviations(array) / 2D
    ))
  }

  def posterior(likelihood: NormalInverseGamma): NormalInverseGamma = {
    if (likelihood.ν < 1) this
    else {
      if (ν == 0) likelihood
      else {
        NormalInverseGamma(
          (ν * μ + likelihood.ν * likelihood.μMean) / (ν + likelihood.ν),
          ν + likelihood.ν,
          α + likelihood.α,
          β + likelihood.β + ((likelihood.ν * ν) / (likelihood.ν + ν)) * (math.pow(likelihood.μMean - μ, 2) / 2D)
        )
      }
    }
  }
}

object NormalInverseGamma {

  def apply[T: Numeric](likelihood: Seq[T]): NormalInverseGamma = {
    val doubles = likelihood.map(_.toDouble).toArray
    NormalInverseGamma(
      StatUtils.mean(doubles),
      doubles.length,
      doubles.length / 2,
      sumOfSquaredDeviations(doubles) / 2
    )
  }
}
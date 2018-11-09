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

package com.udemy.statistics.algorithm

import com.udemy.statistics.distribution.Normal
import com.udemy.statistics.Probability
import org.apache.commons.math3.distribution._
import scalaz.NonEmptyList

import scala.annotation.tailrec

object MonteCarlo {

  def highestValue[T:Numeric](options: Map[String, NonEmptyList[T]], draws: Int): Map[String, Probability] =
    highestValue(options, draws, (nel: NonEmptyList[T]) => Sampling.randomDraw(nel))

  private def highestValue[T:Numeric](options: Map[String, NonEmptyList[T]], draws: Int, sampler: NonEmptyList[T] => T): Map[String, Probability] =
    probabilitiesOfBeingBest[NonEmptyList[T],T](
      sampler,
      options,
      draws)

  def highestValue(options: Map[String, AbstractRealDistribution], draws: Int, seed: Option[Long] = None): Map[String, Probability] = {
    probabilitiesOfBeingBest[AbstractRealDistribution,Double](
      (d: AbstractRealDistribution) => {
        seed.foreach(s => d.reseedRandomGenerator(s))
        d.sample
      },
      options,
      draws
    )
  }

  private def probabilitiesOfBeingBest[T,U:Numeric](sampler: T => U,
                                                    options: Map[String, T],
                                                    draws: Int): Map[String, Probability] = {
    def randomSampleScores(sampler: T => U, options: Map[String, T]): Map[String, Double] = {
      case class Result(key: String, value: U)

      val randomSample = options.map(o => Result(o._1, sampler(o._2)))
      val max = randomSample.map(_.value).max
      val winnerCount = randomSample.count(_.value == max).toDouble

      randomSample.map(s => (s.key, if (s.value == max) 1D / winnerCount else 0D)).toMap
    }

    // This uses Monte Carlo sampling to determine winners
    @tailrec
    def cumulateWins(sampler: T => U, options: Map[String, T], draws: Int, totalWins: Map[String, Double]): Map[String, Double] = {
      if (draws == 0) totalWins
      else {
        cumulateWins(sampler, options, draws - 1, randomSampleScores(sampler, options).foldLeft(totalWins) {
          (tw, current) => tw + (current._1 -> (tw(current._1) + current._2))
        })
      }
    }

    if (draws <= 0 || options.isEmpty) options.map(o => (o._1, Probability(Double.NaN)))
    else {
      val totalWins: Map[String, Double] = options.map(o => (o._1, 0D))
      cumulateWins(sampler, options, draws, totalWins).map(t => (t._1, Probability(t._2 / draws)))
    }
  }
}

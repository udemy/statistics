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

import com.udemy.statistics.algorithm.MonteCarlo._
import com.udemy.statistics.Probability
import com.udemy.statistics.distribution.Normal
import org.apache.commons.math3.distribution.{AbstractRealDistribution, ConstantRealDistribution, GammaDistribution, NormalDistribution}
import org.scalatest.{FlatSpec, Matchers}
import scalaz.NonEmptyList

class MonteCarloSpec extends FlatSpec with Matchers {
  private val draws = 10
  private val sampleA = "sample A"
  private val sampleB = "sample B"
  private val sampleC = "sample C"
  private val sampleD = "sample D"

  private val datasetA = sampleA -> NonEmptyList(100, 200, 30, 44, 51, 160, 740, 10, 430, 3)
  private val datasets = Map(
    datasetA,
    sampleB -> NonEmptyList(110, 210, 40, 54, -61, 170, 750, 20, 440, 13),
    sampleC -> NonEmptyList(98, 150, 56, -23, 98, 120, 660, -135, 430, -12)
  )

  "probabilitiesOfBeingBest" should "appropriately distribute wins to better datasets" in {
    val expectedOutput = Map(
      sampleA -> Probability(0.1),
      sampleB -> Probability(0.5),
      sampleC -> Probability(0.6))
    val result = probabilitiesOfBeingBest(datasets, draws)

    result(sampleA).value >= expectedOutput(sampleA).value shouldBe true
    result(sampleB).value <= expectedOutput(sampleB).value shouldBe true
    result(sampleC).value <= expectedOutput(sampleC).value shouldBe true
  }

  "probabilitiesOfBeingBest" should "always say a dataset wins if it is the only dataset passed" in {
    val expectedOutput = Map(sampleA -> Probability(1.0))
    probabilitiesOfBeingBest(Map(datasetA), draws) shouldBe expectedOutput
  }

  "probabilitiesOfBeingBest" should "return an empty vector when passed an empty vector" in {
    val expectedOutput = Map.empty[String, Probability]
    probabilitiesOfBeingBest(Map.empty[String, AbstractRealDistribution], draws) shouldBe expectedOutput
  }

  "probabilitiesOfBeingBest" should "return a NaN when asked to draw 0 or fewer times" in {
    for (i <- 0 until -3)
      probabilitiesOfBeingBest(Map(datasetA), i).head._2.value.isNaN shouldBe true
  }

  private val seed = Some(10102006L)
  private val normalA = new NormalDistribution(1D, 1D)
  private val normalB = new NormalDistribution(0D, 2.5D)
  private val normalC = new NormalDistribution(-3D, 9.9D)
  private val normalD = new NormalDistribution(1D, 1D)
  private val normals = Map(
    sampleA -> normalA,
    sampleB -> normalB,
    sampleC -> normalC,
    sampleD -> normalD)

  "probabilitiesOfBeingBest" should "appropriately distribute wins to better normal distributions" in {
    val expectedOutput = Map(
      sampleA -> Probability(0.5),
      sampleB -> Probability(0.0),
      sampleC -> Probability(0.0),
      sampleD -> Probability(0.5))
    probabilitiesOfBeingBest(normals, draws, seed) shouldBe expectedOutput
  }

  "probabilitiesOfBeingBest" should "always say a normal wins if it is the only normal passed" in {
    val expectedOutput = Map(sampleA -> Probability(1.0))
    probabilitiesOfBeingBest(Map(sampleA -> normalA), draws) shouldBe expectedOutput
  }

  "probabilitiesOfBeingBest" should "return an empty vector when passed an empty vector" in {
    val expectedOutput = Map.empty[String, Probability]
    probabilitiesOfBeingBest(Map.empty[String, NormalDistribution], draws) shouldBe expectedOutput
  }

  "probabilitiesOfBeingBest" should "return a NaN when asked to draw 0 or fewer times" in {
    for (i <- 0 until -3)
      probabilitiesOfBeingBest(Map(sampleA -> normalA), i).head._2.value.isNaN shouldBe true
  }

  "probabilitiesOfBeingBest" should "work for all abstract real distributions" in {
    val constants = Map(sampleA -> new ConstantRealDistribution(1D), sampleB -> new GammaDistribution(5D, 2D))
    val expectedOutput = Map(sampleA -> Probability(0.0), sampleB -> Probability(1.0))
    probabilitiesOfBeingBest(constants, draws) shouldBe expectedOutput
  }

}

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

import com.udemy.statistics.algorithm.Sampling._
import com.udemy.statistics.mean
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scalaz.NonEmptyList
import scalaz.Scalaz._

class SamplingSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "drawSample" should "draw randomly with replacement from a vector of numeric types" in {
    forAll (Gen.listOf[Double](Arbitrary.arbDouble.arbitrary), Gen.posNum[Int]) {
      (population: List[Double], sampleSize: Int) =>
        if (population.isEmpty) drawSample(population, sampleSize) shouldEqual List.empty
        else {
          val sample = drawSample(population, sampleSize)

          sample.length shouldEqual sampleSize
          sample.forall(population.contains(_)) shouldBe true
        }
    }
  }

  it should "return an empty Vector when size is less than 1" in {
    forAll (Gen.listOf[Double](Arbitrary.arbDouble.arbitrary), Gen.choose(Int.MinValue, 0)) {
        (population: List[Double], sampleSize: Int) =>
          drawSample(population, sampleSize) shouldBe empty
    }
  }

  "bootstrapMeans" should "generate bootstrap means from a sample" in {
    forAll (Gen.listOf[Double](Arbitrary.arbDouble.arbitrary), Gen.choose(1, 10)) {
      (sample: List[Double], draws: Int) =>
        val means = bootstrap(sample, draws, mean[Double])
        if (sample.isEmpty && draws > 0) {
          means.forall(_.isNaN) shouldBe true
        } else if (sample.isEmpty) {
          means.isEmpty shouldBe true
        } else {
          val minimum = sample.min
          val maximum = sample.max
          means.forall(_ >= minimum) shouldBe true
          means.forall(_ <= maximum) shouldBe true
        }

        means.length shouldEqual draws
    }
  }

  it should "return an empty Vector when draws is less than 1" in {
    forAll (Gen.listOf[Double](Arbitrary.arbDouble.arbitrary), Gen.choose(Int.MinValue, 0)) {
      (sample: List[Double], draws: Int) =>
        bootstrap(sample, draws, mean[Double]) shouldEqual List.empty
    }
  }

  "randomDraw" should "return a random element from the list" in {
    forAll {
      population: List[Double] =>
        population.toNel match {
          case Some(nel) => nel.toList.contains(randomDraw(nel)) shouldBe true
          case None => true shouldBe true
        }
    }
  }
}

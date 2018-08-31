package com.udemy.statistics.distribution

import com.udemy.statistics._
import org.apache.commons.math3.stat.StatUtils
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class NormalInverseGammaSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(
    minSuccessful = 100
  )

  private val anyDouble = 1D
  private val ν = Gen.posNum[Long]

  "σSquaredMean" should "return a positive number or infinity" in {

    forAll ("α", "β") { (α: Double, β: Double) =>
      val posterior = NormalInverseGamma(
        anyDouble,
        anyDouble,
        α,
        β
      )

      if (α > .5) posterior.σSquaredMean shouldEqual β / (α - .5)
      else posterior.σSquaredMean shouldEqual Double.PositiveInfinity
    }
  }

  "σSquaredMode" should "return a positive number" in {

    forAll ("α", "β") { (α: Double, β: Double) =>
      val posterior = NormalInverseGamma(
        anyDouble,
        anyDouble,
        α,
        β
      )

      posterior.σSquaredMode shouldEqual β / (α + 1.5)
    }
  }

  "μMean" should "return μ" in {

    forAll ("μ") { μ: Double =>
      val posterior = NormalInverseGamma(
        μ,
        anyDouble,
        anyDouble,
        anyDouble
      )

      posterior.μMean shouldEqual μ
    }
  }

  "μMode" should "return μ" in {

    forAll ("μ") { μ: Double =>
      val posterior = NormalInverseGamma(
        μ,
        anyDouble,
        anyDouble,
        anyDouble
      )

      posterior.μMode shouldEqual μ
    }
  }

  "posterior" should "return an appropriate posterior mu and nu" in {

    forAll ((Arbitrary.arbDouble.arbitrary, "μ"), (ν, "ν"), (Gen.listOf[Double](Arbitrary.arbDouble.arbitrary), "likelihood")) {
      (μ: Double, ν: Long, likelihoodList: List[Double]) =>
        val likelihood = likelihoodList.toArray
        val prior = NormalInverseGamma(
          μ,
          ν,
          anyDouble,
          anyDouble
        )

        val posterior = prior.posterior(likelihood)

        if (likelihood.nonEmpty) {
          val likelihoodMean = StatUtils.mean(likelihood)
          if (likelihoodMean > μ) {
            (posterior.μ >= μ && posterior.μ <= StatUtils.mean(likelihood)) ||
              posterior.μ == Double.NegativeInfinity ||
              posterior.μ.isNaN shouldEqual true
          }
          else {
            posterior.μ should be <= μ
            posterior.μ should be >= StatUtils.mean(likelihood)
          }

          posterior.ν shouldEqual (ν + likelihood.length)
        }
        else {
          posterior shouldEqual prior
        }
    }
  }

  "posterior" should "return an appropriate posterior alpha" in {

    forAll("α", "likelihood") { (α: Double, likelihood: Vector[Double]) =>
      val prior = NormalInverseGamma(
        anyDouble,
        anyDouble,
        α,
        anyDouble
      )
      val posterior = prior.posterior(likelihood)

      if (likelihood.nonEmpty) {
        posterior.α shouldEqual (α + likelihood.length / 2D)
      }
      else {
        posterior shouldEqual prior
      }
    }
  }

  "posterior" should "return an appropriate posterior beta" in {

    forAll ((Arbitrary.arbDouble.arbitrary, "μ"), (ν, "ν"), (Arbitrary.arbDouble.arbitrary, "β"), (Gen.listOf[Double](Arbitrary.arbDouble.arbitrary), "likelihood")) {
      (μ: Double, ν: Long, β: Double, likelihoodList: List[Double]) =>
        val likelihood = likelihoodList.toVector
        val prior = NormalInverseGamma(
          μ,
          ν,
          anyDouble,
          β
        )
        val posterior = prior.posterior(likelihood)

        if (likelihood.nonEmpty) {
          posterior.β >= β || posterior.β.isNaN shouldEqual true
        }
        else {
          posterior shouldEqual prior
        }
    }
  }

  "posterior" should "return a posterior for two param objects" in {
    forAll ((Arbitrary.arbDouble.arbitrary, "μ"), (ν, "ν"), (Arbitrary.arbDouble.arbitrary, "β"), (Gen.listOf[Double](Arbitrary.arbDouble.arbitrary), "likelihood")) {
      (μ: Double, ν: Long, β: Double, likelihoodList: List[Double]) =>
        val prior = NormalInverseGamma(
          μ,
          ν,
          anyDouble,
          β
        )

        val array = likelihoodList.toArray
        val likelihood = NormalInverseGamma(
          StatUtils.mean(array),
          likelihoodList.length,
          likelihoodList.length / 2D,
          sumOfSquaredDeviations(array) / 2D
        )

        val posterior = prior.posterior(likelihood)

        if (likelihoodList.nonEmpty) {
          posterior.α shouldEqual (anyDouble + likelihoodList.length / 2D)
          posterior.ν shouldEqual (ν + likelihoodList.length)
        }
        else {
          posterior shouldEqual prior
        }
    }
  }

}

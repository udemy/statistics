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

import scala.Numeric.Implicits._
import scala.collection.parallel.immutable.ParVector
import scala.util.Random
import scalaz.NonEmptyList
import scalaz.Scalaz._

object Sampling {

  def randomDraw[T: Numeric](x: NonEmptyList[T]): T =
    x.index(Random.nextInt(x.length)).get

  def sample[T: Numeric](x: Seq[T], size: Int, withReplacement: Boolean = true): Seq[T] = {
    x.toList.toNel match {
      case None => Seq.empty
      case Some(nel) =>
        if (withReplacement) ParVector.fill(size)(randomDraw(nel)).toVector
        else Random.shuffle(x).take(size)
    }
  }

  def bootstrap[T: Numeric](sample: Seq[T], draws: Int, statistic: Seq[T] => Double): Seq[Double] =
    ParVector.fill(draws)(0D).map(zero => statistic(Sampling.sample(sample, sample.length))).toVector

}

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
import scala.util.Random

object Sampling {

  def randomDraw[T:Numeric](x: Seq[T]): T =
    x(Random.nextInt(x.length))

  def drawSample[T: Numeric](x: Seq[T], size: Int): Seq[T] = {
    if (x.isEmpty) Seq()
    else Vector.fill(size)(randomDraw(x))
  }

  def bootstrap[T: Numeric](sample: Seq[T], draws: Int, statistic: Seq[T] => Double): Seq[Double] =
    Vector.fill(draws)(drawSample(sample, sample.length)).map(statistic)

}

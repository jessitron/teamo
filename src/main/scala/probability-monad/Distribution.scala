/*
 * From: https://github.com/jliszka/probability-monad
 * ... which doesn't seem to have releases, or I'd include it from there...
 * License: https://github.com/jliszka/probability-monad/blob/master/LICENSE
 */
package probability_monad

import scala.annotation.tailrec
import scala.math.BigDecimal
import scala.util.Random

trait Distribution[A] {
  self =>
  protected def get: A

  override def toString = "<distribution>"

  def map[B](f: A => B): Distribution[B] = new Distribution[B] {
    override def get = f(self.get)
  }

  def flatMap[B](f: A => Distribution[B]): Distribution[B] = new Distribution[B] {
    override def get = f(self.get).get
  }

  def filter(pred: A => Boolean): Distribution[A] = new Distribution[A] {
    @tailrec
    override def get = {
      val s = self.get
      if (pred(s)) s else this.get
    }
  }

  def withFilter(pred: A => Boolean): Distribution[A] = filter(pred)

  def given(pred: A => Boolean): Distribution[A] = filter(pred)

  def until(pred: List[A] => Boolean): Distribution[List[A]] = new Distribution[List[A]] {
    override def get = {
      @tailrec
      def helper(sofar: List[A]): List[A] = {
        if (pred(sofar)) sofar

        else helper(self.get :: sofar)
      }
      helper(Nil)
    }
  }

  def repeat(n: Int): Distribution[List[A]] = new Distribution[List[A]] {
    override def get = List.fill(n)(self.get)
  }

  /**
   * Using this distribution as a prior, compute the posterior distribution after running an experiment
   * and observing some outcomes and not others.
   */
  def posterior[B](experiment: A => Distribution[B])(observed: B => Boolean): Distribution[A] = {
    case class Trial(p: A, evidence: B)
    val d = for {
      p <- this
      e <- experiment(p)
    } yield Trial(p, e)
    d.filter(t => observed(t.evidence)).map(_.p)
  }

  /**
   * Markov chains
   */

  @tailrec
  final def markov(n: Int)(f: A => Distribution[A]): Distribution[A] = {
    if (n == 0) this
    else this.flatMap(f).markov(n-1)(f)
  }

  def markov(pred: A => Boolean)(f: A => Distribution[A]): Distribution[A] = new Distribution[A] {
    override def get = {
      @tailrec
      def helper(a: A): A = {
        if (pred(a)) a
        else helper(f(a).get)
      }
      helper(self.get)
    }
  }

  private val N = 10000

  def pr(pred: A => Boolean, given: A => Boolean = (a: A) => true, samples: Int = N): Double = {
    1.0 * this.filter(given).sample(samples).count(pred) / samples
  }

  // NB: Expected value only makes sense for real-valued distributions. If you want to find the expected
  // value of a die roll, for example, you have to do die.map(_.toDouble).ev.
  def ev(implicit toDouble: A <:< Double): Double = Stream.fill(N)(toDouble(self.get)).sum / N

  def mean(implicit toDouble: A <:< Double): Double = ev

  private def square(x: Double) = x * x
  private def cube(x: Double) = x * x * x

  def variance(implicit toDouble: A <:< Double): Double = {
    val mean = this.mean
    this.map(x => {
      square(toDouble(x) - mean)
    }).ev
  }

  def stdev(implicit toDouble: A <:< Double): Double = {
    math.sqrt(this.variance)
  }

  def skewness(implicit toDouble: A <:< Double): Double = {
    val mean = this.mean
    val stdev = this.stdev
    this.map(x => {
      cube((toDouble(x) - mean) / stdev)
    }).ev
  }

  def kurtosis(implicit toDouble: A <:< Double): Double = {
    val mean = this.mean
    val variance = this.variance
    this.map(x => {
      square(square(toDouble(x) - mean))
    }).ev / square(variance)
  }

  def sample(n: Int = N): List[A] = List.fill(n)(self.get)

  /**
   * "Freeze" a distribution by taking a sample and serving values out of that sample at random.
   * Useful for when a distribution is expensive to compute and is being sampled from repeatedly.
   */
  def freeze: Distribution[A] = {
    Distribution.discreteUniform(sample(N*10))
  }

  def zip[B](d: Distribution[B]): Distribution[(A, B)] = new Distribution[(A, B)] {
    override def get = (self.get, d.get)
  }

  def zipWith[B, C](d: Distribution[B])(f: (A, B) => C): Distribution[C] = new Distribution[C] {
    override def get = f(self.get, d.get)
  }

  def +(d: Distribution[A])(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.plus(self.get, d.get)
  }
  def +(x: A)(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.plus(self.get, x)
  }
  def -(d: Distribution[A])(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.minus(self.get, d.get)
  }
  def -(x: A)(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.minus(self.get, x)
  }
  def *(d: Distribution[A])(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.times(self.get, d.get)
  }
  def *(x: A)(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.times(self.get, x)
  }
  def /(d: Distribution[A])(implicit toDouble: A <:< Double): Distribution[Double] = new Distribution[Double] {
    override def get = toDouble(self.get) / toDouble(d.get)
  }
  def /(x: A)(implicit toDouble: A <:< Double): Distribution[Double] = new Distribution[Double] {
    override def get = toDouble(self.get) / toDouble(x)
  }

  def hist(implicit ord: Ordering[A] = null, d: A <:< Double = null) = {
    if (d == null) {
      plotHist(ord)
    } else {
      bucketedHist(20)(ord, d)
    }
  }

  def histData: Map[A, Double] = {
    this.sample(N).groupBy(x=>x).mapValues(_.length.toDouble / N)
  }

  private def plotHist(implicit ord: Ordering[A] = null) {
    val histogram = this.histData.toList
    val sorted = if (ord == null) histogram else histogram.sortBy(_._1)(ord)
    doPlot(sorted)
  }

  private def findBucketWidth(min: Double, max: Double, buckets: Int): (BigDecimal, BigDecimal, BigDecimal, Int) = {
    // Use BigDecimal to avoid annoying rounding errors.
    val widths = List(0.1, 0.2, 0.25, 0.5, 1.0, 2.0, 2.5, 5.0, 10.0).map(BigDecimal.apply)
    val span = max - min
    val p = (math.log(span) / math.log(10)).toInt - 1
    val scale = BigDecimal(10).pow(p)
    val scaledWidths = widthsmax / bestWidth).toInt + 1) * bestWidth
    val actualBuckets = ((outerMax - outerMin) / bestWidth).toInt
    (outerMin, outerMax, bestWidth, actualBuckets)
  }

  def bucketedHist(buckets: Int)(implicit ord: Ordering[A], toDouble: A <:< Double) {
    val data = this.sample(N).toList.sorted
    val min = data.head
    val max = data.last
    vadDown: Boolean)
                  (implicit ord: Ordering[A], toDouble: A <:< Double) {
    val rm = if (roundDown) BigDecimal.RoundingMode.DOWN else BigDecimal.RoundingMode.HALF_UP
    val width = (max - min) / nbuckets
    def toBucket(a: A): BigDecimal = ((toDouble(a) - min) / width).setScale(0, rm) * width + min
    val n = data.size
    val bucketToProb = data
      .groupBy(toBucket)
      .mapValues(_.size.toDouble / n)
    val bucketed = istributions
   */

  sealed abstract class Coin
  case object H extends Coin
  case object T extends Coin
  def coin: Distribution[Coin] = discreteUniform(List(H, T))
  def biasedCoin(p: Double): Distribution[Coin] = discrete(H -> p, T -> (1s-dice-coins/
    @tailrec
    private def alias(smaller: List[(A, Double)], bigger: List[(A, Double)], rest: List[(A, Double, Option[A])]): List[(A, Double, Option[A])] = {
      (smaller, bigger) match {
        case ((s, sp) :: ss, (b, pb) :: bb) =>
          val remainder = (b, pb - (1.0 - sp))
          val newRest = (s, sp, Some(b)) :: rest
          if (remainder._2 < 1)
            alias(remainder :: ss, bb, newRest)
          else

      select(uniform.get, uniform.get, table)
    }
  }

  def geometric(p: Double): Distribution[Int] = {
    tf(p).until(_.headOption == Some(true)).map(_.size - 1)
  }

  def binomial(p: Double, n: Int): Distribution[Int] = {
    bernoulli(p).repeat(n).map(_.sum)
  }

  def negativeBinomial(p: Double, r: Int): Distribution[Int] = {
    tf(p {
      z <- normal
      v <- chi2(df)
    } yield z * math.sqrt(df / v)
  }

  def pareto(a: Double, xm: Double = 1.0): Distribution[Double] = {
    for {
      x <- unifom https://en.wikipedia.org/wiki/Gamma_distribution#Generating_gamma-distributed_random_variables
      def helper(): Distribution[Double] = {
        for {
          u1 <- uniform
          u2 <- uniform
          u3 <- uniform
          (zeta, eta) = {
            val v0 = math.E / (math.E + delta)
            if (u1 <= v0) {
              varibution[List[T]] {
    override def get = ds.map(_.get)
  }

  def dirichlet(alphas: List[Double]): Distribution[List[Double]] = {
    sequence(alphas.map(a => gamma(a, 1))).map(ys => {
      val sum = ys.sum
      ys.map(_ / sum)
    })
  })
  }

  /**
   * Determine if a joint probability distribution is composed of 2 independent events.
   * Uses the G-test: http://en.wikipedia.org/wiki/G-test
   */
  def chi2test[A, B](d: Distribution[(A, B)]): Double = {
    val data = d.histData
    val total = data.map(_._2).sum
    val rowValues = data.map(_._1._1).toSet
    val colValues 2
    val df = (rowValues.size - 1) * (colValues.size - 1)
    chi2(df).pr(_ > chi2stat)
  }
}}}}})}))}}}

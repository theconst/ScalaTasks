package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    
    @tailrec
    def loop(left: Int, acc: Int): Boolean = {
      if (left == chars.length) {
        acc == 0
      } else if (acc < 0) {
        false
      } else if (chars(left) == '(') {                  // increment if (
        loop(left + 1, acc + 1)
      } else if (chars(left) == ')') {                  // decrement if )
        loop(left + 1, acc - 1)
      } else {
        loop(left + 1, acc)                            // nothing if not ( or )
      }
    }
    loop(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    
   /**
    * Traverses the subarray for indexes @argument from <= i < @argument until
    * @argument balance - balance of the parentheses
    * @argument minBalance - minimum balance in subarray
    */
    @tailrec
   def traverse(from: Int, until: Int, balance: Int, minBalance: Int): (Int, Int) = {
     if (from >= until) {
       (balance, minBalance)
     } else if (chars(from) == ')') {
       traverse(from + 1, until, balance - 1, Math.min(minBalance, balance - 1))
     } else if (chars(from) == '(') {
       traverse(from + 1, until, balance + 1, minBalance)
     } else {
       traverse(from + 1, until, balance, minBalance)
     }
   }
  
   /** 
    *  Computes the braces array in parallel starting from
    *  @argument from and finishing with the @argument until
    */
   def reduce(from: Int, until: Int): (Int, Int) = {
     if (until - from <= threshold) {
       traverse(from, until, 0, 0)
     } else {
       val mid = from + (until - from) / 2
       val (left, right) = parallel(reduce(from, mid), reduce(mid, until))  
       val totalBalance = left._1 + right._1
       val minimumBalance = Math.min(left._2, left._1 + right._2)
       
       (totalBalance, minimumBalance) 
     }
   }
    
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!
  
}

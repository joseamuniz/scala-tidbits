import collection.mutable.ArrayBuffer

/**
 * Give an O(n**2) time algorithm to find the longest monotonically increasing
 * subsequence of a sequence of n numbers.
 */
class IncreasingSubsequence {

  def longestSubsequence(ls: List[Int]) = {
    /* a[i] longest subsequence that includes ls[i] */
    var a = new Array[ArrayBuffer[Int]](ls size)
    a(0) = ArrayBuffer(ls(0));

    for (i <- 1 until (ls size)) {
      /* Filter out all subsequences from 0 ... i that have elements larger
          than ls(i)  */
      def lsi = ls(i)

      def candidates = a.slice(0, i).filter((seq) => (seq.last <= lsi))
      var best = candidates reduceLeft (
                      (s1, s2) => (if (s1.length > s2.length) s1 else s2));
      best = best.clone();
      best += lsi


      a(i) = best
    }

    def answer = a(ls.length - 1);
    answer.slice(0, answer.length - 1);
  }


}


object IncreasingSubsequence {
  def main(args: Array[String]) {
    def inc = new IncreasingSubsequence
    def ls = List(1, 4, 2, 5, 3, 4, 10, 2, Int.MaxValue);


    println(inc.longestSubsequence((ls)))
  }

}

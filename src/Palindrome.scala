import collection.mutable

/**
 * Give an efficient algorithm to find the longest palindrome that is a
 * subsequence of a given input string.
 *
 * For example, given the input "character", your algorithm should return
 * "carac". What is the running time?
 * > O(n**2)
 */
class Palindrome {

  def findLargestPalindromeSubsequence(word: String)  = {
    /*
      words[i,j] - Largest palindromic subsequence of word between characters
                   [i,j].

      Note: Instead of a 2D array because Scala is annoying
     */
    var words = new mutable.HashMap[(Int, Int), String]();


    /* (1) Initial condition: First diagonal  */
    for (i <- 0 until word.length()) {
      words((i,i)) = word.charAt(i).toString();
    }

    /* (2) Initial Condition: Second diagonal */
    for (i <- 0 until word.length() - 1) {
      def from = i;
      def to = i + 1;

      if (word.charAt(to) == word.charAt(from)) {
        words((from, to)) = word(to) + "" + word(from);
      }
      else {
        words((from, to)) = word(to).toString();
      }

    }


    /* (3) Recursive condition */
    for (i <- 2 until word.length()) {
      for (j <- word.length() - i - 1 to 0 by -1) {
        def row = j;
        def col = j + i;

        /* Either best answer contains characters word[row] and word[col]
           or it doesn't.
        */
        var candidates = List(words((row + 1, col)),
                              words((row, col - 1)));
        if (word(row) == word(col))  {
          var candidate =  word(row) +  words((row + 1, col - 1)) + word(col);
          candidates = candidate :: candidates;
        }


        def winner = candidates.reduceLeft((s1, s2) =>
                            if (s2.length > s1.length) s2 else s1)

        words((row, col)) = winner;

      }

    }

    words((0, word.length() - 1));

  }
}


object Palindrome {
  def pal = new Palindrome();

  def main(args: Array[String]) {
    def words = List("ifi", "abcdedcba", "XabcdedcbaY", "aXbYcFbGa", "abba");
    println (words zip (words map pal.findLargestPalindromeSubsequence));
  }
}
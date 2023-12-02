def lineval(s1 : String): Int = {
    if (s1.startsWith("Game ")) {
      var p1 = s1.indexOf(":")
      var score = s1.substring(5,p1).toInt
      var s2 = s1.substring(p1+2)
      println("Game " , score)
      while (s2.length() > 1) {
         println("next chunk:", s2)
         var ps = s2.indexOf(" ")
         var limit = 0
         var pn = 0
         
         println("checking ", s2.substring(ps+1))
         if (s2.substring(ps+1).startsWith("red")) {
            println("red")
            pn = 3
            limit = 12
         }
         if (s2.substring(ps+1)startsWith("green")) {
            println("green")
            pn = 5
            limit = 13
         }
         if (s2.substring(ps+1)startsWith("blue")) {
            println("blue")
            pn = 4
            limit = 14
         }
         var cn = s2.substring(0,ps).toInt
         println("cn ps pn" , cn, ps, pn)
         if (cn > limit) {
            println("over limit on " , s2 )
            score = 0
         }
         if (ps+pn+3 < s2.length) {
           s2 = s2.substring(ps+pn+3)
         } else {
           s2 = ""
         }
      }
      score
   } else {
      0
   }
}
 
val sum = (ii: Iterator[Int]) => {
  var res: Int = 0
  for (i <- ii) {
     res = res + i
  }
  res
}

var res = sum( scala.io.Source.stdin.getLines().map(lineval))
println("total = ",  res )

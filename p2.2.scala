def lineval(s1 : String): Int = {
    if (s1.startsWith("Game ")) {
      var p1 = s1.indexOf(":")
      var score = s1.substring(5,p1).toInt
      var s2 = s1.substring(p1+2)
      var maxr = 0
      var maxg = 0
      var maxb = 0
      println("Game " , score)
      while (s2.length() > 1) {
         println("next chunk:", s2)
         var ps = s2.indexOf(" ")
         var limit = 0
         var pn = 0
         
         println("checking ", s2.substring(ps+1))
         var cn = s2.substring(0,ps).toInt
         if (s2.substring(ps+1).startsWith("red")) {
            println("red")
            pn = 3
            if (cn > maxr) {
               maxr = cn
            }
         }
         if (s2.substring(ps+1)startsWith("green")) {
            println("green")
            pn = 5
            if (cn > maxg) {
               maxg = cn
            }
         }
         if (s2.substring(ps+1)startsWith("blue")) {
            println("blue")
            pn = 4
            if (cn > maxb) {
               maxb = cn
            }
         }
         println("cn ps pn" , cn, ps, pn)
         if (ps+pn+3 < s2.length) {
           s2 = s2.substring(ps+pn+3)
         } else {
           s2 = ""
         }
         println("maxr,g,b", maxr, maxg, maxb)
      }
      maxr * maxg * maxb
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

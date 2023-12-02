
val digtuple = List( "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ) 

val lineval = (s1: String) => {
    var firstdig = '0'
    var lastdig = '0'
    var sawfirst = false
    var sawsecond = false
    var digv : Int = '1'
    var res = 0
    var s2 = s1
    var i = 0

    while( i < s2.length ) {
       digv = '1'
       for (dig <- digtuple) {
          if (s2.substring(i).startsWith(dig)) {
              s2 = s2.substring(0,i) + "%c".format(digv) + s2.substring(i+1)
              println("replacing " , dig, "%c".format(digv), s2)
          }
          digv = digv + 1
       }
       i = i + 1
    }
       
    for (c <- s2) {
       if (c >= '0' && c  <= '9') {
           if (sawfirst) {
              lastdig = c
              sawsecond = true
           } else {
              firstdig = c
              sawfirst = true
          }
       }
    }
    if (sawsecond) {
      res = 10 * (firstdig - '0') + lastdig - '0'
    } else {
      res = 10 * (firstdig - '0') + firstdig - '0'
    }
    println("Mapping", s1, res)
    res
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

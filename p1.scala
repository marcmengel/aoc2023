
val lineval = (s1: String) => {
    var firstdig = '0'
    var lastdig = '0'
    var sawfirst = false
    var sawsecond = false
    for (c <- s1) {
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
      10 * (firstdig - '0') + lastdig - '0'
    } else {
      10 * (firstdig - '0') + firstdig - '0'
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

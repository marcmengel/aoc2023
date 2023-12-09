
def find_deriv(invec:Array[Integer]) : Array[Integer] = {
   var first=true
   var prev = 0
   var dl : Array[Integer] = new Array[Integer](invec.length - 1)

   for( count <- Range(0, invec.length) ) {
      if( first )
          first=false
      else {
          dl(count-1) = invec(count) - prev
      }
      prev = invec(count)
   }
   return dl
}


def nonzero(dl:Array[Integer]) : Boolean = {
   var dli = dl.iterator
   for ( x <- dli) 
      if (x != 0) 
        return true
   return false
}
def toInteger(s:String) : Integer ={
  var y:Integer = s.toInt + 0
  return y
}

def predict(inline: String): Integer = {
    println(("predict:", inline))
    var deriv = inline.trim.split("  *").map(toInteger)
    for ( d <- deriv  ) {
      print(d, " ")
    }
    println()
    var lastvals: List[Integer] = List()
    while( nonzero(deriv) ) {
        lastvals = deriv(deriv.length-1) :: lastvals
        deriv = find_deriv(deriv)
        print("deriv: ")
        for( d <- deriv) print(d," ")
        println("")
    }
    var pval = 0
    for( x <- lastvals ) {
       pval = x + pval
       println(("pval: ", pval))
    }
    return pval
}

val sum = (ii: Iterator[Int]) => {
  var res: Int = 0
  for (i <- ii) {
     res = res + i
  }
  res
}
var total = sum( scala.io.Source.stdin.getLines().map(predict))
println(("total",total))

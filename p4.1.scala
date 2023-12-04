import scala.util.matching.Regex


def max(a:Int, b: Int): Int = {
  if(a < b) {
    return b
  } else {
    return a
  }
}
def min(a:Int, b: Int): Int = {
  if(a < b) {
    return a
  } else {
    return b
  }
}

val sum = (ii: Iterator[Int]) => {
  var res: Int = 0
  for (i <- ii) {
     res = res + i
  }
  res
}

val cardre : Regex = "Card +([0-9]+): +([0-9 ]+) \\| +([0-9 ]+)".r

def toInt(s:String) : Integer = {
   return Integer.parseInt(s)
}


def cardscorer( card: String ) : Int = {
    var wi = 0
    var pi = 0
    var matchcount = 0

    card match {

      case cardre(cn, wnls, pnls) => {
        println("wnls, pnls", wnls, pnls,":")
        var wnv : Array[Integer] = wnls.split(" +").map(toInt).sorted
        var pnv : Array[Integer] = pnls.split(" +").map(toInt).sorted
        var wnmax = wnv.length
        var pnmax = pnv.length
        println(("card ", cn, wnv, pnv))
        while (wi <  wnmax && pi < pnmax) {
           println("top:", wi, wnv(wi), pi, pnv(pi))
           if( wnv(wi) < pnv(pi) )
             while( wi < wnmax && wnv(wi) < pnv(pi) )  {
               //println("h1", wi)
               wi = wi + 1
             }
           else
             while( pi < pnmax && pnv(pi) < wnv(wi) )  {
               //println("h2", pi)
               pi = pi + 1
             }
           println("after scan: pi, wi" , pi, wi)
           if (wi < wnmax && pi < pnmax && wnv(wi) == pnv(pi)) {
             println("match: " , wnv(wi))
             matchcount = matchcount+1
             wi = wi + 1
             pi = pi + 1
           }
         }
      }
    }
    if ( matchcount > 0 )
       return 1 << (matchcount - 1)
    else
       return 0
}

var res  = sum( scala.io.Source.stdin.getLines().map(cardscorer))
println("total = ",  res)

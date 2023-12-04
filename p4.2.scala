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

val sum = (ii: Iterator[Integer]) => {
  var res: Int = 0
  for (i <- ii) {
     println("sum: " , i)
     res = res + i
  }
  res
}

val cardre : Regex = "Card +([0-9]+): +([0-9 ]+) \\| +([0-9 ]+)".r

def toInt(s:String) : Integer = {
   return Integer.parseInt(s)
}


def cardscorer( card: String ) : Integer = {
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
        //println(("card ", cn, wnv, pnv))
        while (wi <  wnmax && pi < pnmax) {
           //println("top:", wi, wnv(wi), pi, pnv(pi))
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
             //println("match: " , wnv(wi))
             matchcount = matchcount+1
             wi = wi + 1
             pi = pi + 1
           }
         }
      }
    }
    return matchcount
}

def convolve(mi : Iterator[Integer]) : Array[Integer] = {
   var ml = mi.toArray
   var totals = ml.clone

   for( i <- Range(0,ml.length)) {
       // we start off with 1 of each card...
       totals(i) = 1
   }


   for( i <- Range(0,ml.length)) {
       // the ml(i) ones below us get one for each of this card
       for( j <- Range(1,ml(i)+1)) {
          println("cl:", i, j)
          if( i+j < ml.length)
            totals(i+j) = totals(i+j) + totals(i)
            print("clt:", totals(i), totals(i+j))
        }
   }
   println("convolve after:" , ml, totals)

   return totals
}

var basematches = scala.io.Source.stdin.getLines().map(cardscorer)
var convmatches = convolve(basematches)
var res = sum( convmatches.toIterator )
println("total = ",  res)

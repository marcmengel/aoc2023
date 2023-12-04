
def  issymbol(c: Char) : Boolean =  {
  return ('0' > c || '9' < c) && '.' != c
}

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

def gear_value( ls1: Vector[String], start : Int, end: Int ) : Int = {
    var mystart = start
    var myend = end
    var dx = 0
    var dxs = 0
    var dxe = 0
    var dy = 0
    var gs = 0 
    println("gear value", ls1(2).substring(start,end))
    for( k <- Range(start-1, end+1))
       for( j <- Range(1,4)) 
          if ( k >= 0 && k < ls1(j).length() && '*' == ls1(j)(k)) {
              println("found possible gear:", ls1(j)(k), "in:", j, k )
              println(ls1(0).substring(max(0,start-4),min(ls1(0).length,end+4)))
              println(ls1(1).substring(max(0,start-4),min(ls1(1).length,end+4)))
              println(ls1(2).substring(max(0,start-4),min(ls1(2).length,end+4)))
              println(ls1(3).substring(max(0,start-4),min(ls1(2).length,end+4)))
              println(ls1(4).substring(max(0,start-4),min(ls1(2).length,end+4)))
                 
              for(dx <- Range(-1,2)) {
                 for(dy <- Range(-1,2)) {
                   if (0 <= k+dx && k+dx <= ls1(j+dy).length()) {
                     println("at ",dx, dy, ls1(j+dy)(k+dx), k+dx, start, end)
                     if ('0'<= ls1(j+dy)(k+dx) && ls1(j+dy)(k+dx) <= '9' && ! (j+dy == 2 && k+dx >= start && k+dx <= end)) { 
                          println(("checking at ", dx, dy))
                          //found another adjacent digit,find start
                          dxs = dx
                          while( k+dxs >= 0 && '0' <= ls1(j+dy)(k+dxs) && ls1(j+dy)(k+dxs) <= '9' ) {
                             dxs = dxs - 1
                          }
                          dxs=dxs+1
                          dxe=dxs
                          // find end
                          while( k+dxe < ls1(j).length() &&  '0'<= ls1(j+dy)(k+dxe) && ls1(j+dy)(k+dxe) <= '9' ) {
                              dxe=dxe+1
                          }
                          println(("bounds ", dxs, dxe))
                          println(("found other number:", ls1(j+dy).substring(k+dxs, k+dxe).toInt))
                          return ls1(j+dy).substring(k+dxs, k+dxe).toInt
                     }
                   }
                 }
              }
          }

    return 0
}

def gearscores(ls1 : Vector[String]): Int = {
    var pstart: Int = 0
    var pend: Int = 0
    var line: String = ls1(2)
    var num : Int = 0
    var score : Int = 0
    var gs : Int = 0

    while( pstart < line.length() ) {
       pend = pstart
       while (pend < line.length() && '0' <= line(pend)  && line(pend) <= '9' )
          pend = pend+1
          
       if (pend > pstart) {
           // found a number
           gs = gear_value(ls1, pstart, pend)
           if ( gs != 0) {
              score = score + line.substring(pstart,pend).toInt * gs
           }
           pstart = pend
       }
       pstart = pstart + 1
    }
    println("returning ", score)
    score
}


class PadIter( var i: Iterator[String], var pad: String ) extends Iterator[String] {
   var didpre = false
   var didpost = false
   def hasNext() : Boolean  = { ! didpost }
   def next() : String  = {
      if( didpre ) {
         if (i.hasNext) {
           return i.next
         } else {
           didpost = true
           return pad
         }
      } else {
        didpre = true
        return pad
     }
   }
}
  
class PentaIter( var  i: Iterator[String] ) extends Iterator[Vector[String]] {
    private var prev1: String = ""
    private var prev2: String = i.next()
    private var curline: String  = i.next()
    private var next1 : String =  i.next()
    private var next2 : String =  i.next()

    def hasNext() : Boolean = { i.hasNext }
    def next() : Vector[String] = { prev1=prev2; prev2=curline; curline=next1; next1=next2; next2=i.next(); return Vector(prev1,prev2,curline,next1,next2)}
}

val sum = (ii: Iterator[Int]) => {
  var res: Int = 0
  for (i <- ii) {
     res = res + i
  }
  res
}

val longline="............................................................................................................................................"
var res = 0
var fi = scala.io.Source.stdin.getLines()
var p = new PadIter(new PadIter( fi , longline), longline)
var t = new PentaIter(p)
res = sum( t.map(gearscores))
// it double counts, so...
println("total = ",  res, res / 2)

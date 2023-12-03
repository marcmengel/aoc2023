
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

def symbol_adjacent( ls1: Vector[String], start : Int, end: Int ) : Boolean = {
    var mystart = start
    var myend = end
    for( k <- Range(start-1, end+1))
       for( j <- Range(0,3)) 
          if ( k >= 0 && k < ls1(j).length() && issymbol(ls1(j)(k))) {
              println("found symbol:", ls1(j)(k), "in:")
              println(ls1(0).substring(max(0,start-1),min(ls1(0).length,end+1)))
              println(ls1(1).substring(max(0,start-1),min(ls1(1).length,end+1)))
              println(ls1(2).substring(max(0,start-1),min(ls1(2).length,end+1)))
              return true
          }
    return false
}

def linetripleval(ls1 : Vector[String]): Int = {
    var score: Int  = 0
    var pstart: Int = 0
    var pend: Int = 0
    var line: String = ls1(1)

    while( pstart < line.length() ) {
       pend = pstart
       while (pend < line.length() && '0' <= line(pend)  && line(pend) <= '9' )
          pend = pend+1
          
       if (pend > pstart) {
           // found a number
           if (symbol_adjacent(ls1, pstart, pend)) {
              score += line.substring(pstart,pend).toInt
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
  
class TripleIter( var  i: Iterator[String] ) extends Iterator[Vector[String]] {
    private var prevline: String = ""
    private var curline: String  = i.next()
    private var nextline : String =  i.next()

    def hasNext() : Boolean = { i.hasNext }
    def next() : Vector[String] = { prevline = curline; curline = nextline; nextline = i.next(); Vector(prevline, curline, nextline) }
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
var p = new PadIter( fi , longline)
var t = new TripleIter(p)
res = sum(t.map(linetripleval))
println("total = ",  res )

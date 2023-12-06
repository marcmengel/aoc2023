
import scala.math
import scala.io.StdIn.readLine

def stoLong(s:String): Long = { s.toLong }

def score( tt: Long, d: Long ): Long = {
    // algebra
    // -------
    // d distance
    // pt press time
    // tt total time
    //    d  = pt * (tt - pt)  (velocity times remaining time)
    //       = pt * tt - pt^2
    // so
    //    pt^2 - tt * pt + d = 0
    // then
    //    d = (tt +/- sqrt( tt*tt - 4d ))/2
    //
    // so the two times that get d are the plus and minus
    var pt2 =  (tt + math.sqrt( tt*tt - 4.0 * d ))/2.0
    var pt1 =  (tt - math.sqrt( tt*tt - 4.0 * d ))/2.0
    println("race: wins ", pt1, "..", pt2 )
    // and the count of the integer times between those two are the score
    return -math.round(math.floor(pt1) -  math.ceil(pt2) + 1.0)
}



def parseline(s:String, lead:String ):Array[Long] = {
    if (! s.startsWith(lead)) {
        println("ouch! expecting", lead)
    }
    return s.substring(lead.length).trim().split("  *").map(stoLong)
}

var times = parseline(readLine(),     "Time: ")
var distances = parseline(readLine(), "Distance: ")

var res = 1L
var ti = times.iterator
var di = distances.iterator
while( ti.hasNext && di .hasNext )
  res = res * score(ti.next, di.next)

println("product = ",  res )

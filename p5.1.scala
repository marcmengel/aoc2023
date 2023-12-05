
import scala.util.matching.Regex

def toInt(s:String) : Integer = {
   return Integer.parseInt(s)
}

class Map(nl: Array[Integer]) {

    def apply(n: Integer): Integer = {
        var res = n
        //println(("apply", n, nl(0), nl(1), nl(2)))
        if (n >= nl(0) && n <= n+nl(2)) {
            res = n - nl(0) + nl(1)
            println("mapping:", n, res)
        }
        res
    }
}

def getmaps(lines: Iterator[String]): List[Map] = {
   var res : List[Map] = List()
   println("entering getmaps")
   for( line <- lines) {
       println("getmaps: line:  ", line)
       if (0 != line.length && '0' <= line(0)  && line(0) <= '9') {
          var nl = line.split("  *").map(toInt)
          res = new Map(nl) :: res
       }
   }
   return res.reverse
}

def apply_mappings(ml: List[Map], snin: Integer): Integer = {
    var sn = snin
    for (m <- ml) {
        //println(("applying", m))
        sn = m.apply(sn)
    }
    sn
}

var loc: Integer = 0
var seeds: List[Integer] = List()
var lines = scala.io.Source.stdin.getLines()
var seedline = lines.next
if (seedline.startsWith("seeds:" )){
    seeds = seedline.substring(7).split(" ").toList.map(toInt)
}
var mappings = getmaps(lines)

var minloc=99999
for (s <- seeds) {
    loc = apply_mappings(mappings, s)
    println(("seed", s, "maps to", loc))
    if (loc < minloc) {
         minloc = loc
    }
}
println(("minloc = ",  minloc))

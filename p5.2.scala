
import scala.util.matching.Regex

def stoLong(s:String): Long = { s.toLong }

class Map(nl: List[Long]) {

    def apply(n: Long): Long = {
        var res = n
        //println(("apply", n, nl(0), nl(1), nl(2)))
        if (n >= nl(1) && n < nl(1) + nl(2)) {
            println(("applying", n, nl(0), nl(1), nl(2)))
            res = n - nl(1) + nl(0)
            println("mapping:", n, res)
        }
        res
    }
}

class MapSet(_name: String, ml: Array[Map]) {
  def apply(snin: Long): Long = {
      var sn = snin
      for (m <- ml) {
          //println(("applying", m))
          sn = m.apply(sn)
          if (sn != snin) {
             return sn
          }
      }
      sn
  }
  def name(): String = { _name }
}

def apply_mapset(ml: Array[MapSet], snin: Long): Long = {
    var sn = snin
    for (m <- ml) {
        sn = m.apply(sn)
    }
    sn
}

def getmaps(lines: Iterator[String]): List[MapSet] = {
   var res : List[MapSet] = List()
   var ml: List[Map] = List()
   var name: String = ""
   println("entering getmaps")
   for( line <- lines) {
       println("getmaps: line:  ", line)
       if (0 != line.length && 'a' <= line(0) && line(0) < 'z') {
          if (name != "") {
             res = new MapSet(name, ml.toArray.reverse) :: res
          }
          name = line
          ml = List()
       }
       if (0 != line.length && '0' <= line(0)  && line(0) <= '9') {
          var nl : List[Long] = line.split("  *").toList.map(stoLong)
          ml = new Map(nl) :: ml
       }
   }
   if (name != "" && ml.length > 0) {
       res = new MapSet(name, ml.toArray.reverse) :: res
   }
   return res.reverse
}

def apply_mappings(ml: List[MapSet], snin: Long): Long = {
    var sn = snin
    for (m <- ml) {
        println(("applying", m.name))
        sn = m.apply(sn)
    }
    sn
}

def pairup(ll : List[Long]): List[List[Long]] = {
    var res: List[List[Long]] = List()
    var odd: Boolean = true
    var last: Long = 0
    for( l <- ll ) {
       if ( odd ) {
         last = l
       } else {
         res = List(last, l) :: res
         println("pair: ", last, l)
       }
       odd = !odd
    }
    res
}

var loc: Long = 0
var seedpairs: List[List[Long]] = List()
var lines = scala.io.Source.stdin.getLines()
var seedline = lines.next
if (seedline.startsWith("seeds:" )){
    var seeds = seedline.substring(7).split(" ").toList.map(stoLong)
    seedpairs = pairup(seeds)

}
var mappings : List[MapSet] = getmaps(lines)

var minloc:Long = 9999999999L
for (sp <- seedpairs) {
    for (s <-sp(0) to (sp(0) + sp(1))) {
      loc = apply_mappings(mappings, s)
      println(("seed", s, "maps to", loc))
      if (loc < minloc) {
           minloc = loc
      }
    }
}
println(("minloc = ",  minloc))

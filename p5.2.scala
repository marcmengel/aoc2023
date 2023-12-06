
import scala.util.matching.Regex

def stoLong(s:String): Long = { s.toLong }

class Map(nl: List[Long]) {

    def apply(n: Long): Long = {
        var res = n
        if (n >= nl(1) && n < nl(1) + nl(2)) {
            res = n - nl(1) + nl(0)
            println(("mapping", n, "->", res, "(",nl(0), nl(1), nl(2),")"))
        }
        res
    }
    def partition(ll : List[Long]) : List[Long] = {
        var llout = ll
        var llit = ll.iterator
        var prev = 0L
        var odd: Boolean = false
        for( cur <- llit ) {
            odd = !odd
            if (odd) {
                 prev = cur
            } else { 
              //println("checking range:", prev, cur)
              var end = nl(1) + nl(2) - 1

              if( prev < end && end+1 < cur ) {
                    llout = end :: end+1  :: llout
                    println("partitioning at ", end)
              }
              if( prev < nl(1)-1 && nl(1) < cur ) {
                    llout = nl(1)-1 :: nl(1) :: llout
                    println("partitioning at ", nl(1))
              }
              var llseq : Seq[Long] = llout
              llseq = llseq.sorted
              llout = llseq.toList
          }
        }
        var llseq : Seq[Long] = llout
        llseq = llseq.sorted
        return llseq.toList
    }
}

class MapSet(_name: String, ml: Array[Map]) {
  def apply(snin: Long): Long = {
      var sn = snin
      for (m <- ml) {
          sn = m.apply(sn)
          if (sn != snin) {
             return sn
          }
      }
      sn
  }
  def name(): String = { _name }
  def partition(ll : List[Long]) : List[Long] = {
      var llout = ll
      for (m <- ml) {
          llout = m.partition(llout)
      }
      return llout
   }
}

def partition_and_map(ml: List[MapSet], ll: List[Long]): List[Long] = {
   var llout = ll
   for (m <- ml) {
      llout = m.partition(llout)
      llout = llout.map(m.apply)
      var llseq : Seq[Long] = llout
      llseq = llseq.sorted
      llout = llseq.toList
      println("p_and_m: ", m.name(), llout.length, llout)
   }
   return llout
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

def pairparts(sl : List[List[Long]] ) : List[Long] = {
    var llout : List[Long] = List()
    for( sp <- sl ) {
        llout = sp(0) :: sp(0) + sp(1) - 1 :: llout
    }
    var llseq : Seq[Long] = llout
    llseq = llseq.sorted
    return llseq.toList
    return llout
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
// make initial partition lists
var llwork = pairparts(seedpairs)
print("inital seed ranges: ", llwork)
llwork = partition_and_map( mappings, llwork)
for( loc <- llwork) {
    if (loc < minloc) {
         println("new min", loc)
         minloc = loc
    }
}
println(("minloc = ",  minloc))

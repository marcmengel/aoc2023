import collection.mutable.Set
def expand(map: Array[String]) : List[(Long, Long)] = {

   // first do vertical expansion, and build list of galaxies coords
   // and colums that are taken == have galaxies
   var glist : List[(Long, Long)] = List()
   var taken : Set[Long] = Set()
   var row = 0L
   for(s <- map) {
      if (s.indexOf('#') == -1) {
         // empty row, expand to 1 million
         row = row + 999999
      } else {
        var col:Long = s.indexOf('#') 
        while( col != -1 ) {
          taken += col
          glist = (row, col) :: glist
          col = s.indexOf('#',col.toInt + 1)
        }
      }
      row = row + 1
    }

    // find empty columns
    var ecolumns : List[Long] = List()
    for( i <- Range(0,map(0).length) )
        if ( !taken(i) )
          ecolumns = i :: ecolumns

    // now build v-expanded list of coords
    var vexp : List[(Long,Long)] = List()
    for( pair <- glist) {
      var col = pair._2
      for( ecol <- ecolumns ) {
         if (pair._2 > ecol) {
            col = col + 999999
         }
      }
      vexp = (pair._1, col) :: vexp
    }
    return vexp
}

def pairs(lin: List[(Long, Long)]): List[((Long,Long),(Long, Long))] = {
   var res : List[((Long,Long),(Long, Long))] = List()
   var sl = lin
   for(x <- lin) {
      sl = sl.tail
      for( y <- sl) {
        res = (x, y) :: res
      }
   }
   return res
}

def getLen( pair: ((Long,Long),(Long, Long))) : Long = {
    return ( (pair._2._1 - pair._1._1).abs +  (pair._2._2 - pair._1._2).abs)
}

val sum = (ii: List[Long]) => {
  var res: Long = 0
  for (i <- ii) {
     res = res + i
  }
  res
}

var map = scala.io.Source.stdin.getLines().toArray

var loclist : List[(Long, Long)] =  expand(map)

var res = sum( pairs(loclist).map(getLen) )
println(("sum: ", res))

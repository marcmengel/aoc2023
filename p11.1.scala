import collection.mutable.Set
def expand(map: Array[String]) : List[(Int, Int)] = {

   // first do vertical expansion, and build list of galaxies coords
   // and colums that are taken == have galaxies
   var glist : List[(Int, Int)] = List()
   var taken : Set[Int] = Set()
   var row = 0
   for(s <- map) {
      if (s.indexOf('#') == -1) {
         // empty row, double
         row = row + 1
      } else {
        var col = s.indexOf('#') 
        while( col != -1 ) {
          taken += col
          glist = (row, col) :: glist
          col = s.indexOf('#',col+1)
        }
      }
      row = row + 1
    }

    // find empty columns
    var ecolumns : List[Int] = List()
    for( i <- Range(0,map(0).length) )
        if ( !taken(i) )
          ecolumns = i :: ecolumns

    // now build v-expanded list of coords
    var vexp : List[(Int,Int)] = List()
    for( pair <- glist) {
      var col = pair._2
      for( ecol <- ecolumns ) {
         if (pair._2 > ecol) {
            col = col + 1
         }
      }
      vexp = (pair._1, col) :: vexp
    }
    return vexp
}

def pairs(lin: List[(Int, Int)]): List[((Int,Int),(Int, Int))] = {
   var res : List[((Int,Int),(Int, Int))] = List()
   var sl = lin
   for(x <- lin) {
      sl = sl.tail
      for( y <- sl) {
        res = (x, y) :: res
      }
   }
   return res
}

def getLen( pair: ((Int,Int),(Int, Int))) : Int = {
    return ( (pair._2._1 - pair._1._1).abs +  (pair._2._2 - pair._1._2).abs)
}

val sum = (ii: List[Int]) => {
  var res: Int = 0
  for (i <- ii) {
     res = res + i
  }
  res
}

var map = scala.io.Source.stdin.getLines().toArray

var loclist : List[(Int, Int)] =  expand(map)

var res = sum( pairs(loclist).map(getLen) )
println(("sum: ", res))

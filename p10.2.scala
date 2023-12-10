
def find_s (map: Array[String] ) : (Int,Int) = {
  var count = 0
  for( s <- map ) {
     var n: Int = s.indexOf('S')
     if (n > 0) 
        return (count, n)
     count = count +1
  }
  return(-1, -1)
}
   
def find_adj( t: (Int, Int, Int), map: Array[String] ): List[(Int,Int)] = {
   var res : List[(Int, Int)] = List()

   // look for pipes adjacent to t
   if ( List('S','|','J','L').contains(map(t._1)(t._2)) && List('|','F','7').contains(map(t._1 - 1)(t._2)) )
     res = (t._1 - 1, t._2) :: res
   if ( List('S','|','F','7').contains(map(t._1)(t._2)) && List('|','J','L').contains(map(t._1 + 1)(t._2)) )
     res = (t._1 + 1, t._2) :: res
   if ( List('S','-','7','J').contains(map(t._1)(t._2)) && List('-','F','L').contains(map(t._1 )(t._2 - 1)) )
     res = (t._1, t._2 - 1) :: res
   if ( List('S','-','F','L').contains(map(t._1)(t._2)) && List('-','7','J').contains(map(t._1 )(t._2 + 1)) )
     res = (t._1, t._2 + 1) :: res
   return res
}

import scala.collection.mutable._

def fp(map: Array[String]) : Int = {
    var t: (Int, Int, Int) = (0,0,0)
    var cs = find_s(map)
    var found = true

    println(("found s at:", cs))

    var visited : List[(Int,Int)] = List(cs)

    var workq : Queue[(Int,Int,Int)] = Queue()

    workq += ((cs._1, cs._2, 0))
    while( ! (workq.isEmpty)) {
      //println(("found loop: workq, visited: ", workq, visited))
      found = false
      t = workq.dequeue
      println(("visiting:", t))
      visited = (t._1,  t._2) :: visited
      for (cc <- find_adj(t, map)) {
         println(("adj:", cc))
         if ( !visited.contains( (cc._1,  cc._2)) ) {
           println("queing..")
           workq += ((cc._1, cc._2, t._3 + 1))
           found = true
        }
      }
    }
    var area = 0
    // scan for inside/outside.  If we have crossed an odd number of edges
    // we are inside, if we have crossed an even number we are outside.
    var outside : Boolean = true
    var h_edge_start : Char = '.'
    for( i <- Range(0,map.length) ) {
      outside = true  // we start each row outside...
      for( j <- Range(0,map(i).length)) {
         var pc = map(i)(j)
         if (visited.contains((i,j))) {
            if (map(i)(j) == '|') {
               //println(("| bound at",i,j))
               outside = ! outside
            } else {
              // should be cleverer, but for the test case this should work
              if (map(i)(j) == 'S')
                  h_edge_start = 'F'

              if (map(i)(j) == 'L' || map(i)(j)=='F')  {
                 h_edge_start = map(i)(j)
              } else {
                 if( map(i)(j) == '7' && h_edge_start == 'L')
                   outside = ! outside
                 if( map(i)(j) == 'J' && h_edge_start == 'F')
                   outside = ! outside
              }
            }
            print("[1m")
            print(pc)
            print("[m")
         } else {
           if ( !outside ) {
              area = area + 1
              pc = 'I'
           }
           print(pc)
         }
      }
      println("")
    }
    return area
}

var map = scala.io.Source.stdin.getLines().toArray
println("map:")
for (s <- map) 
   println(s)

var md = fp(map)
println(("area",md))

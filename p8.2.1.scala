
import scala.util.matching.Regex

val graphre: Regex = "([a-zA-Z]+) *= *\\(([a-zA-Z]+), *([a-zA-Z]+)\\)".r

def parse_graph( lines : Iterator[String] ) : Map[String, (String, String)] = {
    var m : Map[String, (String, String)] = Map()
    for( line <- lines ) {
       line match {
         case graphre( name, left, right ) => {
             m = m.updated(name, (left, right))
           }
       }
       println("parse_graph", m )
    }
    return m
}


def main():Unit = {
  var lines = scala.io.Source.stdin.getLines()

  var directions = lines.next
  var skip = lines.next

  var graph = parse_graph(lines)

  var gcur: Map[String, String] = Map()

  for (pg <- graph.keys ) {
     if(pg(2) == 'A') {
        gcur = gcur.updated(pg, pg)
     }
  }

  println(("graph",graph))

  var cur = ""
  var dl = directions.length
  for (start <- gcur.keys) {
   var count = 0
   for (rep <- Range(0,5)) {
    if (rep == 0) {
       cur = start
    } else {
       if (directions(((count % dl)+dl)%dl) == 'L') {
          //println(("move: ", cur, "L", graph(cur)._1))
          cur = graph(cur)._1
       } else {
          //println(("move: ", cur, "R", graph(cur)._2))
          cur = graph(cur)._2
       }
    }
    println(("ghost, rep :",start, rep))
    while( cur(2) != 'Z' ) {
       
       var prev = cur
       if (directions(((count % dl)+dl)%dl) == 'L') {
          //println(("move: ", cur, "L", graph(cur)._1))
          cur = graph(cur)._1
       } else {
          //println(("move: ", cur, "R", graph(cur)._2))
          cur = graph(cur)._2
       }
       if (cur == prev) {
          println("Loop detected")
          return ()
       }
       count = count + 1
    }
    println(("count", count))
  }
  }
}

main

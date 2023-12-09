
import scala.util.matching.Regex

// Note:  if you run this on the AOC data it should find the answer... in about 50 years.
//        But you can use the output to figure out how often each of the ghosts get to
//        their final state at the same time as the last ghost, and do some Least Common Multiple
//        math to get the answer.    Or you can wait :-)

val graphre: Regex = "([a-zA-Z0-9]+) *= *\\(([a-zA-Z0-9]+), *([a-zA-Z0-9]+)\\)".r

def parse_graph( lines : Iterator[String] ) : Map[String, (String, String)] = {
    var m : Map[String, (String, String)] = Map()
    for( line <- lines ) {
       line match {
         case graphre( name, left, right ) => {
             m = m.updated(name, (left, right))
           }
       }
    }
    println("parse_graph", m )
    return m
}

def isdone( gcur :Map[String,String]):Boolean = {
  for( g <- gcur.keys ){
     if (gcur(g)(2) != 'Z') {
        return false
     }
  }
  return true
}

def main():Unit = {
  var lines = scala.io.Source.stdin.getLines()

  var directions = lines.next.strip()
  var skip = lines.next

  var graph = parse_graph(lines)

  var gcur: Map[String, String] = Map()

  for (pg <- graph.keys ) {
     if(pg(2) == 'A') {
        gcur = gcur.updated(pg, pg)
     }
  }

  var count = 0
  println(("graph",graph))
  val dl = directions.length
  println("dl: ", dl)
  while( !isdone(gcur) ) {
     for( g <- gcur.keys ) {
       var prev = gcur(g)
       if (directions(((count % dl) + dl)% dl) == 'L') {
          //println(("move: ", gcur(g), "L", graph(gcur(g))._1))
          gcur = gcur.updated(g,  graph(gcur(g))._1)
       } else {
          //println(("move: ", gcur(g), "R", graph(gcur(g))._2))
          gcur = gcur.updated(g,  graph(gcur(g))._2)
       }
       if (gcur(g) == prev) {
          println("Loop detected")
          return ()
       }
    }
    println(("s: ", count, gcur))
    count = count + 1
  }
  println(("count", count))
}

main

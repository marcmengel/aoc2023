
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

  var cur = "AAA"
  var count = 0
  println(("graph",graph))
  while( cur != "ZZZ" ) {
     var prev = cur
     if (directions(count % directions.length) == 'L') {
        println(("move: ", cur, "L", graph(cur)._1))
        cur = graph(cur)._1
     } else {
        println(("move: ", cur, "R", graph(cur)._2))
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

main

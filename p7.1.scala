

import scala.util.matching.Regex


class HandStore(val htype: Integer, val hand: String , val bid:Long) 

def get_handtype(hand:  String) : Integer = {
    var htype: Integer = 0
    var counts : Map[Char, Int] = Map()

    for(c <- hand) {
       counts = counts.updated(c, counts.getOrElse(c, 0) + 1)
    }
    var countl = counts.toList.sortBy( (x:(Char,Int))=>(-x._2) )
    println("countl", countl)

    if (countl(0)._2 == 5) 
       htype=7  // five of a kind
    if (countl(0)._2 == 4) 
       htype=6  // four of a kind
    if (countl(0)._2 == 3) 
       if( countl(1)._2 == 2)
         htype=5 // full house
       else
         htype=4 // three of a kind
    if (countl(0)._2 == 2) 
       if( countl(1)._2 == 2)
         htype=3 // two pair
       else
         htype=2 // one pair
    htype
}

var hsl : List[HandStore] = List()
var lines = scala.io.Source.stdin.getLines()
for (l <- lines) {
   var f = l.strip().split("  *")
   println("Got fields",f(0), f(1))
   // replace A with Z and K with Y, then string sorting hands works to order them...
   hsl = new HandStore(get_handtype(f(0)), f(0).replace('A','Z').replace('K','Y').replace('T','B'),  f(1).toInt) :: hsl
}

var hss = hsl.sortBy( (x:HandStore)=>(x.htype.toString + x.hand))
var score = 0L
var i = 1L
for (hs <- hss) {
    println(("Rank, hand, bid: ", i, hs.htype, hs.hand, hs.bid))
    score = score + (hs.bid * i)
    i = i + 1
}
println(("score"),score)



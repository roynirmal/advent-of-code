import scala.io.Source
import scala.collection.mutable.ListBuffer
val filename = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/calorie.txt"
val rps_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/rps.txt"


@main def maxcalorie: Unit = 
  
  var c: Int = 0;
  var max: Int = 0;
  println("Before")
  println( max)
  for(line <- Source.fromFile(filename).getLines)
      { 
        if (line.isEmpty()){
          if (c >= max){
            max = c
          }
          c = 0; 
        } else {
          var num = line.toInt
          c += num
        }
      }
  println("After")
  println(max)


@main def top3elves: Unit = 
  
  var c: Int = 0;
  val all_c = new ListBuffer[Int]()
  var tmp: Int = 0;
  for(line <- Source.fromFile(filename).getLines)
      { 
        if (line.isEmpty()){
          all_c += c
          c = 0; 
          tmp += 1
        } else {
          var num = line.toInt
          c += num
        }
      }
      all_c += c
      println(all_c.sortWith(_ > _).take(3).sum)  // println()


val rps = Map("X" -> 1, "Y" -> 2, "Z" -> 3)
@main def rps_1: Unit = 
  
  val outcome = Map("A X" -> 3, "A Y" -> 6, "A Z" -> 0, "B X" -> 0, "B Y" -> 3, "B Z" -> 6, "C X" -> 6, "C Y" -> 0, "C Z" -> 3)
  var total: Int = 0;
  for(line <- Source.fromFile(rps_file).getLines) {
    total += outcome(line)
    total += rps(line.split(" ")(1))
    }
  println(total)


@main def rps_2: Unit = 
  val rps_score = Map("X" -> 0, "Y" -> 3, "Z" -> 6)
  val strategy = Map("A X" -> "Z", "A Y" -> "X", "A Z" -> "Y", "B X" -> "X", "B Y" -> "Y", "B Z" -> "Z", "C X" -> "Y", "C Y" -> "Z", "C Z" -> "X")
  var total: Int = 0;
  for(line <- Source.fromFile(rps_file).getLines) {
    total += rps(strategy(line))
    total += rps_score(line.split(" ")(1))
    }
  println(total)
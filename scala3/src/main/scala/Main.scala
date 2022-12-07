import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._
val filename = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/calorie.txt"
val rps_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/rps.txt"
val rucksack_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/rucksack.txt"
val cleanup_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/cleanup.txt"
val cont_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/container.txt"

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

val small = ('a' to 'z').toList
val big = ('A' to 'Z').toList
val full = small ::: big
@main def ruck_1: Unit = 
  var output: Int = 0;
  for(line <- Source.fromFile(rucksack_file).getLines) {
    val parts = line.grouped(line.length()/2).toList
    val common = (parts(0) intersect parts(1))(0)
    output += full.indexOf(common) + 1
    }
  println(output)

@main def ruck_2: Unit = 
  val groups = new ListBuffer[String]()
  var output: Int = 0;
  var counter: Int = 0;
  for(line <- Source.fromFile(rucksack_file).getLines) {
    groups += line
    counter += 1
    
    if (counter == 3) {
      val parts = groups.toList
      val common = (parts(0) intersect parts(1) intersect parts(2))(0)
      output += full.indexOf(common) + 1
      counter = 0
      groups.clear()
    }
  }
  println(output)


@main def cleanup: Unit =
  var c_1 : Int = 0;
  var c_2 : Int = 0;
  for(line <- Source.fromFile(cleanup_file).getLines)
      {
        val fe = line.split(",")(0)
        val se = line.split(",")(1)

        val ferf = fe.split("-")(0).toInt to fe.split("-")(1).toInt
        val serf = se.split("-")(0).toInt to se.split("-")(1).toInt
        val interL = (ferf intersect serf).length

        if (interL == ferf.length || interL == serf.length){
          c_1 += 1
        } 
        if (interL > 0) {
          c_2 += 1
        }
      }
    println(c_1)
    println(c_2)

val x = Map(1 -> "HRBDZFLS", 2 -> "TBMZR", 3-> "ZLCHNS", 4 -> "SCFJ", 5->"PGHWRZB", 6->"VJZGDNMT", 7-> "GLNWFSPQ", 8-> "MZR", 9->"MCLGVRT")
var ship: scala.collection.mutable.Map[Int, String] = scala.collection.mutable.Map(x.toSeq: _*)
var ship_2: scala.collection.mutable.Map[Int, String] = scala.collection.mutable.Map(x.toSeq: _*)

@main def ship_1: Unit =
  for(line <- Source.fromFile(cont_file).getLines) {
    val num = line.split(" ")(1).toInt
    val from = line.split(" ")(3).toInt
    val to = line.split(" ")(5).toInt
    // println(from)
    for (a <- 1 to num ){
      ship(to) += ship(from).takeRight(1)
      ship(from)  = ship(from).dropRight(1)
    }
    ship_2(to) += ship_2(from).takeRight(num)
    ship_2(from)  = ship_2(from).dropRight(num)
  }
  var result = ""
  ship_2.foreach{ case (k,v) => 
    result += v.takeRight(1)
    } 
  println(result)
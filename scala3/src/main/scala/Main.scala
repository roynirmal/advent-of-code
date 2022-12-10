import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._
import scala.collection.mutable.{Map => MutableMap}
import collection.JavaConverters.seqAsJavaListConverter
// import scala.collection.mutable.{Map => MutableMap, MutableList}
val filename = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/calorie.txt"
val rps_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/rps.txt"
val rucksack_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/rucksack.txt"
val cleanup_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/cleanup.txt"
val cont_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/container.txt"
val sig_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/signal.txt"
val size_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/filesize.txt"
val tree_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/tree.txt"
val rope_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/rope.txt"
val noop_file = "/Users/nirmalroy/Desktop/SearchX/advent-of-code/scala3/data/noop.txt"

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

var c: Int = 0;
var check: Int = 0; 
val start_marker: Int = 14;

@main def signal_1: Unit =
  var old_char: String = ""
  for(line <- Source.fromFile(sig_file).getLines) {
  breakable{
    for (item <- line){
      if (old_char.length < start_marker){
        old_char += item.toString
      }
      if (old_char.length == start_marker){
        if (old_char.distinct.length == old_char.length){
          println(c)
          break()
        }
        old_char = old_char.drop(1)
        old_char += item.toString
      }
      c += 1
    }
  }
}


@main def dir_1: Unit = 
  var listing: Boolean = false
  var last_dir: String = ""
  var dir_size = scala.collection.mutable.Map[String, Int]()
  var dir_cum_size = scala.collection.mutable.Map[String, Int]()
  var dir_struct = MutableMap[String, ListBuffer[String]]()
  var parent: String = ""
  var me: String = ""
  case class Tree(value: Int, child: Tree)
  // val sessionVariable: MutableMap[String, Int] = MutableMap()
  for(line <- Source.fromFile(size_file).getLines) {
    var commands = line.split(" ")
    if (commands(0) == "$"){
      listing = false
    }
    if (listing == true){
      if (commands(0) == "dir"){
        var child = commands(1)
        // path_string += last_dir + "_" + commands(1)
        println(parent + "_" + me + "_" + child)  

      } else {
        // println(commands(1))
        if (dir_size.exists(_._1 == last_dir)){
          dir_size(last_dir) += commands(0).toInt
        } else {
          dir_size(last_dir) = commands(0).toInt
        }
        
      }
    }
    if (commands(1) == "cd"){
      parent = me
      me = commands(2)  
    }
    if(commands(1) == "ls"){
      // println(last_dir)  
      listing = true
    }
  }
  println(dir_size)

@main def tree_1: Unit =
  var twoDimenstionalList =  scala.collection.mutable.Map[Int, Array[Int]]()
  var row = 0
  for(line <- Source.fromFile(tree_file).getLines) {
    var col = 0
    val p = line.map(_.asDigit).toArray
    twoDimenstionalList(row) = p
    row += 1
  } 
  var c = 0
  var max_scene = 0
  for ( i <- (1 to row - 2)){
    for (j <- (1 to row - 2)){
      val el = twoDimenstionalList(i)(j)
      var seen = false
      var left = 0
      var right = 0
      var up = 0
      var down = 0
      if( el != 0){
        // go left
        breakable {
          for (k <- (j-1) to (0,-1)){
            if (twoDimenstionalList(i)(k) >= el){
              left += 1
              break
            }
            left += 1
          }
          seen = true
        }
        // go right
        breakable {
          for (k <- (j+1) to (row - 1)){            
            if (twoDimenstionalList(i)(k) >= el){
              right += 1
              break
            } 
            right += 1           
          }
          seen = true
        }
                // go down        
        breakable {
          for (k <- (i+1) to (row - 1)){
            if (twoDimenstionalList(k)(j) >= el){
              down +=1
              break
            }
            down += 1
          }
          seen = true
        }
                // go up
        breakable {
          for (k <- (i-1) to (0,-1)){
            if (twoDimenstionalList(k)(j) >= el){
              up += 1
              break
            }
            up += 1
          }
          seen = true
        }
      }
      if (seen == true) c += 1
      // println(max_scene)
      if (max_scene < left*right*up*down) max_scene = left*right*up*down
    }
  }
  println(max_scene)


@main def rope_1: Unit =
  var tail_visits = new ListBuffer[(Int, Int)]()
  var head_cd = (0,0)
  var tails =  scala.collection.mutable.Map[Int, (Int, Int)]()
  for (i <- 1 to 9){
    tails(i) = ((0,0))
  }
  println(tails)
  def check_tail_near_head(h: (Int, Int), t: (Int, Int)): Boolean = {
    var poss_coords = new ListBuffer[(Int, Int)]() 
    val inc = List((0,0), (1,0), (0,1), (-1, 0), (0, -1), (1,1), (1, -1), (-1, 1), (-1, -1))
    for (i <- inc){
      poss_coords += ((t(0)+i(0), t(1) +i (1) ) )
    }
    if (poss_coords contains h) {
      return true
    }
    return false
  }
  for(line <- Source.fromFile(rope_file).getLines) {
    var dir = line.split(" ")(0)
    var steps = line.split(" ")(1).toInt

    for (i <- 1 to steps){
      if (dir == "U"){
        head_cd = (head_cd(0)+1, head_cd(1))
      }
      if (dir == "D"){
        head_cd = (head_cd(0)-1, head_cd(1))
      }
      if (dir == "R"){
        head_cd = (head_cd(0), head_cd(1)+1)
      }
      if (dir == "L"){
        head_cd = (head_cd(0), head_cd(1)-1)
      }
      var big_head = head_cd
      for (i <- 1 to 9){
        var tail_cd = tails(i)
        if (!(check_tail_near_head(big_head, tail_cd))){
          if (big_head(0) == tail_cd(0)){
            tail_cd = (tail_cd(0), (big_head(1)- tail_cd(1)).sign + tail_cd(1))
          } else if (big_head(1) == tail_cd(1)){
            tail_cd = ( (big_head(0)- tail_cd(0)).sign + tail_cd(0), tail_cd(1))
          } else {
            tail_cd = ( (big_head(0)- tail_cd(0)).sign + tail_cd(0), (big_head(1)- tail_cd(1)).sign + tail_cd(1))
          }
        }
        tails(i) = tail_cd
        big_head = tails(i)
      }
      // println(tails(9))
      tail_visits += tails(9)
    }
  }
  println(tail_visits.distinct.length)

@main def noop: Unit = 
  var X: Int = 1;
  var clock: Int = 1;
  val checkpoints = List(20, 60, 100, 140, 180, 220)
  var output: Int = 0;
  var pixel: String = "";
  def check_clock(x_val: Int) : Unit = {
    clock += 1
    if (checkpoints contains clock) {
      output += (x_val*clock)
    }
  }
  def draw_pixel(s_mid: Int) : Unit = {
    if (List(s_mid, s_mid+1, s_mid-1) contains pixel.length){
      pixel += "#"
    } else {
      pixel += "."
    }
    if (clock % 40 == 0){
      println(pixel)
      pixel = ""
    }
  }

  for(line <- Source.fromFile(noop_file).getLines) {
    val instruct = line.split(" ")(0)
    if (instruct == "noop"){
      check_clock(X)
      draw_pixel(X)
    } else {
      for (i <- 1 to 2){
        if (i == 2) {
          X += line.split(" ")(1).toInt
        }
        check_clock(X)
        draw_pixel(X)
      }
    }
  }
  println(output)
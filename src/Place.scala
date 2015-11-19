

import java.awt.Point
import javax.swing.ImageIcon

class Place (p :Point, n : String) {
  val pos :Point = p
  val name :String = n
}


class Tunnel (p :Point, n :String, ex : Place, ico :ImageIcon) extends Place (p, n){
  val exit :Place = ex
  var ant :Boolean = false
  var typeant :String = " "
  var bees :List[Insect] = Nil
  val icon: ImageIcon = ico
  val im = icon.getImage()

  def addbee (b :List[Insect]) = {
    bees = bees ::: b
  }
  def removebee (b : Insect) = {
  bees =  bees diff List(b)
  }
  def removeant = {
    ant = false
  }
  def addant (t : String) {
    if (!ant) {
       ant = true
       typeant = t
    }
  }
  class Cell (p : Point, n : String, t :String) extends Place (p, n) {
    val typeant :String = t
    var is_selected :Boolean = false
    
    def buy_ant(money : Int, cost : Int, tun : Tunnel) = {
      if (money > cost && !tun.ant) {
        tun.ant = true
        tun.typeant = typeant
        //money = money - cost. how to remove money? pointers?
      }
    }
  }
  class Hive (L : List[Cell]) extends Place (new Point(0,0), "Hive") {
    val Cells : List[Cell] = L
    def select (c: Cell) {
      for (cell <- Cells) {
        cell.is_selected = false
      }
      c.is_selected = true
    }
  }
}
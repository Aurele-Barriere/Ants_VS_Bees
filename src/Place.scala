

import java.awt.Point
import javax.swing.ImageIcon

class Place(p: Point, n: String) {
  val pos: Point = p
  val name: String = n
}

class Tunnel(p: Point, n: String, ex: Place, ico: ImageIcon) extends Place(p, n) {
  val exit: Place = ex

  var typeant: Ant = new None(this)
  var bees: List[Bee] = Nil
  val icon: ImageIcon = ico
  val im = icon.getImage()

  def addbee(b: Bee) = {
    bees = b :: bees
  }
  def removebee(b: Bee) = {
    bees = bees diff List(b)
  }
  def removeant = {
    typeant = new None(this)
  }
  def addant(t: Ant) {
    typeant match {
      case n: None => typeant = t
    }
  }

  def left_neighbour(n: Int):Place = {
    n match {
      case 0 => return this
      case m: Int => this.exit match {
        case t: Tunnel => return  t.left_neighbour(m - 1: Int)
        case h: Hive   => return h
      }
    }
  }
  def right_neighbour(n: Int, l: List[Tunnel]) :Place = {
    for (t:Tunnel <- l:List[Tunnel]) {
      if (t.left_neighbour(n) == this) { return t}
    }
    return this
  }
}
 
class Cell(p: Point, n: String) extends Place(p, n) {
  var is_selected: Boolean = false
}

class Bye(p: Point, n: String) extends Cell(p, n) {
  def erase(tun: Tunnel) = {
    tun.typeant = new None(tun)
  }
}

class CellAnt(p: Point, n: String, t: Ant) extends Cell(p, n) {
  val typeant: Ant = t

  def buy_ant(p: Purse, tun: Tunnel) = {
    tun.typeant match {
      case n: None => if (p.money >= this.typeant.cost) {
        p.take_money(this.typeant.cost)
        tun.typeant = this.typeant
      }
    }
  }
}

class Hive(L: List[Cell]) extends Place(new Point(0, 0), "Hive") {
  val Cells: List[Cell] = L
  def select(c: Cell) {
    for (cell <- Cells) {
      cell.is_selected = false
    }
    c.is_selected = true
  }
}

class Entrance(p: Point, n: String, t: Tunnel) extends Place(p, n) {
  val exit: Tunnel = t
  val bees: List[Bee] = Nil

}


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
}

class Cell(p: Point, n: String, t: Ant) extends Place(p, n) {
  val typeant: Ant = t
  var is_selected: Boolean = false

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
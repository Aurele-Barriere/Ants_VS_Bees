

import java.awt.Point
import javax.swing.ImageIcon

class Place(p: Point) {
  val pos: Point = p
}

class Tunnel(p: Point, ex: Place, en: Place, ico: ImageIcon) extends Place(p) {
  lazy val exit: Place = ex
  lazy val entrance: Place = en
  val ground = true
  var ant: Option[Ant] = None
  var bees: List[Bee] = Nil
  val icon: ImageIcon = ico
  val im = icon.getImage()

  def is_clicked(click: Point) = {
    if (click.x < p.x + icon.getIconWidth() && click.x > p.x && click.y < p.y + icon.getIconHeight() && click.y > p.y) { true } else { false }
  }
  def addbee(b: Bee) = {
    bees = b :: bees
  }
  def removebee(b: Bee) = {
    bees = bees diff List(b)
  }
  def removeant ()= {
    ant match {case Some(a) => a.armor = 0}
    ant = None
  }
  def addant(t: Ant) {
    if (ant == None) {ant = Some(t)}
  }
// deprecated neighbour code? You might want to check on this
  def left_neighbour(n: Int): Place = {
    n match {
      case 0 => return this
      case m: Int => this.exit match {
        case h: Hive   => return h
        case t: Tunnel => return t.left_neighbour(m - 1)
      }
    }
  }
  def right_neighbour(n: Int): Place = {
    n match {
      case 0 => return this
      case m: Int => this.entrance match {
        case e: Entrance => return e
        case t: Tunnel   => return t.right_neighbour(m - 1)
      }
    }
  }
}

class Cell(p: Point) extends Place(p) {
  var is_selected: Boolean = false
  val width: Int = 100
  val height: Int = 100
  def is_clicked(click: Point) = {
    if (click.x < p.x + width && click.x > p.x && click.y < p.y + height && click.y > p.y) { true } else { false }
  }
  def buy_ant(p: Purse, tun: Tunnel) = {

  }
}

class Bye(p: Point) extends Cell(p) {
  override def buy_ant(p: Purse, tun: Tunnel) = {
    tun.removeant()
  }
}

class CellAnt(p: Point, t: Ant) extends Cell(p) {
  val typeant: Ant = t

  override def buy_ant(p: Purse, tun: Tunnel) = {
    if (AntsBees.state.purse.money > this.typeant.cost && tun.ant == None && (this.typeant.watersafe || tun.ground)) {
    this.typeant match {
      case a: Harvester     => tun.ant = Some(new Harvester(new Point(tun.pos.x, tun.pos.y), tun))
      case a: Thrower       => tun.ant = Some(new Thrower(new Point(tun.pos.x, tun.pos.y), tun))
      case a: Short_Thrower => tun.ant = Some(new Short_Thrower(new Point(tun.pos.x, tun.pos.y), tun))
      case a: Long_Thrower  => tun.ant = Some(new Long_Thrower(new Point(tun.pos.x, tun.pos.y), tun))
      case a: Fire          => tun.ant = Some(new Fire(new Point(tun.pos.x, tun.pos.y), tun))
      case a: Scuba         => tun.ant = Some(new Scuba(new Point(tun.pos.x, tun.pos.y), tun))
      case a: Ninja         => tun.ant = Some(new Ninja(new Point(tun.pos.x, tun.pos.y), tun))
      case a: Hungry        => tun.ant = Some(new Hungry(new Point(tun.pos.x, tun.pos.y), tun))
      case a: Queen         => tun.ant = Some(new Queen(new Point(tun.pos.x, tun.pos.y), tun))

      //to do? or is there a more simpler way? 
    }
    tun.ant match {
      case Some(a) => 
        AntsBees.state.Insects = a :: AntsBees.state.Insects
        AntsBees.state.purse.money -= this.typeant.cost //taking money
    }
   }
  }
}
class Hive(L: List[Cell], t: Tunnel) extends Place(new Point(0, 0)) {
  lazy val entrance: Tunnel = t
  lazy val Cells: List[Cell] = L
  def select(c: Cell) {
    for (cell <- this.Cells) {
      cell.is_selected = false
    }
    c.is_selected = true
  }
}

class Entrance(p: Point, t: Tunnel) extends Place(p) {
  lazy val exit: Tunnel = t
  var bees: List[Bee] = Nil
  def removebee(b: Bee) = {
    bees = bees diff List(b)
  }
}
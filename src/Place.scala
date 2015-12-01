

import java.awt.Point
import javax.swing.ImageIcon

class Place(p: Point) {
  val pos: Point = p
}

class Tunnel(p: Point, ex: Place, en: Place, ico: ImageIcon) extends Place(p) {
  var exit: Place = ex
  var entrance: Place = en //this might be ugly, but lazy declaration doesn't work for now
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
    ant match {case Some(a) => a.armor = 0 case None => }
    ant = None
  }
  def addant(t: Ant) {
    ant match {
      case None => {
        ant = Some(t)}
        if (t.unique) {
        AntsBees.state.uniqueUnits += 1
      }
      case Some(a) => {
        if (t.canContain(a)) {
          t.ant = Some(a)
          ant = Some(t)
        }
        if (a.canContain(t)) {
          a.ant = Some(t)
          ant = Some(a)
        }
      }
    }
        AntsBees.state.Insects = t :: AntsBees.state.Insects
  }
// deprecated neighbour code? You might want to check on this. Yes I don't think we will use it. 
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

class Water(p: Point, ex: Place, en: Place, ico: ImageIcon) extends Tunnel(p, ex, en, ico) {
  override val ground = false  
}

class CellAnt(p: Point, t: Ant) extends Cell(p) {
  val typeant: Ant = t

  override def buy_ant(p: Purse, tun: Tunnel) = {
    if (AntsBees.state.purse.money > this.typeant.cost && (this.typeant.watersafe || tun.ground)) {
    this.typeant match {
      case a: Harvester     => tun.addant(new Harvester(tun))
      case a: Thrower       => tun.addant(new Thrower(tun))
      case a: Short_Thrower => tun.addant(new Short_Thrower(tun))
      case a: Long_Thrower  => tun.addant(new Long_Thrower(tun))
      case a: Fire          => tun.addant(new Fire(tun))
      case a: Scuba         => tun.addant(new Scuba(tun))
      case a: Ninja         => tun.addant(new Ninja(tun))
      case a: Hungry        => tun.addant(new Hungry(tun))
      case a: Wall          => tun.addant(new Wall(tun))
      case a: Bodyguard     => tun.addant(new Bodyguard(tun))
      case a: Queen         => tun.addant(new Queen(tun))

      //to do? or is there a more simpler way? 
    }
    AntsBees.state.purse.money -= this.typeant.cost //taking money    
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
  def createbees(n :Int) :Unit = {
    if (n> 0) {
      val b = new Bee(this)
      this.bees = b :: this.bees
      AntsBees.state.Insects = b :: AntsBees.state.Insects
      createbees(n-1)
    }
  }
  
  
  
}
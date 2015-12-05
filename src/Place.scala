import java.awt.Point
import javax.swing.ImageIcon

class Place(p: Point) {
  val pos: Point = p
}

class Tunnel(p: Point, ex: Place, en: Place, ico: ImageIcon) extends Place(p) {
  var exit: Place = ex
  var entrance: Place = en
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
  def removeant() = {
    ant match { case Some(a) => a.armor = 0 case None => }
    ant = None
  }
  def addant(t: Ant) {
    ant match {
      case None =>
        {
          ant = Some(t)
          AntsBees.state.purse.money -= t.cost
        }
        if (t.unique) {
          if (AntsBees.state.uniqueUnits != 0) {
            t.isImpostor = true
          }
          AntsBees.state.uniqueUnits += 1
        }
      case Some(a) => {
        if (t.canContain(a)) {
          t.ant = Some(a)
          ant = Some(t)
          AntsBees.state.purse.money -= t.cost
        }
        if (a.canContain(t)) {
          a.ant = Some(t)
          ant = Some(a)
          AntsBees.state.purse.money -= t.cost
        }
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
    tun.ant match { case Some(a: Queen) => case _ => tun.removeant() }
  }
}

class Water(p: Point, ex: Place, en: Place, ico: ImageIcon) extends Tunnel(p, ex, en, ico) {
  override val ground = false
}

class CellAnt(p: Point, t: Ant) extends Cell(p) {
  val typeant: Ant = t

  override def buy_ant(p: Purse, tun: Tunnel) = {
    if (AntsBees.state.purse.money >= this.typeant.cost && (this.typeant.watersafe || tun.ground)) {

      val args = Array(tun).asInstanceOf[Array[AnyRef]]
      tun.addant((typeant.getClass.getConstructors()(0).newInstance(args: _*)).asInstanceOf[Ant])
    }
  }
}

class Hive(L: List[Cell]) extends Place(new Point(0, 0)) {
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
  def createbees(n: Int): Unit = {
    if (n > 0) {
      val b = new Bee(this)
      this.bees = b :: this.bees
      createbees(n - 1)
    }
  }
}

class Cave(alt: Int, h: Hive, tun: Int) {
  val waterProba = 10 //percentage of flooded tunnels
  val altitude: Int = alt
  val hive: Hive = h
  val numTunnels: Int = tun
  var Tunnels: List[Tunnel] = Nil
  val tunnelIcon: ImageIcon = new ImageIcon("img/tunnel.png")
  val waterIcon: ImageIcon = new ImageIcon("img/tunnel_water.png")
  val width = tunnelIcon.getIconWidth()
  val height = tunnelIcon.getIconHeight()
  var frequency = 1
  val t0 = new Tunnel(new Point(0, 0), null, null, tunnelIcon)
  var t1 = new Tunnel(new Point(0, alt * height + 300), hive, t0, tunnelIcon)
  Tunnels = t1 :: Tunnels
  for (i <- 2 to tun) {
    if (AntsBees.state.rng.nextInt(100) > waterProba) {
      Tunnels = new Tunnel(new Point(width * (i - 1), alt * height + 300), Tunnels.head, t0, tunnelIcon) :: Tunnels
    } else {
      Tunnels = new Water(new Point(width * (i - 1), alt * height + 300), Tunnels.head, t0, waterIcon) :: Tunnels
    }
  }
  for (i <- 1 to (tun - 1)) {
    Tunnels.apply(i).entrance = Tunnels.apply(i - 1)
  }
  var entrance = new Entrance(new Point(tun * width, alt), Tunnels.head)
  Tunnels.head.entrance = entrance
}
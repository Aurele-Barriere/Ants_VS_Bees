

import java.awt.Point
import javax.swing.ImageIcon

class Insect(p: Point, ico: ImageIcon, arm: Int) {
  val pos: Point = p
  val icon: ImageIcon = ico
  val im = icon.getImage()
  val width: Int = icon.getIconWidth()
  val height: Int = icon.getIconHeight()
  var armor: Int = arm //when goes down to 0, the insect dies :'(

  def inSprite(p: Point) = { //returns true if a given point is in the sprite
    (pos.x < p.x && p.x < pos.x + width &&
      pos.y < p.y && p.y < pos.y + height)
  }
}

class Bee(p: Point, lo: Place) extends Insect(p, new ImageIcon("img/bee.png"), 2) {

  var location: Place = lo
  def move() = {
    location match {
      case t: Tunnel => t.removebee(this)
    }
  }
}

// Ants

abstract class Ant(p: Point, ico: ImageIcon, arm: Int, co: Int, lo: Tunnel) extends Insect(p, ico, arm) {
  val location: Tunnel = lo
  val cost: Int = co
  def attack() = {

  }
}

class None(lo: Tunnel) extends Ant(new Point(0, 0), new ImageIcon("img/bee.png"), 100, 0, lo) {

}

// Basic Units

class Harvester(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_harvester.png"), 1, 2, lo) {
  override def attack() = { AntsBees.state.purse.add_money(1) }
}


class Thrower(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_thrower.png"), 1, 2, lo) {
  val damage:Int = 1
  def attacking (pl : Tunnel) :Unit = {
    pl.entrance match {
      case t:Tunnel => t.bees match {
        case Nil => this.attacking(t)
        case l:List[Bee] => l.head.armor -= damage
      }
    }
  }
  override def attack() = {attacking (this.location)}
}

class Short_Thrower(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_shortthrower.png"), 1, 3, lo) {
  val damage:Int = 1
  val range :Int = 2
  def attacking(pl :Place, n :Int) :Unit= {
    if (n>0) {
      pl match {
        case t :Tunnel => t.bees match {
          case Nil => attacking (t.entrance, n-1)
          case l :List[Bee] => l.head.armor -= damage
        }
      }
    }
  }
  override def attack() = {attacking(this.location, range)}
}

class Long_Thrower(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_longthrower.png"), 1, 3, lo) {
  val damage:Int = 1
  val deadrange :Int = 3
  def attacking (pl : Tunnel) :Unit = {
    pl.entrance match {
      case t:Tunnel => t.bees match {
        case Nil => this.attacking(t)
        case l:List[Bee] => l.head.armor -= damage
      }
    }
  }
  //We first need to jump over the dead space
  def charging (pl : Tunnel, n : Int) :Unit = {
    if (n>0) {
      charging(pl,n-1)
    } else {
      attacking(pl)
    }
  }
  override def attack() = {charging(this.location, deadrange)}
}

// Gimmicky ants

class Fire(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_thrower.png"), 5, 3, lo) {
  val damage:Int = 3
  def reduceArmor() : Unit= {
    if (this.armor < 1) {
      for (b <- this.location.bees) {
        b.armor -= damage
      }
    }
  }
  override def attack() : Unit = {
    reduceArmor ()
  }
}

class Scuba(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_scuba.png"), 5, 1, lo) {

}

class Wall(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_wall.png"), 4, 4, lo) {

}

class Ninja(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_ninja.png"), 6, 1, lo) {

}

class Hungry(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_hungry.png"), 4, 1, lo) {

}

// Bodyguard Ant currently has no sprite, thrower spite used as placeholder
class Bodyguard(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_thrower.png"), 4, 2, lo) {

}

// Here comes the queen

class Queen(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_queen.png"), 6, 2, lo) {

}


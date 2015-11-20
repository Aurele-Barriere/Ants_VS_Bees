

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

abstract class Ant(p: Point, ico: ImageIcon, arm: Int, co: Int, lo: Tunnel) extends Insect(p, ico, arm) {
  val location: Tunnel = lo
  val cost: Int = co
}

class None(lo: Tunnel) extends Ant(new Point(0, 0), new ImageIcon("img/bee.png"), 100, 0, lo) {}

class Short_Thrower(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_shortthrower.png"), 1, 3, lo) {

}


class Long_Thrower(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_longthrower.png"), 1, 3, lo) {

}
/we need to do more classes of ants
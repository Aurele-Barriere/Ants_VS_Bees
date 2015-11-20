

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
}


class None(lo: Tunnel) extends Ant(new Point(0, 0), new ImageIcon("img/bee.png"), 100, 0, lo) {
  
}


// Basic Units

class Harvester(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_harvester.png"), 1, 2, lo) {
  
}

class Thrower(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_thrower.png"), 1, 2, lo) {

}

class Short(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_shortthrower.png"), 1, 3, lo) {

}

class Long(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_longthrower.png"), 1, 3, lo) {

}
<<<<<<< HEAD

// Gimmicky ants

class Fire(p: Point, lo: Tunnel) extends Ant(p, new ImageIcon("img/ant_thrower.png"), 5, 3, lo) {

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

// we need to do more classes of ants
=======
//we need to do more classes of ants
>>>>>>> 83835c23b656b3860900c360220fce9793313963

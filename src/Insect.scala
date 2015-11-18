

import java.awt.Point

abstract class Insect {
  val speed: Point
  val pos: Point
  val width: Int
  val height: Int

  def update() = {
    pos.x += speed.x
    pos.y += speed.y
  }

  def inSprite(p: Point) = { //returns true if a given point is in the sprite
    (pos.x < p.x && p.x < pos.x + width &&
      pos.y < p.y && p.y < pos.y + height)
  }
  def SpeedIncrease (alpha : Int) = {
      speed.x = if (speed.x == 0) 1 else alpha * speed.x
      speed.y = if (speed.y == 0) 1 else alpha * speed.y
  }
  

}
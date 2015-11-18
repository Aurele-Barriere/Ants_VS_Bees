

import java.awt.Point
import javax.swing.ImageIcon

class Insect(sp: Point, p: Point, ico: ImageIcon) {
  val speed: Point = sp
  val pos: Point = p
  val icon: ImageIcon = ico
  val im = icon.getImage()
  val width: Int = icon.getIconWidth()
  val height: Int = icon.getIconHeight()

  def update(ui_width: Int, ui_height: Int) = { /*calculates the next pos and speed
    of the insect, given the bounds of the picture */
    pos.x += speed.x
    pos.y += speed.y
    if (pos.y < 0) {
      pos.y = 0
      speed.y = -speed.y
    }
    if (pos.x < 0) {
      pos.x = 0
      speed.x = -speed.x
    }
    if (pos.x + width > ui_width) {
      pos.x = ui_width - width
      speed.x = -speed.x
    }
    if (pos.y + height > ui_height) {
      pos.y = ui_height - height
      speed.y = -speed.y
    }
  }

  def inSprite(p: Point) = { //returns true if a given point is in the sprite
    (pos.x < p.x && p.x < pos.x + width &&
      pos.y < p.y && p.y < pos.y + height)
  }
  def SpeedIncrease(alpha: Int) = {
    speed.x = if (speed.x == 0) 1 else alpha * speed.x
    speed.y = if (speed.y == 0) 1 else alpha * speed.y
  }
  def SpeedDecrease(alpha: Int) = {
    if (alpha != 0) {
      speed.x = if (speed.x == 0) 1 else speed.x / alpha
      speed.y = if (speed.y == 0) 1 else speed.y / alpha
    }
  }

}
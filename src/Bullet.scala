

import javax.swing.ImageIcon
import java.awt.Point

class Bullet(f: Point, t: Point, ico: ImageIcon, imm :Int) {
  val travelled :Int = imm //the distance already travelled by the target
  val from: Point = new Point(f.x, f.y)
  val to: Point = new Point(t.x - travelled, t.y)
  var timer : Int = 0
  val icon: ImageIcon = ico
  var pos: Point = new Point(from.x, from.y)
  var isThere : Boolean = true //when this is false, the bullet has reached its destination
  
  
  def update() = {
    pos.x = from.x + ((timer * (to.x - from.x))/ AntsBees.state.framesPerTurn)
    timer += 1
    if (pos.x == to.x) {isThere = false}
  }

}
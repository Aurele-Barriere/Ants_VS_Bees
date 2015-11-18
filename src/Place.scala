

import java.awt.Point
import javax.swing.ImageIcon

class Place (p :Point, n : String) {
  val pos :Point = p
  val name :String = n
}


class Tunnel (p :Point, n :String, ex : Place, ico :ImageIcon) extends Place (p, n){
  val exit :Place = ex
  var ant :Boolean = false
  var typeant :String = " "
  var bees :List[Insect] = Nil
  val icon: ImageIcon = ico
  val im = icon.getImage()

  def addbee (b :List[Insect]) = {
    bees = bees ::: b
  }
  def removeant = {
    ant = false
  }
  def addant (t : String) {
    if (!ant) {
       ant = true
       typeant = t
    }
  }
  
}
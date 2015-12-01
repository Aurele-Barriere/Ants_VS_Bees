

import java.awt.Point
import javax.swing.ImageIcon

class Insect(p: Place, ico: ImageIcon, arm: Int) {
  var location: Place = p
  var damage = 1 
  val icon: ImageIcon = ico
  val im = icon.getImage()
  val width: Int = icon.getIconWidth()
  val height: Int = icon.getIconHeight()
  val watersafe = false
  var armor: Int = arm //when goes down to 0, the insect dies :'(

  def inSprite(p: Point) = { //returns true if a given point is in the sprite
    (location.pos.x < p.x && p.x < location.pos.x + width &&
      location.pos.y < p.y && p.y < location.pos.y + height)
  }
  def onDeath():Unit = {} // Not sure about the name, pretty cool kiskool death effect
}

class Bee(p: Place) extends Insect(p, new ImageIcon("img/bee.png"), 2) {
  override val watersafe = true
  
  def move() = {
    location match {
      case t: Tunnel => {
        t.ant match {
          case None => {
            t.removebee(this)
            this.location = t.exit
            t.exit match {
              case s: Tunnel => s.addbee(this)
              case h: Hive   => AntsBees.state.lost = true
            }
          }
          case Some(a) if !a.blocksPath => { // ugly af I know, but pattern matching is a b****
            t.removebee(this)
            this.location = t.exit
            t.exit match {
              case s: Tunnel => s.addbee(this)
              case h: Hive   => AntsBees.state.lost = true
            }
          }
          case Some(a) => (a.armor -= damage)
        }
      }
      case e: Entrance =>
        e.removebee(this)
        e.exit.addbee(this)
        this.location = e.exit
      case h:Hive => 
    }
  }
}

// Ants

abstract class Ant(p: Tunnel, ico: ImageIcon, arm: Int, co: Int) extends Insect(p, ico, arm) {
  //val location: Tunnel = lo
  val container = false
  var ant: Option[Ant] = None // For containers
  val cost: Int = co
  val blocksPath = true
  val unique = false
  var buffed = false
  def attack() = {}
  def canContain(t:Ant):Boolean = {
    this.container && !t.container && this.ant == None
  }
}

// Basic Units

class Harvester(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_harvester.png"), 1, 2) {
  damage = 0
  override def attack() = { AntsBees.state.purse.add_money(1) }
}

class Thrower(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_thrower.png"), 1, 2) {
  def attacking(pl: Tunnel): Unit = {
    pl.entrance match {
      case t: Tunnel => t.bees match {
        case Nil       => this.attacking(t)
        case l: List[Bee] => l.head.armor -= damage
      }
      case e:Entrance =>
    }
  }
  override def attack() = { attacking(p) }
}

class Short_Thrower(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_shortthrower.png"), 1, 3) {
  val range: Int = 2
  def attacking(pl: Place, n: Int): Unit = {
    if (n > 0) {
      pl match {
        case t: Tunnel => t.bees match {
          case Nil         => attacking(t.entrance, n - 1)
          case l: List[Bee] => l.head.armor -= damage
        }
      }
    }
  }
  override def attack() = { attacking(this.location, range) }
}

class Long_Thrower(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_longthrower.png"), 1, 3) {
  val deadrange: Int = 3
  def attacking(pl: Place): Unit = {
    
    pl match {
      case t: Tunnel => t.bees match {
        case Nil          =>  this.attacking(t.entrance)
        case l: List[Bee] =>  l.head.armor -= damage
        
      }
      case e: Entrance => //empty case so scala doesn't freaks out
    }
  }
  //We first need to jump over the dead space
  def charging(pl: Place, n: Int): Unit = {
    
    if (n > 0) {
      pl match { case t : Tunnel => charging (t.entrance, n-1) case _:Entrance => }
    
    } else {
      attacking(pl)
    }
  }
  override def attack() = { charging(this.location, deadrange) }
}

// Gimmicky ants

class Fire(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_fire.png"), 3, 5) {
  damage = 3
  override def onDeath() = {
      for (b <- p.bees) {
        b.armor -= damage
    }
  }
}

class Scuba(p :Tunnel) extends Ant(p, new ImageIcon("img/ant_scuba.png"), 1, 5) {
  override val watersafe = true
  def attacking(pl: Tunnel): Unit = {
    pl.entrance match {
      case t: Tunnel => t.bees match {
        case Nil          => this.attacking(t)
        case l: List[Bee] => l.head.armor -= damage
      }
    }
  }
  override def attack() = { attacking(p) }
}

class Wall(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_wall.png"), 4, 4) {
  damage = 0 
}

class Ninja(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_ninja.png"), 1, 6) {
  override val blocksPath = false
  override def attack(): Unit = {
    for (b <- p.bees) {
      b.armor -= damage
    }
  }
}

class Hungry(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_hungry.png"), 1, 4) {
  var digesting = 0
  damage = 0
  def eat(): Unit = {
    if (p.bees != Nil) {
      p.bees.head.armor = 0
      this digesting = 3
    }
  }
  def digest(): Unit = {
    if (this.digesting > 0) {
      this.digesting -= 1
    }
  }
  override def attack(): Unit = {
    if (this.digesting == 0) {
      eat()
    } else {
      digest()
    }
  }
}

class Bodyguard(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_weeds.png"), 2, 4) {
  override val container = true
  override def onDeath() = {
    this.ant match {
      case Some(a) => p.ant = Some(a)
      case None => p.ant = None
    }
  }
}

// Here comes the queen

class Queen(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_queen.png"), 2, 6) {
  override val unique = true
  override val watersafe = true
  buffed = true
  def attacking(pl: Tunnel): Unit = {
    pl.entrance match {
      case t: Tunnel => {
        t.bees match {
          case Nil          => this.attacking(t)
          case l: List[Bee] => l.head.armor -= damage
        }
      }
    }
  }
  override def onDeath() = {
    AntsBees.state.lost = true
  }
  def inspire(pl:Tunnel): Unit = {
    pl.entrance match {
      case t:Tunnel => {
        t.ant match {
          case Some(a) if !a.buffed => {
            a.buffed = true
            a.damage += 2
          }
        }
        inspire(t)
      }
      case e:Entrance =>
    }
  }
  def impostor(): Unit = {
    if (AntsBees.state.uniqueUnits > 1) {
      this.armor = 0
      AntsBees.state.uniqueUnits -= 1 
    }
  }
  override def attack() = { 
    //attacking(p)
    //inspire(p)
    impostor()
  }
}


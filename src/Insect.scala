

import java.awt.Point
import javax.swing.ImageIcon

class Insect(p: Place, ico: ImageIcon, arm: Int) {
  var location: Place = p
  
  var icon: ImageIcon = ico
  var im = icon.getImage()
  val width: Int = icon.getIconWidth()
  val height: Int = icon.getIconHeight()
  val watersafe = false
  var armor: Int = arm //when goes down to 0, the insect dies 

  def inSprite(p: Point) = { //returns true if a given point is in the sprite. is this used?
    (location.pos.x < p.x && p.x < location.pos.x + width &&
      location.pos.y < p.y && p.y < location.pos.y + height)
  }
  def onDeath(): Unit = {} // Not sure about the name, pretty cool kiskool death effect
}

class Bee(p: Place) extends Insect(p, new ImageIcon("img/1bee.png"), 2) {
  override val watersafe = true
  var deathByBullet: Boolean = false // if you die by a bullet, you'll be erased after the end of the turn
  var hasMoved :Boolean = false
  var damage = 1
  
  def move() = {
    location match {
      case t: Tunnel => {
        t.ant match {
          case None => {
            t.removebee(this)
            this.location = t.exit
            t.exit match {
              case s: Tunnel => s.bees = this :: s.bees
              case h: Hive   => AntsBees.state.lost = true
            }
          }
          case Some(a) if !a.blocksPath => { 
            t.removebee(this)
            this.location = t.exit
            t.exit match {
              case s: Tunnel => s.bees = this :: s.bees
              case h: Hive   => AntsBees.state.lost = true
            }
          }
          case Some(a) => (a.armor -= damage)
        }
      }
      case e: Entrance =>
        e.removebee(this)
        e.exit.bees = this :: e.exit.bees
        this.location = e.exit
      case h: Hive =>
    }
    hasMoved = true
  }
}

// Ants

abstract class Ant(p: Tunnel, ico: ImageIcon, arm: Int, co: Int) extends Insect(p, ico, arm) {
  val container = false
  var ant: Option[Ant] = None //For containers
  val cost: Int = co
  val blocksPath = true
  val unique = false
  var buffed = false
  def attack() = {} //will be overrided for most ants
  var damage = 1
  if (AntsBees.state.isQueen) {damage *= 2}
  def canContain(t: Ant): Boolean = {
    this.container && !t.container && this.ant == None
  }
}

// Basic Units

class Harvester(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_harvester.png"), 1, 2) {
  damage = 0
  override def attack() = { AntsBees.state.purse.add_money(1) }
}

class Thrower(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_thrower.png"), 1, 6) {
  def attacking(pl: Tunnel): Unit = {
    pl.entrance match {
      case t: Tunnel => t.bees match {
        case Nil => this.attacking(t)
        case l: List[Bee] =>
          l.head.armor -= damage
          AntsBees.state.Bullets = new Bullet(this.location.pos, t.exit.pos, new ImageIcon("img/bullet.png")) :: AntsBees.state.Bullets
          if (l.head.armor == 0) { l.head.deathByBullet = true }
      }
      case _ =>
    }
  }
  override def attack() = { attacking(p) }
}

class Short_Thrower(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_shortthrower.png"), 1, 3) {
  val range: Int = 3
  def attacking(pl: Place, n: Int): Unit = {
    if (n > 0) {
      pl match {
        case t: Tunnel => t.bees match {
          case Nil => attacking(t.entrance, n - 1)
          case l: List[Bee] =>
            l.head.armor -= damage
            var pos: Point = if (t.exit.pos.x < this.location.pos.x) { t.pos } else { t.exit.pos }
            AntsBees.state.Bullets = new Bullet(this.location.pos, pos, new ImageIcon("img/short_bullet.png")) :: AntsBees.state.Bullets
            if (l.head.armor == 0) { l.head.deathByBullet = true }
        }
        case _ =>
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
        case Nil => this.attacking(t.entrance)
        case l: List[Bee] =>
          l.head.armor -= damage
          AntsBees.state.Bullets = new Bullet(this.location.pos, t.exit.pos, new ImageIcon("img/long_bullet.png")) :: AntsBees.state.Bullets
          if (l.head.armor == 0) { l.head.deathByBullet = true }
      }
      case e: Entrance => //empty case so scala doesn't freaks out
    }
  }
  //We first need to jump over the dead space
  def charging(pl: Place, n: Int): Unit = {

    if (n > 0) {
      pl match { case t: Tunnel => charging(t.entrance, n - 1) case _: Entrance => }

    } else {
      attacking(pl)
    }
  }
  override def attack() = { charging(this.location, deadrange) }
}

// Gimmicky ants

class Fire(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_fire.png"), 3, 6) {
  damage = 3
  override def onDeath() = {
    for (b <- p.bees) {
      b.armor -= damage
    }
  }
}

class Scuba(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_scuba.png"), 1, 8) {
  override val watersafe = true
  def attacking(pl: Tunnel): Unit = {
    pl.entrance match {
      case t: Tunnel => t.bees match {
        case Nil => this.attacking(t)
        case l: List[Bee] =>
          l.head.armor -= damage
          AntsBees.state.Bullets = new Bullet(this.location.pos, t.exit.pos, new ImageIcon("img/bullet.png")) :: AntsBees.state.Bullets
          if (l.head.armor == 0) { l.head.deathByBullet = true }
      }
      case _ =>
    }
  }
  override def attack() = { attacking(p) }
}

class Wall(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_wall.png"), 4, 4) {
  damage = 0
}

class Ninja(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_ninja.png"), 1, 10) {
  override val blocksPath = false
  override def attack(): Unit = {
    p.bees match {
      case Nil =>
      case _ => p.bees.head.armor = p.bees.head.armor - damage
    }
  }
}

class Hungry(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_hungry.png"), 3, 5) {
  var digesting = 0
  damage = 0
  def eat(): Unit = {
    if (p.bees != Nil) {
      p.bees.head.armor = 0
      this.digesting = 3
      if (this.armor < 3) {this.armor += AntsBees.state.rng.nextInt(1)}
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
      val ico = new ImageIcon("img/ant_hungry.png")
      this.icon = ico
      this.im = ico.getImage()
    } else {
      val ico = new ImageIcon("img/ant_hungry2.png")
      this.icon = ico
      this.im = ico.getImage()
      digest()
    }
  }
}

class Bodyguard(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_weeds.png"), 2, 3) {
  override val container = true
  override def onDeath() = {
    this.ant match {
      case Some(a) => p.ant = Some(a)
      case None    => p.ant = None
    }
  }
}

// Here comes the queen

class Queen(p: Tunnel) extends Ant(p, new ImageIcon("img/ant_queen.png"), 2, 30) {
  override val unique = true
  override val watersafe = true
  var isImpostor :Boolean = false // is this an impostor queen ?
  
  def attacking(pl: Tunnel): Unit = {
    pl.entrance match {
      case t: Tunnel => t.bees match {
        case Nil => this.attacking(t)
        case l: List[Bee] =>
          l.head.armor -= damage
          AntsBees.state.Bullets = new Bullet(this.location.pos, t.exit.pos, new ImageIcon("img/bullet.png")) :: AntsBees.state.Bullets
          if (l.head.armor == 0) { l.head.deathByBullet = true }
      }
      case _ =>
    }
  }
  override def onDeath() = {
    if (!isImpostor) AntsBees.state.lost = true //if the real queen died
  }
  
  def inspire() = {
    AntsBees.state.isQueen = true //all ants put after the queen will have double damage
  }
  def impostor(): Unit = {
    if (AntsBees.state.uniqueUnits > 1) {
      this.armor = 0
      isImpostor = true
      AntsBees.state.uniqueUnits -= 1
    }
  }
  override def attack() = {
    attacking(p)
    inspire()
    impostor()
  }
}


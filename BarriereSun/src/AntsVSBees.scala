/* This project is presented to you by Aurèle Barrière and Rémy Sun for PROG1 */

import java.awt.Color
import java.awt.Graphics2D
import java.awt.Point
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.geom

import scala.swing.Dimension
import scala.swing.MainFrame
import scala.swing.Panel
import scala.swing.SimpleSwingApplication
import scala.swing.event.FocusLost
import scala.swing.event.KeyTyped
import scala.swing.event.MousePressed

import javax.swing.ImageIcon
import javax.swing.Timer

object AntsBees extends SimpleSwingApplication {

  // Part 1: The data describing the state of the game

  object state {
    val rng = scala.util.Random
    var timer = 0 // Timer to emulate real time
    val framesPerTurn = 50
    var lost: Boolean = false // Have we lost the game?
    var isQueen: Boolean = false // Is there a queen?
    var nextTurn: Boolean = true
    val numberCaves: Int = 4
    val numberTunnels: Int = 8

    //Defining Cells 

    val nulpoint = new Point(0, 0) //an abstract tunnel to put the ants in the cells

    val t0: Tunnel = new Tunnel(nulpoint, t0, t0, new ImageIcon("img/tunnel.png")) //just a place to put the insects in the hive

    // Units selecting cells are at y=50, with a distance of 100 x between each icon
    val harvester = new CellAnt(new Point(50, 50), new Harvester(t0))
    val thrower = new CellAnt(new Point(150, 50), new Thrower(t0))
    val short = new CellAnt(new Point(250, 50), new Short_Thrower(t0))
    val long = new CellAnt(new Point(350, 50), new Long_Thrower(t0))
    val fire = new CellAnt(new Point(450, 50), new Fire(t0))
    val scuba = new CellAnt(new Point(550, 50), new Scuba(t0))
    val wall = new CellAnt(new Point(650, 50), new Wall(t0))
    val ninja = new CellAnt(new Point(50, 150), new Ninja(t0))
    val hungry = new CellAnt(new Point(150, 150), new Hungry(t0))
    val bodyguard = new CellAnt(new Point(250, 150), new Bodyguard(t0))
    val queen = new CellAnt(new Point(350, 150), new Queen(t0))
    val bye = new Bye(new Point(450, 150))
    val C: List[Cell] = List(harvester, thrower, short, long, fire, scuba, wall, ninja, hungry, bodyguard, queen, bye)

    val hive = new Hive(C)

    var Caves: List[Cave] = List(new Cave(0, hive, numberTunnels))
    for (i <- 1 until numberCaves) {
      Caves = new Cave(i, hive, numberTunnels) :: Caves
    }

    // Setting up game elements like money, score and animated bullets

    val purse = new Purse(10)
    var score: Int = 0

    var Bullets: List[Bullet] = Nil

    def update() = { //method to call each frame
      // Removing dead insects in the tunnels
      for (c <- Caves) {
        for (t <- c.Tunnels) {
          t.ant match {
            case Some(a) =>
              a.ant match { case Some(b) => if (b.armor < 1) { a.ant = None; b.onDeath() } case _ => }
              if (a.armor < 1) {
                t.ant = None // Wiping the board
                a.onDeath() // Death effect
              }
            case None =>
          }
          var newbees: List[Bee] = Nil
          for (b <- t.bees) {
            if ((b.armor > 0) || (!nextTurn && b.deathByBullet)) { newbees = b :: newbees } else { score = score + 1 }
          }
          t.bees = newbees
        }
      }

      // Updating bullets
      var newBullets: List[Bullet] = Nil
      for (b <- Bullets) {
        b.update()
        if (b.isThere) { newBullets = b :: newBullets }
      }
      Bullets = newBullets

      // Adding random bees in the entrances
      if (nextTurn) {
        var a = 0
        for (c <- Caves) {
          a = rng.nextInt(100)
          if (a < c.frequency) { c.entrance.createbees(((c.frequency - a) / 50) + 1) }
          c.frequency += 1
        }
      }

      // Insects can now perform actions
      for (c <- Caves) for (t <- c.Tunnels) for (b <- t.bees) { b.hasMoved = false } // At the beginning, none of the bees have moved

      if (nextTurn) for (c <- Caves) {
        for (t <- c.Tunnels) {
          t.ant match {
            case Some(a) =>
              a.attack()
              a.ant match { case Some(an: Ant) => an.attack() case None => }
            case None =>
          }
          for (b <- t.bees) { if (!b.hasMoved) { b.move() } }
        }
        for (b <- c.entrance.bees) { if (!b.hasMoved) { b.move() } }
        timer = 0
      }

      nextTurn = false
      if (timer == framesPerTurn) {
        nextTurn = true
      } else {
        timer += 1
      }

      // When we lose the game
      if (lost) {
        purse.money = 0
        for (c <- Caves) {
          c.frequency = 601
          for (t <- c.Tunnels) {

            t.ant match {
              case Some(a: Ninja) => a.armor = 0
              case _              =>
            }
          }
        }
      }
    }
  }

  // Part 2: the User Interface: main panel on which we will paint everything 

  // In input, it reacts to clicks and key pressed. In output, it draws the game state on itself when asked to 
  lazy val ui = new Panel {
    background = Color.white
    preferredSize = new Dimension(800, 600)

    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys)
    def getPos() = peer.getMousePosition() // (note: peer is the java panel under the Scala wrapper)
    def on_click() = {
      for (c <- state.hive.Cells) if (c.is_clicked(getPos())) (state.hive.select(c))
      for (cave <- state.Caves) {
        for (t <- cave.Tunnels) if (t.is_clicked(getPos())) { for (c <- state.hive.Cells) if (c.is_selected) { c.buy_ant(state.purse, t) } }
      }
    }
    reactions += {
      case e: MousePressed =>
        on_click()

        requestFocusInWindow()

      case KeyTyped(_, 'n', _, _) => state.nextTurn = true
      case KeyTyped(_, 'a', _, _) => savestate.save()
      case KeyTyped(_, 'z', _, _) => savestate.load()
      case KeyTyped(_, 'q', _, _) => state.hive.select(state.harvester)
      case KeyTyped(_, 's', _, _) => state.hive.select(state.thrower)
      case KeyTyped(_, 'd', _, _) => state.hive.select(state.short)
      case KeyTyped(_, 'f', _, _) => state.hive.select(state.long)
      case KeyTyped(_, 'g', _, _) => state.hive.select(state.fire)
      case KeyTyped(_, 'h', _, _) => state.hive.select(state.scuba)
      case KeyTyped(_, 'j', _, _) => state.hive.select(state.wall)
      case KeyTyped(_, 'w', _, _) => state.hive.select(state.ninja)
      case KeyTyped(_, 'x', _, _) => state.hive.select(state.hungry)
      case KeyTyped(_, 'c', _, _) => state.hive.select(state.bodyguard)
      case KeyTyped(_, 'v', _, _) => state.hive.select(state.queen)
      case KeyTyped(_, 'b', _, _) => state.hive.select(state.bye)

      case _: FocusLost           => repaint()
    }

    /* How to draw the screen when instructed to do so */
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.setColor(new Color(100, 100, 100))

      val pos = getPos()

      g.drawString("food : " + state.purse.money, size.width - 200, 10)
      g.drawString("score : " + state.score, size.width - 200, 30)
      if (state.lost) { g.drawString("lost", 0, 0) }
      g.setColor(Color.black)

      for (c <- state.Caves) {
        for (t <- c.Tunnels) {
          g.drawImage(t.im, t.pos.x, t.pos.y, peer)
        }
      }

      var blocks = true
      for (c <- state.Caves) {
        for (t <- c.Tunnels) {

          blocks = true
          t.ant match {
            case Some(a) =>
              g.drawImage(a.im, a.location.pos.x, a.location.pos.y, peer)
              a.ant match { case Some(ant) => g.drawImage(ant.im, ant.location.pos.x, ant.location.pos.y, peer) case None => }
              if (!a.blocksPath) { blocks = false }
            case None => blocks = false
          }
          var pos = 0
          if (blocks) { pos = t.pos.x } else { pos = t.pos.x - ((state.timer * t.icon.getIconWidth()) / state.framesPerTurn) }
          t.bees.length match {
            case 1 => g.drawImage(new ImageIcon("img/1bee.png").getImage(), pos, t.pos.y, peer)
            case 2 => g.drawImage(new ImageIcon("img/2bee.png").getImage(), pos, t.pos.y, peer)
            case 3 => g.drawImage(new ImageIcon("img/3bee.png").getImage(), pos, t.pos.y, peer)
            case 4 => g.drawImage(new ImageIcon("img/4bee.png").getImage(), pos, t.pos.y, peer)
            case 5 => g.drawImage(new ImageIcon("img/5bee.png").getImage(), pos, t.pos.y, peer)
            case 0 =>
            case _ => g.drawImage(new ImageIcon("img/6bee.png").getImage(), pos, t.pos.y, peer) //6 bees or more
          }
        }
      }
      // Drawing bullets
      for (b <- state.Bullets) {
        g.drawImage((b.icon).getImage(), b.pos.x, b.pos.y, peer)
      }

      for (c <- state.hive.Cells) {
        c match {
          case a: CellAnt =>
            g.drawImage(a.typeant.im, a.pos.x, a.pos.y, peer)
            g.drawString((a.typeant.cost).toString(), a.pos.x + (c.width / 2), a.pos.y + (c.height / 5))
          case b: Bye => g.drawImage((new ImageIcon("img/remover.png")).getImage(), b.pos.x, b.pos.y, peer)
        }
        if (c.is_selected) {
          val boxPath = new geom.GeneralPath
          boxPath.moveTo(c.pos.x, c.pos.y)
          boxPath.lineTo(c.pos.x + c.width, c.pos.y)
          boxPath.lineTo(c.pos.x + c.width, c.pos.y + c.height)
          boxPath.lineTo(c.pos.x, c.pos.y + c.height)
          boxPath.lineTo(c.pos.x, c.pos.y)
          g.draw(boxPath)
        }
      }

    }
  }

  // Part 3: Animation timer: calls state.update() and ui.repaint() 50 times per second
  ///////////////////////////
  class MyTimer extends ActionListener {
    /* Configuration */
    val fpsTarget = 50 // Desired amount of frames per second
    var delay = 1000 / fpsTarget

    /* The swing timer */
    val timer = new Timer(delay, this)
    timer.setCoalesce(true) // Please restart by yourself
    timer.start() // Let's go

    /* react to the timer events */
    def actionPerformed(e: ActionEvent): Unit = {
      state.update()
      ui.repaint() // Tell Scala that the image should be redrawn
    }
  }
  val t = new MyTimer()

  // Part 4: Main initialization: Create a new window and populate it

  def top = new MainFrame {
    title = "Ants VS Bees"
    contents = ui
  }
}
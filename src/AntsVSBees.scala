// This demonstrates the major Scala Swing functionalities that we need in this project

// Compile with: scalac -cp scala-swing.jar:. FirstTest.scala
// Then execute with: scala -cp scala-swing.jar:. FirstTest

import scala.swing._
import scala.swing.{ SimpleSwingApplication, MainFrame, Panel }
import scala.swing.event._
import java.awt.event.{ ActionEvent, ActionListener }
import java.awt.{ Color, Graphics2D, Point, geom, MouseInfo }
import javax.swing.{ ImageIcon, Timer }

// That object is your application
object AntsBees extends SimpleSwingApplication {

  // Part 1: The data describing the state of the game
  ////////////////////////////////////////////////////

  object state {
    var lost: Boolean = false //have we lost the game?
    val tunnel_icon: ImageIcon = new ImageIcon("img/tunnel.png")
    val tunnel_im = tunnel_icon.getImage()
    val tun: Int = 8 //number of tunnel places
    var Insects: List[Insect] = Nil
    //Defining Cells :
    val nulpoint = new Point(0, 0)

    //var hive = new Hive(C, t1: Tunnel)
    val width = tunnel_icon.getIconWidth()
    val alt = 300 //where is the tunnel on the y-axis
    val t0: Tunnel = new Tunnel(nulpoint, null, null, tunnel_icon) //just a place to put the insects in the hive
    val t1: Tunnel = new Tunnel(new Point(0, alt), hive, t2, tunnel_icon)
    val t2: Tunnel = new Tunnel(new Point(width, alt), hive, t3, tunnel_icon)
    val t3: Tunnel = new Tunnel(new Point(2 * width, alt), hive, t4, tunnel_icon)
    val t4: Tunnel = new Tunnel(new Point(3 * width, alt), hive, t5, tunnel_icon)
    val t5: Tunnel = new Tunnel(new Point(4 * width, alt), hive, t6, tunnel_icon)
    val t6: Tunnel = new Tunnel(new Point(5 * width, alt), hive, t7, tunnel_icon)
    val t7: Tunnel = new Tunnel(new Point(6 * width, alt), hive, t8, tunnel_icon)
    val t8: Tunnel = new Tunnel(new Point(7 * width, alt), hive, entrance, tunnel_icon)
    val entrance = new Entrance(new Point(8 * width, alt), t8)
    // Units selecting cells are at y=50, with a distance of 100 x between each icon
    val harvester = new CellAnt(new Point(50, 50), new Harvester(nulpoint, t0))
    val thrower = new CellAnt(new Point(150, 50), new Thrower(nulpoint, t0))
    val short = new CellAnt(new Point(250, 50), new Short_Thrower(nulpoint, t0))
    val long = new CellAnt(new Point(350, 50), new Long_Thrower(nulpoint, t0))
    val fire = new CellAnt(new Point(450, 50), new Fire(nulpoint, t0))
    val scuba = new CellAnt(new Point(550, 50), new Scuba(nulpoint, t0))
    val wall = new CellAnt(new Point(650, 50), new Wall(nulpoint, t0))
    val ninja = new CellAnt(new Point(50, 150), new Ninja(nulpoint, t0))
    val hungry = new CellAnt(new Point(150, 150), new Hungry(nulpoint, t0))
    val bodyguard = new CellAnt(new Point(250, 150), new Bodyguard(nulpoint, t0))
    val queen = new CellAnt(new Point(350, 150), new Queen(nulpoint, t0))
    val bye = new Bye(new Point(450, 150))
    val C: List[Cell] = List(harvester, thrower, short, long, fire, scuba, wall, ninja, hungry, bodyguard, queen, bye)
    var Tunnels: List[Tunnel] = List(t1, t2, t3, t4, t5, t6, t7, t8)
    val hive = new Hive(C, t1)

    val purse = new Purse(10)
    def update() = {
      Insects = for (i <- Insects; if (i.armor > 0)) yield (i) //removing dead insects
      for (i <- Insects) {
        i match {
          case a: Ant => a.attack()
          case b: Bee => b.move() //will attack if there's an ant
        }
      }

    }
    /* reset(): empties the screen */
    def reset() = {
      Insects = Nil
    }
  }

  // Part 2: the User Interface: main panel on which we will paint everything 
  /////////////////////////////

  // In input, it reacts to clicks and key pressed. In output, it draws the game state on itself when asked to 
  lazy val ui = new Panel {
    background = Color.white
    preferredSize = new Dimension(800, 600)

    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys)
    def getPos() = peer.getMousePosition() // (note: peer is the java panel under the Scala wrapper)
    def on_click() = {
      for (c <- state.hive.Cells) if (c.is_clicked(getPos())) (state.hive.select(c))
      for (t <- state.Tunnels) if (t.is_clicked(getPos())) { for (c <- state.hive.Cells) if (c.is_selected) { c.buy_ant(state.purse, t) } }

    }
    reactions += {
      case e: MousePressed =>
        on_click()

        //state.removeSpriteAt(e.point)
        requestFocusInWindow()
      case e: MouseDragged        => /* Nothing for now */
      case e: MouseReleased       => /* Nothing for now */
      case KeyTyped(_, 'c', _, _) => state.reset()
      //case KeyTyped(_, 'i', _, _) => state.addImage(getPos())
      //case KeyTyped(_, 'a', _, _) => state.speedIncrease()
      //case KeyTyped(_, 'z', _, _) => state.speedDecrease()

      case _: FocusLost           => repaint()
    }

    /* Returns the current position of the mouse (or null if it's not over the panel */

    /* A nice box */

    /* How to draw the screen when instructed to do so */
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.setColor(new Color(100, 100, 100))
      //g.drawString(" Press 'i' to add sprites, 'c' to remove them all. Click on sprite to destroy them", 10, size.height - 10)
      val pos = getPos()
      if (pos != null)
        g.drawString("food : " + state.purse.money, size.width - 200, 10)

      g.setColor(Color.black)
      //g.draw(boxPath)

      for (ins <- state.Insects) {
        g.drawImage(ins.im, ins.pos.x, ins.pos.y, peer)
      }
      for (t <- state.Tunnels) {
        g.drawImage(t.im, t.pos.x, t.pos.y, peer)
      }
      for (c <- state.hive.Cells) {
        c match {
          case a: CellAnt => g.drawImage(a.typeant.im, a.pos.x, a.pos.y, peer)
          case b: Bye     => g.drawImage((new ImageIcon("img/remover.png")).getImage(), b.pos.x, b.pos.y, peer)
        }
        if (c.is_selected) {
          val boxPath = new geom.GeneralPath
          boxPath.moveTo(c.pos.x, c.pos.y)
          boxPath.lineTo(c.pos.x + 100, c.pos.y)
          boxPath.lineTo(c.pos.x + 100, c.pos.y + 100)
          boxPath.lineTo(c.pos.x, c.pos.y + 100)
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
  //////////////////////////////
  def top = new MainFrame {
    title = "Ants VS Bees"
    contents = ui
  }
}
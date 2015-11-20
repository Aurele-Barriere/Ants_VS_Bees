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
object DemoApp extends SimpleSwingApplication {

  // Part 1: The data describing the state of the game
  ////////////////////////////////////////////////////

  object state {
    /* records the images that move on screen */
    // This is a (ugly) set of sprites that are animated on screen. A set of objects would be *much* better

    val tunnel_icon: ImageIcon = new ImageIcon("img/tunnel.png")
    val tunnel_im = tunnel_icon.getImage()
    val tun: Int = 8 //number of tunnels
    var Insects: List[Insect] = Nil

    val hive = new Hive(Nil)
    val T1 = new Tunnel(new Point(0, 200), 1.toString, hive, tunnel_icon)
    var Tunnels: List[Tunnel] = Nil
    Tunnels = T1 :: Tunnels
    for (a <- 2 to tun) {
      Tunnels = new Tunnel(new Point(tunnel_icon.getIconWidth() * (a - 1), 200), a.toString, Tunnels.head, tunnel_icon) :: Tunnels
    }
    val entrance = new Entrance(new Point (500,500), "entrance", Tunnels.head)
    val purse = new Purse(0)
    def update() = {
      if (purse.money < 100000) { purse.money += 1 }
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

    reactions += {
      case e: MousePressed =>
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
    def getPos() = peer.getMousePosition() // (note: peer is the java panel under the Scala wrapper)

    /* A nice box */
    val boxPath = new geom.GeneralPath
    boxPath.moveTo(10, 100)
    boxPath.lineTo(10, 10)
    boxPath.lineTo(110, 10)
    boxPath.lineTo(110, 100)
    boxPath.lineTo(10, 100)

    /* How to draw the screen when instructed to do so */
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.setColor(new Color(100, 100, 100))
      g.drawString(" Press 'i' to add sprites, 'c' to remove them all. Click on sprite to destroy them", 10, size.height - 10)
      val pos = getPos()
      if (pos != null)
        g.drawString("x: " + pos.x + " y: " + pos.y + state.purse.money, size.width - 85, 15)

      g.setColor(Color.black)
      g.draw(boxPath)

      for (ins <- state.Insects) {
        g.drawImage(ins.im, ins.pos.x, ins.pos.y, peer)
      }
      for (t <- state.Tunnels) {
        g.drawImage(t.im, t.pos.x, t.pos.y, peer)
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
    title = "Simple Demo Application"
    contents = ui
  }
}
// This demonstrates the major Scala Swing functionalities that we need in this project

// Compile with: scalac -cp scala-swing.jar:. FirstTest.scala
// Then execute with: scala -cp scala-swing.jar:. FirstTest

// Usage:
//   - 'i' to add a moving sprite under the mouse
//   - 'a'/'z' to increase/decrease the speed of every moving objects 
//   - 'c' to clear the state (and thus the screen)

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
    val icon: ImageIcon = new ImageIcon("img/ant_ninja.png")
    val im = icon.getImage()
    var imagesPos: List[Point] = Nil
    var imagesSpeed: List[Point] = Nil
    var Insects: List[Insect] = Nil
    /* addImage(): Add a new image to the game state at the specified position */
    def addImage(position: Point) = {
      /* Get the current position of the mouse, and add it to the list of images to draw */
      if (position != null) { // position is null if the mouse is out of the screen
        imagesPos = position :: imagesPos
        imagesSpeed = new Point(1, 1) :: imagesSpeed // Initial speed: dx=1;dy=1
      }
    }
    /* update(): gets called 50 times per second to update the game state */
    def update() = {
      // Array(1,2,3) zip Array('a', 'b', 'c') evaluates to Array((1,'a'), (2,'b'), (3, 'c'))
      // So the following retrieve the pos and speed of a given sprite. Again, having a class for sprites would be WAYS better
      for ((pos, speed) <- imagesPos zip imagesSpeed) {
        /* Apply the change */
        pos.x += speed.x
        pos.y += speed.y
        /* Bump on borders on need */
        if (pos.y < 0) {
          pos.y = 0
          speed.y = -speed.y
        }
        if (pos.x < 0) {
          pos.x = 0
          speed.x = -speed.x
        }
        if (pos.x + icon.getIconWidth() > ui.size.getWidth()) {
          pos.x = ui.size.getWidth().toInt - icon.getIconWidth()
          speed.x = -speed.x
        }
        if (pos.y + icon.getIconHeight() > ui.size.getHeight()) {
          pos.y = ui.size.getHeight().toInt - icon.getIconHeight()
          speed.y = -speed.y
        }
      }
    }
    /* speedIncrease(): changes the speed of every existing sprite */
    def speedIncrease() = {
      for (pos <- imagesSpeed) {
        pos.x = if (pos.x == 0) 1 else pos.x * 2
        pos.y = if (pos.y == 0) 1 else pos.y * 2
      }
    }
    /* speedDecrease(): changes the speed of every existing sprite */
    def speedDecrease() = {
      for (pos <- imagesSpeed) {
        pos.x /= 2
        pos.y /= 2
      }
    }
    def removeSpriteAt(click: Point) {
      /* Filter the sprite out. This code is made particularly ugly by the lack of Sprite class :-( */
      var p2: List[Point] = Nil
      var s2: List[Point] = Nil
      for ((pos, speed) <- imagesPos zip imagesSpeed) {
        if (pos.x < click.x && click.x < pos.x + icon.getIconWidth() &&
          pos.y < click.y && click.y < pos.y + icon.getIconHeight()) {
          /* this object was clicked. Don't read it */
        } else {
          p2 = pos :: p2
          s2 = speed :: s2
        }
      }
      imagesPos = p2
      imagesSpeed = s2
    }

    /* reset(): empties the screen */
    def reset() = {
      imagesPos = Nil
      imagesSpeed = Nil
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
        state.removeSpriteAt(e.point)
        requestFocusInWindow()
      case e: MouseDragged        => /* Nothing for now */
      case e: MouseReleased       => /* Nothing for now */
      case KeyTyped(_, 'c', _, _) => state.reset()
      case KeyTyped(_, 'i', _, _) => state.addImage(getPos())
      case KeyTyped(_, 'a', _, _) => state.speedIncrease()
      case KeyTyped(_, 'z', _, _) => state.speedDecrease()

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
        g.drawString("x: " + pos.x + " y: " + pos.y, size.width - 85, 15)

      g.setColor(Color.black)
      g.draw(boxPath)

      for (p <- state.imagesPos) {
        g.drawImage(state.im, p.x, p.y, peer)
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
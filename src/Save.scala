import scala.io.Source
import java.awt.Point

object savestate {
  val fileName = "saves.txt"
  def printToFile(content: String, location: String = fileName) =
    Some(new java.io.PrintWriter(location)).foreach { f => try { f.write(content) } finally { f.close } }
  def save() = {
    var saving = AntsBees.state.purse.money.toString() + "\n" // Money
    saving += AntsBees.state.uniqueUnits.toString() + "\n" // Number of unique units
    saving += AntsBees.state.lost.toString() + "\n" // Is the game lost?
    saving += AntsBees.state.timer.toString() + "\n"
    var caves = AntsBees.state.Caves
    while (caves != Nil) {
      var cave = caves.head
      caves = caves.tail
      var tunnels = cave.Tunnels
      while (tunnels != Nil) {
        val tunnel = tunnels.head
        tunnels = tunnels.tail
        tunnel match {
          case t: Water  => saving += "1\n"
          case t: Tunnel => saving += "0\n"
        }
        tunnel.ant match { // storing specs of local ant
          case None => saving += "0\n\n\n\n"
          case Some(a) => { // type ant
            a match {
              case a: Harvester     => saving += "1\n"
              case a: Thrower       => saving += "2\n"
              case a: Short_Thrower => saving += "3\n"
              case a: Long_Thrower  => saving += "4\n"
              case a: Fire          => saving += "5\n"
              case a: Scuba         => saving += "6\n"
              case a: Ninja         => saving += "7\n"
              case a: Hungry        => saving += "8\n"
              case a: Wall          => saving += "9\n"
              case a: Bodyguard     => saving += "10\n"
              case a: Queen         => saving += "11\n"
            }
            saving += a.armor.toString() + "\n" // armor
            saving += a.damage.toString() + "\n" //damage
            saving += a.buffed.toString() + "\n"
          }
        }
        saving += tunnel.bees.length + "\n" // number of bees
      }
    }
    printToFile(saving)
  }
  def load() = {
    val lines = Source.fromFile(fileName).getLines.toArray
    AntsBees.state.purse.money = lines(0).toInt
    AntsBees.state.uniqueUnits = lines(1).toInt
    AntsBees.state.lost = lines(2).toBoolean
    AntsBees.state.timer = lines(3).toInt
    // Creating 4 caves
    var caves: List[Cave] = Nil
    for (i <- 0 until AntsBees.state.numberCaves) {
      var cave = new Cave(i, AntsBees.state.hive, AntsBees.state.numberTunnels)
      var tunnels: List[Tunnel] = Nil
      val t0 = new Tunnel(new Point(0, 0), null, null, cave.tunnelIcon)
      tunnels = t0 :: tunnels
      // Creating 8 tunnels for the cave
      for (j <- 0 until AntsBees.state.numberTunnels) {
        var occupied = true
        var tunnel = new Tunnel(new Point(cave.width * j, cave.altitude * cave.height + 300), tunnels.head, t0, cave.tunnelIcon)
        var ant: Ant = new Harvester(tunnel)
        val tunnelLine = 4 + (3 - i) * 48 + (7 - j) * 6 // Index of the first term of the current tunnel in saves.txt
        lines(tunnelLine).toInt match {
          case 0 =>
          case 1 => tunnel = new Water(new Point(cave.width * j, cave.altitude * cave.height + 300), tunnels.head, t0, cave.waterIcon)
        }
        
        if (j == 0) { // initialization of t1
          tunnel.exit = cave.hive
        }
        lines(tunnelLine + 1).toInt match {
          case 0  => occupied = false
          case 1  =>
          case 2  => ant = new Thrower(tunnel)
          case 3  => ant = new Short_Thrower(tunnel)
          case 4  => ant = new Long_Thrower(tunnel)
          case 5  => ant = new Fire(tunnel)
          case 6  => ant = new Scuba(tunnel)
          case 7  => ant = new Ninja(tunnel)
          case 8  => ant = new Hungry(tunnel)
          case 9  => ant = new Wall(tunnel)
          case 10 => ant = new Bodyguard(tunnel)
          case 11 => ant = new Queen(tunnel)
        }
        if (occupied) {
          ant.armor = lines(tunnelLine + 2).toInt
          ant.damage = lines(tunnelLine + 3).toInt
          ant.buffed = lines(tunnelLine + 4).toBoolean
          tunnel.ant = Some(ant)
        }
        for (k <- 0 until lines(tunnelLine + 5).toInt) {
          val b = new Bee(tunnel)
          tunnel.bees = b :: tunnel.bees
        }
        if (j == 7) {
          cave.entrance = new Entrance(new Point(AntsBees.state.numberTunnels * cave.width, cave.altitude), tunnel)
          tunnel.entrance = cave.entrance
        }
        tunnels = tunnel :: tunnels
      }
      tunnels = tunnels diff List(t0)
      for (i <- 1 until AntsBees.state.numberTunnels) {
        tunnels.apply(i).entrance = tunnels.apply(i - 1)
      }
      cave.Tunnels = tunnels
      caves = cave :: caves
    }
    AntsBees.state.Caves = caves
  }
}

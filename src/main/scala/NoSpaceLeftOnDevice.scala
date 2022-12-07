object NoSpaceLeftOnDevice {

  case class DirectoryHistory(private val history: Map[String, List[Long]]) {
    def directoriesSize = history.map { dir =>
        (dir._1, history.filter(_._1.contains(dir._1)).map(_._2.sum).sum)
      }
  }
  object DirectoryHistory {
    private val patternCommand = "\\$ ([a-z]+) (.+)".r
    private val patternFile = "([0-9]+) ([a-z]+)(.[a-z])*".r
    def apply(commandsRaw: List[String]): DirectoryHistory =
      DirectoryHistory(commandsRaw.foldLeft((Map[String, List[Long]](), "")) { (acc, command) => command match {
        case patternCommand(command, args) if command == "cd" && args != ".." => (acc._1.updated(acc._2 + "-" + args, List(0L)), acc._2 + "-" + args)
        case patternCommand(command, args) if command == "cd" && args == ".." => (acc._1, acc._2.split("-").dropRight(1).mkString("-"))
        case patternFile(size, name, extension) => (acc._1.updated(acc._2, size.toLong :: acc._1(acc._2)), acc._2)
        case _ => acc
        }
      }._1)
  }

  def totalSumDirFilesWithAtMost100000(commandsRaw: List[String]) =
    val history = DirectoryHistory(commandsRaw)
    history.directoriesSize.filter(_._2 < 100000L).foldLeft(0L)((s, l) => l._2 + s)

  def sizeOfFileToDeleteToFree300000000(commandsRaw: List[String]) =
    val history = DirectoryHistory(commandsRaw)
    val directoriesSizes = DirectoryHistory(commandsRaw).directoriesSize
    val rootSize = directoriesSizes.getOrElse("-/", 0L)
    val spaceNeededToFreeUp = 30000000 - (70000000L - rootSize)
    directoriesSizes.values.filter(_ >= spaceNeededToFreeUp).min
}

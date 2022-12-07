object NoSpaceLeftOnDevice {

  case class DirectoriesWithFileSizes(private val history: Map[String, List[Long]]) {
    private lazy val totalSizesDirectory = history.map { dir =>
        (dir._1, history.filter(_._1.contains(dir._1)).map(_._2.sum).sum)
      }
    def getRootSize = totalSizesDirectory.getOrElse("-/", 0L)
    def filterSizes(f: Long => Boolean) = totalSizesDirectory.values.filter(f)
  }
  object DirectoriesWithFileSizes {
    private val patternCommand = "\\$ ([a-z]+) (.+)".r
    private val patternFile = "([0-9]+) ([a-z]+)(.[a-z])*".r
    def apply(commandsRaw: List[String]): DirectoriesWithFileSizes =
      DirectoriesWithFileSizes(commandsRaw.foldLeft((Map[String, List[Long]](), "")) { (acc, command) => command match {
        case patternCommand(command, args) if command == "cd" && args != ".." => (acc._1.updated(acc._2 + "-" + args, List(0L)), acc._2 + "-" + args)
        case patternCommand(command, args) if command == "cd" && args == ".." => (acc._1, acc._2.split("-").dropRight(1).mkString("-"))
        case patternFile(size, name, extension) => (acc._1.updated(acc._2, size.toLong :: acc._1(acc._2)), acc._2)
        case _ => acc
        }
      }._1)
  }

  def totalSumDirFilesWithAtMost100000(commandsRaw: List[String]) = DirectoriesWithFileSizes(commandsRaw).filterSizes(_ < 100000L).sum

  def sizeOfFileToDeleteToFree300000000(commandsRaw: List[String]) =
    val directoriesWithFileSizes = DirectoriesWithFileSizes(commandsRaw)
    directoriesWithFileSizes.filterSizes(_ >= 30000000 - (70000000L - directoriesWithFileSizes.getRootSize)).min
}

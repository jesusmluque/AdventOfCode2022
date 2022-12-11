
object CathodeRayTube:

  def getSumSygnalStrengthAfterInstruction(rawInstruction: List[String]) =
    val pattern = "addx (-*[0-9]+)".r
    val result = calculateInstructions(rawInstruction)
    drawCRT(result)
    20*result(19) + 60*result(59) + 100*result(99) + 140*result(139) + 180*result(179) + 220*result(219)

  def calculateInstructions(instructions: List[String]) =
    val pattern = "addx (-*[0-9]+)".r
    instructions.foldLeft(List(1L)) { (acc, ins) => ins match {
      case "noop" => acc.head :: acc
      case pattern(value) => (value.toLong + acc.head) :: acc.head :: acc
    }
    }.reverse.toVector
  
  def drawCRT(register5History: Vector[Long]) =
    def drawRow(range: Range) =
      range.foreach { cycle =>
        val currentValue = register5History(cycle)
        if cycle + 1 - range.head < currentValue + 3 && cycle + 1 - range.head >= currentValue then
          System.out.print("#")
        else
          System.out.print("·")
      }
    drawRow(0 to 39)
    System.out.print("\n")
    drawRow(40 to 79)
    System.out.print("\n")
    drawRow(80 to 119)
    System.out.print("\n")
    drawRow(120 to 159)
    System.out.print("\n")
    drawRow(160 to 199)
    System.out.print("\n")
    drawRow(200 to 239)
    
    
/*--------------------------------------------DrawCRT IO pure without side effect-------------------------------------------------*/
  //Alternative approach to use IO Monad having a referential transparency version of the drawCRT method, that now is pure
  //First, Monad and Console monads:
  trait Monad[F[_]] {
    def pure[A](a: A):F[A]
    def map[A,B](fa: F[A])(f: A => B):F[B]
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  }
  implicit class monadOps[F[_]: Monad,A](fa: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = implicitly[Monad[F]].flatMap(fa)(f)
    def map[B](f: A => B): F[B] = implicitly[Monad[F]].map(fa)(f)

  }
  object Monad {
    def pure[F[_]: Monad, A](a: A):F[A] = implicitly[Monad[F]].pure(a)
  }

  trait Console[F[_]] {
    def putStrLn(str: String): F[Unit]
  }

  def putStrLn[F[_]: Console](str: String) = implicitly[Console[F]].putStrLn(str)

  //new drawCRT method returning a generic Adt F that represent the console output without side effect.
  def drawCRTIO[F[_]: Monad: Console](register5History: Vector[Long]):F[Unit] =
    def drawRow(range: Range) =
      putStrLn(range.map { cycle =>
        val currentValue = register5History(cycle)
        if cycle + 1 - range.head < currentValue + 3 && cycle + 1 - range.head >= currentValue then
          "#"
        else
          "·"
      }.mkString(""))
    for {
      _ <- drawRow(0 to 39)
      _ <- putStrLn("\n")
      _ <- drawRow(40 to 79)
      _ <- putStrLn("\n")
      _ <- drawRow(80 to 119)
      _ <- putStrLn("\n")
      _ <- drawRow(120 to 159)
      _ <- putStrLn("\n")
      _ <- drawRow(160 to 199)
      _ <- putStrLn("\n")
      _ <- drawRow(200 to 239)
    } yield ()
import CathodeRayTube.calculateInstructions
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class CathodeRayTubeTest extends AnyFlatSpec {

  "The sum of the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles for the cathode-ray-tube1 file " should " be 13140 " in {
    assert(CathodeRayTube.getSumSygnalStrengthAfterInstruction(Source.fromResource("cathode-ray-tube1").getLines().toList) == 13140L)
  }

  "The sum of the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles for the cathode-ray-tube2 file " should " be 14520 " in {
    assert(CathodeRayTube.getSumSygnalStrengthAfterInstruction(Source.fromResource("cathode-ray-tube2").getLines().toList) == 14520L)
  }


  /*-------------------------------------------DrawCRT IO pure without side effect-------------------------------------------------*/

  //Creating the type class instance for Monad and Console ADTs through the IO monad
  //The IO implement both, Console and Monad type classes and use the system.out to print
  //It is possible to have more than one implementation of the Console and Monad due to the generic ADT F
  //We can have a special F insntace for test where we could compare the result instead of printing it. We are splitting
  //the behavior of the program logic from the way the result is print it or even sent or any other side effect.
  case class IO[A](performEff: () => A)
  object IO:
    def pure[A](a: A): IO[A] = IO(() => a)

    given ioMonad: CathodeRayTube.Monad[IO] with
      override def pure[A](a: A): IO[A] = IO.pure(a)
      override def map[A, B](fa: IO[A])(f: A => B): IO[B] = IO(() => f(fa.performEff()))
      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = IO(() => f(fa.performEff()).performEff())

    given consoleIO: CathodeRayTube.Console[IO] with
      override def putStrLn(str: String): IO[Unit] = IO(() => System.out.print(str))


  "Tagless Final execution example " should " be " in {
    val result = calculateInstructions(Source.fromResource("cathode-ray-tube2").getLines().toList)
    //here is where the side effect really happen (perfromEFF)
    CathodeRayTube.drawCRTIO[IO](result).performEff()
    assert(true)
  }
}

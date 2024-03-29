package com.michaelmaysonet74.handsonscala.chapters

import java.io._

import scala.concurrent.{ExecutionContext, Future}

case class Msg(
  id: Int,
  parent: Option[Int],
  txt: String
)

final case class Chapter3()(implicit val ec: ExecutionContext) {

  import Chapter3._

  def execute(): Future[Unit] = Future {
    fizzBuzz().map { r => println(r.mkString("\n")) }

    printMessages(
      List(
        new Msg(0, None, "Hello"),
        new Msg(1, Some(0), "World"),
        new Msg(2, None, "I am Cow"),
        new Msg(3, Some(2), "Hear me moo"),
        new Msg(4, Some(2), "Here I stand"),
        new Msg(5, Some(2), "I am Cow"),
        new Msg(6, Some(5), "Here me moo, moo")
      )
    )

    withFileWriter("/Users/michael/code/hands-on-scala/results/File.txt") { writer =>
      writer.write("Hello\n")
      writer.write("World!")
    }

    val result =
      withFileReader("/Users/michael/code/hands-on-scala/results/File.txt") { reader =>
        reader.readLine + "\n" + reader.readLine
      }.getOrElse("Failed")

    assert(result == "Hello\nWorld!")
  }

  private def fizzBuzz(): Future[List[String]] =
    Future.successful((1 to 100).map {
      case n if n % 3 == 0 && n % 5 == 0 => "FizzBuzz"
      case n if n % 3 == 0               => "Fizz"
      case n if n % 5 == 0               => "Buzz"
      case n                             => n.toString
    }.toList)

}

object Chapter3 {

  private def printMessages(
    messages: Seq[Msg],
    indent: String = "",
    parent: Option[Int] = None
  ): Unit =
    for {
      m <- messages
      if m.parent == parent
    } {
      println(s"$indent#${m.id} ${m.txt}")
      printMessages(messages, s"$indent    ", Some(m.id))
    }

  def withFileWriter(fileName: String)(cb: BufferedWriter => Unit): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))

    try {
      cb(bw)
    } catch {
      case e: IOException =>
        println("Had an IOException trying to write that file")
    } finally {
      bw.close
    }
  }

  def withFileReader(
    fileName: String
  )(cb: BufferedReader => String): Option[String] =
    try {
      val file = new File(fileName)
      val br = new BufferedReader(new FileReader(file))

      try {
        Some(cb(br))
      } catch {
        case e: IOException => {
          println(s"Had an IOException trying to read file: $fileName")
          None
        }
      } finally {
        br.close
      }
    } catch {
      case e: Exception => {
        println(s"Had a problem trying to read file: $fileName")
        None
      }
    }

}

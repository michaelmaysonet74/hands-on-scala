package handsonscala

import java.io._

case class Msg(
	val id: Int,
	val parent: Option[Int],
	val txt: String
)

object Chapter3 {
	def flexibleFizzBuzz(cb: String => Unit) = {
		for (n <- Range.inclusive(1, 100)) {
			cb(
				if (n % 3 == 0 && n % 5 == 0) "FizzBuzz"
				else if (n % 3 == 0) "Fizz"
				else if (n % 5 == 0) "Buzz"
				else n.toString
			)
		}
	}

	def printMessages(messages: Array[Msg], indent: String = "", parent: Option[Int] = None): Unit = {
		for (m <- messages if m.parent == parent) {
			println(s"$indent#${m.id} ${m.txt}")
      		printMessages(messages, indent + "    ", Some(m.id))
		}
	}

	def withFileWriter(fileName: String)(cb: BufferedWriter => Unit): Unit = {
		val file = new File(fileName)
		val bw = new BufferedWriter(new FileWriter(file))
		
		try {
			cb(bw)
		} catch {
			case e: IOException => println("Had an IOException trying to write that file")
		} finally {
			bw.close
		}
	}

	def withFileReader(fileName: String)(cb: BufferedReader => String): Option[String] = {
		try {
			val file = new File(fileName)
			val br = new BufferedReader(new FileReader(file))

			try {
				Some(cb(br))
			} catch {
				case e: IOException => {
					println("Had an IOException trying to read that file")
					None
				}
			} finally {
				br.close
			}
		} catch {
			case e: Exception => {
				println("Had a problem trying to read that file")
				None
			}
		}
	}

	def main(args: Array[String]) = {
		flexibleFizzBuzz(s => {})
		flexibleFizzBuzz(s => println(s))

		var i = 0
		val output = new Array[String](100)

		flexibleFizzBuzz { s =>
			output(i) = s
			i += 1
		}

		println(output.mkString("\n"))

		printMessages(Array(
			new Msg(0, None, "Hello"),
			new Msg(1, Some(0), "World"),
			new Msg(2, None, "I am Cow"),
			new Msg(3, Some(2), "Hear me moo"),
			new Msg(4, Some(2), "Here I stand"),
			new Msg(5, Some(2), "I am Cow"),
			new Msg(6, Some(5), "Here me moo, moo")
		))

		withFileWriter("File.txt") { writer =>
			writer.write("Hello\n")
			writer.write("World!")
		}

		val result = withFileReader("File.txt") { reader =>
			reader.readLine + "\n" + reader.readLine
		}.getOrElse("Failed")

		assert(result == "Hello\nWorld!")
	}
}

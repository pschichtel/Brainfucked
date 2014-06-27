import java.io.File

import scala.io.Source

object Main {


  def main(args: Array[String]) {
    if (args.isEmpty) {
      println("Usage: java -jar brainfacked.jar <file>")
      return
    }

    args.foreach(go)
  }

  def go(path: String) {
    Parser.fromPath(path) match {
      case Some(parser) =>
        val root = parser.parse(Source.fromFile(new File(path)))
        root.run(new Memory)
      case None => println("Unknown type!")
    }
  }
}

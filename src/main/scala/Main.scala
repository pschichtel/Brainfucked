import java.io.File

import scala.io.Source

object Main {

  type Action = (RootNode, Array[String]) => Unit

  private val actions = Map[String, Action](
    "interpret" -> intepret,
    "transform" -> transform
  )

  def main(args: Array[String]) {
    if (args.length < 2) {
      println("Usage: java -jar brainfacked.jar <command> <file> [args...]")
      return
    }

    val path = new File(args(1))
    val actionName = args(0).toLowerCase

    actions.get(actionName) match {
      case Some(action) =>
        Language.fromPath(path.getPath) match {
          case Some(parser) =>
            val root = parser.parse(Source.fromFile(path))
            action(root, args.slice(2, args.length))
          case None => println("Unknown type!")
        }
      case None => println("Unknown action!")
    }
  }

  def transform(root: RootNode, args: Array[String]) {
    if (args.length > 0) {
      Language.fromName(args(0)) match {
        case Some(p) =>
          val out = new StringBuilder
          p.generate(root, out)
          println(out.toString())
        case None => println("Unknown parser!")
      }
    }
  }

  def intepret(root: RootNode, args: Array[String]) {
    root.run(new Memory)
  }
}

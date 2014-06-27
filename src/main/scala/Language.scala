import scala.collection.mutable.ArrayBuffer
import scala.io.Source

abstract class Language {
  def parse(source: Source): RootNode
  def generate(root: Node, out: StringBuilder)
}

object Language {
  def fromPath(path: String): Option[Language] = {
    val lastDot = path.lastIndexOf(".")
    if (lastDot != -1) {
      fromName(path.substring(lastDot + 1))
    } else {
      None
    }
  }

  def fromName(name: String): Option[Language] = {
    name.toLowerCase match {
      case "bf"  => Some(new BrainfuckLanguage)
      case "ook" => Some(new OokLanguage)
      case _     => None
    }
  }
}

class BrainfuckLanguage extends Language {
  override def parse(source: Source): RootNode = {
    val root = new RootNode
    var current: Node = root

    for (b <- source) b match {
      case '[' => current = current += new WhileNode
      case ']' => current = current.parent
      case '>' => current += new PtrIncrNode
      case '<' => current += new PtrDecrNode
      case '+' => current += new ValIncrNode
      case '-' => current += new ValDecrNode
      case '.' => current += new PutNode
      case ',' => current += new GetNode
      case _ =>
    }

    root
  }

  override def generate(root: Node, out: StringBuilder) = {
    root.children.foreach {
      case _: ValIncrNode => out.append('+')
      case _: ValDecrNode => out.append('-')
      case _: PtrIncrNode => out.append('>')
      case _: PtrDecrNode => out.append('<')
      case _: PutNode     => out.append('.')
      case _: GetNode     => out.append(',')
      case w: WhileNode   =>
        out.append('[')
        generate(w, out)
        out.append(']')
      case _ => throw new IllegalArgumentException("Unknown node in generator")
    }
  }
}

class OokLanguage extends Language {
  override def parse(source: Source): RootNode = {

    var token = new StringBuilder
    val singleTokens = new ArrayBuffer[String]

    for (c <- source) c match {
      case c: Char if c.isWhitespace =>
        if (token.nonEmpty) {
          singleTokens.append(token.toString().toLowerCase)
          token = new StringBuilder
        }
       case c: Char => token.append(c)
    }

    if (token.nonEmpty) {
      singleTokens.append(token.toString().toLowerCase)
    }

    if (singleTokens.length % 2 == 1) {
      throw new IllegalArgumentException("Odd number of instructions!")
    }

    val tokens = new ArrayBuffer[(String, String)]

    for (i <- 0 until singleTokens.length by 2) {
      tokens.append((singleTokens(i), singleTokens(i + 1)))
    }

    val root = new RootNode
    var current: Node = root

    for (token <- tokens) token match {
      case ("ook.", "ook.") => current += new ValIncrNode
      case ("ook!", "ook!") => current += new ValDecrNode
      case ("ook.", "ook?") => current += new PtrIncrNode
      case ("ook?", "ook.") => current += new PtrDecrNode
      case ("ook!", "ook?") => current = current += new WhileNode
      case ("ook?", "ook!") => current = current.parent
      case ("ook!", "ook.") => current += new PutNode
      case ("ook.", "ook!") => current += new GetNode
      case _ => throw new IllegalArgumentException(token.toString())
    }

    root
  }

  override def generate(root: Node, out: StringBuilder) = {
    val tokens = new ArrayBuffer[String]
    generate(root.children, tokens)
    out.append(tokens.mkString(" "))
  }

  private def generate(children: Seq[Node], out: ArrayBuffer[String]): Unit = {
    for (c <- children) c match {
      case _: ValIncrNode => out += "Ook. Ook."
      case _: ValDecrNode => out += "Ook! Ook!"
      case _: PtrIncrNode => out += "Ook. Ook?"
      case _: PtrDecrNode => out += "Ook? Ook."
      case _: PutNode     => out += "Ook! Ook."
      case _: GetNode     => out += "Ook. Ook!"
      case w: WhileNode =>
        out += "Ook! Ook?"
        generate(w.children, out)
        out += "Ook? Ook!"
      case _ => throw new IllegalArgumentException("Unknown node in generator")
    }
  }
}

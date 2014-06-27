import scala.collection.mutable.ArrayBuffer
import scala.io.Source

abstract class Parser {
  def parse(source: Source): RootNode
}

object Parser {
  def fromPath(path: String): Option[Parser] = {
    val lastDot = path.lastIndexOf(".")
    if (lastDot != -1) {
      path.substring(lastDot + 1).toLowerCase match {
        case "bf"  => Some(new BrainfuckParser)
        case "ook" => Some(new OokParser)
      }
    } else {
      None
    }
  }
}

class BrainfuckParser extends Parser {
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
}

class OokParser extends Parser {
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
}

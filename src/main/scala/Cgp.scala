import org.rogach.scallop._
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.io.FileOutputStream

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val file = trailArg[String]()
  verify()
}

object Cgp extends App {
  val conf = new Conf(args)
  val source = conf.file()
  val sourcePath = Paths.get(source)
  val target = if (source.endsWith(".cg")) {
    source.dropRight(3) + ".cbor"
  } else {
    source + ".cbor"
  }
  val parsed = try {
    Parser(sourcePath)
  } catch {
    case e: SyntaxError =>
      System.err.println(e.toString)
      System.exit(1)
    case e =>
      System.err.println("Unexpected error: " + e.toString)
      System.exit(1)
  }
  val targetFile = new FileOutputStream(target)
  targetFile.write(EncodeAST(parsed.asInstanceOf[AST]))
}

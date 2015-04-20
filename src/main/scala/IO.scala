import scala.tools.nsc.Global

/**
 * Author:  Ming Fang
 * Date:    4/17/15.
 * Email:   mingf@cs.cmu.edu
 */


//serialization of Tree data structures, used to pass between compiler and runtime
class IO(val global: Global) {
  import global._
  private def encodeObj(output_stream: java.io.OutputStream, obj: Object) {
    val ostream = new java.io.ObjectOutputStream(output_stream)
    ostream.writeObject(obj)
    ostream.flush()

  }

  private def decodeObj[T](input_stream: java.io.InputStream): T = {
    val istream = new java.io.ObjectInputStream(input_stream)
    val obj = istream.readObject().asInstanceOf[T]
    obj
  }


  def encodeAST(output_stream: java.io.OutputStream, t: Tree) {
    encodeObj(output_stream, t)
  }

  def decodeAST(input_stream: java.io.InputStream): Tree = {
    val tree = decodeObj[Tree](input_stream)
    tree
  }

  /*
  private def moduleFile(s: String): String = "%s.lobj".format(s)

  private def module_path = Config.getOrElse("module-path", List(".")).asInstanceOf[List[String]]

  private def find_module(name: String): Option[java.io.File] = {
    val file_name = moduleFile(name)
    val file_path = module_path.find {
      path => (new java.io.File(path, file_name)).exists()
    }
    for (path <- file_path)
      yield new java.io.File(path, file_name)
  }


  def load(name: String): Option[Tree] = {
    for (file <- find_module(name)) yield {
      val fs = new java.io.FileInputStream(file)
      val trt = IO.decodeAST(fs)
      fs.close()
      trt
    }
  }
  */

  def save(path: String, tree: Tree) {
    val file_name = "tree.ast"
    val file = new java.io.File(path, file_name)
    val fs = new java.io.FileOutputStream(file)
    encodeAST(fs, tree)
  }
}
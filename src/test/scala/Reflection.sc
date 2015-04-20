import scala.reflect.runtime.universe._

val expr1 = reify {
  (x: Int , y: Int) => x + y
}

expr1.tree

val expr = reify {
  object Main {
    val input = Array[Int](10);
  }
}

expr.tree match {
  case md @ ModuleDef(mods,name,templ) => "yes"
  case md @ ClassDef(mods,name,tparams,templ) => "yes2"
  case 1 => "no"
}

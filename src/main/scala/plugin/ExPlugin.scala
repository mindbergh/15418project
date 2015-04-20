import scala.annotation.StaticAnnotation
import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.tools.nsc.transform.{TypingTransformers, Transform}


class cuSparkCode extends StaticAnnotation


class cuSparkPlugin(val global: Global) extends Plugin with TypingTransformers {
  import global._


  val name = "cuSpark"
  val description = "cuSpark scala plugin"
  val components = List[PluginComponent](Component, ASTComponent)


  def hasCuSparkMod(m : Modifiers) : Boolean = {
    def isCuSparkMod(t : Tree) = t match {
      case Apply(Select(New(Ident(n)),_),List())  if n.toString == "cuSparkCode" => true
      case _ => false
    }
    !m.annotations.find(isCuSparkMod).isEmpty
  }

  private trait cuSparkTransformer extends Transformer {
    def cuSparkTransform(tree: ModuleDef) : ModuleDef
    def cuSparkTransform(tree: ClassDef) : ClassDef

    def preTransform(tree: Tree) : Tree = {
      tree match {
        case md @ ModuleDef(mods,name,templ) if hasCuSparkMod(mods) => {
          reporter.echo(tree.pos,  "Ming: this is a cuSpark object.\n" + mods.toString())
          cuSparkTransform(md)
        }

        case md @ ModuleDef(mods,name,templ)  => {
          reporter.echo(tree.pos, "Ming: this is not a cuSpark object.\n" + mods.toString())
          cuSparkTransform(md)
        }
        case _ => tree
      }
    }

    def postTransform(tree: Tree) : Tree = tree
    override def transform(tree: Tree): Tree = postTransform(super.transform(preTransform(tree)))
  }

  private object Component extends PluginComponent with Transform {
    val global: cuSparkPlugin.this.global.type = cuSparkPlugin.this.global
    val runsAfter = List[String]("parser")
    val phaseName = cuSparkPlugin.this.name + "Test"
    override val description = "This is a test phase"
    override val runsBefore = List("namer")


    override def newTransformer(unit : CompilationUnit) = new cuSparkPhase(unit)
    class cuSparkPhase(unit: CompilationUnit) extends cuSparkTransformer {
      def modifyTemplate(tmpl: Template): Template = {
        def isConstructor(stmt : Tree) : Boolean = stmt match {
          case dd : DefDef if dd.name == nme.CONSTRUCTOR => true
          case _ => false
        }
        val (other,stmts) = tmpl.body partition isConstructor

        val cuSpark_stmts = stmts.map(_.duplicate)

        val unit_literal = Literal(Constant(()))
        val cuSpark_ast = Block(cuSpark_stmts,unit_literal)
        val cuSpark_body_init = Apply(Ident("equals"), List(cuSpark_ast)) //this.equals( { cuSpark code goes here } )

        val newStmts = stmts flatMap {
          case dd @ DefDef(mods,name,tparams,vparamss,tpt,exp) => List(dd)
          case vd @ ValDef(mods,name,tpt,exp) => List( treeCopy.ValDef(vd,mods | scala.tools.nsc.symtab.Flags.LAZY,name,tpt,exp) )
          case _ => List()
        }
        treeCopy.Template(tmpl,tmpl.parents,tmpl.self,cuSpark_body_init :: (other ::: newStmts))
      }
      def cuSparkTransform(tree : ModuleDef) : ModuleDef = {
        val template = modifyTemplate(tree.impl)
        treeCopy.ModuleDef(tree,tree.mods,tree.name,template)
      }
      def cuSparkTransform(tree : ClassDef) : ClassDef = {
        val template = modifyTemplate(tree.impl)
        treeCopy.ClassDef(tree,tree.mods,tree.name,tree.tparams,template)
      }
    }
  }


  private object ASTComponent extends PluginComponent with Transform {
    val global : cuSparkPlugin.this.global.type = cuSparkPlugin.this.global
    val runsAfter = List("refchecks")
    val phaseName = cuSparkPlugin.this.name + "ASTTranslate"
    override val description = "This phase is for saving tree"

    override def newTransformer(unit : CompilationUnit) = { new cuSparkPhase(unit) }
    class cuSparkPhase(unit : CompilationUnit) extends TypingTransformer(unit) {


      def preTransform(tree : Tree) : Tree = tree match {
        case tmp @ Template(parents_,self,stmts) if tmp.symbol.owner.annotationsString == "(cuSparkCode)" => {
          /*
          val trt = new Liszt.Trees.Trait(module_name,type_args,parents,modules,lisztCode)
          Liszt.Trees.Trait.save(module_output_path,trt)
          */
          //reporter.echo("Temp anno: \n" + tmp.symbol.owner.annotationsString)
          //save("tree", tree)
          //save("tree", treeCopy.Template(tmp, parents_, self, stmts))

          def typeRefToName(t : Type) = t match {
            case TypeRef(_,sym,_) => sym.name.toString
            case _ => {
              reporter.error(tmp.pos, "Type miss match")
            }
          }
          val (module_name,type_args) = tmp.tpe match {
            case tr @ TypeRef(_,sym,args) => {
              (typeRefToName(tr), args map typeRefToName)
            }
            case _ => reporter.error(tmp.pos, "Type miss match")
          }


          //reporter.echo(parents_.toString)

          val equals_call :: scala_stmts = stmts
          val Apply(_, List(ast)) = equals_call
          reporter.echo(ast.toString())

          tree
          //treeCopy.Template(tmp,parents_,self,stmts)
        }

       case _ => {
          //reporter.echo("Not a template \n" + x.tpe.toString())
          //reporter.echo(tree.toString())
          tree
        }
      }

      def postTransform(tree : Tree) : Tree = tree
      override def transform(tree : Tree) : Tree = postTransform(super.transform(preTransform(tree)))
    }

    private def encodeObj(output_stream: java.io.OutputStream, obj: Object) {
      val ostream = new java.io.ObjectOutputStream(output_stream)
      ostream.writeObject(obj)
      ostream.flush()
    }

    def encodeAST(output_stream: java.io.OutputStream, t: Tree) {
      encodeObj(output_stream, t)
    }

    def save(path: String, tree: Tree) {
      val file_name = "tree.ast"
      val file = new java.io.File(path, file_name)
      val fs = new java.io.FileOutputStream(file)
      encodeAST(fs, tree)
    }



  }

  /*
  private object Component extends PluginComponent {
    val global: cuSpark.this.global.type = cuSpark.this.global
    //val runsAfter = "refchecks"
    // Using the Scala Compiler 2.8.x the runsAfter should be written as below
    val runsAfter = List[String]("refchecks");
    val phaseName = cuSpark.this.name + "_divbyzero"
    def newPhase(_prev: Phase) = new DivByZeroPhase(_prev)

    class DivByZeroPhase(prev: Phase) extends StdPhase(prev) {
      override def name = cuSpark.this.name
      def apply(unit: CompilationUnit) {
        for ( tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body;
              if rcvr.tpe <:< definitions.IntClass.tpe)
        {
          unit.error(tree.pos, "Ming: definitely division by zero")
        }
      }

    }
  }
  */
}
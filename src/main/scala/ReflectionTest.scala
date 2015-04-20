//List of Scala Bugs I hit while trying to implement this
//https://lampsvn.epfl.ch/trac/scala/ticket/1133
//https://lampsvn.epfl.ch/trac/scala/ticket/425
//http://old.nabble.com/behavior-of-implicit-views-td27185320.html
package scala.LisztPlugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.ast.TreeBrowsers
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.symtab.Flags
import nsc.transform.Transform

import Liszt.Types.{Type => LType}
import Liszt.Types._
import Liszt.Log

class Main(val global : Global) extends Plugin with TypeTranslation with TreeTranslation with nsc.transform.TypingTransformers {
  import global._
  val name = "LisztPlugin"
  val description = "Specialized handling of Liszt code in scala."
  val components = List[PluginComponent](CreateCompilationUnits,ASTTranslate)


  var module_output_path = "."
  override def processOptions(options: List[String], error: String => Unit) {
    for(o <- options) {
      val key = "module_output_path:"
      if(o.startsWith(key)) {
        module_output_path = o.substring(key.length)
      }
    }
  }

  def hasLisztMod(m : Modifiers) : Boolean = {
    def isLisztMod(t : Tree) = t match {
      case Apply(Select(New(Ident(n)),_),List())  if n.toString == "lisztcode" => true
      case _ => false
    }
    !m.annotations.find(isLisztMod).isEmpty
  }

  private trait LisztTransformer extends Transformer {
    def lisztTransform(tree : ModuleDef) : ModuleDef
    def lisztTransform(tree : ClassDef) : ClassDef

    def preTransform(tree : Tree) : Tree = {
      tree match {
        case md @ ModuleDef(mods,name,templ) if hasLisztMod(mods) => lisztTransform(md)
        case md @ ClassDef(mods,name,tparams,templ) if hasLisztMod(mods) => lisztTransform(md)
        case _ => tree
      }
    }
    def postTransform(tree : Tree) : Tree = tree
    override def transform(tree : Tree) : Tree = postTransform(super.transform(preTransform(tree)))
  }

  //all modules annotated with @lisztcode undergo slight pre-typing transformations:
  //Their code is moved into the constructor argument for ComplationUnit
  //Definitions such as variables, classes, and functions are left in the code
  //body so that the typechecker still knows about them
  private object CreateCompilationUnits extends PluginComponent with Transform {
    object Log extends Liszt.Log("Phase1")
    def view(t : Tree) { if(Log.logEnabled) global.treeBrowsers.create().browse(t) }
    val global : Main.this.global.type = Main.this.global
    val runsAfter = List("parser")
    override val runsBefore = List("namer")
    val phaseName = Main.this.name + "_CreateCompilationUnits"

    override def newTransformer(unit : CompilationUnit) = { view(unit.body); new LisztPhase(unit) }
    class LisztPhase(unit : CompilationUnit) extends LisztTransformer {
      def modifyTemplate(t : Template) : Template = {
        def isConstructor(stmt : Tree) : Boolean = stmt match {
          case dd : DefDef if dd.name == nme.CONSTRUCTOR => true
          case _ => false
        }
        val (other,stmts) = t.body partition isConstructor

        val liszt_stmts = stmts.map(_.duplicate)

        val unit_literal = Literal(Constant(()))
        val liszt_ast = Block(liszt_stmts,unit_literal)
        val liszt_body_init = Apply(Ident("equals"), List(liszt_ast)) //this.equals( { liszt code goes here } )

        val newStmts = stmts flatMap {
          case dd @ DefDef(mods,name,tparams,vparamss,tpt,exp) => List(dd)
          case vd @ ValDef(mods,name,tpt,exp) => List( treeCopy.ValDef(vd,mods | scala.tools.nsc.symtab.Flags.LAZY,name,tpt,exp) )
          case _ => List()
        }
        treeCopy.Template(t,t.parents,t.self,liszt_body_init :: (other ::: newStmts))
      }
      def lisztTransform(tree : ModuleDef) : ModuleDef = {
        val template = modifyTemplate(tree.impl)
        treeCopy.ModuleDef(tree,tree.mods,tree.name,template)
      }
      def lisztTransform(tree : ClassDef) : ClassDef = {
        val template = modifyTemplate(tree.impl)
        treeCopy.ClassDef(tree,tree.mods,tree.name,tree.tparams,template)
      }
    }
  }

  //Main part of the plugin: The Liszt code found in the argument to the compilatin unit is translated
  //into the Liszt AST
  private object ASTTranslate extends PluginComponent
  with Transform {
    object Log extends Liszt.Log("Phase2")
    def view(t : Tree) { if(Log.logEnabled) global.treeBrowsers.create().browse(t) }
    val global : Main.this.global.type = Main.this.global
    val runsAfter = List("refchecks")
    val phaseName = Main.this.name + "_ASTTranslate"
    override def newTransformer(unit : CompilationUnit) = { new LisztPhase(unit) }
    class LisztPhase(unit : CompilationUnit) extends TypingTransformer(unit) {
      val liszt_code_annotation = definitions.getClass("Liszt.Language.lisztcode")
      val liszt_solver_annotation = definitions.getClass("Liszt.Language.lisztsolver")
      val liszt_exclude_annotation = definitions.getClass("Liszt.Language.lisztexclude")
      def preTransform(tree : Tree) : Tree = tree match {
        case tmp @ Template(parents_,self,stmts) if tmp.symbol.owner.hasAnnotation(liszt_code_annotation) => {
          view(tmp)

          val parent_excludes = for(x <- List("java.lang.Object", "scala.ScalaObject")) yield definitions.getClass(x).tpe

          def isLinearSolver(s : Symbol) = {
            s.hasAnnotation(liszt_solver_annotation)
          }

          val parents = parents_ flatMap { p =>
            p.tpe match {
              case TypeRef( _ , sym ,args) =>
                if(parent_excludes.contains(p.tpe.normalize)) List()
                else if(sym.hasAnnotation(liszt_exclude_annotation)) List()
                else List( new Liszt.Trees.TraitApply(sym.name.toString,for(a <- args) yield a.lisztType, isLinearSolver(sym)) )
              case _ => Log.error("deriving from ?")
            }
          }
          val equals_call :: scala_stmts = stmts
          val Apply(_,List(liszt_ast)) = equals_call

          def typeRefToName(t : Type) = t match {
            case TypeRef(_,sym,_) => sym.name.toString
            case _ => Log.ierror("unexpected type")
          }
          val (module_name,type_args) = tmp.tpe match {
            case tr @ TypeRef(_,sym,args) => {
              (typeRefToName(tr), args map typeRefToName)
            }
            case _ => Log.ierror("unexpected type")
          }

          val (lisztCode,modules) = scalaToLiszt(liszt_ast,module_name)

          val trt = new Liszt.Trees.Trait(module_name,type_args,parents,modules,lisztCode)
          Liszt.Trees.Trait.save(module_output_path,trt)
          treeCopy.Template(tmp,parents_,self,scala_stmts)
        }

        case _ => tree
      }

      def postTransform(tree : Tree) : Tree = tree
      override def transform(tree : Tree) : Tree = postTransform(super.transform(preTransform(tree)))
    }
  }
}

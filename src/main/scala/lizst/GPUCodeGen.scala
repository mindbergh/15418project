package Liszt
import scala.collection.immutable.{Set => SSet}
import Liszt.Types._
import Liszt.Trees._

class GPUCodeGen(val top : CompilationUnit, globals : SSet[String] ) extends CLikeCodeGen {
  import ASTUtil._
  import CodeLineImplicits._
  val Log = new Log("CodeGen")
  val liszt_obj = "__liszt"
  val fluent_file = "__fluent"

  var inTemplate = false
  var kernel_number = 0
  var onGpu : Boolean = false
  var cpu_fncs = SSet[String]()    //only functions called on the CPU
  var gpu_fncs = SSet[String]()    //only functions called on the GPU
  var kernel_fncs = SSet[String]() //
  var global_vars = SSet[String]()
  var workgroup_triviality : Map[Int,Boolean] = Map() //workgroup_id to isTrivial

  val closeForalls = new CloseForalls(top)

  override def tHelperFunctionModifiers() = "__device__ __host__"
  def isGlobal(e : String) : Boolean = globals.contains(e) || {
    //Note: this is a hack to detect globals that were passed through closures and had their
    //names mangled
    e.endsWith("_passed") && globals.contains(e.substring(0,e.length - 7))
  }
  def isInGlobalContext(v : Var) : String = {
    if (global_vars.contains(v.name))
      if (onGpu) { "gpuGC()." } else { "cpuGC." }
    else
      ""
  }

  def tFunctionHeader(e : DefFunc, isDecl : Boolean) = {
    val fnt = getCType(e).asInstanceOf[Func]
    val name = e.name
    val args = for(a <- e.args) yield a.name
    val template_header = if(fnt.args.exists(_.isInstanceOf[SpecialType])) {
      val template_args = for( SpecialType(_,typ) <- fnt.args) yield "typename %s".format(typ)
      inTemplate = true
      "template< %s > ".format(template_args.mkString(","))
    } else {
      ""
    }
    val argwt = for( (t,a) <- fnt.args zip e.args) yield "%s %s".format({
      val tt = tType(t)
      if(a.global)
        "Global< %s, true >".format(tt)
      else
        tt
    },a.name)
    val return_type = tType(fnt.range)

    val gpumodifier = if (cpu_fncs.contains(name)) {
      "__host__"
    } else if (gpu_fncs.contains(name)) {
      "__device__"
    } else if (kernel_fncs.contains(name)) {
      "__global__"
    } else {
      Log.ierror("unknown type of function")
    }
    "%s %s %s %s(%s)".format(template_header, gpumodifier, return_type,name,argwt.mkString(","))
  }

  def tIndex(e : Index) = Log.ierror("unexpected tIndex")

  def isPointerType(t : Type) = false
  // override def tEnsureFieldState(e : EnsureFieldState) = List()


  def isSpecialCUDAType(t : Type) : Boolean = {
    if(Config.getOrElse("use_cuda_types",false).asInstanceOf[Boolean]) {
      t match {
        case v : VectorT =>
          val cuda_vec_types = Map ( "double" -> List(1,2),
            "float" -> List(1,2,3,4),
            "int" -> List(1,2,3,4) )
          val scalar_type = tType(v.t)
          cuda_vec_types.get(scalar_type) match {
            case Some(nums) => nums.contains(v.n.asInstanceOf[MetaIntT].a)
            case None => false
          }
        case _ => false
      }
    } else {
      false
    }
  }
  override def tType(t : Type) = {
    t match {
      case StringT => "const char *"
      case v : VectorT if isSpecialCUDAType(v) => tType(v.t) + tType(v.n)
      case _ => super.tType(t)
    }
  }
  override def tVectorL(e : VectorL) = {
    if(isSpecialCUDAType(e.t))
      "make_%s(%s)".format(tType(e.t),e.es.map(tVar).mkString(","))
    else
      super.tVectorL(e)
  }
  override def tPerfCounter(e : PerfCounter) : StmtResult = "GRDM_INCREMENT_FLOPS(%d)".format(e.count)
  override def tCall(e : Call) = {
    if (kernel_fncs.contains(e.func.name)) {
      tKernelCall(e)
    } else if (e.func.name == "mesh") {
      if (onGpu) {
        "dMesh[0]"
      } else {
        "hMesh"
      }
    } else {
      super.tCall(e)
    }
  }
  def tKernelCall(e : Call) = {
    val showPrint = if (debuggingEnabled) ";\n    LisztGPU_DISPLAY_PRINTF;\n cpuGC.print_context.count++;" else ";\n"
    val mesh_set = tVar(e.args.head)

    val func_name = tVar(e.func)
    val workgroup_id = func_name.split("_").last.toInt //TODO(zach): the way we find what workgroup id we are launching right now is a hack--we simply name the function that launches the kernel forall_<workgroupID>
    val workgroup_istrivial = workgroup_triviality(workgroup_id).toString();
    Log.log("Workgroup " + workgroup_id + " is trivial? " + workgroup_istrivial)
    val ensure_sizes = for(write <- closeForalls.global_writes_map(workgroup_id))
      yield tEnsureSize(write,"batch.kernel_size()")

    val kernel_args_name = "KERNEL_ARGS_%d".format(workgroup_id)
    val ensure_size_name = "ENSURE_SIZE_%d".format(workgroup_id)
    val kernel_args_M = "#define %s(_) %s".format(kernel_args_name,(e.args.tail.map(x => "_(%s)".format(x.name))).mkString(""," ",""))
    val ensure_size_M = "#define %s(_) %s".format(ensure_size_name,ensure_sizes.toList.map(x => "_(%s)".format(x)).mkString(""," ",""))

    Log.log("tKernelCall: ", e.position.toString())

    """%s
		%s
		KERNEL_CALL(%d,%s,%s,%s,%s,%s);
		#undef %s
		#undef %s 
		%s""".format(ensure_size_M,
        kernel_args_M,
        workgroup_id,
        mesh_set,
        workgroup_istrivial,
        e.func.name,
        ensure_size_name,
        kernel_args_name,
        ensure_size_name,
        kernel_args_name,
        showPrint)
  }

  def tSetInit(e : SetInit) = Log.ierror("set init unexpected.")
  def tIfExp(e : IfExp) = Log.ierror("unexpected ifexp")

  override def tForallPrefix(e : Forall) : StmtResult = if(debuggingEnabled) {
    val pc = ASTUtil.Names.create("print_context")
    "{" :::
      "PrintContext %s;".format(pc) :::
      "PrintContext * %s = PrintContext_push(&%s);".format(ASTUtil.Names.tag(e.set.name,"saved"),pc)
  } else List()
  override def tForallSuffix(e : Forall) : StmtResult = if(debuggingEnabled) {
    "PrintContext_pop(%s);".format(ASTUtil.Names.tag(e.set.name,"saved")) :::
      "}"
  } else List()
  override def tForallBodyPrefix(e : Forall) : StmtResult = if(debuggingEnabled) {
    "PrintContext_mark_%s(%s);".format(typeID(e.set.t.asInstanceOf[Set].t),e.name.name)
  } else List()

  def tForall(e : Forall) = {
    val Forall(name,lblop,set,block) = e
    val suffix = if(inTemplate) "_TM" else ""
    val interior =
      "FORALL_SET%s(%s,%s)".format(suffix,name.name,tVar(set)) ::: {
        val stmts = indent(tForallBodyPrefix(e) ::: tStmt(block))
        stmts
      }
    tForallPrefix(e) :::
      { lblop match {
        case Some(lbl) => {
          "{" ::: indent( "int %s = 0;".format(lbl.name) :::
            interior :::
            indent("%s++;".format(lbl.name)) :::
            "ENDSET") :::
            "}"
        }
        case None => interior ::: "ENDSET"
      } } ::: tForallSuffix(e)
  }
  override def tVar(e : Var) = {
    val prefix = isInGlobalContext(e)
    val suffix = if(e.global) (if (onGpu) ".read_gpu_entry()" else ".read_cpu_entry()") else ""
    prefix + e.name + suffix
  }
  override def tTypeFor(e : Tree) = e match {
    case dv : DefVar if dv.global =>
      "Global< %s , %s >".format(super.tTypeFor(e), if(onGpu) "false" else "true")
    case _ => super.tTypeFor(e)
  }
  // override def tDefVar(e : DefVar) = {
  // 	e.init match {
  // 		case EmptyExp => "%s %s;".format(tTypeFor(e),e.name)
  // 		case exp : IfExp => removeIfExp(e) flatMap tStmt
  // 		case exp => "%s %s = %s;".format(tTypeFor(e),e.name,tExp(exp))
  // 	}
  // }
  //
  override def tDefFunc(e : DefFunc) = e match {
    case DefFunc(ftype,name,args,ExternStmt) => super.tDefFunc(e)
    case DefFunc(ftype,name,args,body) if (kernel_fncs.contains(name)) => tKernelFunc(e)
    case _ => super.tDefFunc(e)
  }
  def tKernelFunc(e : DefFunc) = {
    val DefFunc(ftype,name,args,wg @ Workgroup(List(body))) = e
    //KERNEL FUNCTION BODY CODEGEN
    val Forall(threadLoopingVar, lblop, loopSet, kernelbody) = body
    val kernel_args = "%d,%s,%s,%s,%s".format(
      e.id,
      threadLoopingVar.name,
      tVar(loopSet),
      loopSet.t.asInstanceOf[Set].t,
      typeID(loopSet.t.asInstanceOf[Set].t))
    Log.log("tKernelFunc: ", e.position.toString())
    NewStmt.
      add("%s {".format(tFunctionHeader(e,false))).
      begin.
      add("KERNEL_DEFINE_BEGIN(%s)".format(kernel_args)).
      begin.
      add(tStmt(kernelbody)).
      end.
      add("KERNEL_DEFINE_END(%s)".format(kernel_args)).
      end.
      add("}")
  }


  override def tEnsureFieldState(e : EnsureFieldState) = {
    val prefix = isInGlobalContext(e.field)
    if(e.field.global) "%s%s.ensureState(%s_TYPE);".format(prefix,e.field.name,tReductionOp(e.state)) else List()
  }
  def tEnsureSize(global : String, size_exp : String) : String = {
    val prefix = isInGlobalContext(Var(IntT,global)) //durrrr... hack because it expects a variable.
    "%s%s.ensureSize(%s)".format(prefix, global,size_exp)
  }

  // FIELDS & SETS

  def tFieldInit(e : FieldInit) = Log.ierror("field init unexpected.")

  override def tFieldWrite(e : FieldWrite) = e match {
    case FieldWrite(field,arg,op @ Ops.Reduce(_ : Ops.PickOp),rhs) => tAssign(Assign(Var(rhs.t,"%s[%s]".format(tVar(field),tVar(arg))),op,rhs))
    case _ => super.tFieldWrite(e)
  }
  override def tAssign(e : Assign) = e.lv match {
    case v : Var if v.global => {
      val prefix = isInGlobalContext(v)
      val we = if (onGpu) "write_gpu_entry" else "write_cpu_entry"
      super.tAssign(e.copy(lv = Var(v.t,"%s%s.%s()".format(prefix,v.name,we))))
    }
    case ve @ VecExtract(t,v,i) if v.global => {
      val prefix = isInGlobalContext(v)
      val we = if (onGpu) "write_gpu_entry" else "write_cpu_entry"
      val nv = Var(v.t,"%s%s.%s()".format(prefix,v.name,we))
      super.tAssign(e.copy(lv = ve.copy(vec = nv)))
    }
    case ve @ MatExtract(t,v,r,c) if v.global => {
      val prefix = isInGlobalContext(v)
      val we = if (onGpu) "write_gpu_entry" else "write_cpu_entry"
      val nv = Var(v.t,"%s%s.%s()".format(prefix,v.name,we))
      super.tAssign(e.copy(lv = ve.copy(mat = nv)))
    }
    case _ => super.tAssign(e)
  }
  override def tDefVar(e : DefVar) = {
    val DefVar(name,init) = e
    def declaration = super.tDefVar(DefVar(e.name,EmptyExp(init.t)) copyAttr e)
    def v = tVar(Var(init.t,name)) //variable refering to this declaration
    val result : List[CodeLine] = init match {
        case fi @ FieldInit(_,init) => {
          val real_name = ASTUtil.nameFromTempHack(e.name)
          val FieldT(mo,value) = e.init.t
          def letter = mo match {
            case Vertex => "vertices"
            case Edge => "edges"
            case Face => "faces"
            case Cell => "cells"
            case _ => Log.ierror("unexpected type: %s",e.t)
          }
          init match {
            case ConstSource(exp) => "Field_initWithConst(&cpuGC.%s,%s);".format(real_name,tVar(exp))
            case StringSource(url) => if(url == "position") {
              "Field_initWithPositions(&cpuGC.%s);".format(real_name)
            } else {
              Log.error(fi.report("url not supported"))
            }
          }
        }
        case SetInit(_,set_name) => {
          val real_name = ASTUtil.nameFromTempHack(e.name)
          val Set(v) = e.init.t
          "BoundarySet_init<%sType>(%s,&cpuGC.%s);".format(tType(v),tStringL(StringL(set_name)),real_name)
        }
        case _ => super.tDefVar(e)
      }
    result
  }
  override def runtimeTypeFor(ctx : CtxT, exptoctype : ExpT, tree : Tree)  = tree match {
    case SetInit(t,_) => SpecialType(t,"BoundarySet")
    case _ => super.runtimeTypeFor(ctx,exptoctype,tree)
  }

  override def tVecExtract(e : VecExtract) = if(isSpecialCUDAType(e.vec.t)) {

    def extract(s : String) = "%s.%s".format(tVar(e.vec),s)
    e.n match {
      case IntL(0) => extract("x")
      case IntL(1) => extract("y")
      case IntL(2) => extract("z")
      case IntL(3) => extract("w")
      case _ =>  Log.error("stop using variables to extract from vectors, it may be hazardous to your health")
    }
  } else {
    super.tVecExtract(e)
  }
  override def tPrint(e : Print) = {
    def formatForType(t : Type) : String = t match {
      case StringT => "%s"
      case IntT => "%d"
      case FloatT => "%f"
      case DoubleT => "%f"
      case BoolT => "%d"
      case VectorT(MetaIntT(n),t) => {
        val fmt = formatForType(t)
        val fmts = for(i <- 0 until n) yield fmt
        "(%s)".format(fmts.mkString(","))
      }
      case MatrixT(MetaIntT(n : Int), MetaIntT(m : Int), t : Type) => {
        val sfmt = formatForType(t)
        val fmt = { for(i <- 0 until m) yield sfmt }.mkString(",")
        val fmts = for(i <- 0 until n) yield fmt
        "[%s]".format(fmts.mkString("|"))
      }
      case Vertex => "Vertex %d"
      case Edge => "Edge %d"
      case Face => "Face %d"
      case Cell =>  "Cell %d"
      case _ => Log.ierror("GPU does not supporting printing type: %s",t)
    }
    //return the string
    def argumentsForVar(t : Type, v : String) : List[String] = t match {
      case VectorT(MetaIntT(n),t) =>
        { for(i <- 0 until n;
              a <- argumentsForVar(t,"%s[%d]".format(v,i)))
          yield a }.toList
      case MatrixT(MetaIntT(n),MetaIntT(m),t) =>
        { for(i <- 0 until n;
              j <- 0 until m;
              a <- argumentsForVar(t,"%s(%d,%d)".format(v,i,j)))
          yield a }.toList
      case Vertex => List("ID_v(%s)".format(v))
      case Edge => List("ID_e(%s)".format(v))
      case Face => List("ID_f(%s)".format(v))
      case Cell => List("ID_c(%s)".format(v))

      case _ => List(v)
    }
    var fmtString = { for(e <- e.es) yield formatForType(e.t) }.mkString("")
    var argString = { for(e <- e.es; a <- argumentsForVar(e.t,tVar(e))) yield a }.mkString(",")

    val printfn = if(onGpu) "LisztGPU__printf" else "printf"

    if(debuggingEnabled) {
      val buf_name = ASTUtil.Names.create("buf")
      "{" :::
        indent {
          //TODO(zach): size the buffer to guarentee it is large enough, at least (N + 1) * (24), where N is the maximum nesting depth
          "char %s[256];".format(buf_name) :::
            "PrintContext_print(%s);".format(buf_name) :::
            "%s(\"%%s liszt: %s\\n\",%s,%s);".format(printfn,fmtString,buf_name,argString)
        } :::
        "}"
    } else if (!onGpu) {
      "%s(\"liszt: %s\\n\",%s);".format(printfn,fmtString,argString)
    } else {
      ""
    }

  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //ANALYSIS FUNCTIONS
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  //Creates two separate blocks of DefFuncs from the AST, one for functions on the GPU, one for functions on the CPU
  def separateCpuGpuFuncDefs(func_defs : Block, cpu_fncs : SSet[String], gpu_fncs : SSet[String], kernel_fncs : SSet[String]) : (Block,Block,Block) = {
    val cpu_func_defs_toprocess = Block(func_defs.stmts.filter(stmt => stmt match {
      case DefFunc(ftype,name,args,ExternStmt) => cpu_fncs contains name
      case DefFunc(ftype,name,args,value) => cpu_fncs contains name
      case _ => false
    }),EmptyExp(UnitT))

    val gpu_func_defs_toprocess = Block(func_defs.stmts.filter(stmt => stmt match {
      case DefFunc(ftype,name,args,ExternStmt) => gpu_fncs contains name
      case DefFunc(ftype,name,args,value) => gpu_fncs contains name
      case _ => false
    }),EmptyExp(UnitT))

    val gpu_kernel_defs_toprocess = Block(func_defs.stmts.filter(stmt => stmt match {
      case DefFunc(ftype,name,args,ExternStmt) => kernel_fncs contains name
      case DefFunc(ftype,name,args,value) => kernel_fncs contains name
      case _ => false
    }),EmptyExp(UnitT))
    return (cpu_func_defs_toprocess,gpu_func_defs_toprocess,gpu_kernel_defs_toprocess)
  }

  def create(build_dir : String) : Unit = {
    //todo(njoubert, zdevito): abstract interpretation should work after the closeForalls
    Log.log("%s\nGPU CodeGen Started.\n%s", "="*80, "="*80)

    top.code = closeForalls.code
    val (tgraph,workgroup_info,_) = AbstractInterpretation.create(top)

    //calculate triviality of each workgroup
    workgroup_triviality = for ((w,l) <- workgroup_info) yield (w.id, l.forall( w => w.isTrivial))
    //for((i,t) <- workgroup_triviality) println("workgroup " + i + " triviality " + t)

    val coloring_generator = new ColoringGenerator(tgraph,workgroup_info,workgroup_triviality,tType)

    val toplevel = normalizeGlobals(top.code)
    cpu_fncs = closeForalls.unnested_funcs
    gpu_fncs = closeForalls.nested_funcs
    kernel_fncs = SSet.empty ++ { for (DefFunc(_,fname,_,_) <- closeForalls.forallFuncts) yield fname }
    val (cpu_func_defs_toprocess,gpu_func_defs_toprocess,gpu_kernel_defs_toprocess) = separateCpuGpuFuncDefs(toplevel.func_defs,cpu_fncs,gpu_fncs,kernel_fncs)

    global_vars = closeForalls.global_variables
    val var_decls = indent(tStmt(toplevel.var_decls)).mkString("\n")
    val func_decls = tStmt(toplevel.func_decls).mkString("\n")

    val filtered_var_defs = Util.removeAssignmentsToFieldsAndSets(toplevel.var_defs)
    val var_defs = indent(tStmt(filtered_var_defs)).mkString("\n")
    onGpu = false
    val cpu_func_defs = tStmt(cpu_func_defs_toprocess).mkString("\n")
    onGpu = true
    val gpu_func_defs = tStmt(gpu_func_defs_toprocess).mkString("\n")
    val gpu_kernel_defs = tStmt(gpu_kernel_defs_toprocess).mkString("\n")
    onGpu = false
    val log_str = if(Config.getOrElse("redirect-to-log",false).asInstanceOf[Boolean]) "" else "//"
    val helper_fns = tHelperFunctions().mkString("\n")

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //CPU MAIN BODY
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val mainstr = """

#include "Gpu/GPURuntimeHost.h"
#include "GlobalContext.h"
#include "Gpu/GPUMeshFunctions.h"
extern GlobalContext cpuGC;
//create coloring metadata
%s
                  """.format(coloring_generator.write())

    //necessary for IDE to not mess up syntax coloring"

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //KERNEL BODY
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val printfDebugBool = if (debuggingEnabled) "true" else "false"
    val kernelbodystr = """
#include "Gpu/GPURuntimeDevice.h"
#include "GlobalContext.h"
using namespace LisztGPU;

//LANGUAGE HELPER FUNCTIONS
%s

event_pair timer;
//LANGUAGE-PROVIDED VARIABLES
Mesh* hMesh; //host based structure that contains pointers to device memory locations
Mesh* dummydMesh; //cpu pointer to device based structure with device pointers
MeshIO::id_t * dummyRenamed_to_global_ids[MeshIO::TYPE_SIZE];

__constant__ Mesh* dMesh[1]; //gpu pointer to device based structure with device pointers
__constant__ MeshIO::id_t * renamed_to_global_ids[MeshIO::TYPE_SIZE];
__device__ static inline id_type GPU_getGlobalID(MeshIO::IOElemType typ, id_type id) {
	MeshIO::id_t * arr = renamed_to_global_ids[typ];
	if(arr == NULL) { //if we did not partition, then we will set renamed_global_ids[0:4] to NULL
		return id;
	} else {
		return arr[id];
	}
}
//VARIABLE DECLARATIONS

//let's stop nvcc from complaining about empty contructors
__constant__ double gc_buffer[sizeof(GlobalContext)/sizeof(double) + sizeof(double)]; //fixes memory alignment issues
__device__ GlobalContext & gpuGC() {
	return *(GlobalContext*) gc_buffer;
}
GlobalContext cpuGC;

void GlobalContext_copyToGPU(GlobalContext* cpu) {
	cudaMemcpyToSymbol("gc_buffer", cpu, sizeof(GlobalContext), 0, cudaMemcpyHostToDevice);
	check_launch("gpuGC");
}
#ifdef PROJECT_DEBUG
PrintContext * PrintContext_top() {
#ifdef __CUDA_ARCH__
	return &gpuGC().print_context;
#else
	return &cpuGC.print_context;
#endif
}
#endif

//FUNCTION DECLARATIONS
%s
//GPU KERNEL DEFINITIONS
%s
//GPU FUNCTION DEFINITIONS
%s
//CPU FUNCTION DEFINITIONS
%s
// GPU INITIALIZATION FUNCTION
void liszt_initialize_globals_gpu()
{
	check_launch("before");
    if (%s) {
      LisztGPU_INIT_PRINTF;
    }
    cudaMemcpyToSymbol(dMesh, &dummydMesh, sizeof(Mesh*), 0, cudaMemcpyHostToDevice);
    check_launch("dMesh");
    cudaMemcpyToSymbol(renamed_to_global_ids,dummyRenamed_to_global_ids,sizeof(MeshIO::id_t *) * MeshIO::TYPE_SIZE, 0, cudaMemcpyHostToDevice);
    check_launch("renamed_to_global_ids");
    //VARIABLE DEFINITIONS
%s
}	
// GPU MAIN RUN FUNCTION
void liszt_run()
{
    _liszt_start();
    if (%s) {
      LisztGPU_TERMINATE_PRINTF;
    }
}

                        """.format(tHelperFunctions().mkString("\n"), func_decls, gpu_kernel_defs, gpu_func_defs, cpu_func_defs, printfDebugBool, var_defs, printfDebugBool)
    val globalcontextstr = """
#include "Gpu/Global.h"
#include "Gpu/Field.h"
#include "Gpu/BoundarySet.h"
struct GlobalContext {
#ifdef PROJECT_DEBUG
PrintContext print_context;
#endif
%s
};
                           		""".format(var_decls)

    Util.WriteFile("%s/gpu/main.cpp".format(build_dir)) {
      out =>
        out println mainstr
    }
    Util.WriteFile("%s/gpu/kernels.cu".format(build_dir)) {
      out =>
        out println kernelbodystr
    }
    Util.WriteFile("%s/gpu/GlobalContext.h".format(build_dir)) {
      out =>
        out println globalcontextstr
    }
    Log.log("%s\nGPU Compiler Stage Done. C++ files written.\n%s".format("="*80,"="*80))
  }

}

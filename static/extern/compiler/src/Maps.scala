package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext} //RefinedManifest
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

//import scala.virtualization.lms.internal._

//trait SmallMapOpsExp {
//trait SmallMapOpsExp extends DeliteMapFatExp {

//abstract class SmallMap[K,V]
trait SmallMapOpsExp /*extends DeliteArrayFatExp*/ extends EffectExp {
  this: DeliteOpsExp => //?

  //abstract class CompilerMap[K,V]
  type SmallMap[K,V] = scala.collection.mutable.HashMap[K,V] //CompilerMap[K,V]
  abstract class SmallMapDef[K:Manifest,V:Manifest,R:Manifest] extends Def[R] {
    val mK = manifest[K]
    val mV = manifest[V]
  }
  //type SmallMap[K,V] = scala.collection.mutable.HashMap[K,V]
  implicit def smallMapManifest[K:Manifest, V:Manifest] = manifest[scala.collection.mutable.HashMap[K,V]]
  
  case class MapNew[K:Manifest, V:Manifest]() extends SmallMapDef[K,V,SmallMap[K,V]]
  case class MapPut[K:Manifest, V:Manifest](m: Exp[SmallMap[K,V]], key: Exp[K], value: Exp[V]) extends SmallMapDef[K,V,Unit]
  //https://issues.scala-lang.org/browse/SI-7090
  case class MapGetOrElse[K:Manifest, V:Manifest](m: Exp[SmallMap[K,V]], key: Exp[K], default: /*=>*/ Exp[V]) extends SmallMapDef[K,V,Any] /* V */
  case class MapContains[K:Manifest, V:Manifest](m: Exp[SmallMap[K,V]], key: Exp[K]) extends SmallMapDef[K,V,Boolean] 

  def map_empty[K:Manifest, V:Manifest]()(implicit __imp0: SourceContext): Rep[SmallMap[K,V]] 
    = reflectMutable(MapNew[K,V]()) 
  def map_put[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K], value: Rep[V])(implicit __imp0: SourceContext): Rep[Unit]
    = reflectWrite(m)(MapPut[K,V](m, key, value))
  def map_getOrElse[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K], default: /*=>*/ Rep[V])(implicit __imp0: SourceContext): Rep[Any] /*V*/ 
    = MapGetOrElse[K,V](m, key, default) //m.getOrElse(key, default) // Options?
  def map_contains[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K])(implicit __imp0: SourceContext): Rep[Boolean] /*V*/ 
    = MapContains[K,V](m, key) 

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = ( e match {
    case e@MapContains(m,k) => map_contains(f(m),f(k))(e.mK,e.mV,pos)
    case e@MapGetOrElse(m,k,d) => map_getOrElse(f(m),f(k),f(d))(e.mK,e.mV,pos)
    case Reflect(e@MapNew(), u, es) => reflectMirrored(Reflect(MapNew()(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MapContains(m,k), u, es) => reflectMirrored(Reflect(MapContains(f(m),f(k))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MapGetOrElse(m,k,d), u, es) => reflectMirrored(Reflect(MapGetOrElse(f(m),f(k),f(d))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MapPut(m,k,v), u, es) => reflectMirrored(Reflect(MapPut(f(m),f(k),f(v))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
  
}

trait ScalaGenSmallMapOps /* extends ScalaGenDeliteMapOps */ extends ScalaGenPrimitiveOps with ScalaGenObjectOps {
  val IR: SmallMapOpsExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case m@MapNew() => emitValDef(sym, "new scala.collection.mutable.HashMap["+remap(m.mK)+"," + remap(m.mV)+"]()")
    case MapPut(m,k,v) => emitValDef(sym, "" + quote(m) + ".put(" + quote(k) + ", " + quote(v) + ")")
    case MapGetOrElse(m,k,d) => emitValDef(sym, "" + quote(m) + ".getOrElse(" + quote(k) + ", " + quote(d) + ")")
    case MapContains(m,k) => emitValDef(sym, "" + quote(m) + ".contains(" + quote(k) + ")")
    case _ => super.emitNode(sym,rhs)
  }
}

// what is C like?
trait CLikeGenSmallMapOps extends CLikeGenBase {
  val IR: SmallMapOpsExp with DeliteOpsExp
  import IR._
/*
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MapPut(m,k,v) => emitValDef(sym, quote(m) + ".put(" + quote(k) + ", " + quote(v) + ")")
    case MapGetOrElse(m,k,d) => emitValDef(sym, "" + quote(m) + ".getOrElse(" + quote(k) + ", " + quote(d) + ")")
    case _ => super.emitNode(sym,rhs)
  }
*/
}

trait CudaGenSmallMapOps /* extends CudaGenDeliteMapOps */ extends CLikeGenSmallMapOps with CudaGenPrimitiveOps with CudaGenObjectOps { val IR: SmallMapOpsExp with DeliteOpsExp }
trait OpenCLGenSmallMapOps /* extends OpenCLGenDeliteArrayOps */ extends CLikeGenSmallMapOps with OpenCLGenPrimitiveOps with OpenCLGenObjectOps { val IR: SmallMapOpsExp with DeliteOpsExp }
trait CGenSmallMapOps /* extends CGenDeliteMapOps */ extends CGenPrimitiveOps with CGenObjectOps {
  val IR: SmallMapOpsExp with DeliteOpsExp
  import IR._
/*
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MapPut(m,k,v) => emitValDef(sym, "Map not supported in C languages")
    case MapGetOrElse(m,k,d) => emitValDef(sym, "Map not supported in C Languages")
    case _ => super.emitNode(sym, rhs)
  }
*/
}

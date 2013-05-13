package LOWERCASE_DSL_NAME.library.extern

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import LOWERCASE_DSL_NAME.library.HUMAN_DSL_NAMEBase

trait ForgeArrayWrapper extends HUMAN_DSL_NAMEBase {
  type ForgeArray[T] = scala.Array[T]
  implicit def forgeArrayManifest[T:Manifest] = manifest[Array[T]]
  
  def array_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[T]] 
    = new ForgeArray[T](__arg0)
  def array_copy[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArray[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
    = System.arraycopy(__arg0,__arg1,__arg2,__arg3,__arg4)
  def array_update[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
    = __arg0(__arg1) = __arg2
  def array_apply[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = __arg0(__arg1)
  def array_length[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[Int]          
    = __arg0.length
  def array_asimmutable[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = __arg0

  def array_map[A:Manifest,B:Manifest](a: Rep[DeliteArray[A]], f: Rep[A] => Rep[B]): Rep[DeliteArray[B]]
    = a.map(f)
  // note - I changed to zip to match Forge
  def array_zip[A:Manifest,B:Manifest,R:Manifest](x: Rep[DeliteArray[A]], y: Rep[DeliteArray[B]], f: (Rep[A],Rep[B]) => Rep[R]): Rep[DeliteArray[R]]
    = x.zip(y).map(f)
  def array_reduce[A:Manifest](x: Rep[DeliteArray[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A]): Rep[A]
    = x.reduce(f)
  def array_filter[A:Manifest](x: Rep[DeliteArray[A]], f: Rep[A] => Rep[Boolean]): Rep[DeliteArray[A]]
    = x.filter(f)
  def array_mkstring[A:Manifest](a: Rep[DeliteArray[A]], del: Rep[String]): Rep[String]
    = __arg0
  def array_union[A:Manifest](lhs: Rep[DeliteArray[A]], rhs: Rep[DeliteArray[A]]): Rep[DeliteArray[A]]
    = __arg0
  def array_intersect[A:Manifest](lhs: Rep[DeliteArray[A]], rhs: Rep[DeliteArray[A]]): Rep[DeliteArray[A]]
    = __arg0
  def array_take[A:Manifest](lhs: Rep[DeliteArray[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
    = __arg0
  def array_sort[A:Manifest](lhs: Rep[DeliteArray[A]]): Rep[DeliteArray[A]]
    = __arg0
  def array_range(st: Rep[Int], en: Rep[Int]): Rep[DeliteArray[Int]]
    = __arg0
  def array_toseq[A:Manifest](a: Rep[DeliteArray[A]]): Rep[Seq[A]]
    = __arg0

}

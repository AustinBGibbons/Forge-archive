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
    
  def scala_array_apply[T:Manifest](__arg0: Rep[Array[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T] 
    = array_apply(__arg0,__arg1)

  def array_map[A:Manifest,B:Manifest](a: Rep[ForgeArray[A]], f: Rep[A] => Rep[B])(implicit __imp0: SourceContext): Rep[ForgeArray[B]]
    = a.map(f)
  // unnecessary constraint that f creates a ForgeArray?
  def array_flatmap[A:Manifest,B:Manifest](a: Rep[ForgeArray[A]], f: Rep[A] => Rep[ForgeArray[B]])(implicit __imp0: SourceContext): Rep[ForgeArray[B]]
    //= a.flatMap(f)
    = a.map(f).reduce(_ union _)
  def array_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Rep[ForgeArray[A]], y: Rep[ForgeArray[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArray[R]]
    = x.zip(y).map{case(x,y) => f(x,y)}
  def array_reduce[A:Manifest](x: Rep[ForgeArray[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit __imp0: SourceContext): Rep[A]
    = x.reduce(f)
  def array_filter[A:Manifest](x: Rep[ForgeArray[A]], f: Rep[A] => Rep[Boolean])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
    = x.filter(f)
  def array_mkstring[A:Manifest](a: Rep[ForgeArray[A]], del: Rep[String])(implicit __imp0: SourceContext): Rep[String]
    = a.mkString(del)
  def array_union[A:Manifest](lhs: Rep[ForgeArray[A]], rhs: Rep[ForgeArray[A]])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
    = lhs.union(rhs)
  def array_intersect[A:Manifest](lhs: Rep[ForgeArray[A]], rhs: Rep[ForgeArray[A]])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
    = lhs.intersect(rhs)
  def array_take[A:Manifest](lhs: Rep[ForgeArray[A]], n: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
    = lhs.take(n)
  //def array_sort[A:Manifest](lhs: Rep[ForgeArray[A]])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
  //  = { scala.math.Sorting.quickSort(lhs) ; lhs }
  def array_range(st: Rep[Int], en: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[Int]]
    = Array.range(st, en)
  def array_toseq[A:Manifest](a: Rep[ForgeArray[A]])(implicit __imp0: SourceContext): Rep[Seq[A]]
    = a.toSeq
}

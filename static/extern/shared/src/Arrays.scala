package LOWERCASE_DSL_NAME.shared.extern

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

// Front-end
trait LiftForgeArrays
trait ForgeArrayOps extends Base {
  /**
   * We use ForgeArray[T] instead of T so we don't get any subtle errors related to shadowing Array[T]
   */
  type ForgeArray[T]
  implicit def forgeArrayManifest[T:Manifest]: Manifest[ForgeArray[T]]
}
trait ForgeArrayCompilerOps extends ForgeArrayOps {  
  
  /**
   * There are some unfortunate scalac typer crashes when we try to use the nicer front-end from DSLs :(
   */
  object Array {
    def empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext) = array_empty[T](__arg0)
    def copy[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArray[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext) = array_copy(__arg0,__arg1,__arg2,__arg3,__arg4)
  }
  
  implicit class ForgeArrayOps[T:Manifest](x: Rep[ForgeArray[T]]) {
    def update(n: Rep[Int], y: Rep[T])(implicit ctx: SourceContext) = array_update(x,n,y)
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = array_apply(x,n)
    def asImmutable(implicit ctx: SourceContext) = array_asimmutable(x)
  }
  
  def array_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
  def array_copy[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArray[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
  def array_update[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
  def array_apply[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
  def array_length[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[Int]          
  def array_asimmutable[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]

  def array_map[A:Manifest,B:Manifest](a: Rep[ForgeArray[A]], f: Rep[A] => Rep[B])(implicit __imp0: SourceContext): Rep[ForgeArray[B]]
  def array_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Rep[ForgeArray[A]], y: Rep[ForgeArray[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArray[R]]
  def array_reduce[A:Manifest](x: Rep[ForgeArray[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit __imp0: SourceContext): Rep[A]
  def array_filter[A:Manifest](x: Rep[ForgeArray[A]], f: Rep[A] => Rep[Boolean])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
  def array_mkstring[A:Manifest](a: Rep[ForgeArray[A]], del: Rep[String])(implicit __imp0: SourceContext): Rep[String]
  def array_union[A:Manifest](lhs: Rep[ForgeArray[A]], rhs: Rep[ForgeArray[A]])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
  def array_intersect[A:Manifest](lhs: Rep[ForgeArray[A]], rhs: Rep[ForgeArray[A]])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
  def array_take[A:Manifest](lhs: Rep[ForgeArray[A]], n: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
  //def array_sort[A:Manifest](lhs: Rep[ForgeArray[A]])(implicit __imp0: SourceContext): Rep[ForgeArray[A]]
  def array_range(st: Rep[Int], en: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[Int]]
  def array_toseq[A:Manifest](a: Rep[ForgeArray[A]])(implicit __imp0: SourceContext): Rep[Seq[A]]
}

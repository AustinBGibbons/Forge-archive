package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

// Front-end
trait LiftSmallMaps
trait SmallMapOps extends Base {
  type SmallMap[K,V]
  implicit def smallMapManifest[K:Manifest, V:Manifest]: Manifest[SmallMap[K,V]]
}
trait SmallMapCompilerOps extends SmallMapOps {    
  def map_empty[K:Manifest, V:Manifest]()(implicit __imp0: SourceContext): Rep[SmallMap[K,V]]
  def map_put[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K], value: Rep[V])(implicit __imp0: SourceContext): Rep[Unit]
  def map_remove[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K])(implicit __imp0: SourceContext): Rep[Any] // V
  //def map_get[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K])(implicit __imp0: SourceContext): Rep[Option[R]]
  def map_getOrElse[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K], default: /*=>*/ Rep[V])(implicit __imp0: SourceContext): Rep[Any]
  def map_contains[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K])(implicit __imp0: SourceContext): Rep[Boolean]
  
}

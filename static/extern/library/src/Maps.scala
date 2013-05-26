package LOWERCASE_DSL_NAME.library.extern

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import LOWERCASE_DSL_NAME.library.HUMAN_DSL_NAMEBase

//abstract class SmallMap[K,V]
trait SmallMapWrapper extends HUMAN_DSL_NAMEBase {
  //abstract class SmallMap[K,V]
  type SmallMap[K,V] = scala.collection.mutable.HashMap[K,V]
  implicit def smallMapManifest[K:Manifest, V:Manifest] = manifest[scala.collection.mutable.HashMap[K,V]] 

  def map_empty[K:Manifest, V:Manifest]()(implicit __imp0: SourceContext): Rep[SmallMap[K,V]] 
    = new SmallMap[K,V]()

  // how about this. how about this.
  //def scala_map_put
  //def scala_map_get

  def map_put[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K], value: Rep[V])(implicit __imp0: SourceContext): Rep[Unit] 
    = m.put(key, value)
  //def map_get[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K])(implicit __imp0: SourceContext): Rep[Optiom[R]]
  def map_getOrElse[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K], default: /*=>*/ Rep[V])(implicit __imp0: SourceContext): Rep[Any]
    = m.getOrElse(key, default) // options?
  def map_contains[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K])(implicit __imp0: SourceContext): Rep[Boolean]
    = m.contains(key)
}


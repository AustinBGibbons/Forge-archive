package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

// Front-end
//abstract class SmallMap[K,V]
trait LiftSmallMaps
trait SmallMapOps extends Base {
  //abstract class SmallMap[K,V]
  type SmallMap[K,V]
  implicit def smallMapManifest[K:Manifest, V:Manifest]: Manifest[SmallMap[K,V]]
  
  /* Required for apps to be able use 'args' */
/*
  implicit class ScalaMapOps[K:Manifest, V:Manifest](x: Rep[Map[K,V]]) {
    def put(key: Rep[K], value: Rep[V])(implicit ctx: SourceContext) = scala_map_put(x,key,value)
    def getOrElse(key: Rep[K], default: => Rep[V])(implicit ctx: SourceContext) = scala_map_getOrElse(x,key,default)
  }
  def scala_map_put[K:Manifest, V:Manifest](x: Rep[Map[T]], key: Rep[K], value: Rep[V])(implicit __imp0: SourceContext): Rep[Unit]
  def scala_map_get[K:Manifest, V:Manifest](x: Rep[Map[T]], key: Rep[K], default: => Rep[V])(implicit __imp0: SourceContext): Rep[V]
*/
}
trait SmallMapCompilerOps extends SmallMapOps {    
  /**
   * There are some unfortunate scalac typer crashes when we try to use the nicer front-end from DSLs :(
   */
/*
  object Map { 
    def empty[K:Manifest, V:Manifest]()(implicit __imp0: SourceContext) = map_empty[K,V]()
  }
*/
  /*
  implicit class SmallMapOps[K:Manifest, V:Manifest](x: Rep[SmallMap[K,V]]) {
    def update(y: Rep[K], z: Rep[V])(implicit ctx: SourceContext) = map_update(x,y,z)
    def apply(y: Rep[K])(implicit ctx: SourceContext) = map_apply(x,y)
    def asImmutable(implicit ctx: SourceContext) = map_asimmutable(x)
  }
  */
  
  def map_empty[K:Manifest, V:Manifest]()(implicit __imp0: SourceContext): Rep[SmallMap[K,V]]
  def map_put[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K], value: Rep[V])(implicit __imp0: SourceContext): Rep[Unit]
  //def map_get[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K])(implicit __imp0: SourceContext): Rep[Option[R]]
  def map_getOrElse[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K], default: /*=>*/ Rep[V])(implicit __imp0: SourceContext): Rep[Any]
  def map_contains[K:Manifest, V:Manifest](m: Rep[SmallMap[K,V]], key: Rep[K])(implicit __imp0: SourceContext): Rep[Boolean]
  
}

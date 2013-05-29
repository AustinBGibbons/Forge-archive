package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

// Front-end
trait ForgeProfileWrapper extends HUMAN_DSL_NAMEBase {
  var start = 0L
  def start(deps: Rep[Any]*)(implicit ctx: SourceContext): Rep[Unit] 
    = _profile_start(unit("app"),deps)
  def start(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext): Rep[Unit] 
    = _profile_start(component,deps)
  def stop(deps: Rep[Any]*)(implicit ctx: SourceContext): Rep[Unit] 
    = _profile_stop(unit("app"),unit(List()))
  def stop(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext): Rep[Unit] 
    = _profile_stop(component,deps)
  def _profile_start(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit] = {
    start = System.currentTimeMillis()
  }
  def _profile_stop(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit] = {
      println("elapsed time: " + (System.currentTimeMillis() - start) / 1000.0)
    }
}

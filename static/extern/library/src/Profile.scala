package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

// Front-end
trait ForgeProfileWrapper extends HUMAN_DSL_NAMEBase {
  var start = 0L
  def _tic(deps: Rep[Any]*)(implicit ctx: SourceContext) = _profile_start(unit("app"),deps)
  def _tic(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = _profile_start(component,deps)
  def _toc(deps: Rep[Any]*)(implicit ctx: SourceContext) = _profile_stop(unit("app"),unit(List()))
  def _toc(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = _profile_stop(component,deps)
  def _profile_start(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit] = {
    start = System.currentTimeMillis()
  }
  def _profile_stop(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit] = {
      println("elapsed time: " + (System.currentTimeMillis() - start) / 1000.0)
    }
}

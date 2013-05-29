package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

// Front-end
trait LiftForgeProfiles
trait ForgeProfileOps extends Base
trait ForgeProfileCompilerOps extends ForgeProfileOps {    
  def _tic(deps: Rep[Any]*)(implicit ctx: SourceContext) //= tic(deps)
  def _tic(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) //= tic(component)
  def _toc(deps: Rep[Any]*)(implicit ctx: SourceContext) //= toc(deps)
  def _toc(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) //= 
  def _profile_start(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
  def _profile_stop(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
}

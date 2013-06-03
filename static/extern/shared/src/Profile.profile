package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

// Front-end
trait LiftForgeProfiles
trait ForgeProfileOps extends Base
trait ForgeProfileCompilerOps extends ForgeProfileOps {    
  def start(deps: Rep[Any]*)(implicit ctx: SourceContext): Rep[Unit] //= tic(deps)
  def start(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext): Rep[Unit] //= tic(component)
  def stop(deps: Rep[Any]*)(implicit ctx: SourceContext): Rep[Unit] //= toc(deps)
  def stop(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext): Rep[Unit] //= 
  def _profile_start(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
  def _profile_stop(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
}

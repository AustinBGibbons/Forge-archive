package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

import ppl.delite.framework.DeliteILOps
//import ppl.delite.framework._
import ppl.delite.framework.ops.DeliteOpsExp

// Front-end
trait ForgeProfileOpsExp extends EffectExp /*with DeliteILOps*/ {
  this: DeliteILOps => 

  def start(deps: Rep[Any]*)(implicit ctx: SourceContext): Rep[Unit]
    = tic(deps: _*) 
  def start(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext): Rep[Unit]
    = tic(component, deps: _*)
  def stop(deps: Rep[Any]*)(implicit ctx: SourceContext) 
    = toc(deps: _*)
  def stop(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) 
    = toc(component, deps: _*)
  def _profile_start(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
    = profile_start(component, deps)
  def _profile_stop(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
    = profile_stop(component, deps)
}

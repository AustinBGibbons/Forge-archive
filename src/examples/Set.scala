package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}
//import templates.Utilities.nl

trait SetOps extends ForgeApplication with ScalaOps {
  def addSetOps() = {
    addScalaOps()

    val T = tpePar("T")
    val Set = tpe("Set", T) 
    // What should MBoolean be - MInt for multiset?
    val SetType = tpeInst(MMap, List(T, MBoolean))
    data(Set, ("_data", SetType))

    op (Set) ("apply", static, T, Nil :: Set, effect = mutable) implements allocates(Set, ${map_empty[T, Boolean]()})

    val SetOps = withTpe (Set)
    SetOps {
      "data" is (compiler, Nil :: SetType) implements getter(0, "_data")
      "put" is (infix, T :: MUnit, effect = write(0)) implements composite ${
        map_put[T, Boolean](data($self), $1, true)
      }
      "contains" is (infix, T :: MBoolean) implements composite ${
        map_getOrElse[T, Boolean](data($self), $1, false) match {
         case b: Boolean => b
          case r: Rep[Boolean] => r
        }
      }
    }
  }
}

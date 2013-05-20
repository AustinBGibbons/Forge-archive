package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}
//import templates.Utilities.nl

trait SetOps extends ForgeApplication with ScalaOps {
  def addSetOps() = {
    addScalaOps()

    val T = tpePar("T")
    val XSet = tpe("XSet", T) 
    // What should MBoolean be - MInt for multiset?
    val SetType = tpeInst(MMap, List(T, MBoolean))
    data(XSet, ("_data", SetType))

    static (XSet) ("apply", T, Nil :: XSet(T), effect = mutable) implements allocates(XSet, ${map_empty[T, Boolean]()})
    static (XSet) ("empty_set", T, Nil :: XSet(T), effect = mutable) implements allocates(XSet, ${map_empty[T, Boolean]()})

    val SetOps = withTpe (XSet)
    SetOps {
      compiler ("data") (Nil :: SetType) implements getter(0, "_data")
      infix ("put") (T :: MUnit, effect = write(0)) implements composite ${
        map_put[T, Boolean](data($self), $1, true)
      }
      infix ("remove") (T :: MUnit, effect = write(0)) implements composite ${
        map_put[T, Boolean](data($self), $1, false)
      }
      infix ("contains") (T :: MBoolean) implements composite ${
        map_getOrElse[T, Boolean](data($self), $1, false) match {
          case b: Boolean => b
          case r: Rep[Boolean] => r
        }
      }
    }
  }
}

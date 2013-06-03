package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

// This object lets us build our DSL
object MapTestDSLRunner extends ForgeApplicationRunner with MapTestDSL

trait MapTestDSL extends ForgeApplication {
  def dslName = "MapTest"
    
  def specification() = {
    importScalaOps()
        
    importMapTestOps()
  }
    
  def importMapTestOps() {
    val MultiSet = tpe("MultiSet")

    val MFI = tpeInst(MMap, List(MFloat, MInt))

    data(MultiSet, ("_map", MFI))

    static (MultiSet) ("apply", Nil, Nil :: MultiSet, effect = mutable) implements allocates (MultiSet, ${
      map_empty[Float, Int]()
    })

    val MultiSetOps = withTpe (MultiSet)
    MultiSetOps {
      compiler ("getMap") (Nil :: MFI) implements getter (0, "_map")
      compiler ("setMap") (MFI :: MUnit, effect=write(0)) implements setter(0, "_map", ${$1})
  
      infix ("getOrElseWrapper") ((MFI, MFloat) :: MInt) implements composite ${
        map_getOrElse($1, $2, 0) match {
          case x: Int => x
          case x: Rep[Int] => x
        }
      }

      infix ("add") (MFloat :: MUnit, effect=write(0)) implements composite ${
        val m = getMap($self)
        map_put(m, $1, forge_int_plus($self.getOrElseWrapper(m, $1), 1))
      }

      infix ("remove") (MFloat :: MInt, effect=write(0)) implements composite ${
        val m = getMap($self)
        val curr = $self.count($1)
        if(curr == 1) {
          map_remove(m, $1)
          0
        }
        else if (curr == 0) {
          0
        }
        else {
          map_put(m, $1, forge_int_minus(curr, 1))
          forge_int_minus(curr, 1)
        }
      }

      infix ("contains") (MFloat :: MBoolean) implements composite ${
        map_contains(getMap($self), $1)
      }

      infix ("count") (MFloat :: MInt) implements composite ${
        map_getOrElse(getMap($self), $1, 0) match {
          case x: Int => x
          case x: Rep[Int] => x
        }
      }
    }

    ()
  }
}
 

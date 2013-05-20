package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object UserSetDSLRunner extends ForgeApplicationRunner with UserSetDSL

trait UserSetDSL /*extends ForgeApplication with ScalaOps*/ extends SetOps {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "UserSet"
  
  /**
   * The specification is the DSL definition (types, data structures, ops)
   */
  def specification() = {
    //addScalaOps() 
    addSetOps() 
    addUserSetOps()
  }
  
  def addUserSetOps() {            
    // generic type parameters we will use 
    val T = tpePar("T") 
    val UserSet = tpe("UserSet", T) 
  
    /* From Set */
    val XSet = tpe("XSet", T)
    //val SetType = tpeInst(MMap, List(T, MBoolean))

    // data fields     
    data(UserSet, ("_data", XSet))      
  
    // allocation
    //op (UserSet) ("apply", static, T, Nil :: UserSet(T), effect = mutable) implements allocates(UserSet, /*${$0},*/ ${ empty_set() })
    op (UserSet) ("apply", static, T, Nil :: UserSet(T), effect = mutable) implements allocates (UserSet, ${XSet[T]()})
    
    // doesn't rewrite correctly if we use "withTpe (UserSet) {", but works if we use:
    val UserSetOps = withTpe (UserSet)
          
    UserSetOps {
      //"apply" is (static, T, Nil :: UserSet(T), effect = mutable) implements single ${Set[T]()}
      //"userset_data" is (compiler, Nil :: Set) implements getter (0, "_data")
      "userContains" is (infix, T :: MBoolean) implements composite ${
        //userset_data($self).contains($1)
        false
/*
        map_getOrElse(userset_data($self), $1, false) match {
         case b: Boolean => b
          case r: Rep[Boolean] => r
        }
*/
        //false
      }
      /*
      "test" is (infix, Nil :: MInt) implements composite ${
        1
      }
      */
    }                    

    ()    
  }
}
 

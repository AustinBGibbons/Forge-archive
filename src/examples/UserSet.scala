package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object UserSetDSLRunner extends ForgeApplicationRunner with UserSetDSL

trait UserSetDSL /*extends ForgeApplication with ScalaOps*/ extends SetOps {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "UserSetDSL"
  
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
    val XSet = lookupTpe("XSet")
    //val SetType = tpeInst(MMap, List(T, MBoolean))

    // data fields     
    data(UserSet, ("_data", XSet(T)))      
  
    // allocation
    //op (UserSet) ("apply", static, T, Nil :: UserSet(T), effect = mutable) implements allocates(UserSet, ${ XSet[T]() })
    val empty_set = lookup("XSet", "empty_set")
    static (UserSet) ("apply", T, Nil :: UserSet(T), effect = mutable) implements allocates (UserSet, ${XSet[T]()})
    static (UserSet) ("apply", T, XSet(T) :: UserSet(T), effect = mutable) implements allocates (UserSet, ${$0})
    
    // doesn't rewrite correctly if we use "withTpe (UserSet) {", but works if we use:
    val UserSetOps = withTpe (UserSet)
          
    UserSetOps {
      //"apply" is (static, T, Nil :: UserSet(T), effect = mutable) implements single ${Set[T]()}
      compiler ("userset_data") (Nil :: XSet) implements getter (0, "_data")
      compiler ("userset_set_data") (XSet :: MUnit, effect=write(0)) implements setter (0, "_data", quotedArg(1))
      infix ("userContains") (T :: MBoolean) implements composite ${
        userset_data($self).contains($1)
      }

      infix ("emptySet") (Nil :: XSet(T)) implements composite ${
        userset_set_data($self, XSet[T]())
        userset_data($self)
      }
      //static ("apply") (Nil :: UserSet(T)) implements allocates (UserSet, ${XSet[T]()})
    }                    

    ()    
  }
}
 

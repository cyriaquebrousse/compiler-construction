package toolc
package analyzer

import Symbols._

object Types {
    trait Typed {
        self =>

        private var _tpe: Type = TUntyped

        def setType(tpe: Type): self.type = { _tpe = tpe; this }
        def getType: Type = _tpe
    }

    sealed abstract class Type {
        def isSubTypeOf(tpe: Type): Boolean
    }

    case object TError extends Type {
        override def isSubTypeOf(tpe: Type): Boolean = true
        override def toString = "[error]"
    }

    case object TUntyped extends Type {
        override def isSubTypeOf(tpe: Type): Boolean = false
        override def toString = "[untyped]"
    }

    // Primitive types 
    
    case object TInt extends Type {
        override def isSubTypeOf(tpe: Type): Boolean = tpe match {
            case TInt => true
            case _ => false
        }
        override def toString = "int"
    }
    
    case object TBoolean extends Type {
        override def isSubTypeOf(tpe: Type): Boolean = tpe match {
            case TBoolean => true
            case _ => false
        }
        override def toString = "boolean"
    }
    
    case object TString extends Type {
        override def isSubTypeOf(tpe: Type): Boolean = tpe match {
            case TString => true
            case _ => false
        }
        override def toString = "String"
    }
    
    case object TIntArray extends Type {
        override def isSubTypeOf(tpe: Type): Boolean = tpe match {
            case TIntArray => true
            case _ => false
        }
        override def toString = "int[]"
    }

    // Object types

    case class TObject(classSymbol: ClassSymbol) extends Type {
        def isChildOf(expected: ClassSymbol) : Boolean = {
            def loop(cs: Option[ClassSymbol], expected: ClassSymbol) : Boolean = cs match {
	            case Some(currentClass) => currentClass.equals(expected) || loop(currentClass.parent, expected)
	            case None => false
	        }
            loop(Some(classSymbol), expected)
        }
        override def isSubTypeOf(tpe: Type): Boolean = tpe match {
            case TObject(cs) => this isChildOf cs
            case TAnyObject => true
            case _ => false
        }
        override def toString = classSymbol.name
    }

    // special object to implement the fact that all objects are its subclasses
    case object TAnyObject extends Type {
        override def isSubTypeOf(tpe: Type): Boolean = tpe match {
            case TAnyObject => true
            case _ => false
        }
        override def toString = "Object"
    }
}

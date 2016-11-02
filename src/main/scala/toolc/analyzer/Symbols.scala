package toolc
package analyzer

import utils._
import ast.Trees.Tree
import Types._

object Symbols {
    
    trait Symbolic[S <: Symbol] {
        private var _sym: Option[S] = None

        def setSymbol(sym: S): this.type = {
            _sym = Some(sym)
            this
        }

        def getSymbol: S = _sym match {
            case Some(s) => s
            case None => sys.error("Accessing undefined symbol.")
        }
    }

    sealed abstract class Symbol extends Positioned with Typed {
        val id: Int = ID.next
        val name: String
        var used: Boolean = false
        
        override def toString = "#"+id
    }

    private object ID {
        private var c: Int = 0

        def next: Int = {
            val ret = c
            c = c + 1
            ret
        }
    }

    class GlobalScope {
        var mainClass: ClassSymbol = _
        var classes = Map[String, ClassSymbol]()

        def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)
        
        def add(key: String, value: ClassSymbol)(implicit ctx: Context): Unit = {
            if (classes contains key)
                ctx.reporter.fatal("class "+key+" has already been declared", value)
            else if (mainClass.name.equals(key))
                ctx.reporter.fatal("class "+key+" has the same name as the main object", value)
            else
            	classes += (key -> value)
        }
    }

    class ClassSymbol(val name: String) extends Symbol {
        var parent: Option[ClassSymbol] = None
        var methods = Map[String, MethodSymbol]()
        var members = Map[String, VariableSymbol]()

        def lookupMethod(n: String): Option[MethodSymbol] = methods.get(n) match {
            case Some(m) => Some(m)
            case None => parent match {
                case Some(p) => p.lookupMethod(n)
                case None => None
            }
        }
        def lookupVar(n: String): Option[VariableSymbol] = members.get(n) match {
            case Some(v) => Some(v)
            case None => parent match {
                case Some(p) => p.lookupVar(n)
                case None => None
            }
        }
        
        
        def add(key: String, value: MethodSymbol)(implicit ctx: Context): Unit = {
            if (methods contains key)
                ctx.reporter.fatal("methods "+key+" has already been declared in class "+name, value)
            else
            	methods += (key -> value)
        }
        def add(key: String, value: VariableSymbol)(implicit ctx: Context): Unit = {
            if (members contains key)
                ctx.reporter.fatal("variable "+key+" has already been declared in class "+name, value)
            else
            	members += (key -> value)
        }
    }

    class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
        var params = Map[String, VariableSymbol]()
        var members = Map[String, VariableSymbol]()
        var argList: List[VariableSymbol] = Nil
        var overridden: Option[MethodSymbol] = None

        def lookupVar(n: String)(implicit ctx: Context): Option[VariableSymbol] = (params.get(n), members.get(n)) match {
            case (Some(p), Some(m)) => ctx.reporter.fatal("A member shadows a parameter in method "+name, p)
            case (Some(param), None) => Some(param)
            case (None, Some(member)) => Some(member)
            case (None, None) => overridden match {
                case Some(ms) => ms.lookupVar(n) match {
                    case Some(vs) => Some(vs)
                    case None => classSymbol.lookupVar(n)
                }
                case None => classSymbol.lookupVar(n)
            }
        }
        
        def addParam(key: String, value: VariableSymbol)(implicit ctx: Context): Unit = {
            if (params contains key)
                ctx.reporter.error("parameter "+key+" has already been declared in method "+name, value)
            else {
            	params += (key -> value)
            	argList = argList ::: List(value)
            }
        }
        def addMember(key: String, value: VariableSymbol)(implicit ctx: Context): Unit = {
            if (members contains key)
                ctx.reporter.error("variable "+key+" has already been declared in method "+name, value)
            else
            	members += (key -> value)
        }
        
    }

    class VariableSymbol(val name: String) extends Symbol
    
}

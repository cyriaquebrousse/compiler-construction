package toolc
package eval

import ast.Trees._
import utils._

class Evaluator(ctx: Context, prog: Program) {
    import ctx.reporter._

    def eval() {
        // Initialize the context for the main method
        val ectx = new MainMethodContext

        // Evaluate each statement of the main method
        prog.main.stats.foreach(evalStatement(ectx, _))
    }

    def evalStatement(ectx: EvaluationContext, stmt: StatTree): Unit = stmt match {
        case Block(stats) => ectx match {
            case m:MethodContext =>
                stats.foreach(s => if (!m.returnStatementReached) evalStatement(ectx, s))
            case _ =>
	            stats.foreach(evalStatement(ectx, _))
        }
        
        case If(expr, thn, els) =>
            if (evalExpr(ectx, expr).asBool) evalStatement(ectx, thn)
            else els match {
                case Some(s) => evalStatement(ectx, s)
                case None =>
            }
            
        case While(expr, stat) =>
            while (evalExpr(ectx, expr).asBool)
                evalStatement(ectx, stat)

        case Println(expr) =>
            println(evalExpr(ectx, expr).toString())

        case Assign(id, expr) =>
            ectx.setVariable(id.value, evalExpr(ectx, expr))

        case ArrayAssign(id, index, expr) =>
            val indexv = evalExpr(ectx, index).asInt
            val value = evalExpr(ectx, expr).asInt
            ectx.getVariable(id.value).asArray.setIndex(indexv, value)
            
        case Return(retExpr) => ectx match {
            case m:MethodContext =>
                m.returnStatementReached = true
                m.returnValue = evalExpr(ectx, retExpr)
            case _ =>
	            fatal("No return statement allowed in main method", stmt)
        	}
            

        case _ =>
            fatal("unnexpected statement", stmt)
    }

    def evalExpr(ectx: EvaluationContext, e: ExprTree): Value = e match {
        case IntLit(value) => IntValue(value)
        case StringLit(value) => StringValue(value)
        case True() => BoolValue(true)
        case False() => BoolValue(false)

        case And(lhs, rhs) =>
            val res = evalExpr(ectx, lhs).asBool match {
                case false => false
                case _ => evalExpr(ectx, rhs).asBool
            }
            BoolValue(res)

        case Or(lhs, rhs) =>
            val res = evalExpr(ectx, lhs).asBool match {
                case true => true
                case _ => evalExpr(ectx, rhs).asBool
            }
            BoolValue(res)

        case Plus(lhs, rhs) =>
            val lv = evalExpr(ectx, lhs)
            val rv = evalExpr(ectx, rhs)
            (lv, rv) match {
                case (IntValue(a), IntValue(b)) => IntValue(a + b)
                case (a, b) => StringValue(a.toString() + b.toString())
                // See redefinition of method toString() below
            }

        case Minus(lhs, rhs) =>
            val lv = evalExpr(ectx, lhs)
            val rv = evalExpr(ectx, rhs)
            val res = lv.asInt - rv.asInt
            IntValue(res)

        case Times(lhs, rhs) =>
            val lv = evalExpr(ectx, lhs)
            val rv = evalExpr(ectx, rhs)
            val res = lv.asInt * rv.asInt
            IntValue(res)

        case Div(lhs, rhs) =>
            val lv = evalExpr(ectx, lhs)
            val rv = evalExpr(ectx, rhs)
            val res = lv.asInt / rv.asInt
            IntValue(res)

        case LessThan(lhs, rhs) =>
            val lv = evalExpr(ectx, lhs)
            val rv = evalExpr(ectx, rhs)
            val res = lv.asInt < rv.asInt
            BoolValue(res)

        case Not(expr) =>
            val ev = evalExpr(ectx, expr)
            val res = !ev.asBool
            BoolValue(res)

        case Equals(lhs, rhs) =>
            val lv = evalExpr(ectx, lhs)
            val rv = evalExpr(ectx, rhs)
            val res = (lv, rv) match {
                case (IntValue(l), IntValue(r)) => l == r
                case (BoolValue(l), BoolValue(r)) => l == r
                case (lr, rr) => lr eq rr
            }
            BoolValue(res)

        case ArrayRead(arr, index) =>
            val arv = evalExpr(ectx, arr)
            val indexv = evalExpr(ectx, index)
            val res = arv.asArray.getIndex(indexv.asInt)
            IntValue(res)

        case ArrayLength(arr) =>
            val arv = evalExpr(ectx, arr)
            val res = arv.asArray.size
            IntValue(res)

        case MethodCall(obj, meth, args) =>
            val objv = evalExpr(ectx, obj).asObject
            val argsv = args map { evalExpr(ectx, _) }
            val methv = meth.value
            val methDecl = findMethod(objv.cd, methv)
            val methCtx = new MethodContext(objv)
            val arguments = methDecl.args zip argsv

            methDecl.vars foreach { n => methCtx.declareVariable(n.id.value) }
            methDecl.args foreach { n => methCtx.declareVariable(n.id.value) }
            arguments foreach { case (n, v) => methCtx.setVariable(n.id.value, v) }

            methDecl.stats foreach (s => if (!methCtx.returnStatementReached) evalStatement(methCtx, s))
            val retValue = methCtx.returnValue
            retValue match {
                case Undefined => fatal("No return statement in method call")
                case _ => retValue
            }

        case Identifier(name) =>
            ectx.getVariable(name)

        case New(tpe) =>
            val cd = findClass(tpe.value)
            val obj = new ObjectValue(cd)
            fieldsOfClass(cd) foreach (obj.declareField(_))
            obj

        case This() =>
            ectx match {
                case ctx: MethodContext => ctx.obj
                case _ => fatal("Calling \"This()\" in static method")
            }

        case NewIntArray(size) =>
            val s = evalExpr(ectx, size).asInt
            ArrayValue(new Array[Int](s), s)
    }

    // Define the scope of evaluation, with methods to access/declare/set local variables(or arguments)
    abstract class EvaluationContext {
        def getVariable(name: String): Value
        def setVariable(name: String, v: Value): Unit
        def declareVariable(name: String): Unit
    }

    // A Method context consists of the execution context within an object method.
    // getVariable can fallback to the fields of the current object
    class MethodContext(val obj: ObjectValue) extends EvaluationContext {
        var vars = Map[String, Option[Value]]()
        var returnStatementReached = false
        var returnValue : Value = Undefined

        def getVariable(name: String): Value = {
            vars.get(name) match {
                case Some(ov) =>
                    ov.getOrElse(fatal("Uninitialized variable '" + name + "'"))
                case _ =>
                    obj.getField(name)
            }
        }

        def setVariable(name: String, v: Value) {
            if (vars contains name) {
                vars += name -> Some(v)
            } else {
                obj.setField(name, v)
            }
        }

        def declareVariable(name: String) {
            vars += name -> None
        }
        
    }

    // Special execution context for the main method, which is very limitted.
    class MainMethodContext extends EvaluationContext {
        def getVariable(name: String): Value = fatal("The main method contains no variable and/or field")
        def setVariable(name: String, v: Value): Unit = fatal("The main method contains no variable and/or field")
        def declareVariable(name: String): Unit = fatal("The main method contains no variable and/or field")
    }

    // Helper functions to query the current program
    def findMethod(cd: ClassDecl, name: String): MethodDecl = {
        cd.methods.find(_.id.value == name).orElse(
            cd.parent.map(p => findMethod(findClass(p.value), name))).getOrElse(fatal("Unknown method " + cd.id + "." + name))
    }

    def findClass(name: String): ClassDecl = {
        prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '" + name + "'"))
    }

    def fieldsOfClass(cl: ClassDecl): Set[String] = {
        cl.vars.map(_.id.value).toSet ++
            cl.parent.map(p => fieldsOfClass(findClass(p.value))).getOrElse(Set())
    }

    // Runtime evaluation values, with as* methods which act as typecasts for convenience.
    sealed abstract class Value {
        def asInt: Int = fatal("Unnexpected value, found " + this + " expected Int")
        def asString: String = fatal("Unnexpected value, found " + this + " expected String")
        def asBool: Boolean = fatal("Unnexpected value, found " + this + " expected Boolean")
        def asObject: ObjectValue = fatal("Unnexpected value, found " + this + " expected Object")
        def asArray: ArrayValue = fatal("Unnexpected value, found " + this + " expected Array")
    }

    case class ObjectValue(cd: ClassDecl) extends Value {
        var fields = Map[String, Option[Value]]()

        def setField(name: String, v: Value) {
            if (fields contains name) {
                fields += name -> Some(v)
            } else {
                fatal("Unknown field '" + name + "'")
            }
        }

        def getField(name: String) = {
            fields.get(name).flatten.getOrElse(fatal("Unknown field '" + name + "'"))
        }

        def declareField(name: String) {
            fields += name -> None
        }

        override def asObject = this
    }

    case class ArrayValue(var entries: Array[Int], val size: Int) extends Value {
        def setIndex(i: Int, v: Int) {
            if (i >= size || i < 0) {
                fatal("Index '" + i + "' out of bounds (0 .. " + size + ")")
            }
            entries(i) = v
        }

        def getIndex(i: Int) = {
            if (i >= size || i < 0) {
                fatal("Index '" + i + "' out of bounds (0 .. " + size + ")")
            }
            entries(i)
        }

        override def asArray = this
        override def toString = entries.toString()
    }

    case class StringValue(var v: String) extends Value {
        override def asString = v
        override def toString = v
    }

    case class IntValue(var v: Int) extends Value {
        override def asInt = v
        override def toString = v.toString
    }

    case class BoolValue(var v: Boolean) extends Value {
        override def asBool = v
        override def toString = v.toString
    }
    
    object Undefined extends Value
}


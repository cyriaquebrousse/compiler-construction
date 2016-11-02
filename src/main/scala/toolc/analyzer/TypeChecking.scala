package toolc
package analyzer

import ast.Trees._
import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {
    /**
     * Typechecking does not produce a value, but has the side effect of
     * attaching types to trees and potentially outputting error messages.
     */
    def run(ctx: Context)(prog: Program): Program = {
        import ctx.reporter._

        def tcExpr(expr: ExprTree, expected: Type*): Type = {
            val tpe: Type = expr match {

                case And(lhs, rhs) =>
                    tcExpr(lhs, TBoolean)
                    tcExpr(rhs, TBoolean)
                    TBoolean

                case Or(lhs, rhs) =>
                    tcExpr(lhs, TBoolean)
                    tcExpr(rhs, TBoolean)
                    TBoolean

                case Plus(lhs, rhs) =>
                    val left = tcExpr(lhs, TInt, TString)
                    val right = tcExpr(rhs, TInt, TString)
                    (left, right) match {
                        case (TInt, TInt) => TInt
                        case (TInt, TString) => TString
                        case (TString, TInt) => TString
                        case (TString, TString) => TString
                        case _ => TError
                    }

                case Minus(lhs, rhs) =>
                    tcExpr(lhs, TInt)
                    tcExpr(rhs, TInt)
                    TInt

                case Times(lhs, rhs) =>
                    tcExpr(lhs, TInt)
                    tcExpr(rhs, TInt)
                    TInt

                case Div(lhs, rhs) =>
                    tcExpr(lhs, TInt)
                    tcExpr(rhs, TInt)
                    TInt

                case LessThan(lhs, rhs) =>
                    tcExpr(lhs, TInt)
                    tcExpr(rhs, TInt)
                    TBoolean

                case Equals(lhs, rhs) =>
                    val left = tcExpr(lhs)
                    val right = tcExpr(rhs)
                    (left, right) match {
                        case (a, b) if a.isSubTypeOf(TAnyObject) && b.isSubTypeOf(TAnyObject) =>
                        case (TInt, TInt) =>
                        case (TIntArray, TIntArray) =>
                        case (TString, TString) =>
                        case (TBoolean, TBoolean) =>

                        case _ =>
                            error("cannot compare type " + left + " and " + right, expr)
                    }
                    TBoolean

                case Not(expr) =>
                    tcExpr(expr, TBoolean)
                    TBoolean

                case ArrayRead(arr, index) =>
                    tcExpr(index, TInt)
                    tcExpr(arr, TIntArray)
                    TInt

                case ArrayLength(arr) =>
                    tcExpr(arr, TIntArray)
                    TInt

                case MethodCall(obj, id, args) =>
                    tcExpr(obj, TAnyObject) match {
                        case TObject(cs) => cs.lookupMethod(id.value) match {
                            case Some(ms) =>
                                if (ms.argList.size == args.size) {
                                    ms.argList zip args foreach {
                                        case (p, a) =>
                                            val tpe = tcExpr(a)
                                            if (!tpe.isSubTypeOf(p.getType))
                                                error("Type error: Expected: " + p.getType + ", found: " + tpe, expr)
                                    }

                                } else {
                                    error("wrong number of arguments given to method " + id.value, expr)
                                }
                                id.setSymbol(ms)
                                ms.used = true
                                ms.getType
                            case None =>
                                error("method " + id.value + " not found in class " + cs.name, expr)
                                TError
                        }
                        case _ => TError
                    }

                case NewIntArray(size) =>
                    tcExpr(size, TInt)
                    TIntArray

                case New(id) =>
                    tcExpr(id)

                case id: Identifier =>
                    id.getSymbol.getType

                case IntLit(value) =>
                    TInt

                case StringLit(value) =>
                    TString

                case True() =>
                    TBoolean

                case False() =>
                    TBoolean

                case id: This =>
                    id.getSymbol.getType

            }

            // Check result and return a valid type in case of error
            val exprType =
                if (expected.isEmpty) {
                    tpe
                } else {
                    if (!expected.exists(e => tpe.isSubTypeOf(e))) {
                        error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
                        expected.head
                    } else {
                        tpe
                    }
                }

            // Keeping the type in memory
            expr.setType(exprType)
            exprType
        }

        def tcStat(stat: StatTree)(implicit scope : Symbol): Unit = stat match {
            case Block(stats) =>
                stats foreach tcStat

            case If(expr, thn, els) =>
                tcExpr(expr, TBoolean)
                tcStat(thn)
                els match {
                    case Some(s) => tcStat(s)
                    case None =>
                }

            case While(expr, stat) =>
                tcExpr(expr, TBoolean)
                tcStat(stat)

            case Println(expr) =>
                tcExpr(expr, TString, TBoolean, TInt)

            case Assign(id, expr) =>
                tcExpr(expr, id.getType)

            case ArrayAssign(id, index, expr) =>
                tcExpr(id, TIntArray)
                tcExpr(index, TInt)
                tcExpr(expr, TInt)
                
            case Return(retExpr) =>
                tcExpr(retExpr, scope.getType)
        }

        def typeCheck(t: Tree): Unit = t match {
            case Program(main, classes) =>
                typeCheck(main)
                classes foreach typeCheck

            case m:MainObject =>
                implicit val scope = m.getSymbol;
                tcExpr(m.id)
                m.stats foreach tcStat
                
            case ClassDecl(id, parent, vars, methods) =>
                tcExpr(id)
                parent match {
                    case Some(p) => tcExpr(p)
                    case None =>
                }
                vars foreach typeCheck
                methods foreach typeCheck
                
            case m:MethodDecl =>
                tcExpr(m.id)
                implicit val scope = m.getSymbol;
                m.args foreach typeCheck
                m.vars foreach typeCheck
                m.stats foreach tcStat

            case VarDecl(tpe, id) =>
                tcExpr(id)
                
            case Formal(tpe, id) =>
                tcExpr(id)
            case _ =>
        }        
        
        def warnIfUnused(s: Symbol): Unit = if (!s.used) warning(s.name + " declared but not used", s)


        // Type Checking
        typeCheck(prog)
        
        // Warn unused method
        prog.classes map (_.getSymbol) foreach (_.methods.values foreach warnIfUnused)
        
        prog
    }
}

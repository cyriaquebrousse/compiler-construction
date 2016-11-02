package toolc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Pipeline[Program, Program] {

    def run(ctx: Context)(prog: Program): Program = {
        import ctx.reporter._

        implicit val context = ctx

        val globalScope = new GlobalScope

        //----------------------//
        //--------STEP-1--------//
        //----------------------//

        def collectGlobal(t: Tree): Unit = t match {
            case Program(main, classes) =>
                collectGlobal(main)
                classes foreach collectGlobal
                collectInheritance(classes)

            case MainObject(id, stats) =>
                val mainClassSymbol = new ClassSymbol(id.value).setPos(t).setType(TAnyObject)

                // Set Symbolics
                t.asInstanceOf[MainObject].setSymbol(mainClassSymbol)
                id.setSymbol(mainClassSymbol)

                // Update scope
                globalScope.mainClass = mainClassSymbol

            case ClassDecl(id, parent, vars, methods) =>
                val classSymbol = new ClassSymbol(id.value).setPos(t)
                
                // Set Type
                val tpe = new TObject(classSymbol)
                classSymbol.setType(tpe)

                // Set Symbolics
                t.asInstanceOf[ClassDecl].setSymbol(classSymbol)
                id.setSymbol(classSymbol)

                // Update scope
                globalScope.add(id.value, classSymbol)

            case _ =>
                sys.error("Tree that are not Program, MainObject or ClassDecl must be collected with a scope")

        }

        def collectInheritance(classes: List[ClassDecl]): Unit = {

            var alreadyExplored: Set[ClassSymbol] = Set.empty

            def explore(c: ClassDecl): Unit = {

                // Find parent
                c.getSymbol.parent = c.parent match {
                    case Some(idParent) =>
                        globalScope.lookupClass(idParent.value) match {
                            case Some(cs) =>
                                setSymbol(idParent, cs)
                                Some(cs)
                            case None => fatal("could not find parent " + idParent.value + " of class " + c.id.value, c)
                        }
                    case None => None
                }

                // Explore
                collectAll(c.vars, c.getSymbol)
                collectAll(c.methods, c.getSymbol)
            }

            def checkCircularInheritance(c: ClassSymbol, explored: Set[ClassSymbol]): Unit = {
                c.parent match {
                    case Some(parentClass) =>
                        if (explored contains parentClass) {
                            fatal("circular inheritance", c)
                        }
                        alreadyExplored += c
                        checkCircularInheritance(parentClass, explored + c)
                    case None =>
                }
            }

            classes foreach explore
            classes foreach {
                case c if alreadyExplored contains c.getSymbol =>
                case c => checkCircularInheritance(c.getSymbol, Set.empty)
            }

        }

        def collect(t: Tree, scope: Symbol): Unit = t match {
            case VarDecl(tpe, id) =>
                val variableSymbol = new VariableSymbol(id.value).setPos(t)

                // Set Type
                val varType = findType(tpe)
                variableSymbol.setType(varType)
                tpe.setType(varType)

                // Set Symbolics
                t.asInstanceOf[VarDecl].setSymbol(variableSymbol)
                id.setSymbol(variableSymbol)

                // Update scope
                scope match {
                    case c: ClassSymbol =>
                        c.add(id.value, variableSymbol)
                    case m: MethodSymbol =>
                        m.addMember(id.value, variableSymbol)
                    case _ =>
                        sys.error("VarDecl has scope ClassSymbol or MethodSymbol only")
                }

            case MethodDecl(retType, id, formals, vars, stats) => scope match {
                case c: ClassSymbol =>
                    val methodSymbol = new MethodSymbol(id.value, c).setPos(t)

                    // Set Type
                    val varType = findType(retType)
                    methodSymbol.setType(varType)
                    retType.setType(varType)

                    // Set Symbolics
                    t.asInstanceOf[MethodDecl].setSymbol(methodSymbol)
                    id.setSymbol(methodSymbol)

                    // Update scope
                    c.add(id.value, methodSymbol)

                    // next
                    collectAll(formals, methodSymbol)
                    collectAll(vars, methodSymbol)

                case _ =>
                    sys.error("MethodDecl has scope ClassSymbol only")
            }

            case Formal(tpe, id) => scope match {
                case m: MethodSymbol =>
                    val variableSymbol = new VariableSymbol(id.value).setPos(t)

                    // Set Type
                    val varType = findType(tpe)
                    variableSymbol.setType(varType)
                    tpe.setType(varType)

                    // Set Symbolics
                    t.asInstanceOf[Formal].setSymbol(variableSymbol)
                    id.setSymbol(variableSymbol)

                    // Update scope
                    m.addParam(id.value, variableSymbol)

                case _ =>
                    sys.error("Formal has scope MethodSymbol only")
            }

            case _ =>
                sys.error("Collecting should stop after analyzing classes and methods declarations")

        }

        def collectAll(list: List[Tree], scope: Symbol): Unit = list foreach (collect(_, scope))

        //----------------------//
        //--------STEP-2--------//
        //----------------------//

        def browseGlobal(t: Tree): Unit = t match {
            case Program(main, classes) =>
                browseGlobal(main)
                classes foreach browseGlobal

            case MainObject(id, stats) =>
                val mainClassSymbol = t.asInstanceOf[MainObject].getSymbol
                browseAll(stats, mainClassSymbol)

            case ClassDecl(id, parent, vars, methods) =>
                val classSymbol = t.asInstanceOf[ClassDecl].getSymbol

                // next
                browseAll(vars, classSymbol)
                browseAll(methods, classSymbol)

            case _ =>
                sys.error("Tree that are not Program, MainObject or ClassDecl must be browsed with a scope")

        }

        def browse(t: Tree, scope: Symbol): Unit = t match {
            case VarDecl(tpe, id) =>

                def findOverrides(v: VariableSymbol, c: Option[ClassSymbol]): Unit = c match {
                    case None =>
                    case Some(c) => c.lookupVar(v.name) match {
                        case Some(parentVariable) =>
                            fatal("overriding variables is not allowed", v)
                        case None => findOverrides(v, c.parent)
                    }
                }

                scope match {
                    case c: ClassSymbol =>
                        findOverrides(t.asInstanceOf[VarDecl].getSymbol, c.parent)
                    case _ =>
                }

            case Formal(tpe, id) =>

            case MethodDecl(retType, id, formals, vars, stats) =>

                def findOverrides(m: MethodSymbol, c: Option[ClassSymbol]): Option[MethodSymbol] = c match {
                    case None => None
                    case Some(c) => c.lookupMethod(m.name) match {
                        case Some(parentMethod) =>
                            if (m.params.size == parentMethod.params.size) {
                                if (m.getType equals parentMethod.getType) {
                                    val zip = m.argList.map(_.getType) zip parentMethod.argList.map(_.getType)
                                	if (zip forall { case(a,b) => a equals b } ) {
                                		Some(parentMethod)
                                	} else {
                                	    fatal("overriding a method with different parameter type", m)
                                	}
                                } else {
                                    fatal("overriding a method with different return type", m)
                                }
                            } else {
                                fatal("overriding a method with different number of parameters", m)
                            }
                        case None => findOverrides(m, c.parent)
                    }
                }

                val methodSymbol = t.asInstanceOf[MethodDecl].getSymbol

                methodSymbol.overridden = findOverrides(methodSymbol, methodSymbol.classSymbol.parent)

                browseAll(formals, methodSymbol)
                browseAll(vars, methodSymbol)

                browseAll(stats, methodSymbol)

            case Block(stats) =>
                browseAll(stats, scope)

            case If(expr, thn, els) =>
                browse(expr, scope)
                browse(thn, scope)
                els match {
                    case Some(elseStatement) => browse(elseStatement, scope)
                    case None =>
                }

            case While(expr, stat) =>
                browse(expr, scope)
                browse(stat, scope)

            case Println(expr) =>
                browse(expr, scope)

            case Assign(id, expr) =>
                browse(id, scope)
                browse(expr, scope)

            case ArrayAssign(id, index, expr) =>
                browse(id, scope)
                browse(index, scope)
                browse(expr, scope)
                
            case Return(retExpr) => scope match {
                case m:MethodSymbol =>
                	browse(retExpr, scope)
                case _ =>
                    fatal("Return statements are not allowed in main method", t)
            }

            case And(lhs, rhs) =>
                browse(lhs, scope); browse(rhs, scope)
            case Or(lhs, rhs) =>
                browse(lhs, scope); browse(rhs, scope)
            case Plus(lhs, rhs) =>
                browse(lhs, scope); browse(rhs, scope)
            case Minus(lhs, rhs) =>
                browse(lhs, scope); browse(rhs, scope)
            case Times(lhs, rhs) =>
                browse(lhs, scope); browse(rhs, scope)
            case Div(lhs, rhs) =>
                browse(lhs, scope); browse(rhs, scope)
            case LessThan(lhs, rhs) =>
                browse(lhs, scope); browse(rhs, scope)
            case Equals(lhs, rhs) =>
                browse(lhs, scope); browse(rhs, scope)
            case Not(expr) => browse(expr, scope)

            case ArrayRead(arr, index) =>
                browse(arr, scope); browse(index, scope)
            case ArrayLength(arr) => browse(arr, scope)

            case MethodCall(obj, id, args) =>
                // lab04 : ignore id in MethodCall (type checking needed - lab05)
                // setSymbol(id, UndefinedSymbol)

                browse(obj, scope)
                browseAll(args, scope)

            case NewIntArray(size) =>
                browse(size, scope)

            case New(id) => scope match {
                case m: MethodSymbol =>
                    setSymbolOrThrow(id, globalScope.lookupClass(id.value))
                case c: ClassSymbol if globalScope.mainClass == c =>
                    setSymbolOrThrow(id, globalScope.lookupClass(id.value))
                case _ => sys.error("Wrong scope")
            }

            case id: Identifier => scope match {
                case m: MethodSymbol =>
                    setSymbolOrThrow(id, m.lookupVar(id.value))
                case c: ClassSymbol if globalScope.mainClass == c =>
                    setSymbolOrThrow(id, c.lookupVar(id.value))
                case _ => sys.error("Wrong scope")
            }

            case IntLit(value) =>
            case StringLit(value) =>
            case True() =>
            case False() =>

            case This() => scope match {
                case m: MethodSymbol =>
                    setThisSymbol(t.asInstanceOf[This], m.classSymbol)
                case c: ClassSymbol =>
                    // Should not happen though (because no This in mainObject)
                    setThisSymbol(t.asInstanceOf[This], c)
                case _ =>
                    sys.error("This() cannot have a scope different than MethodSymbol or ClassSymbol")
            }

            case t:BooleanType =>
                t.setType(TBoolean)
            case t:StringType =>
                t.setType(TString)
            case t:IntType =>
                t.setType(TInt)
            case t:IntArrayType =>
                t.setType(TIntArray)

            case _ =>
                sys.error("Program, MainObject or ClassDecl are browsed without any scope")

        }

        def browseAll(list: List[Tree], scope: Symbol): Unit = list foreach (browse(_, scope))

        def setSymbolOrThrow(id: Identifier, symbol: Option[Symbol]): Unit = symbol match {
            case Some(s) => setSymbol(id, s)
            case None => fatal("identifier " + id.value + " has not been declared", id)
        }

        def setSymbol(id: Identifier, symbol: Symbol): Unit = {
            id.setSymbol(symbol)
            symbol.used = true
        }

        def setThisSymbol(t: This, symbol: ClassSymbol): Unit = {
            t.setSymbol(symbol)
            symbol.used = true
        }

        def setTypeIfIdentifier(tpe: TypeTree): Unit = tpe match {
            case id: Identifier =>
                setSymbolOrThrow(id, globalScope.lookupClass(id.value))
            case _ =>
        }

        def findType(t: TypeTree): Type = t match {
            case IntArrayType() => TIntArray
            case IntType() => TInt
            case BooleanType() => TBoolean
            case StringType() => TString
            case id: Identifier =>
                setSymbolOrThrow(id, globalScope.lookupClass(id.value))
                id.getSymbol.getType
        }

        //----------------------//
        //--------STEP-3--------//
        //----------------------//

        def checkUnusedSymbols(): Unit = {
            globalScope.classes.values foreach { c =>
                warnIfUnused(c)
                c.members.values foreach warnIfUnused
                c.methods.values foreach { m =>
                    // 'warnIfUnused(m)' needs lab05 : Type checking
                    m.members.values foreach warnIfUnused
                    m.params.values foreach warnIfUnused
                }
            }
        }
        

        //----------------------//
        //--------STEP-4--------//
        //----------------------//
        
        def checkReturnStatements(prog : Program): Unit = {
            prog.classes foreach { c =>
                c.methods foreach { m => 
                    val methodIsTerminal = checkTerminals(m.stats) 
                    if (!methodIsTerminal) 
                        fatal("The method misses some return statements", m)
                }
            }
        }
        
        def checkTerminals(stats: List[StatTree]) : Boolean = {
            var terminal = false
            
            stats foreach { s =>
                if (terminal)
                    fatal("Unreachable code", s)
                    
                if (isTerminal(s))
                	terminal = true;
            }
            terminal
        }
        
        def isTerminal(t: StatTree): Boolean = t match {
		    case Block(stats) =>
	            var terminal = false
	            stats foreach { s =>
	                if (terminal)
	                    fatal("Unreachable code", s)
	                if (isTerminal(s))
	                	terminal = true;
	            }
	            t.terminal = terminal
	            terminal
		        
		    case If(expr, thn, els) => els match {
		        case Some(e) => 
		            val b = isTerminal(thn) && isTerminal(e)
		            t.terminal = b
		            b
		        case None => false
		    }
		    
		    // final value
		    case _ => t.terminal
        }

        def warnIfUnused(s: Symbol): Unit = if (!s.used) warning(s.name + " declared but not used", s)

        // Step 1: Collect symbols in declarations
        collectGlobal(prog)

        // Step 2: Attach symbols to identifiers (except method calls) in method bodies
        browseGlobal(prog)

        // Step 3: Warn whenever a symbol is not used
        checkUnusedSymbols
        
        // Step 4: Check return statements
        checkReturnStatements(prog)

        prog
    }
}

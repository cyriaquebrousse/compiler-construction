package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{ New => _, _ }
import ByteCodes._
import utils._
import scala.collection.immutable.HashMap
import com.sun.org.apache.bcel.internal.generic.ISTORE

object CodeGeneration extends Pipeline[Program, Unit] {

    def run(ctx: Context)(prog: Program): Unit = {
        import ctx.reporter._
        
	    sealed class Slots(val ch: CodeHandler) {
            var slots: Map[String, Int] = Map.empty
	        
		    private object ID {
		        private var c: Int = 1
		
		        def next: Int = {
		            val ret = c
		            c = c + 1
		            ret
		        }
		    }
            
	        def slotFor(id: Identifier): Int = slots.getOrElse(id.value, fatal("Local variable not found",id))
	        def slotFor(id: String): Int = slots.getOrElse(id, fatal("Local variable not found"))
	        
	        def assignSlot(id: Identifier): Unit = { ch.getFreshVar; slots += (id.value -> ID.next) }
	        def assignSlot(id: String): Unit = { ch.getFreshVar; slots += (id -> ID.next) }
	    }

        /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
        def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {

            val classSym = ct.getSymbol
            val classFile = new cafebabe.ClassFile(classSym.name, ct.parent map (_.value))

            classFile.setSourceFile(classSym.name + ".java")
            classFile.addDefaultConstructor
            
            ct.vars foreach { v => 
                    classFile.addField(typeToSignature(v.tpe.getType), v.id.value) 
            }

            ct.methods foreach { mt =>
                val ch = classFile.addMethod(
                    typeToSignature(mt.retType.getType),
                    mt.getSymbol.name,
                    mt.args map { f => typeToSignature(f.tpe.getType) }).codeHandler
                generateMethodCode(ch, mt)
            }

            val directory = if (dir.equals("")) "./" else dir
            classFile.writeToFile(directory + ct.getSymbol.name + ".class")
        }

        // a mapping from variable symbols to positions in the local variables
        // of the stack frame
        def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
            val methSym = mt.getSymbol
            val slots = new Slots(ch)
            
            mt.args foreach (f => slots.assignSlot(f.id))
            mt.vars foreach (v => slots.assignSlot(v.id))

            // Emit code
            val startLabel = getFreshLabel(ch)
            ch << Label(startLabel)
            mt.stats foreach { compile(_)(ch, methSym, slots, startLabel) }
            
            ch.freeze
        }

        def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
        	val slots = new Slots(ch)
            
            // Emit code
            val startLabel = getFreshLabel(ch)
            ch << Label(startLabel)
            stmts foreach { compile(_)(ch, prog.main.getSymbol, slots, startLabel) }
        	ch << RETURN
        	
            ch.freeze
        }
        
        def typeToSignature(t: Type): String = t match {
            case TInt => "I"
            case TIntArray => "[I"
            case TBoolean => "Z"
            case TString => "Ljava/lang/String;"
            case TAnyObject => "Ljava/lang/Object;"

            // TODO : package ?
            case TObject(cs) => "L" + cs.name + ";"

            // TODO : fatal error ?
            case TError => ""
            case TUntyped => ""
        }
        
        def getFreshLabel(implicit ch: CodeHandler) = ch.getFreshLabel("cc_");
        
        def isFieldOfClass(id: Identifier, m: MethodSymbol) : Boolean = {
            val isInClass = ! m.classSymbol.lookupVar(id.value).isEmpty
            isInClass && !m.members.contains(id.value) && !m.params.contains(id.value)
        } 

        def compile(t: Tree)(implicit ch: CodeHandler, scope: Symbol, slots: Slots, startLabel : String): Unit = t match {
            
            case Block(stat) =>
                stat foreach compile
                
            case If(expr, thn, els) => els match {
                case None =>
                    val after = getFreshLabel
                    compile(expr)
                    ch << IfEq(after)
                    compile(thn)
                    ch << Label(after)
                
                case Some(e) =>
                    val elsebr = getFreshLabel
	                val after = getFreshLabel
	                compile(expr)
	                ch << IfEq(elsebr)
	                compile(thn)
	                if (thn.terminal) {
	                	ch << Label(elsebr)
	                	compile(e)
	                } else {
	                	ch << Goto(after)
	                	ch << Label(elsebr)
	                	compile(e)
	                	ch << Label(after)
	                }
            }
            
            case While(expr, stat) =>
                val before = getFreshLabel
                val after = getFreshLabel
                ch << Label(before)
                compile(expr)
                ch << IfEq(after)
                compile(stat)
                ch << Goto(before)
                ch << Label(after)
                
            case Println(expr) =>
                ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
                compile(expr)
                expr.getType match {
                    case TInt =>
                    	ch << InvokeVirtual("java/io/PrintStream", "println", "(I)V")
                    case TBoolean =>
                    	ch << InvokeVirtual("java/io/PrintStream", "println", "(Z)V")
                    case _ =>
                    	ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
                }
                
            case Assign(id, expr) => scope match {
                case m: MethodSymbol => 
                    if (isFieldOfClass(id,m)) {
                    	ch << ArgLoad(0)
	                    compile(expr)
	                    ch << PutField(m.classSymbol.name, id.value, typeToSignature(id.getType))
                    } else {
                    	compile(expr)
                    	id.getType match {
                    	    case TInt|TBoolean => 
                    	    	ch << IStore(slots.slotFor(id))
                    	    case _ =>
                    	    	ch << AStore(slots.slotFor(id))
                    	}
                    }
                    
                case c: ClassSymbol =>
                	compile(expr)
                	id.getType match {
                	    case TInt|TBoolean => 
                	    	ch << IStore(slots.slotFor(id))
                	    case _ =>
                	    	ch << AStore(slots.slotFor(id))
                	}
                	
                case _ =>
                    fatal("A variable cannot have a VariableSymbol as a scope",id)
            }
                
            case ArrayAssign(arr, index, value) =>
                compile(arr)
                compile(index)
                compile(value)
                ch << IASTORE
                
            case Return(retExpr) => scope match {
	                case m:MethodSymbol =>
	                    
		                def compileReturn() = {
				            compile(retExpr)
				            m.getType match {
				                case TInt|TBoolean => ch << IRETURN
				                case _ => ch << ARETURN
			                }
		                }
		                
		                retExpr match {
			                case MethodCall(obj, meth, args) => obj match {
			                    case This() =>
			                        if (meth.getSymbol equals m) {
			                            val argsAndNewValues = m.argList zip args
			                            argsAndNewValues foreach { case (v,e) =>
			                                compile(e)
					                    	v.getType match {
					                    	    case TInt|TBoolean => 
					                    	    	ch << IStore(slots.slotFor(v.name))
					                    	    case _ =>
					                    	    	ch << AStore(slots.slotFor(v.name))
					                    	}
			                            }
			                            ch << Goto(startLabel)
			                        } else {
			                            compileReturn
			                        }
			                    case _ => compileReturn
			                }
			                case _ => compileReturn
			            }
		                
	                case _ =>
	            }
            
            case And(lhs, rhs) =>
                val after = getFreshLabel
                ch << ICONST_0
                compile(lhs)
                ch << IfEq(after)
                ch << POP
                compile(rhs)
                ch << Label(after)
                
            case Or(lhs, rhs) =>
                val after = getFreshLabel
                ch << ICONST_1
                compile(lhs)
                ch << IfNe(after)
                ch << POP
                compile(rhs)
                ch << Label(after)
            
            case Plus(lhs, rhs) => (lhs.getType, rhs.getType) match {
                case (TInt, TInt) =>
	                compile(lhs)
	                compile(rhs)
	                ch << IADD
	                
                case _ =>
                    ch << DefaultNew("java/lang/StringBuilder")
	                compile(lhs)
	                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + typeToSignature(lhs.getType) + ")Ljava/lang/StringBuilder;")
	                compile(rhs)
	                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + typeToSignature(rhs.getType) + ")Ljava/lang/StringBuilder;")
	                ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")

            }
                
            case Minus(lhs, rhs) => 
                compile(lhs)
                compile(rhs)
                ch << ISUB
                
            case Times(lhs, rhs) =>
                compile(lhs)
                compile(rhs)
                ch << IMUL
                
            case Div(lhs, rhs) =>
                compile(lhs)
                compile(rhs)
                ch << IDIV
                
            case LessThan(lhs, rhs) =>
                val greaterEqBr = getFreshLabel
                val after = getFreshLabel
                ch << ICONST_1
                compile(lhs)
                compile(rhs)
                ch << If_ICmpLt(after)
                ch << POP
                ch << ICONST_0
                ch << Label(after)
            
            case Equals(lhs, rhs) => (lhs.getType, rhs.getType) match {
                case (TInt, TInt) | (TBoolean, TBoolean) =>
                    val after = getFreshLabel
                    ch << ICONST_1
                    compile(lhs)
                    compile(rhs)
                    ch << If_ICmpEq(after)
                    ch << POP
                    ch << ICONST_0
                    ch << Label(after)
                    
                case _ =>
                    val after = getFreshLabel
                    ch << ICONST_1
                    compile(lhs)
                    compile(rhs)
                    ch << If_ACmpEq(after)
                    ch << POP
                    ch << ICONST_0
                    ch << Label(after)
            }
            
            case ArrayRead(arr, index) =>
                compile(arr)
                compile(index)
                ch << IALOAD
            
            case ArrayLength(arr) => 
                compile(arr)
                ch << ARRAYLENGTH
            
            case MethodCall(obj, id, args) =>

                compile(obj)
                args foreach compile
                val ms = id.getSymbol.asInstanceOf[MethodSymbol]
                ch << InvokeVirtual(
                        ms.classSymbol.name, 
                        ms.name, 
                        "(" + (ms.argList map { f => typeToSignature(f.getType) } mkString "") + ")" + typeToSignature(ms.getType))
            
            case NewIntArray(size) =>
                compile(size)
                ch << NewArray(10)
            
            case New(id) =>
                ch << DefaultNew(id.value)
            
            case Not(expr) =>
                val after = getFreshLabel
                compile(expr)
                ch << ICONST_1
                ch << SWAP
                ch << IfEq(after)
                ch << POP
                ch << ICONST_0
                ch << Label(after)
            
            case id: Identifier => scope match {
                case m: MethodSymbol => 
                    if (isFieldOfClass(id,m)) {
	                    ch << ArgLoad(0)
	                    ch << GetField(m.classSymbol.name, id.value, typeToSignature(id.getType))
                    } else {
	                	id.getType match {
	                	    case TInt|TBoolean => 
	                	    	ch << ILoad(slots.slotFor(id))
	                	    case _ =>
	                	    	ch << ALoad(slots.slotFor(id))
	                	}
                    }
                case c: ClassSymbol =>
                	id.getType match {
                	    case TInt|TBoolean => 
                	    	ch << ILoad(slots.slotFor(id))
                	    case _ =>
                	    	ch << ALoad(slots.slotFor(id))
                	}
                	
                case _ =>
                    fatal("A variable cannot have a VariableSymbol as a scope",id)
            }
                
            case IntLit(value) =>
                ch << Ldc(value)
            
            case StringLit(value) =>
                ch << Ldc(value)
            
            case True() =>
                ch << ICONST_1
                
            case False() =>
                ch << ICONST_0
                
            case This() =>
                ch << ArgLoad(0)
            
            // Should never be trigger (only statements and expressions here)
            case _ => fatal("Only Statements and Expressions should emit code", t)
        }

        val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

        val f = new java.io.File(outDir)
        if (!f.exists()) {
            f.mkdir()
        }

        val sourceName = ctx.file.getName

        // output code
        prog.classes foreach {
            ct => generateClassFile(sourceName, ct, outDir)
        }

        // output main method
        val mainName = prog.main.id.value
        val mainFile = new cafebabe.ClassFile(mainName, None)

        mainFile.setSourceFile(mainName + ".java")
        mainFile.addDefaultConstructor
        val ch = mainFile.addMainMethod.codeHandler
        generateMainMethodCode(ch, prog.main.stats, mainName)

        mainFile.writeToFile(outDir + mainName + ".class")
    }

}

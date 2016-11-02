package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
    def run(ctx: Context)(tokens: Iterator[Token]): Program = {
        import ctx.reporter._

        // Store the current token, as read from the lexer.
        var currentToken: Token = new Token(BAD)
        
        
        //--------------//
        //---Provided---//
        //--------------//

        def readToken: Unit = {
            if (tokens.hasNext) {
                currentToken = tokens.next

                // skips bad tokens
                while (currentToken.kind == BAD && tokens.hasNext) {
                    currentToken = tokens.next
                }
            }
        }

        // ''Eats'' the expected token, or terminates with an error.
        def eat(kind: TokenKind): Unit = {
            if (currentToken.kind == kind) {
                readToken
            } else {
                expected(kind)
            }
        }

        // Complains that what was found was not expected.
        def expected(kind: TokenKind, more: TokenKind*): Nothing = {
            fatal("expected: " + (kind :: more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
        }
        
        //-------------//
        //---Helpers---//
        //-------------//
        
            
        // isFirst
        def isFirstOfStatement(first: Token) : Boolean = Set(LBRACE, IF, WHILE, PRINTLN, IDKIND, RETURN).contains(first.kind)
        def isFirstOfClass(first: Token) : Boolean = Set[TokenKind](CLASS).contains(first.kind)
        def isFirstOfVarDecl(first: Token) : Boolean = Set[TokenKind](VAR).contains(first.kind)
        def isFirstOfMethDecl(first: Token) : Boolean = Set[TokenKind](DEF).contains(first.kind)
            
        // getters
        def getStatements() : List[StatTree] = construct(isFirstOfStatement, statmt)
        def getVarDeclarations() : List[VarDecl] = construct(isFirstOfVarDecl, varDecl)
        def getMethodDeclarations() : List[MethodDecl] = construct(isFirstOfMethDecl, methodDecl)
        def getClasses() : List[ClassDecl] = construct(isFirstOfClass, classDecl)
        
        def getExpressions() : List[ExprTree] = constructInParenthesis(expr)
        def getFormals() : List[Formal] = constructInParenthesis(getFormal)
        
        def construct[A](isFirstOfA : (Token => Boolean), getA : () => A) : List[A] = {
	        def loop(list: List[A]) : List[A] = currentToken match {
				case x if !isFirstOfA(x) => list
				case _ => loop(getA() :: list)
			}
            loop(Nil).reverse
        }
        
        def constructInParenthesis[A](getA : () => A) : List[A] = {
            def endOfParenthesis(t:Token) : Boolean = t.kind == RPAREN
            
	        def loop(list: List[A]) : List[A] = currentToken match {
				case x if endOfParenthesis(x) =>
				    list
				case _ => list match {
				    case Nil =>
					    loop(getA() :: list)
				    case _ =>
				        eat(COMMA)
					    loop(getA() :: list)
				}
			}
            loop(Nil).reverse
        }
        
        
        def getIdentifier() : Identifier = {
            val token = currentToken
            if (token.kind == IDKIND) {
                readToken
                Identifier(token.asInstanceOf[ID].value).setPos(token)
            } else {
                expected(IDKIND)
            }
        }
        
        def getType() : TypeTree = {
            val token = currentToken
            currentToken.kind match {
	            case INT =>
	                readToken
	                if (currentToken.kind == LBRACKET) {
	                    eat(LBRACKET)
	                    eat(RBRACKET)
	                    IntArrayType().setPos(token)
	                } else {
	                    IntType().setPos(token)
	                }
	            
	            case IDKIND =>
	                getIdentifier
	                
	            case BOOLEAN =>
	                readToken
	                BooleanType().setPos(token)
	                
	            case STRING =>
	                readToken 
	                StringType().setPos(token)
	                
	            case _ => 
	                expected(INT, IDKIND, BOOLEAN, STRING)
            }
        }
        
        def getFormal() : Formal = {
            val id = getIdentifier
            eat(COLON)
            val t = getType
            Formal(t, id).setPos(id)
        }
        
        //-----------------//
        //---Expressions---//
        //-----------------//
        
        // expr = orExpr (|| orExpr)*
        def expr() : ExprTree = {
            var e = orExpr
            while (currentToken.kind == OR) {
                readToken
                e = Or(e,orExpr).setPos(e)
            }
            e
        }
        
        // orExpr = andExpr (&& andExpr)*
        def orExpr() : ExprTree = {
            var e = andExpr
            while (currentToken.kind == AND) {
                readToken
                e = And(e, andExpr).setPos(e)
            }
            e
        }
        
        // andExpr = eqExpr (<|== eqExpr)*
        def andExpr() : ExprTree = {
            var e = eqExpr
            while (currentToken.kind == LESSTHAN || currentToken.kind == EQUALS) {
                e = currentToken.kind match {
                    case LESSTHAN => readToken; LessThan(e, eqExpr).setPos(e)
                    case EQUALS => readToken; Equals(e, eqExpr).setPos(e)
                    case _ => expected(LESSTHAN,EQUALS)
                }
            }
            e
        }
        
        // eqExpr = addExpr (+|- addExpr)*
        def eqExpr() : ExprTree = {
            var e = addExpr
            while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
                e = currentToken.kind match {
                    case PLUS => readToken; Plus(e, addExpr).setPos(e)
                    case MINUS => readToken; Minus(e, addExpr).setPos(e)
                    case _ => expected(PLUS,MINUS)
                }
            }
            e
        }
        
        // addExpr = timeExpr (*|/ timeExpr)*
        def addExpr() : ExprTree = {
            var e = timeExpr
            while (currentToken.kind == TIMES || currentToken.kind == DIV) {
                e = currentToken.kind match {
                    case TIMES => readToken; Times(e, timeExpr).setPos(e)
                    case DIV => readToken; Div(e, timeExpr).setPos(e)
                    case _ => expected(TIMES,DIV)
                }
            }
            e
        }
        
        // timeExpr = !+ hardExpr
        def timeExpr() : ExprTree = currentToken.kind match {
            case BANG => 
                val token = currentToken
                readToken
                Not(hardExpr).setPos(token)
            case _ => 
                hardExpr
        }
        
        // hardExpr = finalExpr([expr])* | finalExpr.afterDotExpr(parent)
        def hardExpr() : ExprTree = {
            var e = finalExpr
            while(currentToken.kind == LBRACKET || currentToken.kind == DOT) {
	            e = currentToken.kind match {
	                case LBRACKET =>
	                    eat(LBRACKET)
	                    val index = expr
	                    eat(RBRACKET)
		                if(currentToken.kind == DOT) {
		                    readToken
		                    afterDotExpr(ArrayRead(e, index).setPos(e))
		                }
		                else ArrayRead(e, index).setPos(e)
	                    
	                case DOT => 
	                    eat(DOT)
	                    afterDotExpr(e)
	                    
	                case _ => e
	            }
            }
            e
        }
        
        def finalExpr() : ExprTree = {
            val token = currentToken
        	token.kind match {
	            case LPAREN =>
	                readToken
	                val e = expr
	                eat(RPAREN)
	                e.setPos(token)
	
	            case NEW =>
	                readToken
	                
	                currentToken.kind match {
	                    case IDKIND =>
	                        val id = getIdentifier
			                eat(LPAREN)
			                eat(RPAREN)
			                if(currentToken.kind == DOT) {
			                    readToken
			                    afterDotExpr(New(id).setPos(token))
			                }
			                else New(id).setPos(token)
	                        
	                    case INT =>
	                        readToken
			                eat(LBRACKET)
	                        val size = expr
			                eat(RBRACKET)
	                        NewIntArray(size).setPos(token)
	                        // TODO : can we use: new Int[size].something() ?
	                        
	                    case _ => expected(IDKIND, INT)
	                }
	
	                
	            case THIS =>
	                readToken
	                if (currentToken.kind == DOT) {
	                    readToken
	                    afterDotExpr(This().setPos(token))
	                }
	                else {
	                    This().setPos(token)
	                }
	                
	            case IDKIND =>
	                getIdentifier
	                
	            case STRLITKIND =>
	                val strvalue = currentToken.asInstanceOf[STRLIT].value
	                readToken
	                StringLit(strvalue).setPos(token)
	                
	            case INTLITKIND =>
	                val intvalue = currentToken.asInstanceOf[INTLIT].value
	                readToken
	                IntLit(intvalue).setPos(token)
	                
	            case TRUE =>
	                readToken
	                True().setPos(token)
	                
	            case FALSE =>
	                readToken
	                False().setPos(token)
	            
	            case _ =>
	                expected(LPAREN, IDKIND, THIS, NEW, STRLITKIND, INTLITKIND, TRUE, FALSE)
            }
        }
        
        def afterDotExpr(parent: ExprTree): ExprTree = currentToken.kind match {
            case LENGTH =>
            	readToken
            	ArrayLength(parent).setPos(parent)
                
            case IDKIND =>
                val id = getIdentifier
                eat(LPAREN)
                val args = getExpressions
                eat(RPAREN)

                // identifier().
                if (currentToken.kind == DOT) {
                    readToken
                    afterDotExpr(MethodCall(parent, id, args).setPos(parent))
                }
                
                // identifier()
                else {
                    MethodCall(parent, id, args).setPos(parent)
                }
                
            case _ =>
                expected(IDKIND, LENGTH)
        }
        
        
        //----------------//
        //---Statements---//
        //----------------//
        
        // Invariant : after a expr|statmt, currentToken return the next token to evaluate
        def statmt() : StatTree = {
            val token = currentToken
            token.kind match {
	            case LBRACE =>
	                readToken
	                val list = getStatements
	                eat(RBRACE)
	                Block(list).setPos(token)
	                
	            case IF =>
	                readToken
	                eat(LPAREN)
	                val e = expr
	                eat(RPAREN)
	                val sT = statmt
	                val sF = if (currentToken.kind == ELSE) {
	                    readToken
	                    Some(statmt)
	                } else None
	                If(e,sT,sF).setPos(token)
	                
	            case WHILE =>
	                readToken
	                eat(LPAREN)
	                val e = expr
	                eat(RPAREN)
	                val s = statmt
	                While(e,s).setPos(token)
	                
	            case PRINTLN =>
	                readToken
	                eat(LPAREN)
	                val e = expr
	                eat(RPAREN)
	                eat(SEMICOLON)
	                Println(e).setPos(token)
	                
	            case IDKIND =>
	                val id = getIdentifier
	                currentToken.kind match {
	                    case EQSIGN =>
	                        readToken
	                        val e = expr
	                        eat(SEMICOLON)
	                        Assign(id, e).setPos(token)
	                        
	                    case LBRACKET =>
	                        readToken
	                        val index = expr
	                        eat(RBRACKET)
	                        eat(EQSIGN)
	                        val e = expr
	                        eat(SEMICOLON)
	                        ArrayAssign(id, index, e).setPos(token)
	                        
	                    case _ =>
	                        expected(EQSIGN, LBRACKET)
	                }
	                
	            case RETURN =>
		            readToken
		            val retExpr = expr
		            eat(SEMICOLON)
		            Return(retExpr).setPos(token)
	                
	            case _ =>
	                expected(LBRACE, IF, WHILE, PRINTLN, IDKIND, RETURN)
            }  
        }

        //----------------//
        //---MainObject---//
        //----------------//

        // object Identifier { def main ( ) : Unit = { ( Statement )* } }
        def mainObject: MainObject = {
            val token = currentToken
            eat(OBJECT)
            val id = getIdentifier
            eat(LBRACE)
            eat(DEF)
            eat(MAIN)
            eat(LPAREN)
            eat(RPAREN)
            eat(COLON)
            eat(UNIT)
            eat(EQSIGN)
            eat(LBRACE)
            val statements = getStatements
            eat(RBRACE)
            eat(RBRACE)
            MainObject(id, statements).setPos(token)
        }
        
        //---------------//
        //---ClassDecl---//
        //---------------//

        // class Identifier ( extends Identifier )? { ( VarDeclaration )* ( MethodDeclaration )* }
        def classDecl(): ClassDecl = {
            val token = currentToken
            eat(CLASS)
            val id = getIdentifier
            val parent = currentToken.kind match {
                case EXTENDS =>
                    readToken
                    Some(getIdentifier)
                case _ => None
            }
            eat(LBRACE)
            val vars = getVarDeclarations
            val methods = getMethodDeclarations
            eat(RBRACE)
            ClassDecl(id, parent, vars, methods).setPos(token)
        }
        
        //-------------//
        //---VarDecl---//
        //-------------//
        
        // var Identifier : Type ;
        def varDecl(): VarDecl = {
            val token = currentToken
            eat(VAR)
            val id = getIdentifier
            eat(COLON)
            val t = getType
            eat(SEMICOLON)
            VarDecl(t, id).setPos(token)
        }
        
        //----------------//
        //---MethodDecl---//
        //----------------//
        
        // def Identifier ( ( Identifier : Type ( , Identifier : Type )* )? ) : Type = 
        // { ( VarDeclaration )* ( Statement )* }
        def methodDecl(): MethodDecl = {
            val token = currentToken
            eat(DEF)
            val id = getIdentifier
            eat(LPAREN)
            val formals = getFormals
            eat(RPAREN)
            eat(COLON)
            val retType = getType
            eat(EQSIGN)
            eat(LBRACE)
            val vars = getVarDeclarations
            val statements = getStatements
            eat(RBRACE)
            MethodDecl(retType, id, formals, vars, statements).setPos(token)
        }
        
        
        //-------------//
        //---Program---//
        //-------------//
        
        // MainObject ( ClassDeclaration )* <EOF>
        def parseGoal: Program = {
            val main = mainObject
            val classes = getClasses
            eat(EOF)
            Program(main, classes).setPos(main)
        }

        // Initialize the first token
        readToken

        // Parse
        parseGoal
    }
}

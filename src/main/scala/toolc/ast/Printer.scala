package toolc
package ast

import Trees._
import toolc.lexer.Tokens._
import toolc.lexer.Token
import toolc.lexer.TokenKind

object Printer {

    var str = new StringBuilder
    var indentLevel: Int = 0

    def apply(t: Tree): String = {
        construct(t)
        str.toString
    }


    def construct(t: Tree): Unit = t match {
        case Program(main, classes) =>
            construct(main, BR) 
            construct(classes)
            
        case MainObject(id, stats) =>
            construct(OBJECT, SP, id, SP, LBRACE)
            incrLevel
            construct(DEF, SP, MAIN, LPAREN, RPAREN, COLON, SP, UNIT, SP, EQSIGN, SP, LBRACE)
            incrLevel
            construct(stats)
            decrLevel
            construct(RBRACE)
            decrLevel
            construct(RBRACE, BR)
            
        case ClassDecl(id, parent, vars, methods) =>
            construct(CLASS, SP, id, SP)
            parent match {
                case Some(p) => construct(EXTENDS, SP, p, SP)
                case None =>
            }
            construct(LBRACE)
            incrLevel
            construct(vars)
            if (!vars.isEmpty) construct(BR)
            construct(methods)
            decrLevel
            construct(RBRACE, BR)
            
        case VarDecl(tpe, id) =>
            construct(VAR, SP, id, COLON, SP, tpe, SEMICOLON, BR)
            
        case MethodDecl(retType, id, formals, vars, stats) =>
            construct(DEF, SP, id, LPAREN)
            constructWithComma(formals)
            construct(RPAREN, COLON, SP, retType, SP, EQSIGN, SP, LBRACE)
            incrLevel
            construct(vars)
            if (!vars.isEmpty) construct(BR)
            construct(stats)
            decrLevel
            construct(RBRACE, BR)
            
        case Formal(tpe, id) =>
            construct(id, COLON, SP, tpe)
            
        case Block(stats) =>
            construct(LBRACE)
            incrLevel
            construct(stats)
            decrLevel
            construct(RBRACE, BR)
            
        case If(expr, thn, els) =>
            construct(IF, LPAREN, expr, RPAREN, SP, thn)
            els match {
                case Some(s) => construct(ELSE, SP, s)
                case None =>
            }
            construct(BR)
            
        case While(expr, stat) =>
            construct(WHILE, LPAREN, expr, RPAREN, SP, stat, BR)
            
        case Println(expr) =>
            construct(PRINTLN, LPAREN, expr, RPAREN, SEMICOLON, BR)
            
        case Assign(id, expr) =>
            construct(id, SP, EQSIGN, SP, expr, SEMICOLON, BR)
            
        case ArrayAssign(id, index, expr) =>
            construct(id, LBRACKET, index, RBRACKET, SP, EQSIGN, SP, expr, SEMICOLON, BR)
            
        case Return(retExpr) =>
            construct(RETURN, SP, retExpr, SEMICOLON)
            
        case And(lhs, rhs) =>
            construct(LPAREN, lhs, SP, AND, SP, rhs, RPAREN)
            
        case Or(lhs, rhs) =>
            construct(LPAREN, lhs, SP, OR, SP, rhs, RPAREN)
            
        case Plus(lhs, rhs) =>
            construct(LPAREN, lhs, SP, PLUS, SP, rhs, RPAREN)
            
        case Minus(lhs, rhs) =>
            construct(LPAREN, lhs, SP, MINUS, SP, rhs, RPAREN)
            
        case Times(lhs, rhs) =>
            construct(LPAREN, lhs, SP, TIMES, SP, rhs, RPAREN)
            
        case Div(lhs, rhs) =>
            construct(LPAREN, lhs, SP, DIV, SP, rhs, RPAREN)
            
        case LessThan(lhs, rhs) =>
            construct(LPAREN, lhs, SP, LESSTHAN, SP, rhs, RPAREN)
            
        case Equals(lhs, rhs) =>
            construct(LPAREN, lhs, SP, EQUALS, SP, rhs, RPAREN)
            
        case ArrayRead(arr, index) =>
            construct(arr, LBRACKET, index, RBRACKET)
            
        case ArrayLength(arr) =>
            construct(arr, DOT, LENGTH)
            
        case MethodCall(obj,id,args) =>
            construct(obj, DOT, id, LPAREN)
            constructWithComma(args)
            construct(RPAREN)
        
        case NewIntArray(size) =>
            construct(NEW, SP, INT, SP, LBRACKET, size, RBRACKET)
            
        case New(id) =>
            construct(NEW, SP, id, LPAREN, RPAREN)
            
        case Not(expr) =>
            construct(BANG, LPAREN, expr, RPAREN)
            
        case Identifier(id) => construct(id, t.asInstanceOf[Identifier].getSymbol.toString)
        case IntLit(value) => construct(value)
        case StringLit(value) => construct("\"", value, "\"")
        case True() => construct(TRUE)
        case False() => construct(FALSE)
        case This() => construct(THIS)
        
        case BooleanType() => construct(BOOLEAN)
        case StringType() => construct(STRING)
        case IntType() => construct(INT)
        case IntArrayType() => construct(INT, LBRACKET, RBRACKET)
            
    }
    
    def construct(o: Any*): Unit = o foreach { 
        case x: TokenKind => str ++= tokenToString(x)
        case x: WhiteSpace => str ++= whiteSpaceToString(x)
        case x: Tree => construct(x)
        case x: String => construct(x)
        case x: Int => construct(x)
        case _ => 
            System.err.println("Matching Error in construct(Any*)")
            System.exit(1)
    }
    
    def constructWithComma(list: List[Tree]): Unit = list match {
        case x :: Nil => construct(x)
        case x :: xs => 
            construct(x)
            xs foreach (construct(COMMA, SP, _))
        case Nil =>
    }
    
    def construct(list: List[Tree]): Unit = construct(list, 0)
    def construct(list: List[Tree], indentLevel: Int): Unit = 
        if (!list.isEmpty) list foreach construct
    
    def construct(value: Int): Unit = str ++= value.toString
    def construct(value: String): Unit = str ++= value

    def indent(): String = (1 to indentLevel) map (_ => whiteSpaceToString(TAB)) mkString
    def incrLevel(): Unit = {
        indentLevel += 1
        construct(BR)
    }
    def decrLevel(): Unit = {
        indentLevel -= 1
        construct(BR)
    }

    def tokenToString(k: TokenKind): String = k match {
        case COLON => ":"
        case SEMICOLON => ";"
        case DOT => "."
        case COMMA => ","
        case EQSIGN => "="
        case EQUALS => "=="
        case BANG => "!"
        case LPAREN => "("
        case RPAREN => ")"
        case LBRACKET => "["
        case RBRACKET => "]"
        case LBRACE => "{"
        case RBRACE => "}"
        case AND => "&&"
        case OR => "||"
        case LESSTHAN => "<"
        case PLUS => "+"
        case MINUS => "-"
        case TIMES => "*"
        case DIV => "/"
        case OBJECT => "object"
        case CLASS => "class"
        case DEF => "def"
        case VAR => "var"
        case UNIT => "Unit"
        case MAIN => "main"
        case STRING => "String"
        case EXTENDS => "extends"
        case INT => "Int"
        case BOOLEAN => "Bool"
        case WHILE => "while"
        case IF => "if"
        case ELSE => "else"
        case RETURN => "return"
        case LENGTH => "length"
        case TRUE => "true"
        case FALSE => "false"
        case THIS => "this"
        case NEW => "new"
        case PRINTLN => "println"
        case _ => ""
    }
    
    def whiteSpaceToString(w: WhiteSpace): String = w match {
        case BR  => "\n" + indent
        case SP  => " "
        case TAB => "    "
        case _ => ""
    }
    
    sealed trait WhiteSpace
    case object BR  extends WhiteSpace 
    case object SP  extends WhiteSpace 
    case object TAB extends WhiteSpace 
}

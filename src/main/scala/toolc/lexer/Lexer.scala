package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
    import Tokens._

    def run(ctx: Context)(f: File): Iterator[Token] = {
        val source = Source.fromFile(f)
        import ctx.reporter._

        def currentPos(): Positioned = {
            new Positioned {}.setPos(f, source.pos)
        }
        
        def next(): Char = {
            if (source hasNext) source next
            else '\u0000'
        }
        

        var nextChar: Char = next

        def readNextToken(): Token = {
            
            def setPos(t : Token, pos : Positioned) : Token = t.setPos(pos)
            
            def isOperator(c: Char): Boolean = Set('+', '-', '*', '/' , '=', '!', '<', '>', '&', '|') contains c
            def isSpecialChar(c: Char): Boolean = (Set(':', ';', '.', ',', '(', ')', '[', ']', '{', '}') contains c) || isOperator(c)
            
            def isWordStopper(c: Char): Boolean = !(c.isLetterOrDigit || c == '_')
            def isIntLitStopper(c: Char): Boolean = !c.isDigit
            def isStrLitStopper(c: Char): Boolean = {
                if(currentPos().col == 1)
                    fatal("unterminated String")
                c == '"'
            }

            def constructToken(rest: List[Char], c: Char, stopperFonction: (Char => Boolean)): (List[Char], Char) = c match {
                case c if stopperFonction(c) => (rest, c)
                case c => constructToken((c :: rest), next, stopperFonction)
            }

            // Need end of line '\n' to exit comment
            def enterInLineComment(): Token = {
                
                nextChar = next
                nextChar match {
                    case '\u0000' =>
                    	readNextToken
                    case _ => currentPos().col match {
		                case 1 =>
		                	readNextToken
		                case _ =>
		                	enterInLineComment
		            }
                        
                }
                
            }

            // Need the pattern "*/" to exit comment
            def enterInBlockComment(): Token = {
                def starReached(): Token = next match {
                    case '/' =>
                        nextChar = next
                        readNextToken
                    case '*' => starReached
                    case _ => enterInBlockComment
                }

                next match {
                    case '*' => starReached
                    case '\u0000' =>
                    	fatal("unterminated block comment")
                    case _ => enterInBlockComment
                }
            }

            def wordToToken(charList: List[Char]): Token = {
                
                val word = charList mkString ""
                word match {
                    // Keywords
                    case "object" => new Token(OBJECT)
                    case "class" => new Token(CLASS)
                    case "def" => new Token(DEF)
                    case "var" => new Token(VAR)
                    case "Unit" => new Token(UNIT)
                    case "main" => new Token(MAIN)
                    case "String" => new Token(STRING)
                    case "extends" => new Token(EXTENDS)
                    case "Int" => new Token(INT)
                    case "Bool" => new Token(BOOLEAN)
                    case "while" => new Token(WHILE)
                    case "if" => new Token(IF)
                    case "else" => new Token(ELSE)
                    case "return" => new Token(RETURN)
                    case "length" => new Token(LENGTH)
                    case "true" => new Token(TRUE)
                    case "false" => new Token(FALSE)
                    case "this" => new Token(THIS)
                    case "new" => new Token(NEW)
                    case "println" => new Token(PRINTLN)

                    // Else it's an identifier
                    case word => new ID(word)
                }
            }

            def specialCharsToToken(charList: List[Char]): Token = {
                val chars = charList mkString ""
                chars match {
                    case ":" => new Token(COLON)
                    case ";" => new Token(SEMICOLON)
                    case "." => new Token(DOT)
                    case "," => new Token(COMMA)
                    case "=" => new Token(EQSIGN)
                    case "==" => new Token(EQUALS)
                    case "!" => new Token(BANG)
                    case "(" => new Token(LPAREN)
                    case ")" => new Token(RPAREN)
                    case "[" => new Token(LBRACKET)
                    case "]" => new Token(RBRACKET)
                    case "{" => new Token(LBRACE)
                    case "}" => new Token(RBRACE)
                    case "&&" => new Token(AND)
                    case "||" => new Token(OR)
                    case "<" => new Token(LESSTHAN)
                    case "+" => new Token(PLUS)
                    case "-" => new Token(MINUS)
                    case "*" => new Token(TIMES)
                    case "/" => new Token(DIV)

                    case _ =>
                        error("invalid operator : " + chars)
                        new Token(BAD)
                }
            }
            
            val firstPos = currentPos;

            nextChar match {

                case c if c isLetter =>
                    val l = constructToken(Nil, nextChar, isWordStopper)
                    nextChar = l._2
                    setPos(wordToToken(l._1.reverse), firstPos)

                case c if c isDigit =>
                    if (c == '0') {
                        nextChar = next
                        setPos(new INTLIT(0), firstPos)
                    }
                    else {
	                    val l = constructToken(Nil, nextChar, isIntLitStopper)
	                    nextChar = l._2
	                    setPos(new INTLIT(l._1.reverse.mkString.toInt), firstPos)
                    }

                case '"' =>
                    val l = constructToken(Nil, next, isStrLitStopper)
                    nextChar = next // not l._2 ! because l._2 = '"'
                    setPos(new STRLIT(l._1.reverse.mkString), firstPos)

                case c if c.isWhitespace =>
                    nextChar = next
                    readNextToken

                case '/' =>
                    val n = next
                    n match {
                        case '/' =>
                            enterInLineComment
                        case '*' =>
                            enterInBlockComment
                        case _ =>
                            nextChar = n
                            setPos(specialCharsToToken(List('/')), firstPos)
                    }

                case '&' =>
                    val n = next
                    if (n == '&') {
                        nextChar = next
                        setPos(specialCharsToToken(List('&', '&')), firstPos)
                    } else {
                        nextChar = n
                        error("single '&'")
                        setPos(new Token(BAD), firstPos)
                    }

                case '|' =>
                    val n = next
                    if (n == '|') {
                        nextChar = next
                        setPos(specialCharsToToken(List('|', '|')), firstPos)
                    } else {
                        nextChar = n
                        error("single '|'")
                        setPos(new Token(BAD), firstPos)
                    }

                case '=' =>
                    val n = next
                    if (n == '=') {
                        nextChar = next
                        setPos(specialCharsToToken(List('=', '=')), firstPos)
                    } else {
                        nextChar = n
                        setPos(specialCharsToToken(List('=')), firstPos)
                    }

                case c if isSpecialChar(c) =>
                    nextChar = next
                    setPos(specialCharsToToken(List(c)), firstPos)

                case '\u0000' =>
                    setPos(new Token(EOF), firstPos)

                case _ =>
                    error("invalid character : " + nextChar)
                    nextChar = next
                    setPos(new Token(BAD), firstPos)
            }
        }

        new Iterator[Token] {
            var nextToken: Token = readNextToken
            var reachedEnd = false

            def hasNext = {
                nextToken.kind != EOF || !reachedEnd
            }

            def next = {
                val r = nextToken
                nextToken = readNextToken
                if (r.kind == EOF) {
                    reachedEnd = true
                }
                r
            }
        }
    }
}

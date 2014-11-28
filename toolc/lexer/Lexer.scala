package toolc
package lexer

import utils._

import scala.io.Source
import java.io.File
import scala.collection.mutable
import scala.util.control.Breaks

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    import ctx.reporter._

    // Global variables
    var nextTokenIsEOF = false
    val keywords = Map[String, TokenKind]("object" -> OBJECT,
      "class" -> CLASS,
      "def" -> DEF,
      "var" -> VAR,
      "Unit" -> UNIT,
      "main" -> MAIN,
      "String" -> STRING,
      "extends" -> EXTENDS,
      "Int" -> INT,
      "Bool" -> BOOLEAN,
      "while" -> WHILE,
      "if" -> IF,
      "else" -> ELSE,
      "return" -> RETURN,
      "length" -> LENGTH,
      "true" -> TRUE,
      "false" -> FALSE,
      "this" -> THIS,
      "new" -> NEW,
      "println" -> PRINTLN)

    // Skips white spaces, tabs, and change of lines. IT DOES NOT SKIP EOF
    def skipWhiteSpaces {
      while (source.ch.isWhitespace && advanceSource) {}
    }

    def advanceSource: Boolean =
      if (source.hasNext) {
        source.next
        true
      } else {
        nextTokenIsEOF = true // Used to signal that the next token is an EOF (used in next)
        false
      }

    def readIdentifier: String = {
      var res: String = ""
      var ch = source.ch
      val loop = new Breaks;
      loop.breakable {
        while (ch.isLetter || ch.isDigit || ch == '_') {
          res = res + ch
          if (!advanceSource) loop.break
          else ch = source.ch
        }
      }
      res
    }

    def readInt: Int = {
      var int: Int = 0
      var ch = source.ch
      val loop = new Breaks;
      loop.breakable {
        while (ch.isDigit) {
          int = int * 10 + ch.asDigit
          if (!advanceSource) loop.break
          else ch = source.ch
        }
      }
      int
    }

    //Skip white spaces at the beginning of the file
    advanceSource
    skipWhiteSpaces

    new Iterator[Token] {
      var current: Token = new Token(BAD)
      var isThereMoreTokens = true;
      def hasNext = {
        isThereMoreTokens
      }

      def next = {
        if (nextTokenIsEOF) {
          current = new Token(EOF)
          current.setPos(f, source.pos)
          isThereMoreTokens = false // Used to end the cycle in hasNext. Do not change anywhere else
        } else {
          val ch = source.ch //Current character
          val pos = source.pos

          // Identifiers and keywords
          if (ch.isLetter) {
            //Read the entire name
            val id = readIdentifier
            if (keywords.contains(id)) { //Word is a keyword
              current = new Token(keywords(id))
              current.setPos(f, pos)
            } else { //Word is an identifier
              current = new ID(id)
              current.setPos(f, pos)
            }
          } // Numbers
          else if (ch.isDigit) {
            if (ch == '0') {
              current = new INTLIT(0)
              current.setPos(f, pos)
              advanceSource
            } else {
              // Read the entire number
              val n = readInt
              current = new INTLIT(n)
              current.setPos(f, pos)
            }
          } //Single characters
          else ch match {
            case '"' => //String literal
              if (advanceSource) {
                var res: String = ""
                var char = source.ch
                var EOFfound = false
                val loop = new Breaks;
                loop.breakable {
                  while (char != '"' && char != '\n' && char != '\r') {
                    res = res + char
                    if (!advanceSource) {
                      EOFfound = true
                      loop.break
                    } else char = source.ch
                  }
                }
                // Constructing the token

                if (EOFfound) { // Ex. "shsjj sjsksEOF
                  current = new Token(BAD)
                  current.setPos(f, pos)
                  error("Unfinished string literal: Expected '\"' found EOF")
                } else {
                  char match {
                    case '"' => // Well constructed
                      current = new STRLIT(res)
                    case _ => // Ex. "shdjskk dkdllsCR
                      current = new Token(BAD)
                      error("Unfinished string literal: Expected '\"' found newLine")
                  }
                  current.setPos(f, pos)
                  advanceSource
                }
              } else { // Ex "EOF
                current = new Token(BAD)
                current.setPos(f, pos)
                error("Unfinished string literal: Expected '\"' found EOF")
              }

            case ':' => //Colon
              current = new Token(COLON)
              current.setPos(f, pos)
              advanceSource
            case ';' => //Semicolon
              current = new Token(SEMICOLON)
              current.setPos(f, pos)
              advanceSource
            case '.' => //Dot
              current = new Token(DOT)
              current.setPos(f, pos)
              advanceSource
            case ',' => //Comma
              current = new Token(COMMA)
              current.setPos(f, pos)
              advanceSource
            case '=' => //EqSign and Equals
              val isEOFafter = !advanceSource
              if (source.ch == '=' && !isEOFafter) { // Ex. ==
                current = new Token(EQUALS)
                current.setPos(f, pos)
                advanceSource
              } else { // Ex. = , =EOF
                current = new Token(EQSIGN)
                current.setPos(f, pos)
              }
            case '!' => //Bang
              current = new Token(BANG)
              current.setPos(f, pos)
              advanceSource
            case '(' => //LParen
              current = new Token(LPAREN)
              current.setPos(f, pos)
              advanceSource
            case ')' => //RParen
              current = new Token(RPAREN)
              current.setPos(f, pos)
              advanceSource
            case '[' => //LBracket
              current = new Token(LBRACKET)
              current.setPos(f, pos)
              advanceSource
            case ']' => //RBracket
              current = new Token(RBRACKET)
              current.setPos(f, pos)
              advanceSource
            case '{' => //LBrace
              current = new Token(LBRACE)
              current.setPos(f, pos)
              advanceSource
            case '}' => //RBrace
              current = new Token(RBRACE)
              current.setPos(f, pos)
              advanceSource
            case '&' => //And
              val isEOFafter = !advanceSource
              if (source.ch == '&' && !isEOFafter) { // Ex. &&
                current = new Token(AND)
                current.setPos(f, pos)
                advanceSource
              } else { // Ex. & , &EOF
                current = new Token(BAD)
                current.setPos(f, pos)
                if (isEOFafter) error("Unfinished AND: Expected &, found EOF")
                else error("Unfinished AND: Expected &, found " + source.ch)
              }
            case '|' => //Or
              val isEOFafter = !advanceSource
              if (source.ch == '|' && !isEOFafter) { // Ex. ||
                current = new Token(OR)
                current.setPos(f, pos)
                advanceSource
              } else { // Ex. | , |EOF
                current = new Token(BAD)
                current.setPos(f, pos)
                if (isEOFafter) error("Unfinished OR: Expected |, found EOF")
                else error("Unfinished OR: Expected |, found " + source.ch)
              }
            case '<' => //LessThan
              current = new Token(LESSTHAN)
              current.setPos(f, pos)
              advanceSource
            case '+' => //Plus
              current = new Token(PLUS)
              current.setPos(f, pos)
              advanceSource
            case '-' => //Minus
              current = new Token(MINUS)
              current.setPos(f, pos)
              advanceSource
            case '*' => //Times
              current = new Token(TIMES)
              current.setPos(f, pos)
              advanceSource
            case '/' => //Division and Comments
              val isEOFafter = !advanceSource
              var char = source.ch

              if (char == '/' && !isEOFafter) { // Ex. //, // hdjddksls, //jdjkskssEOF
                // Skip comment

                val loop = new Breaks;
                loop.breakable {
                  while (char != '\r' && char != '\n') {
                    if (!advanceSource) loop.break
                    else char = source.ch
                  }
                }
                advanceSource
                skipWhiteSpaces
                current = next

              } else if (source.ch == '*' && !isEOFafter) { // Ex /* hjdjd */
                var isCommentClosed: Boolean = false
                var isEndOfFile: Boolean = false
                val loop2 = new Breaks;

                if (!advanceSource) isEndOfFile = true // Ex. /*EOF
                else {
                  char = source.ch
                  while (!isCommentClosed && !isEndOfFile) {
                    loop2.breakable {
                      while (char != '*') {
                        if (!advanceSource) {
                          isEndOfFile = true
                          loop2.break
                        } else char = source.ch
                      }
                      
                      if (!advanceSource) {
                        isEndOfFile = true
                        loop2.break
                      }
                      else char = source.ch
                      
                      if(char == '/'){
                        isCommentClosed = true
                        loop2.break
                      }
                    }
                  }
                }
                
                if (isEndOfFile) { //Ex. /* djdjkskslsEOF, /*EOF
                  current = new Token(BAD)
                  current.setPos(f, pos)
                  error("Unclosed block comment. Expected */, found EOF")
                } else { // Well formed block comment
                  advanceSource
                  skipWhiteSpaces
                  current = next
                }

              } else { // Ex. / , /EOF
                current = new Token(DIV)
                current.setPos(f, pos)
              }

            case _ => //BAD
              error("Unrecognized character: " + source.ch)
              current = new Token(BAD)
              current.setPos(f, pos)
              advanceSource
          }

          // Skip all white spaces until next character
          skipWhiteSpaces
        }
        current
      }
    }
  }
}


/*
 * ANOTATION 1
 * We can parse comments as another token because EOF is a valid token, if EOF were not a token , we would need to
 *  skip the comments outside the next call, so that the hasNext function could give the real value. 
 *  Example: code comment EOF. If EOF is a token, hasNext would return true once in comment. If EOF is not a token,
 *  hasNext should return false once it gets to the last comment
 *  
 */

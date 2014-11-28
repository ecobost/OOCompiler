package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, and one lookahead token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind :: more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def parseGoal: Program = {
      val mainObject: MainObject = parseMainObject
      var classes: List[ClassDecl] = List()
      while (currentToken.kind == CLASS) {
        classes = classes :+ parseClassDeclaration
      }
      eat(EOF)
      new Program(mainObject, classes).setPos(mainObject)
    }

    def parseMainObject: MainObject = {
      val pos: Token = currentToken 	  
      eat(OBJECT)
      val identifier: Identifier = parseIdentifier
      eat(LBRACE)
      eat(DEF)
      eat(MAIN)
      eat(LPAREN)
      eat(RPAREN)
      eat(COLON)
      eat(UNIT)
      eat(EQSIGN)
      eat(LBRACE)
      var stats: List[StatTree] = List()
      while (currentToken.kind != RBRACE) { // Possible because first(Statement) does not contain }
        stats = stats :+ parseStatement 
      }
      eat(RBRACE)
      eat(RBRACE)
      new MainObject(identifier, stats).setPos(pos)
    }

    def parseClassDeclaration: ClassDecl = {
      val pos: Token = currentToken 
      eat(CLASS)
      val identifier: Identifier = parseIdentifier
      val parent: Option[Identifier] = currentToken.kind match {
        case EXTENDS =>
          eat(EXTENDS)
          val identifier2 = parseIdentifier
          Some(identifier2)
        case _ => None
      }
      eat(LBRACE)
      var vars: List[VarDecl] = List()
      while (currentToken.kind == VAR) {
        vars = vars :+ parseVarDeclaration
      }
      var methods: List[MethodDecl] = List()
      while (currentToken.kind == DEF) {
        methods = methods :+ parseMethodDeclaration
      }
      eat(RBRACE)
      new ClassDecl(identifier, parent, vars, methods).setPos(pos)
    }

    def parseVarDeclaration: VarDecl = {
      val pos: Token = currentToken 
      eat(VAR)
      val identifier: Identifier = parseIdentifier
      eat(COLON)
      val type_ : TypeTree = parseType
      eat(SEMICOLON)
      new VarDecl(type_, identifier).setPos(pos)
    }

    def parseMethodDeclaration: MethodDecl = {
      val pos: Token = currentToken 
      eat(DEF)
      val identifier: Identifier = parseIdentifier
      eat(LPAREN)
      var args: List[Formal] = List()
      if (currentToken.kind != RPAREN) { // Parse the arguments
        args = args :+ parseArgument
        while (currentToken.kind == COMMA) {
          eat(COMMA)
          args = args :+ parseArgument
        }
      }
      eat(RPAREN)
      eat(COLON)
      val returnType: TypeTree = parseType
      eat(EQSIGN)
      eat(LBRACE)
      var vars: List[VarDecl] = List()
      while (currentToken.kind == VAR) {
        vars = vars :+ parseVarDeclaration
      }
      var stats: List[StatTree] = List()
      while (currentToken.kind != RETURN) { // Possible because first(Statement) does not contain RETURN
        stats = stats :+ parseStatement
      }
      eat(RETURN)
      val returnExpression: ExprTree = parseExpression
      eat(SEMICOLON)
      eat(RBRACE)
      new MethodDecl(returnType, identifier, args, vars, stats, returnExpression).setPos(pos)
    }

    def parseType: TypeTree = {
      var type_ : TypeTree = null
      val pos: Token = currentToken 
      currentToken.kind match {
        case INT =>
          eat(INT)
          currentToken.kind match {
            case LBRACKET =>
              eat(LBRACKET)
              eat(RBRACKET)
              type_ = new IntArrayType().setPos(pos)
            case _ =>
              type_ = new IntType().setPos(pos)
          }
        case BOOLEAN => 
          eat(BOOLEAN)
          type_ = new BooleanType().setPos(pos)
        case STRING =>
          eat(STRING)
          type_ = new StringType().setPos(pos)
        case IDKIND =>
          val identifier: Identifier = parseIdentifier
          type_ = identifier
        case _ =>
          expected(BOOLEAN, INT, STRING, IDKIND)
      }
      type_
    }

    def parseStatement: StatTree = {
      var stat: StatTree = null
      val pos: Token = currentToken 
      currentToken.kind match {
        case LBRACE =>
          eat(LBRACE)
          var stats: List[StatTree] = List()
          while (currentToken.kind != RBRACE) { // Possible because first(Statement) does not contain }
            stats = stats :+ parseStatement
          }
          eat(RBRACE)
          stat = new Block(stats).setPos(pos)
        case IF =>
          eat(IF)
          eat(LPAREN)
          val expression: ExprTree = parseExpression
          eat(RPAREN)
          val thenStat: StatTree = parseStatement
          val elseStat: Option[StatTree] = currentToken.kind match {
            case ELSE =>
              eat(ELSE)
              Some(parseStatement)
            case _ =>
              None
          }
          stat = new If(expression, thenStat, elseStat).setPos(pos)
        case WHILE =>
          eat(WHILE)
          eat(LPAREN)
          val expression: ExprTree = parseExpression
          eat(RPAREN)
          val whileStat: StatTree = parseStatement
          stat = new While(expression, whileStat).setPos(pos)
        case PRINTLN =>
          eat(PRINTLN)
          eat(LPAREN)
          val expression: ExprTree = parseExpression
          eat(RPAREN)
          eat(SEMICOLON)
          stat = new Println(expression).setPos(pos)
        case IDKIND =>
          val identifier = parseIdentifier
          currentToken.kind match {
            case EQSIGN =>
              eat(EQSIGN)
              val expression: ExprTree = parseExpression
              eat(SEMICOLON)
              stat = new Assign(identifier, expression).setPos(identifier)
            case LBRACKET =>
              eat(LBRACKET)
              val indexExpression: ExprTree = parseExpression
              eat(RBRACKET)
              eat(EQSIGN)
              val expression: ExprTree = parseExpression
              eat(SEMICOLON)
              stat = new ArrayAssign(identifier, indexExpression, expression).setPos(identifier)
            case _ =>
              expected(EQSIGN, LBRACKET)
          }
        case _ =>
          expected(LBRACE, IF, WHILE, PRINTLN, IDKIND)
      }
      stat
    }

    // Difficult one
    def parseExpression: ExprTree = {
      parseOrExpression
    }

    def parseIdentifier: Identifier = {
      val pos: Token = currentToken 
      val identifier: String = currentToken match {
        case token: ID =>
          token.value
        case _ =>
          expected(IDKIND)
      }
      eat(IDKIND)
      new Identifier(identifier).setPos(pos)
    }

    def parseOrExpression: ExprTree = {
      var expr: ExprTree = parseAndExpression
      while (currentToken.kind == OR) {
        eat(OR)
        expr = new Or(expr, parseAndExpression).setPos(expr)
      }
      expr
    }

    def parseAndExpression: ExprTree = {
      var expr: ExprTree = parseLessThanOrEqualsExpr
      while (currentToken.kind == AND) {
        eat(AND)
        expr = new And(expr, parseLessThanOrEqualsExpr).setPos(expr)
      }
      expr
    }

    def parseLessThanOrEqualsExpr: ExprTree = {
      var expr: ExprTree = parseSumOrRestExpression
      while (currentToken.kind == LESSTHAN || currentToken.kind == EQUALS) {
        currentToken.kind match {
          case LESSTHAN =>
            eat(LESSTHAN)
            expr = new LessThan(expr, parseSumOrRestExpression).setPos(expr)
          case EQUALS =>
            eat(EQUALS)
            expr = new Equals(expr, parseSumOrRestExpression).setPos(expr)
          case _ => //Impossible
        }
      }
      expr
    }

    def parseSumOrRestExpression: ExprTree = {
      var expr: ExprTree = parseMultOrDivisionExpr
      while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
        currentToken.kind match {
          case PLUS =>
            eat(PLUS)
            expr = new Plus(expr, parseMultOrDivisionExpr).setPos(expr)
          case MINUS =>
            eat(MINUS)
            expr = new Minus(expr, parseMultOrDivisionExpr).setPos(expr)
          case _ => //Impossible
        }
      }
      expr
    }

    def parseMultOrDivisionExpr: ExprTree = {
      var expr: ExprTree = parseNegativeExpression
      while (currentToken.kind == TIMES || currentToken.kind == DIV) {
        currentToken.kind match {
          case TIMES =>
            eat(TIMES)
            expr = new Times(expr, parseNegativeExpression).setPos(expr)
          case DIV =>
            eat(DIV)
            expr = new Div(expr, parseNegativeExpression).setPos(expr)
          case _ => //Impossible
        }
      }
      expr
    }

    def parseNegativeExpression: ExprTree = {
      var expr: ExprTree = null;
      if (currentToken.kind == BANG) {
        val pos: Token = currentToken 
        eat(BANG)
        expr = new Not(parseNegativeExpression).setPos(pos)
      } else {
        expr = parseMethodOrArrayReadOrLength
      }
      expr
    }

    def parseMethodOrArrayReadOrLength: ExprTree = {
      var expr: ExprTree = parseSimpleExpression
      while (currentToken.kind == DOT || currentToken.kind == LBRACKET) {
        currentToken.kind match {
          case DOT =>
            eat(DOT)
            currentToken.kind match {
              case LENGTH =>
                eat(LENGTH)
                expr = new ArrayLength(expr).setPos(expr)
              case IDKIND =>
                val identifier: Identifier = parseIdentifier
                eat(LPAREN)
                var args: List[ExprTree] = List()
                if (currentToken.kind != RPAREN) { // Parse the arguments
                  args = args :+ parseExpression
                  while (currentToken.kind == COMMA) {
                    eat(COMMA)
                    args = args :+ parseExpression
                  }
                }
                eat(RPAREN)
                expr = new MethodCall(expr, identifier, args).setPos(expr)
              case _ => expected(IDKIND, LENGTH)
            }
          case LBRACKET =>
            eat(LBRACKET)
            val index: ExprTree = parseExpression
            eat(RBRACKET)
            expr = new ArrayRead(expr, index).setPos(expr)
          case _ => //Impossible
        }
      }
      expr
    }

    def parseSimpleExpression: ExprTree = {
      var expr: ExprTree = null
      val pos: Token = currentToken 
      currentToken match {
        case token: INTLIT =>
          expr = new IntLit(token.value).setPos(pos)
          eat(INTLITKIND)
        case token: STRLIT =>
          expr = new StringLit(token.value).setPos(pos)
          eat(STRLITKIND)
        case _ =>
          currentToken.kind match {
            case TRUE =>
              eat(TRUE)
              expr = new True().setPos(pos)
            case FALSE =>
              eat(FALSE)
              expr = new False().setPos(pos)
            case IDKIND =>
              expr = parseIdentifier
            case THIS =>
              eat(THIS)
              expr = new This().setPos(pos)
            case LPAREN =>
              eat(LPAREN)
              expr = parseExpression.setPos(pos) // Should I set the pos to the pos of the ( or the expr ?
              eat(RPAREN)
            case NEW =>
              eat(NEW)
              currentToken.kind match {
                case INT =>
                  eat(INT)
                  eat(LBRACKET)
                  val size: ExprTree = parseExpression
                  eat(RBRACKET)
                  expr = new NewIntArray(size).setPos(pos)
                case IDKIND =>
                  val identifier: Identifier = parseIdentifier
                  eat(LPAREN)
                  eat(RPAREN)
                  expr = new New(identifier).setPos(pos)
                case _ =>
                  expected(INT, IDKIND)
              }
            case _ =>
              expected(INTLITKIND, STRLITKIND, TRUE, FALSE, IDKIND, THIS, LPAREN, NEW)
          }
      }
      expr
    }

    // Utility functions
    def parseArgument: Formal = {
      val identifier: Identifier = parseIdentifier
      eat(COLON)
      val type_ : TypeTree = parseType
      new Formal(type_, identifier).setPos(identifier)
    }

    // Remember:
    // Deal with the position of the parseTrees (DONE)
    // Deal with explicit semantics, like name of classes should not be the same as other name of classes or objects, etc. (IMPLICITLY DONE)
    // Deal with order in the creation of Lists (now they are in reverse order (DONE)

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}

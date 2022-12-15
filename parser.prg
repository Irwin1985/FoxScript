* ================================================================================== *
* The Parser Class
* ================================================================================== *

#ifndef CONSTANTS_LOADED
	#include "FoxScript.h"
#endif

Define Class Parser As Custom
	Hidden oTokens
	Hidden nCurrent
	Hidden previous
	Hidden peek
	Hidden isAtEnd

	Function Init(toTokens)
		This.oTokens = toTokens
		This.nCurrent = 1 && First recognised token
	Endfunc

	Function parse
		Local loStatements
		loStatements = Createobject('Collection')

		Do While !This.isAtEnd
			loStatements.Add(This.declaration())
		Enddo

		Return loStatements
	Endfunc

	Hidden Function declaration
		Local loDeclaration, lSeguir
		loDeclaration = .Null.
		lSeguir = true
		Try
			
			If This.match(TT_CLASS)
				lSeguir = false
				loDeclaration = This.classDeclaration()
			EndIf
			If lSeguir and This.check(TT_FUNCTION)
				Local lnType1
				lnType1 = This.peekNext(1)
				If lnType1 == TT_IDENTIFIER && function declaration
					this.match(TT_FUNCTION)
					lSeguir = false
					loDeclaration = This.functionDeclaration("function")
				EndIf
			EndIf
			if lSeguir and This.match(TT_VAR)
				lSeguir = false
				loDeclaration = This.varDeclaration()
			EndIf
			If lSeguir
				loDeclaration = This.statement()
			EndIf
		Catch To loEx
			If Type('loEx.Type') == 'C'
				This.synchronize()
			Else
				* DEBUG
				Local lcMsg
				lcMsg = "ERROR NRO: " + Alltrim(Str(loEx.ErrorNo))
				lcMsg = lcMsg + Chr(13) + "LINEA: "  	+ Alltrim(Str(loEx.Lineno))
				lcMsg = lcMsg + Chr(13) + "MESSAGE: "  	+ Alltrim(loEx.Message)
				lcMsg = lcMsg + Chr(13) + "LUGAR: "  	+ Alltrim(loEx.Procedure)
				Messagebox(lcMsg, 16)
				* DEBUG
			Endif
		Endtry
		Return loDeclaration
	Endfunc

	Hidden Function classDeclaration
		Local loName, loSuperClass, loMethods
		loName = This.consume(TT_IDENTIFIER, "Expect class name.")
		loSuperClass = .Null.

		If This.match(TT_EXTENDS) && this class extends from another class (inheritance)
			This.consume(TT_IDENTIFIER, "Expect superclass name.")
			loSuperClass = Createobject("Variable", This.previous)
		Endif

		This.consume(TT_LBRACE, "Expect '{' before class body.")

		loMethods = Createobject('Collection')
		Do While !This.Check(TT_RBRACE) And !This.isAtEnd
			loMethods.Add(This.functionDeclaration("method"))
		Enddo

		This.consume(TT_RBRACE, "Expect '}' after class body.")

		Return Createobject("ClassNode", loName, loSuperClass, loMethods)
	Endfunc

	Hidden Function statement
		If This.match(TT_FOR)
			Return This.forStatement()
		Endif
		If This.match(TT_IF)
			Return This.ifStatement()
		Endif
		If This.match(TT_PRINT)
			Return This.printStatement()
		Endif
		If This.match(TT_RETURN)
			Return This.returnStatement()
		Endif
		If This.match(TT_WHILE)
			Return This.whileStatement()
		Endif
		If This.Check(TT_LBRACE)
			Local lnType1, lnType2, lSeguir
			lnType1 = This.peekNext(1)
			lnType2 = This.peekNext(2)
			lSeguir = TRUE
			If Inlist(lnType1, TT_IDENTIFIER, TT_STRING) And lnType2 == TT_COLON
				lSeguir = false
			Endif
			If lSeguir
				This.advance() && '{'
				Return Createobject("Block", This.blockStatement())
			Endif
		Endif
		Return This.expressionStatement()
	Endfunc
	* TODO(irwin): finish this later...
	Hidden Function forStatement
		Return .Null.
	Endfunc

	Hidden Function ifStatement
		Local loCondition, loThenBranch, loElseBranch
		This.consume(TT_LPAREN, "Expect '(' after 'if'.")
		loCondition = This.Expression()
		This.consume(TT_RPAREN, "Expect ')' after if condition.")
		loThenBranch = This.statement()
		loElseBranch = .Null.
		If This.match(TT_ELSE)
			loElseBranch = This.statement()
		Endif

		Return Createobject("IfNode", loCondition, loThenBranch, loElseBranch)
	Endfunc

	Hidden Function printStatement
		Local loExpression
		loExpression = This.Expression()
		This.consume(TT_SEMICOLON, "Expect ';' after value.")
		Return Createobject("Print", loExpression)
	Endfunc

	Hidden Function returnStatement
		Local loKeyword, loValue
		loKeyword = This.previous
		loValue = .Null.
		If !This.Check(TT_SEMICOLON)
			loValue = This.Expression()
		Endif
		This.consume(TT_SEMICOLON, "Expect ';' after return value.")
		Return Createobject("ReturnNode", loKeyword, loValue)
	Endfunc

	Hidden Function varDeclaration
		Local loName, loInitializer
		loName = This.consume(TT_IDENTIFIER, "Expect variable name.")
		loInitializer = .Null.
		If This.match(TT_SIMPLE_ASSIGN)
			loInitializer = This.Expression()
		Endif
		This.consume(TT_SEMICOLON, "Expect ';' after variable declaration.")

		Return Createobject("Var", loName, loInitializer)
	Endfunc

	Hidden Function whileStatement
		This.consume(TT_LPAREN, "Expect '(' after 'while'.")
		Local loCondition, loBody
		loCondition = This.Expression()
		This.consume(TT_RPAREN, "Expect ')' after condition.")
		loBody = This.statement()

		Return Createobject("WhileNode", loCondition, loBody)
	Endfunc

	Hidden Function expressionStatement
		Local loExpr
		loExpr = This.Expression()
		This.consume(TT_SEMICOLON, "Expect ';' after expression.")
		Return Createobject("Expression", loExpr)
	Endfunc

	Hidden Function functionDeclaration(tcKind)
		Local loName, loParams, loBody
		loName = This.consume(TT_IDENTIFIER, "Expect " + tcKind + " name.")
		This.consume(TT_LPAREN, "Expect '(' after " + tcKind + " name.")

		loParams = Createobject('Collection')
		If !This.Check(TT_RPAREN)
			loParams.Add(This.consume(TT_IDENTIFIER, "Expect parameter name."))
			Do While This.match(TT_COMMA)
				loParams.Add(This.consume(TT_IDENTIFIER, "Expect parameter name."))
			Enddo
		Endif
		This.consume(TT_RPAREN, "Expect ')' after parameters.")
		* Parse Body
		This.consume(TT_LBRACE, "Expect '{' before " + tcKind + " body.")
		loBody = This.blockStatement()

		Return Createobject("FunctionNode", loName, loParams, loBody)
	Endfunc

	Hidden Function blockStatement
		Local loStatements
		loStatements = Createobject('Collection')

		Do While !This.Check(TT_RBRACE) And !This.isAtEnd
			loStatements.Add(This.declaration())
		Enddo

		This.consume(TT_RBRACE, "Expect '}' after block.")

		Return loStatements
	Endfunc

	* ==========================================================
	* Parsing Expression
	Hidden Function Expression
		Return This.assignment()
	Endfunc

	Hidden Function assignment
		Local loExpr
		loExpr = This.logicalOr()

		If This.match(TT_SIMPLE_ASSIGN)
			Local loEquals, loValue
			loEquals = This.previous && catch the equal token
			loValue = This.assignment() && self recursive
			Do Case
			Case loExpr.Class == 'Variable' &&
				Return Createobject("Assign", loExpr.oName, loValue)
			Case loExpr.Class == 'Get'
				Return Createobject("Set", loExpr.oObject, loExpr.oName, loValue)
			Otherwise
				This.parseError(loEquals, "Invalid assignment target.")
			Endcase
		Endif

		Return loExpr
	Endfunc

	Hidden Function logicalOr
		Local loLeft
		loLeft = This.logicalAnd()
		Do While This.match(TT_LOGICAL_OR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.logicalAnd()
			loLeft  = Createobject("Logical", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function logicalAnd
		Local loLeft
		loLeft = This.equality()
		Do While This.match(TT_LOGICAL_AND)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.equality()
			loLeft  = Createobject("Logical", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function equality
		Local loLeft
		loLeft = This.comparison()
		Do While This.match(TT_EQUALITY_OPERATOR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.comparison()
			loLeft  = Createobject("Binary", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function comparison
		Local loLeft
		loLeft = This.Term()
		Do While This.match(TT_RELATIONAL_OPERATOR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.Term()
			loLeft  = Createobject("Binary", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function Term
		Local loLeft
		loLeft = This.factor()
		Do While This.match(TT_TERM_OPERATOR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.factor()
			loLeft  = Createobject("Binary", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function factor
		Local loLeft
		loLeft = This.unary()
		Do While This.match(TT_FACTOR_OPERATOR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.unary()
			loLeft  = Createobject("Binary", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function unary
		If This.match(TT_TERM_OPERATOR, TT_LOGICAL_NOT)
			Return Createobject("Unary", This.previous, This.unary())
		Endif
		Return This.Call()
	Endfunc

	Hidden Function finishCall(toCallee)
		Local loArguments, loParen
		loArguments = Createobject('Collection')
		If !This.Check(TT_RPAREN)
			loArguments.Add(This.Expression())
			Do While This.match(TT_COMMA)
				loArguments.Add(This.Expression())
			Enddo
		Endif
		loParen = This.consume(TT_RPAREN, "Expect ')' after arguments.")
		Return Createobject("Call", toCallee, loParen, loArguments)
	Endfunc

	Hidden Function Call
		Local loExpr, loName, loBlock
		loExpr = This.Primary()

		Do While TRUE
			Do Case
			Case This.match(TT_LPAREN)
				loExpr = This.finishCall(loExpr)
			Case This.match(TT_DOT)
				loName = This.consume(TT_IDENTIFIER, "Expect property name after '.'.")
				loExpr = Createobject("Get", loExpr, loName)
			Case this.match(TT_ARROW)
				Local loKeyword
				loKeyword = this.previous
				* check parameters type (all types must be variables)
				For each loParam in loExpr.oExpressions
					If loParam.class != 'Variable'
						This.parseError(loKeyword, "Invalid parameter type in function definition.")
					EndIf
				EndFor
				* Parse Body
				This.consume(TT_LBRACE, "Expect '{' before function body.")
				loBody = This.blockStatement()
				loExpr = CreateObject("FunctionArrow", loKeyword, loExpr.oExpressions, loBody)
			Otherwise
				Exit
			Endcase
		Enddo
		Return loExpr
	Endfunc

	Hidden Function Primary
		Do Case
		Case This.match(TT_FALSE)
			Return Createobject("Literal", false)
		Case This.match(TT_TRUE)
			Return Createobject("Literal", TRUE)
		Case This.match(TT_NULL)
			Return Createobject("Literal", .Null.)
		Case This.match(TT_NUMBER, TT_STRING)
			Return Createobject("Literal", This.previous.literal)
		Case This.match(TT_SUPER)
			Local loKeyword, loMethod
			loKeyword = This.previous
			This.consume(TT_DOT, "Expect '.' after 'super'.")
			loMethod = This.consume(TT_IDENTIFIER, "Expect superclass method name.")
			Return Createobject("Super", loKeyword, loMethod)
		Case This.match(TT_THIS)
			Return Createobject("ThisNode", This.previous)
		Case This.match(TT_IDENTIFIER)
			Return Createobject("Variable", This.previous)
		Case This.match(TT_LPAREN)
			Local loExpressions
			loExpressions = CreateObject('Collection')
			If !this.check(TT_RPAREN)
				loExpressions.Add(This.Expression())
				Do while this.match(TT_COMMA)
					loExpressions.Add(This.Expression())
				EndDo
			EndIf
			This.consume(TT_RPAREN, "Expect ')' after expression.")
			Return Createobject("Grouping", loExpressions)
		CASE This.match(TT_NEW)
			Return this.parseNewExpression()			
		Case This.match(TT_LBRACKET)
			Return This.parseArrayLiteral()
		Case This.match(TT_LBRACE)
			Return This.parseObject()
		Case This.match(TT_FUNCTION)		
			Return This.parseFunctionExpr()
		Case this.match(TT_CONTEXT)
			Return CreateObject("Context", this.previous)
		Otherwise
			This.parseError(This.peek, "Expect expression.")
		Endcase
	Endfunc

	Hidden Function parseArrayLiteral
		Local loKeyword, loElements
		loKeyword = This.previous && token '['
		loElements = Createobject('Dictionary')
		If !This.Check(TT_RBRACKET)
			loElements.Add(This.Expression())
			Do While This.match(TT_COMMA)
				loElements.Add(This.Expression())
			Enddo
		Endif
		This.consume(TT_RBRACKET, "Expect ']' after array elements.")

		Return Createobject("ArrayLiteral", loKeyword, loElements)
	Endfunc

	Hidden Function parseObject
		Local loKeyword, loElements, loProperty, loValue
		loKeyword = This.previous && token '{'
		loElements = Createobject('Dictionary')
		If !This.Check(TT_RBRACE)
			loProperty = This.Expression()
			If !This.checkProperty(loProperty)
				This.parseError(loKeyword, "Invalid property name.")
			Endif
			This.consume(TT_COLON, "Expect ':' after property name.")
			loValue = This.Expression()
			loElements.Add(loValue, This.getPropertyName(loProperty))

			Do While This.match(TT_COMMA)
				loProperty = This.Expression()
				If !This.checkProperty(loProperty)
					This.parseError(loKeyword, "Invalid property name.")
				Endif
				This.consume(TT_COLON, "Expect ':' after property name.")
				loValue = This.Expression()
				loElements.Add(loValue, This.getPropertyName(loProperty))
			Enddo
		Endif
		This.consume(TT_RBRACE, "Expect '}' after array elements.")

		Return Createobject("ObjectLiteral", loKeyword, loElements)
	Endfunc

	Hidden Function parseFunctionExpr
		Local loParams, loBody
		This.consume(TT_LPAREN, "Expect '(' after function name.")

		loParams = Createobject('Collection')
		If !This.Check(TT_RPAREN)
			loParams.Add(This.consume(TT_IDENTIFIER, "Expect parameter name."))
			Do While This.match(TT_COMMA)
				loParams.Add(This.consume(TT_IDENTIFIER, "Expect parameter name."))
			Enddo
		Endif
		This.consume(TT_RPAREN, "Expect ')' after parameters.")
		* Parse Body
		This.consume(TT_LBRACE, "Expect '{' before function body.")
		loBody = This.blockStatement()

		Return Createobject("FunctionExpr", loParams, loBody)
	EndFunc
	
	Hidden function parseNewExpression
		Local loKeyword, loClassName, loArguments
		loKeyword = this.previous
		loClassName = this.consume(TT_IDENTIFIER, "Expect class name.")
		this.consume(TT_LPAREN, "Expect '(' after class name.")
		loArguments = CreateObject('Collection')
		If !This.Check(TT_RPAREN)
			loArguments.Add(this.expression())
			Do While This.match(TT_COMMA)
				loArguments.Add(this.expression())
			Enddo
		Endif
		this.consume(TT_RPAREN, "Expect ')' after arguments")
		
		Return CreateObject("New", loKeyword, loClassName, loArguments)	
	EndFunc

	Hidden Function checkProperty(toProperty)
		If toProperty.Class = 'Literal'
			Return Type('toProperty.oValue') == 'C'
		Endif
		Return toProperty.Class = 'Variable'
	Endfunc

	Hidden Function getPropertyName(toProperty)
		If toProperty.Class = 'Literal'
			Return toProperty.oValue
		Endif
		Return toProperty.oName.lexeme
	Endfunc
	* ==========================================================

	Hidden Function match(tnType1, tnType2, tnType3)
		If This.Check(tnType1)
			This.advance()
			Return TRUE
		Endif
		If !Empty(tnType2) And This.Check(tnType2)
			This.advance()
			Return TRUE
		Endif
		If !Empty(tnType3) And This.Check(tnType3)
			This.advance()
			Return TRUE
		Endif
		Return false
	Endfunc

	Hidden Function consume(tnType, tcErrorMsg)
		If This.Check(tnType)
			Return This.advance()
		Endif
		This.parseError(This.peek, tcErrorMsg)
	Endfunc

	Hidden Function Check(tnType)
		If This.isAtEnd
			Return false
		Endif
		Return This.peek.Type == tnType
	Endfunc

	Hidden Function advance
		If !This.isAtEnd
			This.nCurrent = This.nCurrent + 1
		Endif
		Return This.previous
	Endfunc

	Hidden Function isAtEnd_access
		Return This.peek.Type == TT_EOF
	Endfunc

	Hidden Function peek_access
		Return This.oTokens.Item(This.nCurrent)
	Endfunc

	Hidden Function peekNext(tnOffset)
		If (This.nCurrent + tnOffset) < This.oTokens.Count
			Return This.oTokens.Item(This.nCurrent+tnOffset).Type
		Endif
		Return TT_EOF
	Endfunc

	Hidden Function previous_access
		Return This.oTokens.Item(This.nCurrent-1)
	Endfunc

	Hidden Procedure parseError(toToken, tcErrorMessage)
		_Screen.FoxScript.errorToken(toToken, tcErrorMessage)
		_Screen.FoxScript.throwError('ParseError', 'token', toToken, tcErrorMessage)
	Endproc

	Hidden Function synchronize
		This.advance()
		Do While !This.isAtEnd
			If This.previous.Type == TT_SEMICOLON
				Return
			Endif
			If Inlist(This.peek.Type, ;
					TT_CLASS, ;
					TT_FUNCTION, ;
					TT_VAR, ;
					TT_LET, ;
					TT_FOR, ;
					TT_IF, ;
					TT_WHILE, ;
					TT_PRINT, ;
					TT_RETURN)
				Return
			Endif
			This.advance()
		Enddo
	Endfunc
Enddefine
#ifndef CONSTANT_ADDED
	#include "FoxScript.h"
#endif
* =========================================
* The Parser Class
Define Class Parser As Custom
	Hidden oTokens
	Hidden nCurrent
	Hidden previous
	Hidden peek
	Hidden isAtEnd

	Function init(toTokens)
		this.oTokens = toTokens
		this.nCurrent = 1 && First recognised token		
	Endfunc

	Function parse
		Local loStatements
		loStatements = Createobject('Collection')

		Do While !This.isAtEnd
			loStatements.Add(this.declaration())
		Enddo

		Return loStatements
	EndFunc
	
	Hidden Function declaration
		Local loDeclaration
		loDeclaration = .Null.
		Try
			Do case
			case this.match(TT_CLASS)
				loDeclaration = this.classDeclaration()
			Case this.match(TT_FUNCTION)
				loDeclaration = this.functionDeclaration("function")
			Case this.match(TT_VAR)
				loDeclaration = this.varDeclaration()
			Otherwise
				loDeclaration = this.statement()
			endcase
		Catch to loEx
			If Type('loEx.Type') == 'C'
				this.sinchronize()
			Else
				* DEBUG
				MessageBox(loEx.message, 16)
				* DEBUG
			EndIf
		EndTry
		Return loDeclaration
	EndFunc
	
	Hidden Function classDeclaration
		Return .null.
	EndFunc
	
	Hidden Function statement
		Local loStatement
		Do case
		Case this.match(TT_FOR)
			loStatement = this.forStatement()
		Case this.match(TT_IF)
			loStatement = this.ifStatement()
		Case this.match(TT_PRINT)
			loStatement = this.printStatement()
		Case this.match(TT_RETURN)
			loStatement = this.returnStatement()
		Case this.match(TT_WHILE)
			loStatement = this.whileStatement()
		Case this.match(TT_LBRACE)
			loStatement = CreateObject("Block", this.blockStatement())
		Otherwise 
			loStatement = this.expressionStatement()
		EndCase 
		Return loStatement
	EndFunc
	
	Hidden function forStatement
		Return .null.
	EndFunc
	
	Hidden function ifStatement
		Return .null.
	EndFunc
	
	Hidden function printStatement
		Return .null.
	EndFunc
	
	Hidden function returnStatement
		Return .null.
	EndFunc
	
	Hidden function varDeclaration
		Return .null.
	EndFunc
	
	Hidden function whileStatement
		Return .null.
	EndFunc
	
	Hidden function expressionStatement
		Local loExpr
		loExpr = this.expression()
		this.consume(TT_SEMICOLON, "Expect ';' after expression.")
		Return CreateObject("Expression", loExpr)
	EndFunc
	
	Hidden function functionDeclaration
		Return .null.
	EndFunc
	
	Hidden function blockStatement
		Return .null.
	EndFunc
	
	* ==========================================================
	* Parsing Expression
	Hidden function expression
		Return this.assignment()
	EndFunc
	
	Hidden function assignment
		Local loExpr
		loExpr = this.logicalOr()
		
		If this.match(TT_SIMPLE_ASSIGN)
			Local loEquals, loValue
			loEquals = this.previous && catch the equal token
			loValue = this.assignment() && self recursive
			Do case
			case loExpr.class == 'Variable' && 
				Return CreateObject("Assign", loExpr.oName, loValue)
			Case loExpr.class == 'Get'
				Return CreateObject("Set", loExpr.oName, loExpr.oValue)
			Otherwise
				this.parseError(loEquals, "Invalid assignment target.")
			endcase
		EndIf
		
		Return loExpr
	endfunc
	
	Hidden function logicalOr
		Local loLeft
		loLeft = this.logicalAnd()
		Do while this.match(TT_LOGICAL_OR)
			Local loOpe, loRight
			loOpe   = this.previous
			loRight = this.logicalAnd()
			loLeft  = CreateObject("Logical", loLeft, loOpe, loRight)
		EndDo
		Return loLeft
	EndFunc

	Hidden function logicalAnd
		Local loLeft
		loLeft = this.equality()
		Do while this.match(TT_LOGICAL_AND)
			Local loOpe, loRight
			loOpe   = this.previous
			loRight = this.equality()
			loLeft  = CreateObject("Logical", loLeft, loOpe, loRight)
		EndDo
		Return loLeft
	EndFunc
	
	Hidden function equality
		Local loLeft
		loLeft = this.comparison()
		Do while this.match(TT_EQUALITY_OPERATOR)
			Local loOpe, loRight
			loOpe   = this.previous
			loRight = this.comparison()
			loLeft  = CreateObject("Binary", loLeft, loOpe, loRight)
		EndDo
		Return loLeft
	EndFunc

	Hidden function comparison
		Local loLeft
		loLeft = this.term()
		Do while this.match(TT_RELATIONAL_OPERATOR)
			Local loOpe, loRight
			loOpe   = this.previous
			loRight = this.term()
			loLeft  = CreateObject("Binary", loLeft, loOpe, loRight)
		EndDo
		Return loLeft
	EndFunc

	Hidden function term
		Local loLeft
		loLeft = this.factor()
		Do while this.match(TT_TERM_OPERATOR)
			Local loOpe, loRight
			loOpe   = this.previous
			loRight = this.factor()
			loLeft  = CreateObject("Binary", loLeft, loOpe, loRight)
		EndDo
		Return loLeft
	EndFunc

	Hidden function factor
		Local loLeft
		loLeft = this.unary()
		Do while this.match(TT_FACTOR_OPERATOR)
			Local loOpe, loRight
			loOpe   = this.previous
			loRight = this.unary()
			loLeft  = CreateObject("Binary", loLeft, loOpe, loRight)
		EndDo
		Return loLeft
	EndFunc

	Hidden function unary
		If this.match(TT_TERM_OPERATOR, TT_LOGICAL_NOT)		
			Return CreateObject("Unary", this.previous, this.unary())
		EndIf
		Return this.call()
	EndFunc

	Hidden function finishCall(toCallee)
		Local loArguments, loParen
		If !this.check(TT_RPAREN)
			loArguments.add(this.expression())
			Do while this.match(TT_COMMA)
				loArguments.add(this.expression())
			enddo
		EndIf
		loParen = this.consume(TT_RPAREN, "Expect ')' after arguments.")
		Return CreateObject("Call", toCallee, loParen, loArguments)
	endfunc
	
	Hidden function call
		Local loExpr, loName
		loExpr = this.primary()
		
		Do while true
			Do case
			case this.match(TT_LPAREN)
				loExpr = this.finishCall(loExpr)
			Case this.match(TT_DOT)
				loName = this.consume(TT_IDENTIFIER, "Expect property name after '.'.")
				loExpr = CreateObject("Get", loExpr, loName)
			Otherwise
				exit
			EndCase
		EndDo
		Return loExpr
	EndFunc
	
	Hidden function primary	
		Set Step On	
		Do case
		Case this.match(TT_FALSE)
			Return CreateObject("Literal", false)
		Case this.match(TT_TRUE)
			Return CreateObject("Literal", true)
		Case this.match(TT_NULL)
			Return CreateObject("Literal", .null.)
		Case this.match(TT_SUPER)
			Local loKeyword, loMethod
			loKeyword = this.previous
			this.consume(TT_DOT, "Expect '.' after 'super'.")
			loMethod = this.consume(TT_IDENTIFIER, "Expect superclass method name.")
			Return CreateObject("Super", loKeyword, loMethod)
		Case this.match(TT_THIS)
			Return CreateObject("ThisNode", this.previous)
		Case this.match(TT_IDENTIFIER)
			Return CreateObject("Variable", this.previous)
		Case this.match(TT_LPAREN)
			Local loExpression
			loExpression = this.expression()
			this.consume(TT_RPAREN, "Expect ')' after expression.")
			Return CreateObject("Grouping", loExpression)
		Otherwise
			this.parseError(this.peek, "Expect expression.")
		EndCase
	EndFunc
	* ==========================================================

	Hidden Function match(tnType1, tnType2, tnType3)
		If this.check(tnType1)
			this.advance()
			Return true
		EndIf
		If !Empty(tnType2) and this.check(tnType2)
			this.advance()
			Return true
		EndIf
		If !Empty(tnType3) and this.check(tnType3)
			this.advance()
			Return true
		EndIf
		Return false
	endfunc

	Hidden Function consume(tnType, tcErrorMsg)
		If this.check(tnType)
			Return this.advance()
		EndIf
		this.parseError(this.peek, tcErrorMsg)
	EndFunc

	Hidden Function check(tnType)
		If this.isAtEnd
			Return false
		EndIf
		Return this.peek.type == tnType
	EndFunc
	
	Hidden Function advance
		If !this.isAtEnd
			this.nCurrent = this.nCurrent + 1
		EndIf
		Return this.previous
	EndFunc

	Hidden Function isAtEnd_access
		Return This.peek.Type == TT_EOF
	EndFunc

	Hidden Function peek_access
		Return This.oTokens.Item(this.nCurrent)
	EndFunc
	
	Hidden Function previous_access
		Return This.oTokens.Item(this.nCurrent-1)
	Endfunc
	
	Hidden Procedure parseError(toToken, tcErrorMessage)
		Local oExp
		Try
			Throw
		Catch To oExp
			=AddProperty(oExp, 'type', 'ParseError')
			=AddProperty(oExp, 'token', toToken)
			oExp.Message		= tcErrorMessage
			Throw
		Endtry
	endproc
	
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
* ================================================================================== *
* Interpreter Class
* ================================================================================== *
#ifndef CONSTANTS_LOADED
	#include "FoxScript.h"
#endif

Define Class Interpreter As Custom
	oGlobals = .Null.
	oEnvironment = .Null.
	oLocals = .Null.
	oRegEx = .null.
	nDepth = 0
	cFunctionList = ''
	cClassList = ''
	cInitialization = ''
	
	Function Init
		with this
			.cInitialization = ''
			.cFunctionList = ''
			.cClassList = ''
			.oRegEx = CreateObject("VBScript.RegExp")
			.oRegEx.IgnoreCase = .F.
			.oRegEx.Global = .T.
			
			.oGlobals = Createobject("Environment", .Null.)
			.oEnvironment = This.oGlobals
			.oLocals = Createobject("Dictionary")		
		endwith
	Endfunc

	Function interpret(toStatements)
		Local lcOutput
		lcOutput = ''
		Try
			For Each loStatement In toStatements
				lcOutput = lcOutput + This.Execute(loStatement)
			Endfor
		Catch To loEx
			If Type('loEx.Type') == 'C'
				_Screen.FoxScript.runtimeError(loEx)
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
		Return lcOutput
	Endfunc

	Function Evaluate(toExpr)
		Return toExpr.Accept(This)
	Endfunc

	Function Execute(toStmt)
		Return toStmt.Accept(This)
	Endfunc

	Function resolve(toExpr, tnDepth)
		This.oLocals.put(toExpr.cHash, tnDepth)
	Endfunc

	Function executeBlock(toStatements)
		Local loResult
		loResult = ''		
		For Each loStatement In toStatements
			loResult = loResult + Replicate(Space(4), this.nDepth) + This.Execute(loStatement)
		Endfor
		Return loResult
	Endfunc

	Function visitArrayLiteralExpr(toExpr)
		Local loArray
		loArray = Createobject('Dictionary')
		For Each loElement In toExpr.oElements
			loArray.Add(This.Evaluate(loElement))
		Endfor

		Return loArray
	Endfunc

	Function visitBlockStmt(toStmt)
		this.nDepth = this.nDepth + 1
		Return This.executeBlock(toStmt.oStatements)
	Endfunc

	Function visitClassNodeStmt(toStmt)
		LOCAL lcOutput, lcHeader, i, lcClassBody, lcProperties, lparseFields
		lcProperties = ''
		lcClassBody = ''
		lcHeader = 'DEFINE CLASS ' + toStmt.oName.lexeme + ' AS '
		If !Isnull(toStmt.oSuperclass)
			lcHeader = lcHeader + toStmt.oSuperclass.oName		
		Else
			lcHeader = lcHeader + 'CUSTOM' && all base classes will inherit from Custom
		Endif		

		For Each loMethod In toStmt.oMethods
			lcClassBody = lcClassBody + CRLF + 'FUNCTION ' + loMethod.oName.lexeme + '('
			i = 0
			For each loParam in loMethod.oParams
				i = i + 1
				If i > 1
					lcClassBody = lcClassBody + ', '
				EndIf
				lcClassBody = lcClassBody + loParam.lexeme
			EndFor
			lcClassBody = lcClassBody + ')'
			If loMethod.oName.lexeme == 'init'
				For each loStmt in loMethod.oBody
					if loStmt.Class == "Expression" and loStmt.oExpression.Class == "Set"
						lcProperties = lcProperties + loStmt.oExpression.oName.lexeme + ' = .null.' + CRLF
					EndIf
				EndFor
			EndIf
			lcClassBody = lcClassBody + CRLF + this.executeBlock(loMethod.oBody) + CRLF
			lcClassBody = lcClassBody + 'ENDFUNC' + CRLF
		Endfor

		lcOutput = lcHeader + CRLF + lcProperties + lcClassBody + CRLF + 'ENDDEFINE' + CRLF
		this.cClassList = this.cClassList + lcOutput
		
		RETURN ''
	Endfunc

	Function visitExpressionStmt(toStmt)
		Return This.Evaluate(toStmt.oExpression) + CRLF
	Endfunc

	Function visitFunctionNodeStmt(toStmt)
		this.nDepth = this.nDepth + 1
		Local lcOutput, i
		lcOutput = 'FUNCTION ' + toStmt.oName.lexeme + '('
		i = 0
		For each loParam in toStmt.oParams
			i = i + 1
			If i > 1
				lcOutput = lcOutput + ', '
			EndIf
			lcOutput = lcOutput + loParam.lexeme
		EndFor
		lcOutput = lcOutput + ')'
		lcOutput = lcOutput + CRLF + this.executeBlock(toStmt.oBody) + CRLF
		lcOutput = lcOutput + 'ENDFUNC' + CRLF
		this.cFunctionList = this.cFunctionList +  lcOutput
		
		RETURN ''
	Endfunc

	Function visitIfNodeStmt(toStmt)
		Local lcOutput
		lcOutput = 'IF ' + This.Evaluate(toStmt.oCondition) + ' THEN' + CRLF
		lcOutput = lcOutput + This.Execute(toStmt.oThenBranch)
		If !Isnull(toStmt.oElseBranch)
			lcOutput = lcOutput + 'ELSE' + CRLF
			lcOutput = lcOutput + This.Execute(toStmt.oElseBranch)
		EndIf
		lcOutput = lcOutput + 'ENDIF' + CRLF
		Return lcOutput

*!*			If This.isTruthy(This.Evaluate(toStmt.oCondition))
*!*				This.Execute(toStmt.oThenBranch)
*!*			Else
*!*				If !Isnull(toStmt.oElseBranch)
*!*					This.Execute(toStmt.oElseBranch)
*!*				Endif
*!*			Endif
*!*			Return lcOutput
	Endfunc

	Function visitObjectLiteralExpr(toExpr)
		Local loInstance
		loInstance = Createobject('RuntimeObject', .Null.)

		Local i, lcKey, loValue
		For i=1 To toExpr.oElements.Count
			lcKey = toExpr.oElements.GetKey(i)
			loValue = This.Evaluate(toExpr.oElements.Item(i))
			loInstance.oFields.put(lcKey, loValue)
		Endfor

		Return loInstance
	Endfunc

	Function visitPrintStmt(toStmt)
		Local loValue, lcOutput
		loValue = This.Evaluate(toStmt.oExpression)
		lcOutput = ''
		If Type('loValue') == 'O' and Type('loValue.Tag') == 'C' and loValue.Tag == 'Grouping'
			For each loVal in loValue
				lcOutput = lcOutput + CRLF + 'MESSAGEBOX(' + loVal + ')'
			EndFor
		Else			
			lcOutput = 'MESSAGEBOX(' + loValue + ')'
		EndIf
		Return lcOutput + CRLF
	Endfunc

	Function visitReturnNodeStmt(toStmt)
		Local loValue
		loValue = ''
		If !Isnull(toStmt.oValue)
			loValue = This.Evaluate(toStmt.oValue)
		Endif
		Return 'RETURN ' + loValue + CRLF
	Endfunc

	Function visitVarStmt(toStmt)
		Local lcValue, lcOutput
		lcOutput = 'PRIVATE ' + toStmt.oName.lexeme + CRLF
		If !Isnull(toStmt.oInitializer)
			lcValue = This.Evaluate(toStmt.oInitializer)
			lcOutput = lcOutput + toStmt.oName.lexeme + ' = ' + lcValue + CRLF
			If toStmt.oInitializer.class == 'Functionexpr'
				* visitar las variables
				This.oEnvironment.Define(toStmt.oName.lexeme, lcValue)
			EndIf
		EndIf
		Return lcOutput
	Endfunc

	Function visitWhileNodeStmt(toStmt)
		return 'DO WHILE ' + This.Evaluate(toStmt.oCondition) + CRLF + This.Execute(toStmt.oBody) + 'ENDDO' + CRLF
	Endfunc

	Function visitAssignExpr(toExpr)
		Local loValue
		loValue = This.Evaluate(toExpr.oValue)

		Return toExpr.oName.lexeme + ' = ' + loValue + CRLF
	Endfunc

	Function visitBinaryExpr(toExpr)
		Local loLeft, loRight
		loLeft = This.Evaluate(toExpr.oLeft)
		loRight = This.Evaluate(toExpr.oRight)

		Do Case
		Case toExpr.oOperator.category == TC_NOT_EQ
			Return loLeft + ' != ' + loRight
		Case toExpr.oOperator.category == TC_EQUAL
			Return loLeft + ' == ' + loRight
		Case toExpr.oOperator.category == TC_GREATER
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' > ' + loRight
		Case toExpr.oOperator.category == TC_GREATER_EQ
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' >= ' + loRight
		Case toExpr.oOperator.category == TC_LESS
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' < ' + loRight
		Case toExpr.oOperator.category == TC_LESS_EQ
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' <= ' + loRight
		Case toExpr.oOperator.category == TC_MINUS
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' - ' + loRight
		Case toExpr.oOperator.category == TC_PLUS
			Local lcLeftType, lcRightType
			lcLeftType = this.getType(loLeft)
			lcRightType = this.getType(loRight)
			If !InList(lcLeftType, 'C', 'N', 'W') and !InList(lcRightType, 'C', 'N', 'W')
				This.runtimeError(toExpr.oOperator, "Operands must be two numbers or two strings.")
			EndIf
			
			If lcLeftType == lcRightType
				Return loLeft + ' + ' + loRight
			EndIf
			If lcLeftType == 'C'
				Return loLeft + ' + TRANSFORM(' + loRight + ')'
			EndIf
			If lcLeftType == 'N' and lcRightType == 'C'
				Return loLeft + ' + VAL(' + loRight + ')'
			EndIf
			Return loLeft + ' + ' + loRight
			* This.runtimeError(toExpr.oOperator, "Operands must be two numbers or two strings.")
		Case toExpr.oOperator.category == TC_DIV
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' / ' + loRight
		Case toExpr.oOperator.category == TC_MUL
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' * ' + loRight
		Endcase
		Return '.Null.'
	Endfunc

	Function visitCallExpr(toExpr)
		Local lcOutput, i, lcArguments
		lcOutput = This.Evaluate(toExpr.oCallee)
		i = 0
		lcArguments = ''
		For Each loArgument In toExpr.oArguments
			i = i + 1
			If i > 1
				lcArguments = lcArguments + ','
			EndIf
			lcArguments = lcArguments + This.Evaluate(loArgument)
		EndFor

		If '.execute(:PARAMS)'$lcOutput
			Return 'EVALUATE(' + Strtran(lcOutput, ':PARAMS', lcArguments) + ')'
		Else			
			Return lcOutput + '(' + lcArguments + ')'
		EndIf
	Endfunc

	Function visitContextExpr(toExpr)
		Return This.oGlobals.Get(toExpr.oKeyword)
	Endfunc

	Function visitGetExpr(toExpr)
		Local loObject
		loObject = This.Evaluate(toExpr.oObject)
		Return loObject + '.' + toExpr.oName.lexeme
	Endfunc

	Function visitFunctionArrowExpr(toExpr)
		Local loFunction
		loFunction = Createobject("RuntimeFunction", toExpr, This.oEnvironment, false)
		If Type('loFunction') != 'O' Or loFunction.ParentClass != 'Callable'
			This.runtimeError(toExpr.oParen, "The current object is not a callable object.")
		Endif
		Return loFunction
	Endfunc

	Function visitFunctionExpr(toExpr)
		* FunctionExpr
		* var saludo = () => { return "Hola mundo"; };
		* // lo anterior se convierte a Fox:
		* DEFINE CLASS RandomName AS CUSTOM
		*	FUNCTION execute
		*		RETURN "Hola mundo"
		*	ENDFUNC
		* ENDDEFINE
		*		
		* 1. Crear clase y la instancia
		* 2. Asignar asignar el método.
		* Create the runtime function object and installs it in the current environment.
		Local lcClassDefinition, i, lcClassName
		lcClassName = Sys(2015)
		lcClassDefinition = 'DEFINE CLASS ' + lcClassName + ' AS CUSTOM' + CRLF
		lcClassDefinition = lcClassDefinition + Space(4) + 'FUNCTION execute('
		i = 0
		* function parameters
		For each loParam in toExpr.oParams
			i = i + 1
			If i > 1
				lcClassDefinition = lcClassDefinition + ','
			EndIf
			lcClassDefinition = lcClassDefinition + loParam.lexeme
		EndFor
		lcClassDefinition = lcClassDefinition + ')' + CRLF
		* function body
		lcClassDefinition = lcClassDefinition + this.executeBlock(toExpr.oBody)
		lcClassDefinition = lcClassDefinition + Space(4) + 'ENDFUNC' + CRLF
		lcClassDefinition = lcClassDefinition + 'ENDDEFINE' + CRLF
		this.cClassList = this.cClassList + lcClassDefinition
		Local lcOutput
		Text to lcOutput noshow pretext 7 textmerge
			Public go<<lcClassName>>
			go<<lcClassName>> = CreateObject("<<lcClassName>>")			
		EndText
		this.cInitialization = this.cInitialization + lcOutput
		Return '[go' + lcClassName + '.execute(:PARAMS)]'
	EndFunc
	
	Function visitGroupingExpr(toExpr)
		Local loResult
		loResult = CreateObject('Collection')
		loResult.tag = 'Grouping'
		For each loExpr in toExpr.oExpressions
			loResult.Add(This.Evaluate(loExpr))
		EndFor
		Return loResult
	Endfunc

	Function visitLiteralExpr(toExpr)
		Return this.stringify(toExpr.oValue)
	Endfunc

	Function visitLogicalExpr(toExpr)
		Local loLeft, loRight, lcOpe
		loLeft = This.Evaluate(toExpr.oLeft)
		loRight = this.Evaluate(toExpr.oRight)
		If toExpr.oOperator.lexeme == '||'
			lcOpe = ' OR '
		Else
			lcOpe = ' AND '
		EndIf
		Return loLeft + lcOpe + loRight	
	Endfunc

	Function visitNewExpr(toExpr)
		Local lcOutput
		lcOutput = 'CREATEOBJECT("' + toExpr.oClassName.lexeme + '"'
		For each loArg in toExpr.oArguments
			lcOutput = lcOutput + ', '
			lcOutput = lcOutput + this.Evaluate(loArg)
		EndFor
		lcOutput = lcOutput + ')'
		
		Return lcOutput
	Endfunc

	Function visitSetExpr(toExpr)
		Local loObject, loValue
		loObject = This.Evaluate(toExpr.oObject)		
		loValue = This.Evaluate(toExpr.oValue)
		Return loObject + '.' + toExpr.oName.lexeme + ' = ' + loValue + CRLF
	Endfunc

	Function visitSuperExpr(toExpr)
		Local lnDistance, loSuperclass, loObject, loMethod

		lnDistance = This.oLocals.Get(toExpr.cHash)
		loSuperclass = This.oEnvironment.getAt(lnDistance, "super")
		loObject = This.oEnvironment.getAt(lnDistance-1, "this")
		loMethod = loSuperclass.findMethod(toExpr.oMethod.lexeme)

		If !Isnull(loMethod)
			This.runtimeError(toExpr.oMethod, "Undefined property '" + toExpr.oMethod.lexeme + "'.")
		Endif

		Return loMethod.Bind(loObject)
	Endfunc

	Function visitThisNodeExpr(toExpr)
		Return 'THIS'
	Endfunc

	Function visitUnaryExpr(toExpr)
		Local lcRight
		lcRight = This.Evaluate(toExpr.oRight)
		Do Case
		Case toExpr.oOperator.category == TC_BANG
			Return '!' + lcRight
		Case toExpr.oOperator.category == TC_MINUS
			This.checkNumberOperand(toExpr.oOperator, lcRight)
			Return '-' + lcRight
		Case toExpr.oOperator.category == TC_PLUS
			This.checkNumberOperand(toExpr.oOperator, lcRight)
			Return lcRight
		Endcase

		Return '.Null.'
	Endfunc

	Function visitVariableExpr(toExpr)	
		Local lcValue
		lcValue = This.oEnvironment.Get(toExpr.oName)
*!*				Return '&' + toExpr.oName.lexeme
		If !Empty(lcValue)
			Return lcValue
		EndIf
		Return toExpr.oName.lexeme
	Endfunc

	Function lookUpVariable(toName, toExpr)
		Local lnDistance
		lnDistance = This.oLocals.Get(toExpr.cHash)
		If !Isnull(lnDistance)
			Return This.oEnvironment.getAt(lnDistance, toName.lexeme)
		Else
			Return This.oGlobals.Get(toName)
		Endif
	Endfunc

	Function getType(tcObject)
		DO CASE
		CASE this.isNumber(tcObject)
			Return 'N'
		CASE this.isString(tcObject)
			Return 'C'
		Case this.isIdent(tcObject)
			Return 'W'
		CASE this.isBoolean(tcObject)
			Return 'L'
		CASE this.isNull(tcObject)
			Return 'X'
		OTHERWISE
			Return 'U'
		ENDCASE
	EndFunc

	Function isNumber(tcObject)
		this.oRegEx.Pattern = '\d+'
		Return this.oRegEx.Test(tcObject)
	EndFunc
	
	Function isString(tcObject)
		Return InList(Left(tcObject,1), '"', "'") and InList(right(tcObject,1), '"', "'")
	EndFunc
	
	Function isBoolean(tcObject)
		Return InList(Lower(tcObject), '.f.', '.t.')
	EndFunc
	
	Function isNull(tcObject)
		Return Lower(tcObject) == '.null.'
	EndFunc	
	
	Function isIdent(tcObject)
		&& TODO(irwin): revisar el identifier.
		this.oRegEx.Pattern = '^[_a-zA-Z][_a-zA-Z0-9]*'
		Return this.oRegEx.Test(tcObject)
	EndFunc	

	Function checkNumberOperand(toOperator, tcOperand)
		If this.isNumber(tcOperand) or this.isIdent(tcOperand)
			Return
		Endif
		This.runtimeError(toOperator, "Operand must be a number.")
	Endfunc

	Function checkNumberOperands(toOperator, toLeft, toRight)
		If (this.isNumber(toLeft) or this.isIdent(toLeft)) and (this.isNumber(toRight) or this.isIdent(toRight))
			Return
		Endif
		This.runtimeError(toOperator, "Operands must be numbers.")
	Endfunc

	Function isTruthy(toObject)		
		If this.isNull(toObject)
			Return false
		EndIf
		
		If this.isBoolean(toObject)
			Return toObject
		Endif
		Return True
	Endfunc

	Function isEqual(toA, toB)
		If this.Isnull(toA) And this.Isnull(toB)
			Return TRUE
		Endif
		If this.Isnull(toA)
			Return false
		Endif
		Return toA == toB
	Endfunc

	Function stringify(toObject)
		Return Transform(toObject)
	Endfunc

	Hidden Procedure runtimeError(toToken, tcErrorMessage)
		*_Screen.FoxScript.errorToken(toToken, tcErrorMessage)
		_Screen.FoxScript.throwError('RuntimeError', 'token', toToken, tcErrorMessage)
	EndProc
	
Enddefine
* ================================================================================== *
* ASTPrinter class
* ================================================================================== *
#ifndef CONSTANTS_LOADED
	#include "FoxScript.h"
#endif
Define Class ASTPrinter As Custom

	Function Print(toStmt)
		Local lcOutput
		lcOutput = ''
		For Each loStmt In toStmt
			lcOutput = lcOutput + loStmt.Accept(This) + CRLF
		Endfor
		Return lcOutput
	Endfunc

	Function Evaluate(toExpr)
		Return toExpr.Accept(This)
	Endfunc

	Function visitArrayLiteralExpr(toExpr)
		Local lcOutput, i
		lcOutput = '['
		i = 0
		For Each loElement In toExpr.oElements
			i = i + 1
			If i > 1
				lcOutput = lcOutput + ','
			Endif
			lcOutput = lcOutput + This.Evaluate(loElement)
		Endfor

		lcOutput = lcOutput + ']'
		Return lcOutput
	Endfunc

	Function visitBlockStmt(toStmt)
		Local lcOutput
		lcOutput = '{' + CRLF

		For Each loStmt In toStmt.oStatements
			lcOutput = lcOutput + loStmt.Accept(This) + CRLF
		Endfor
		lcOutput = lcOutput + CRLF + '}'
		Return lcOutput
	Endfunc

	Function visitClassNodeStmt(toStmt)
		Local lcOutput
		lcOutput = 'class ' + toStmt.oName.lexeme
		If !Isnull(toStmt.oSuperclass)
			lcOutput = lcOutput + ' extends ' + toStmt.oSuperclass.oName.lexeme
		Endif

		lcOutput = lcOutput + ' {'
		If !Isnull(toStmt.oMethods)
			For Each loMethod In toStmt.oMethods
				lcOutput = lcOutput + loMethod.Accept(This) + CRLF
			Endfor
		Endif
		lcOutput = lcOutput + ' }'

		Return lcOutput
	Endfunc

	Function visitContextExpr(toExpr)
		Return '_ctx'
	EndFunc
	
	Function visitExpressionStmt(toStmt)
		Return This.Evaluate(toStmt.oExpression) + ';'
	Endfunc

	Function visitFunctionNodeStmt(toStmt)
		Local lcOutput
		lcOutput = "fn " + toStmt.oName.lexeme + "("
		If !Isnull(toStmt.oParams)
			Local i
			i = 0
			For Each loParam In toStmt.oParams
				i = i + 1
				If i > 1
					lcOutput = lcOutput + ','
				Endif
				lcOutput = lcOutput + loParam.lexeme
			Endfor
		Endif
		lcOutput = lcOutput + ')'

		If !Isnull(toStmt.oBody)
			lcOutput = lcOutput + CRLF + '{' + This.Print(toStmt.oBody) + CRLF + '}'
		Endif

		Return lcOutput
	Endfunc

	Function visitGroupingExpr(toExpr)
		Local lcOutput, i
		lcOutput = '('
		i = 0
		For each loExpr in toExpr.oExpressions
			i = i + 1
			If i > 1
				lcOutput = lcOutput + ','
			EndIf
			lcOutput = lcOutput + this.evaluate(loExpr)
		EndFor
		lcOutput = lcOutput + ')'
		Return lcOutput
	Endfunc

	Function visitIfNodeStmt(toStmt)
		Local lcOutput
		lcOutput = 'if (' + This.Evaluate(toStmt.oCondition) + ')' + CRLF
		If !Isnull(toStmt.oThenBranch)
			lcOutput = lcOutput + toStmt.oThenBranch.Accept(This)
		Endif

		If !Isnull(toStmt.oElseBranch)
			lcOutput = lcOutput + 'else '+ CRLF + toStmt.oElseBranch.Accept(This)
		Endif

		Return lcOutput
	Endfunc

	Function visitLiteralExpr(toExpr)
		Local lcValue
		If Isnull(toExpr.oValue)
			lcValue = "null"
		Endif
		If Type('toExpr.oValue') == 'C'
			lcValue = '"' + toExpr.oValue + '"'
		Else
			lcValue = Transform(toExpr.oValue)
		Endif
		Return lcValue
	Endfunc

	Function visitBinaryExpr(toExpr)
		Return '(' + toExpr.oLeft.Accept(This) + toExpr.oOperator.lexeme + toExpr.oRight.Accept(This) + ')'
	Endfunc

	Function visitGetExpr(toExpr)
		Return toExpr.oObject.Accept(This) + '.' + toExpr.oName.lexeme
	EndFunc
	
	Function visitFunctionArrowExpr(toExpr)
		Local lcOutput
		lcOutput = "("
		If !Isnull(toStmt.oParams)
			Local i
			i = 0
			For Each loParam In toStmt.oParams
				i = i + 1
				If i > 1
					lcOutput = lcOutput + ','
				Endif
				lcOutput = lcOutput + loParam.lexeme
			Endfor
		Endif
		lcOutput = lcOutput + ')=>'

		If !Isnull(toStmt.oBody)
			lcOutput = lcOutput + CRLF + '{' + This.Print(toStmt.oBody) + CRLF + '}'
		Endif

		Return lcOutput
	EndFunc

	Function visitFunctionExpr(toExpr)
		Local lcOutput
		lcOutput = "fn ("
		If !Isnull(toStmt.oParams)
			Local i
			i = 0
			For Each loParam In toStmt.oParams
				i = i + 1
				If i > 1
					lcOutput = lcOutput + ','
				Endif
				lcOutput = lcOutput + loParam.lexeme
			Endfor
		Endif
		lcOutput = lcOutput + ')'

		If !Isnull(toStmt.oBody)
			lcOutput = lcOutput + CRLF + '{' + This.Print(toStmt.oBody) + CRLF + '}'
		Endif

		Return lcOutput
	EndFunc

	Function visitObjectLiteralExpr(toExpr)
		Local lcOutput, i, lcKey, loValue
		lcOutput = '{'
		i = 0
		For i = 0 To toExpr.oElements.Count
			lcKey = toExpr.oElements.GetKey(i)
			loValue = This.Evaluate(toExpr.oElements.Item(i))
			If i > 1
				lcOutput = lcOutput + ','
			Endif
			lcOutput = lcOutput + lcKey + ':' + loValue
		Endfor

		lcOutput = lcOutput + '}'
		Return lcOutput
	Endfunc

	Function visitPrintStmt(toStmt)
		Return This.Evaluate(toStmt.oExpression) + ';'
	Endfunc

	Function visitReturnNodeStmt(toStmt)
		If !Isnull(toStmt.oValue)
			Return 'return ' + This.Evaluate(toStmt.oValue) + ';'
		Endif
		Return 'return'
	Endfunc

	Function visitSetExpr(toExpr)
		Return toExpr.oObject.Accept(This) + '.' + toExpr.oName.lexeme + ' = ' + This.Evaluate(toExpr.oValue)
	Endfunc

	Function visitThisNodeExpr(toExpr)
		Return 'this'
	EndFunc

	Function visitUnaryExpr(toExpr)
		Return '(' + toExpr.oOperator.lexeme + toExpr.oRight.Accept(This) + ')'
	Endfunc

	Function visitVarStmt(toStmt)
		Local lcOutput
		lcOutput = "var " + toStmt.oName.lexeme
		If !Isnull(toStmt.oInitializer)
			lcOutput = lcOutput + " = " + This.Evaluate(toStmt.oInitializer)
		Endif
		lcOutput = lcOutput + ";"
		Return lcOutput
	Endfunc

	Function visitVariableExpr(toExpr)
		Return toExpr.oName.lexeme
	Endfunc

	Function visitWhileNodeStmt(toStmt)
		Return 'while (' + This.Evaluate(toStmt.oCondition) + ')' + toStmt.oBody.Accept(This)
	Endfunc

Enddefine
#ifndef CONSTANT_ADDED
	#include "FoxScript.h"
#endif

Define Class ASTPrinter as Custom

	Function print(toStmt)
		Local lcOutput
		lcOutput = ''
		For each loStmt in toStmt
			lcOutput = lcOutput + loStmt.accept(this) + CRLF
		EndFor
		Return lcOutput
	EndFunc

	Function evaluate(toExpr)
		Return toExpr.accept(this)
	EndFunc

	Function visitBlockStmt(toStmt)
		Local lcOutput
		lcOutput = '{' + CRLF
		
		For each loStmt in toStmt.oStatements
			lcOutput = lcOutput + loStmt.accept(this) + CRLF
		EndFor
		lcOutput = lcOutput + CRLF + '}'
		Return lcOutput
	EndFunc

	Function visitClassNodeStmt(toStmt)
		Local lcOutput
		lcOutput = 'class ' + toStmt.oName.lexeme
		If !IsNull(toStmt.oSuperclass)
			lcOutput = lcOutput + ' extends ' + toStmt.oSuperclass.oName.lexeme
		EndIf

		lcOutput = lcOutput + ' {'
		If !IsNull(toStmt.oMethods)
			For each loMethod in toStmt.oMethods
				lcOutput = lcOutput + loMethod.accept(this) + CRLF
			EndFor
		EndIf
		lcOutput = lcOutput + ' }'
		
		Return lcOutput
	EndFunc

	Function visitExpressionStmt(toStmt)
		Return this.evaluate(toStmt.oExpression) + ';'	
	EndFunc

	Function visitFunctionNodeStmt(toStmt)
		Local lcOutput
		lcOutput = "fn " + toStmt.oName.lexeme + "("
		If !IsNull(toStmt.oParams)
			Local i
			i = 0
			For each loParam in toStmt.oParams
				i = i + 1
				If i > 1
					lcOutput = lcOutput + ','
				EndIf
				lcOutput = lcOutput + loParam.lexeme
			EndFor
		EndIf
		lcOutput = lcOutput + ')'
		
		If !IsNull(toStmt.oBody)			
			lcOutput = lcOutput + CRLF + '{' + this.print(toStmt.oBody) + CRLF + '}'
		EndIf
		
		Return lcOutput
	endfunc

	Function visitGroupingExpr(toExpr)
		Return this.evaluate(toExpr.oExpression)
	EndFunc

	Function visitIfNodeStmt(toStmt)
		Local lcOutput
		lcOutput = 'if (' + this.evaluate(toStmt.oCondition) + ')' + CRLF
		If !IsNull(toStmt.oThenBranch)
			lcOutput = lcOutput + toStmt.oThenBranch.accept(this)
		EndIf
		
		If !IsNull(toStmt.oElseBranch)
			lcOutput = lcOutput + 'else '+ CRLF + toStmt.oElseBranch.accept(this)
		EndIf

		Return lcOutput
	EndFunc

	Function visitLiteralExpr(toExpr)
		Local lcValue
		If IsNull(toExpr.oValue)
			lcValue = "null"
		EndIf
		If Type('toExpr.oValue') == 'C'
			lcValue = '"' + toExpr.oValue + '"'
		else
			lcValue = Transform(toExpr.oValue)
		endif
		Return lcValue
	EndFunc

	Function visitBinaryExpr(toExpr)
		Return '(' + toExpr.oLeft.accept(this) + toExpr.oOperator.lexeme + toExpr.oRight.accept(this) + ')'
	EndFunc

	Function visitGetExpr(toExpr)
		Return toExpr.oObject.accept(this) + '.' + toExpr.oName.lexeme
	EndFunc

	Function visitPrintStmt(toStmt)
		Return this.evaluate(toStmt.oExpression) + ';'
	EndFunc

	Function visitReturnNodeStmt(toStmt)
		If !IsNull(toStmt.oValue)
			Return 'return ' + this.evaluate(toStmt.oValue) + ';'
		EndIf
		Return 'return'
	EndFunc
	
	Function visitSetExpr(toExpr)	
		Return toExpr.oObject.accept(this) + '.' + toExpr.oName.lexeme + ' = ' + this.evaluate(toExpr.oValue)
	EndFunc

	Function visitThisNodeExpr(toExpr)
		Return 'this'
	EndFunc

	Function visitUnaryExpr(toExpr)
		Return '(' + toExpr.oOperator.lexeme + toExpr.oRight.accept(this) + ')'
	EndFunc

	Function visitVarStmt(toStmt)
		Local lcOutput
		lcOutput = "var " + toStmt.oName.lexeme
		If !IsNull(toStmt.oInitializer)
			lcOutput = lcOutput + " = " + this.evaluate(toStmt.oInitializer)
		EndIf
		lcOutput = lcOutput + ";"
		Return lcOutput
	EndFunc

	Function visitVariableExpr(toExpr)
		Return toExpr.oName.lexeme
	EndFunc
	
	Function visitWhileNodeStmt(toStmt)		
		Return 'while (' + this.evaluate(toStmt.oCondition) + ')' + toStmt.oBody.accept(this)
	EndFunc	

EndDefine
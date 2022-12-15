* ================================================================================== *
* ArrayLiteral
* ================================================================================== *
#ifndef CONSTANTS_LOADED
	#include "FoxScript.h"
#endif
Define Class ArrayLiteral As Custom
	oKeyword = .Null.
	oElements = .Null.
	cHash = ''

	Function Init(toKeyword, toElements)
		This.oKeyword = toKeyword
		This.oElements = toElements
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitArrayLiteralExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* Assign
* ================================================================================== *
Define Class Assign As Custom
	oName = .Null.
	oValue = .Null.
	cHash = ''

	Function Init(toName, toValue)
		This.oName = toName
		This.oValue = toValue
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitAssignExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* Binary
* ================================================================================== *
Define Class Binary As Custom
	oLeft = .Null.
	oOperator = .Null.
	oRight = .Null.
	cHash = ''

	Function Init(toLeft, toOperator, toRight)
		This.oLeft = toLeft
		This.oOperator = toOperator
		This.oRight = toRight
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitBinaryExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* Call
* ================================================================================== *
Define Class Call As Custom
	oCallee = .Null.
	oParen = .Null.
	oArguments = .Null.
	cHash = ''

	Function Init(toCallee, toParen, toArguments)
		This.oCallee = toCallee
		This.oParen = toParen
		This.oArguments = toArguments
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitCallExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* Context
* ================================================================================== *
Define Class Context As Custom
	oKeyword = .Null.
	cHash = ''

	Function Init(toKeyword)
		This.oKeyword = toKeyword
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitContextExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* FunctionArrow
* ================================================================================== *
Define Class FunctionArrow As Custom
	okeyword = .null.
	oParams = .Null.
	oBody = .Null.
	cHash = ''
	
	Function Init(toKeyword, toParams, toBody)
		This.oParams = toParams
		This.oBody = toBody
		This.oKeyword = toKeyword
		This.cHash = Sys(2015)		
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitFunctionArrowExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* FunctionExpr
* ================================================================================== *
Define Class FunctionExpr As Custom
	oParams = .Null.
	oBody = .Null.

	Function Init(toParams, toBody)
		This.oParams = toParams
		This.oBody = toBody
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitFunctionExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* Get
* ================================================================================== *
Define Class Get As Custom
	oObject = .Null.
	oName = .Null.
	cHash = ''

	Function Init(toObject, toName)
		This.oObject = toObject
		This.oName = toName
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitGetExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* Grouping
* ================================================================================== *
Define Class Grouping As Custom
	oExpressions = .Null.
	cHash = ''

	Function Init(toExpressions)
		This.oExpressions = toExpressions
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitGroupingExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* Literal
* ================================================================================== *
Define Class Literal As Custom
	oValue = .Null.
	cHash = ''

	Function Init(toValue)
		This.oValue = toValue
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitLiteralExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* Logical
* ================================================================================== *
Define Class Logical As Custom
	oLeft = .Null.
	oOperator = .Null.
	oRight = .Null.
	cHash = ''

	Function Init(toLeft, toOperator, toRight)
		This.oLeft = toLeft
		This.oOperator = toOperator
		This.oRight = toRight
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitLogicalExpr(This)
	Endfunc
Enddefine

CreateObject("New", loKeyword, loClassName, loArguments)
* ================================================================================== *
* New
* ================================================================================== *
Define Class New As Custom
	okeyword = .null.
	oClassName = .null.
	oArguments = .null.	
	cHash = ''

	Function Init(toKeyword, toClassName, toArguments)
		this.okeyword = toKeyword
		this.oClassName = toClassName
		this.oArguments = toArguments
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitNewExpr(This)
	Endfunc
EndDefine

* ================================================================================== *
* ObjectLiteral
* ================================================================================== *
Define Class ObjectLiteral As Custom
	oKeyword = .Null.
	oElements = .Null.
	cHash = ''

	Function Init(toKeyword, toElements)
		This.oKeyword = toKeyword
		This.oElements = toElements
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitObjectLiteralExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* Set
* ================================================================================== *
Define Class Set As Custom
	oObject = .Null.
	oName = .Null.
	oValue = .Null.
	cHash = ''

	Function Init(toObject, toName, toValue)
		This.oObject = toObject
		This.oName = toName
		This.oValue = toValue
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitSetExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* Super
* ================================================================================== *
Define Class Super As Custom
	oKeyword = .Null.
	oMethod = .Null.
	cHash = ''

	Function Init(toKeyword, toMethod)
		This.oKeyword = toKeyword
		This.oMethod = toMethod
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitSuperExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* ThisExpr
* ================================================================================== *
Define Class ThisNode As Custom
	oKeyword = .Null.
	cHash = ''

	Function Init(toKeyword)
		This.oKeyword = toKeyword
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitThisNodeExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* Unary
* ================================================================================== *
Define Class Unary As Custom
	oOperator = .Null.
	oRight = .Null.
	cHash = ''

	Function Init(toOperator, toRight)
		This.oOperator = toOperator
		This.oRight = toRight
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitUnaryExpr(This)
	Endfunc
Enddefine
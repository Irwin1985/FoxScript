* ================================================================================== *
* Variable
* ================================================================================== *
Define Class Variable As Custom
	oName = .Null.
	cHash = ''

	Function Init(toName)
		This.oName = toName
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitVariableExpr(This)
	Endfunc
Enddefine

* ================================================================================== *
* Block
* ================================================================================== *
Define Class Block As Custom
	oStatements = .Null.
	Function Init(toStatements)
		This.oStatements = toStatements
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitBlockStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* Class
* ================================================================================== *
Define Class ClassNode As Custom
	oName = .Null.
	oSuperclass = .Null.
	oMethods = .Null.

	Function Init(toName, toSuperClass, toMethods)
		This.oName = toName
		This.oSuperclass = toSuperClass
		This.oMethods = toMethods
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitClassNodeStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* Expression
* ================================================================================== *
Define Class Expression As Custom
	oExpression = .Null.

	Function Init(toExpression)
		This.oExpression = toExpression
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitExpressionStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* FunctionNode
* ================================================================================== *
Define Class FunctionNode As Custom
	oName = .Null.
	oParams = .Null.
	oBody = .Null.

	Function Init(toName, toParams, toBody)
		This.oName = toName
		This.oParams = toParams
		This.oBody = toBody
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitFunctionNodeStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* IfNode
* ================================================================================== *
Define Class IfNode As Custom
	oCondition = .Null.
	oThenBranch = .Null.
	oElseBranch = .Null.

	Function Init(toCondition, toThenBranch, toElseBranch)
		This.oCondition = toCondition
		This.oThenBranch = toThenBranch
		This.oElseBranch = toElseBranch
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitIfNodeStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* Print
* ================================================================================== *
Define Class Print As Custom
	oExpression = .Null.

	Function Init(toExpression)
		This.oExpression = toExpression
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitPrintStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* ReturnNode
* ================================================================================== *
Define Class ReturnNode As Custom
	oKeyword = .Null.
	oValue = .Null.

	Function Init(toKeyword, toValue)
		This.oKeyword = toKeyword
		This.oValue = toValue
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitReturnNodeStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* Var
* ================================================================================== *
Define Class Var As Custom
	oName = .Null.
	oInitializer = .Null.

	Function Init(toName, toInitializer)
		This.oName = toName
		This.oInitializer = toInitializer
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitVarStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* WhileNode
* ================================================================================== *
Define Class WhileNode As Custom
	oCondition = .Null.
	oBody = .Null.

	Function Init(toCondition, toBody)
		This.oCondition = toCondition
		This.oBody = toBody
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitWhileNodeStmt(This)
	Endfunc
Enddefine
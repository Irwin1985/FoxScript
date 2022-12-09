Define Class Interpreter As Custom
	oGlobals = .Null.
	oEnvironment = .Null.
	oLocals = .Null.

	Function Init
		This.oGlobals = Createobject("Environment")
		This.oEnvironment = This.oGlobals
		This.oLocals = Createobject("Dictonary")
	Endfunc

	Function interpret(toStatements)
		Try
			For Each loStatement In toStatements
				This.execute(loStatement)
			Endfor
		Catch To loRuntimeError
			_Screen.foxscript.runtimeError(loRuntimeError)
		Endfunc
	EndFunc
	
	Function evaluate(toExpr)
		Return toExpr.accept(this)
	EndFunc
	
	Function execute(toStmt)
		toStmt.accept(this)
	EndFunc
	
	Function resolve(toExpr, toDepth)
		this.oLocals.put(toExpr, tnDepth)
	EndFunc 
	
	Function executeBlock(toStatements, loEnvironment)
		loPrevious = this.oEnvironment
		Try
			this.oEnvironment = loEnvironment
			For each loStatement in loStatements
				this.execute(loStatement)
			EndFor
		Finally
			this.oEnvironment = loPrevious
		EndTry
	EndFunc
	
	Function visitBlockStmt(toStmt)
		this.executeBlock(toStmt.oStatements, CreateObject("Environment", loEnvironment)
		Return .null.
	EndFunc
	
	Function visitClassNodeStmt(toStmt)
	EndFunc
	
	Function visitExpressionStmt(toStmt)
		this.evaluate(toStmt.oExpression)
		Return .null.
	EndFunc
	
	Function visitFunctionStmt(toStmt)
	EndFunc
	
	Function visitIfStmt(toStmt)
		If this.isTruthy(this.evaluate(toStmt.oCondition))
			this.execute(toStmt.oThenBranch)
		Else
			If !IsNull(toStmt.oElseBranch)
				this.execute(toStmt.oElseBranch)
			endif
		EndIf
		Return .null.
	EndFunc
	
	Function visitPrintStmt(toStmt)
		Local loValue
		loValue = this.evaluate(toStmt.oExpression)
		MessageBox(this.stringify(loValue))
		Return .null.
	EndFunc
	
	Function visitReturnStmt(toStmt)
		Local loValue
		If !IsNull(toStmt.oValue)
			loValue = this.evaluate(toStmt.oValue)
		EndIf
		* TODO(irwin): throw ReturnException
		
	EndFunc
	
	Function visitVarStmt(toStmt)
		Local loValue
		If !IsNull(toStmt.oInitializer)
			loValue = this.evaluate(toStmt.oInitializer)
		EndIf
		this.oEnvironment.define(toStmt.oName.lexeme, loValue)
	EndFunc
	
	Function visitWhileStmt(toStmt)
		Do while this.isTruthy(this.evaluate(toStmt.oCondition))
			this.execute(toStmt.oBody)
		EndDo
		Return .null.
	EndFunc
	
	Function visitAssignExpr(toExpr)
		Local loValue, lnDistance
		loValue = this.evaluate(toExpr.oValue)
		lnDistance = this.oLocals.get(toExpr)
		If !IsNull(lnDistance)
			this.oEnvironment.assignAt(lnDistance, toExpr.oName, loValue)
		Else
			this.oGlobals.assign(toExpr.oName, loValue)
		EndIf
		
		Return loValue
	EndFunc
	
	Function visitBinaryExpr(toExpr)
	EndFunc
	
	Function visitCallExpr(toExpr)
	EndFunc
	
	Function visitGetExpr(toExpr)
	EndFunc
	
	Function visitGroupingExpr(toExpr)
	EndFunc
	
	Function visitLogicalExpr(toExpr)
	EndFunc
	
	Function visitSuperExpr(toExpr)
	EndFunc
	
	Function visitThisNodeExpr(toExpr)
	EndFunc
	
	Function visitUnaryExpr(toExpr)
	EndFunc
	
	Function visitVariableExpr(toExpr)
	EndFunc

	Function checkNumberOperand(toOperator, toOperand)
	EndFunc
	
	Function checkNumberOperands(toOperator, toLeft, toRight)
	EndFunc
	
	Function isTruthy(toObject)
	EndFunc
	
	Function isEqual(toA, toB)
	EndFunc
	
	Function stringify(toObject)
	EndFunc
	
Enddefine
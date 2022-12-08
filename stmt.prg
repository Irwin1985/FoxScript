* -------------------------------------------------
* Block
Define Class Block as Custom
	oStatements = .null.
	Function init(toStatements)
		this.oStatement = toStatements
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitBlockStmt(this)
	EndFunc
EndDefine

* -------------------------------------------------
* Class
Define Class ClassNode as Custom
	oName = .null.
	oSuperClass = .null.
	oMethods = .null.
	
	Function init(toName, toSuperClass, toMethods)
		this.oName = toName
		this.oSuperClass = toSuperClass
		this.oMethods = toMethods
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitClassNodeStmt(this)
	EndFunc
EndDefine

* -------------------------------------------------
* Expression
Define Class Expression as Custom
	oExpression = .null.
		
	Function init(toExpression)
		this.oExpression = toExpression
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitExpressionStmt(this)
	EndFunc
EndDefine

* -------------------------------------------------
* FunctionNode
Define Class FunctionNode as Custom
	oName = .null.
	oParams = .null.
	oBody = .null.
		
	Function init(toName, toParams, toBody)
		this.oName = toName
		this.oParams = toParams
		this.oBody = toBody
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitFunctionNodeStmt(this)
	EndFunc
EndDefine

* -------------------------------------------------
* IfNode
Define Class IfNode as Custom
	oCondition = .null.
	oThenBranch = .null.
	oElseBranch = .null.
		
	Function init(toCondition, toThenBranch, toElseBranch)
		this.oCondition = toCondition
		this.oThenBranch = toThenBranch
		this.oElseBranch = toElseBranch
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitIfNodeStmt(this)
	EndFunc
EndDefine

* -------------------------------------------------
* Print
Define Class Print as Custom
	oExpression = .null.
			
	Function init(toExpression)
		this.oExpression = toExpression
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitPrintStmt(this)
	EndFunc
EndDefine

* -------------------------------------------------
* ReturnNode
Define Class ReturnNode as Custom
	oKeyword = .null.
	oValue = .null.
			
	Function init(toKeyword, toValue)
		this.oKeyword = toKeyword
		this.oValue = toValue
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitReturnNodeStmt(this)
	EndFunc
EndDefine

* -------------------------------------------------
* Var
Define Class Var as Custom
	oName = .null.
	oInitializer = .null.
				
	Function init(toName, toInitializer)
		this.oName = toName
		this.oInitializer = toInitializer
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitVarStmt(this)
	EndFunc
EndDefine

* -------------------------------------------------
* WhileNode
Define Class WhileNode as Custom
	oCondition = .null.
	oBody = .null.
					
	Function init(toCondition, toBody)
		this.oCondition = toCondition
		this.oBody = toBody
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitWhileNodeStmt(this)
	EndFunc
EndDefine
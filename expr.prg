* ---------------------------------------------
* Assign
Define Class Assign as Custom
	oName = .null.
	oValue = .null.
	
	Function init(toName, toValue)
		this.oName = toName
		this.oValue = toValue
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitAssignExpr(this)
	EndFunc	
EndDefine

* ---------------------------------------------
* Binary
Define Class Binary as Custom
	oLeft = .null.
	oOperator = .null.
	oRight = .null.
	
	Function init(toLeft, toOperator, toRight)
		this.oLeft = toLeft
		this.oOperator = toOperator
		this.oRight = toRight
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitBinaryExpr(this)
	EndFunc
EndDefine

* ---------------------------------------------
* Call
Define Class Call as Custom
	oCallee = .null.
	oParen = .null.
	oArguments = .null.
		
	Function init(toCallee, toParen, toArguments)
		this.oCallee = toCallee
		this.oParen = toParen
		this.oArguments = toArguments
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitCallExpr(this)
	EndFunc
EndDefine

* ---------------------------------------------
* Get
Define Class Get as Custom
	oObject = .null.
	oName = .null.
			
	Function init(toObject, toName)
		this.oObject = toObject
		this.oName = toName
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitGetExpr(this)
	EndFunc
EndDefine

* ---------------------------------------------
* Grouping
Define Class Grouping as Custom
	oExpression = .null.
			
	Function init(toExpression)
		this.oExpression = toExpression
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitGroupingExpr(this)
	EndFunc
EndDefine

* ---------------------------------------------
* Literal
Define Class Literal as Custom
	oValue = .null.
	Function init(toValue)
		this.oValue = toValue
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitLiteralExpr(this)
	EndFunc	
EndDefine

* ---------------------------------------------
* Logical
Define Class Logical as Custom
	oLeft = .null.
	oOperator = .null.
	oRight = .null.
	
	Function init(toToken, toLeft, toOperator, toRight)
		this.oLeft = toLeft
		this.oOperator = toOperator
		this.oRight = toRight
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitLogicalExpr(this)
	EndFunc
EndDefine

* ---------------------------------------------
* Set
Define Class Set as Custom
	oObject = .null.
	oName = .null.
	oValue = .null.
			
	Function init(toObject, toName, toValue)
		this.oObject = toObject
		this.oName = toName
		this.oValue = toValue
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitSetExpr(this)
	EndFunc
EndDefine

* ---------------------------------------------
* Super
Define Class Super as Custom
	oKeyword = .null.
	oMethod = .null.
				
	Function init(toKeyword, toMethod)
		this.oKeyword = toKeyword
		this.oMethod = toMethod
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitSuperExpr(this)
	EndFunc
EndDefine

* ---------------------------------------------
* ThisExpr
Define Class ThisNode as Custom
	oKeyword = .null.
	
	Function init(toKeyword)
		this.oKeyword = toKeyword
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitThisNodeExpr(this)
	EndFunc
EndDefine

* ---------------------------------------------
* Unary
Define Class Unary as Custom
	oOperator = .null.
	oRight = .null.
	
	Function init(toOperator, toRight)
		this.oOperator = toOperator
		this.oRight = toRight
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitUnaryExpr(this)
	EndFunc
EndDefine

* ---------------------------------------------
* Variable
Define Class Variable as Custom
	oName = .null.
	
	Function init(toName)
		this.oName = toName
	EndFunc
	
	Function accept(toVisitor)
		Return toVisitor.visitVariableExpr(this)
	EndFunc
EndDefine
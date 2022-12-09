#ifndef CONSTANT_ADDED
	#include "FoxScript.h"
#endif
* Function Type Constants
#Define FT_NONE 		0
#Define FT_FUNCTION 	1
#Define FT_INITIALIZER 	2
#Define FT_METHOD 		3

* Class Type Constants
#Define CLT_NONE		0
#Define CLT_CLASS		1
#Define CLT_SUBCLASS	2

Define Class Resolver as Custom
	oInterpreter = .null.
	oScopes = .null.
	nCurrentFunction = FT_NONE
	nCurrentClass = CLT_NONE

	Function init(toInterpreter)
		this.oInterpreter = toInterpreter
		this.oScopes = CreateObject("Stack")
	EndFunc
	
	Function resolve(toStatements)
		For each loStmt in toStatements
			this.resolveStmt(loStmt)
		EndFor
	EndFunc
	
	function visitBlockStmt(toStmt)
		this.beginScope()
		this.resolve(toStmt.oStatements)
		this.endScope()
		
		Return .null.
	EndFunc
	
	function visitClassNodeStmt(toStmt)
		Local lnEnclosingClass
		lnEnclosingClass = this.nCurrentClass
		
		this.declare(toStmt.oName)
		this.define(toStmt.oName)
		
		If !IsNull(toStmt.oSuperClass) and toStmt.oName.lexeme == toStmt.oSuperClass.oName
			_screen.foxScript.errorToken(toStmt.oSuperClass.oName, "A class can't inherit from itself.")			
		EndIf
		
		If !IsNull(toStmt.oSuperClass)
			this.nCurrentClass = CLT_SUBCLASS
			this.resolveExpr(toStmt.oSuperClass)
		EndIf
		
		If !IsNull(toStmt.oSuperClass)
			this.beginScope()
			Local loScope
			loScope = this.oScopes.peek()
			loScope.put('super', true)
		EndIf
		
		this.beginScope()
		Local loScope, lnDeclaration
		loScope = this.oScopes.peek()
		loScope.put('this', true)
		
		For each loMethod in toStmt.oMethods
			lnDeclaration = FT_METHOD
			If loMethod.oName.lexeme == 'init'
				lnDeclaration = FT_INITIALIZER
			EndIf
			
			this.resolveFunction(loMethod, lnDeclaration)
		EndFor
		
		this.endScope()
		
		If !IsNull(toStmt.oSuperClass)
			this.endScope()
		EndIf
		
		this.nCurrentClass = lnEnclosingClass
		
		Return .null.
	EndFunc
	
	function visitExpressionStmt(toStmt)
		this.resolveExpr(toStmt.oExpression)
		Return .null.
	EndFunc
	
	function visitFunctionStmt(toStmt)
		this.declare(toStmt.oName)
		this.define(toStmt.oName)
		
		this.resolveFunction(toStmt, FT_FUNCTION)
		
		Return .null.
	EndFunc
	
	function visitIfStmt(toStmt)
		this.resolveExpr(toStmt.oCondition)
		this.resolve(toStmt.oThenBranch)
		
		If !IsNull(toStmt.oElseBranch)
			this.resolve(toStmt.oElseBranch)
		EndIf
		
		Return .null.
	EndFunc
	
	function visitPrintStmt(toStmt)
		this.resolveExpr(toStmt.oExpression)
		Return .null.
	EndFunc
	
	function visitReturnNodeStmt(toStmt)
		If this.nCurrentfunction == FT_NONE
			_screen.foxScript.errorToken(toStmt.oKeyword, "Can't return from top-level code.")
		EndIf
		
		If !IsNull(loStmt.oValue)
			If this.nCurrentFunction == FT_INITIALIZER
				_screen.foxScript.errorToken(toStmt.oKeyword, "Can't return a value from an initializer.")
			EndIf
			
			this.resolveExpr(toStmt.oValue)
		EndIf
		
		Return .null.
	EndFunc
	
	function visitVarStmt(toStmt)
		this.declare(toStmt.oName)
		If !IsNull(toStmt.oInitializer)
			this.resolveExpr(toStmt.oInitializer)
		EndIf
		this.define(toStmt.oName)
		
		Return .null.
	EndFunc
	
	function visitWhileStmt(toStmt)
		this.resolveExpr(toStmt.oCondition)
		this.resolve(toStmt.oBody)
		Return .null.
	endfunc

	function visitAssignExpr(toExpr)
		this.resolveExpr(toExpr.oValue)
		this.resolveLocal(toExpr, toExpr.oName)
		Return .null.
	EndFunc

	function visitBinaryExpr(toExpr)
		this.resolveExpr(toExpr.oLeft)
		this.resolveExpr(toExpr.oRight)
		Return .null.
	endfunc

	function visitCallExpr(toExpr)
		this.resolveExpr(toExpr.oCallee)
		For each loArgument in toExpr.oArguments
			this.resolveExpr(loArgument)
		EndFor
		
		Return .null.
	endfunc

	function visitGetExpr(toExpr)
		this.resolveExpr(toExpr.oObject)
		Return .null.
	EndFunc

	function visitGroupingExpr(toExpr)
		this.resolveExpr(toExpr.oExpression)
		Return .null.
	EndFunc

	function visitLiteralExpr(toExpr)
		Return .null.
	EndFunc

	function visitLogicalExpr(toExpr)
		this.resolveExpr(toExpr.oLeft)
		this.resolveExpr(toExpr.oRight)
		Return .null.
	EndFunc

	function visitSetExpr(toExpr)
		this.resolveExpr(toExpr.oValue)
		this.resolveExpr(toExpr.oObject)
		Return .null.
	EndFunc

	function visitSuperExpr(toExpr)
		Do case
		case this.nCurrentClass == CLT_NONE
			_screen.foxScript.errorToken(toExpr.oKeyword, "Can't use 'super' outside of a class.")
		Case this.nCurrentClass != CLT_SUBCLASS
			_screen.foxScript.errorToken(toExpr.oKeyword, "Can't use 'super' in a class with no superclass.")
		EndIf
		this.resolveLocal(toExpr, toExpr.oKeyword)
		
		Return .null.
	EndFunc

	function visitThisNodeExpr(toExpr)
		If this.nCurrentClass == CLT_NONE
			_screen.foxScript.errorToken(toExpr.oKeyword, "Can't use 'this' outside of a class.")
			Return .null.
		EndIf
		this.resolveLocal(toExpr, toExpr.oKeyword)
		
		Return .null.
	EndFunc
	
	function visitUnaryExpr(toExpr)
		this.resolveExpr(toExpr.oRight)
		Return .null.
	EndFunc
	
	function visitVariableExpr(toExpr)
		If !this.oScopes.empty()
			Local loScope
			loScope = this.oScopes.peek()
			If !loScope.get(toExpr.oName.lexeme)
				_screen.foxScript.errorToken(toExpr.oName, "Can't read local variable in its own initializer.")
			EndIf
			this.resolveLocal(toExpr, toExpr.oName)
			
			Return .null.
		endif
	EndFunc
	
	Hidden function resolveStmt(toStmt)
		toStmt.accept(this)
	EndFunc
	
	Hidden function resolveExpr(toExpr)
		toExpr.accept(this)
	EndFunc
	
	Hidden function resolveFunction(toFunction, tnType)
		Local lnEnclosingFunction
		lnEnclosingFunction = this.nCurrentFunction
		this.nCurrentFunction = tnType
		
		this.beginScope()
		
		For each toParam in toFunction.oParams
			this.declare(toParam)
			this.define(toParam)
		EndFor
		this.resolve(toFunction.oBody)
				
		this.endScope()
		
		this.nCurrentFunction = lnEnclosingFunction
	EndFunc
	
	Hidden function beginScope
		this.oScopes.push(CreateObject('Dictionary'))
	EndFunc
	
	Hidden function endScope
		this.oScopes.pop()
	EndFunc
	
	Hidden function declare(toName)
		If this.oScopes.empty()
			Return
		EndIf
		Local loScope
		loScope = this.oScopes.peek()
		If loScope.ContainsKey(toName.lexeme)
			_screen.foxscript.errorToken(toName, "Already variable with this name in this scope.")
		EndIf
		loScope.put(toName.lexeme, false)
	EndFunc
	
	Hidden function define(toName)
		If this.oScopes.empty()
			Return
		EndIf
		Local loScope
		loScope = this.oScopes.peek()
		loScope.put(toName.lexeme, true)
	EndFunc
	
	Hidden function resolveLocal(toExpr, toName)
		Local i, loScope
		* |	  i=5	 |   i=4   |   i=3   |   i=2   |   i=1   |
		* |   5-1    |   5-2   |   5-3   |   5-4   |   5-5   |
		For i = this.oScope.Size() to 1 step -1
			loScope = this.oScopes.get(i)
			If loScope.containsKey(toName.lexeme)
				this.oInterpreter.resolve(toExpr, this.oScopes.Size() - i)
				return
			EndIf
		EndFor
	EndFunc
EndDefine

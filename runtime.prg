* ================================================================================== *
* Environment Class
* ================================================================================== *
Define Class Environment As Custom
	oEnclosing = .Null.
	oValues = .Null.

	Function Init(toEnclosing)
		This.oEnclosing = .Null.
		If !Isnull(toEnclosing)
			This.oEnclosing = toEnclosing
		Endif
		This.oValues = Createobject('Dictionary')
	Endfunc

	Function Get(toName)
		If This.oValues.ContainsKey(toName.lexeme)
			Return This.oValues.Get(toName.lexeme)
		Endif
		If !Isnull(This.oEnclosing)
			Return This.oEnclosing.Get(toName)
		EndIf
		Return .f.
		*_Screen.FoxScript.throwError('RuntimeError', 'token', toName, "Undefined variable '" + toName.lexeme + "'.")
	Endfunc

	Function assign(toName, toValue)
		If This.oValues.ContainsKey(toName.lexeme)
			This.oValues.put(toName.lexeme, toValue)
			Return
		Endif

		If !Isnull(This.oEnclosing)
			This.oEnclosing.assign(toName, toValue)
		Endif
		_Screen.FoxScript.throwError('RuntimeError', 'token', toName, "Undefined variable '" + toName.lexeme + "'.")
	Endfunc

	Function Define(tcName, toValue)
		This.oValues.put(tcName, toValue)
	Endfunc

	Function ancestor(tnDistance)
		Local i, loEnvironment
		loEnvironment = This
		For i = 1 To tnDistance
			loEnvironment = loEnvironment.oEnclosing
		Endfor
		Return loEnvironment
	Endfunc

	Function getAt(tnDistance, tcName)
		Local loEnvironment
		loEnvironment = This.ancestor(tnDistance)
		Return loEnvironment.oValues.Get(tcName)
	Endfunc

	Function assignAt(tnDistance, toName, toValue)
		Local loEnvironment
		loEnvironment = This.ancestor(tnDistance)
		loEnvironment.oValues.put(toName.lexeme, toValue)
	Endfunc

	Function toString
		Local lcResult
		lcResult = This.oValues.toString()
		If !Isnull(This.oEnclosing)
			lcResult = lcResult + " -> " + This.oEnclosing.toString()
		Endif

		Return lcResult
	Endfunc

Enddefine

* ================================================================================== *
* Runtime Collection Classes
* ================================================================================== *
Define Class Callable As Custom
	lValidateArity = .t.
	oCallExpr = .null.
	
	Function arity
		* Abstract
	Endfunc

	Function Call(toInterpreter, toArguments)
		* Abstract
	Endfunc

Enddefine

* ================================================================================== *
* Builtin function Clock
* ================================================================================== *
Define Class BuiltinClock As Callable
	
	Function init
		this.lValidateArity = .f.
	EndFunc

	Function arity
		Return 0
	Endfunc

	Function Call(toInterpreter, toArguments)
		Return Seconds()
	Endfunc

Enddefine

* ================================================================================== *
* Builtin function Call
* ================================================================================== *
Define Class BuiltinCall As Callable
	Function init
		this.lValidateArity = .f.
	EndFunc
	
	Function arity
		Return 0
	Endfunc

	Function Call(toInterpreter, toArguments)		
		If Type('toArguments.Item(1)') != 'C'
			_Screen.FoxScript.throwError('RuntimeError', 'token', this.oCallExpr.oParen, "Invalid method name.")
		EndIf
		Local loContext, lcMacro, i, loResult
		loContext = _screen.foxScript.oContext.oContext
		If IsNull(loContext)
			_Screen.FoxScript.throwError('RuntimeError', 'token', this.oCallExpr.oParen, "Context is not an object.")
		EndIf
		loResult = .null.
		Try
			Local lcMacro, loResult
			lcMacro = "loContext." + toArguments.Item(1) + '('
			For i = 2 to toArguments.Count
				If i > 2
					lcMacro = lcMacro + ','
				EndIf
				lcMacro = lcMacro + 'toArguments.Item(' + Alltrim(Str(i)) + ')'
			EndFor
			lcMacro = lcMacro + ')'
			loResult = &lcMacro
		Catch to loEx
			_Screen.FoxScript.throwError('RuntimeError', 'token', this.oCallExpr.oParen, loEx.Message)
		EndTry
		Return loResult
	Endfunc

Enddefine

* ================================================================================== *
* Builtin function Add
* ================================================================================== *
Define Class BuiltinAdd As Callable
	Function init
		this.lValidateArity = .t.
	EndFunc
	
	Function arity
		Return 1
	Endfunc

	Function Call(toInterpreter, toArguments)		
		Local loContext, lcMacro, i
		loContext = _screen.foxScript.oContext.oContext
		If IsNull(loContext)
			_Screen.FoxScript.throwError('RuntimeError', 'token', this.oCallExpr.oParen, "Context is not an object.")
		EndIf
		Try
			Local lcControlClass, lcControlName, loArg, lcProperty, loClick
			loArg = toArguments.Item(1)
			
			lcControlClass = loArg.oFields.Item(loArg.oFields.GetKey('type'))
			lcControlName = loArg.oFields.Item(loArg.oFields.GetKey('name'))
			
			lcMacro = "loContext.AddObject('" + lcControlName + "', '" + lcControlClass + "')"
			&lcMacro
			For i = 1 to loArg.oFields.Count
				lcProperty = loArg.oFields.GetKey(i)
				If !InList(Lower(lcProperty), 'type', 'name')
					If Lower(lcProperty) != 'onclick'
						lcMacro = 'loContext.' + lcControlName + '.' + lcProperty + ' = loArg.oFields.Item(' + Alltrim(Str(i)) + ')'
					Else
						loClick = loArg.oFields.Item(i)
						_screen.foxScript.oDelayedCode.Add(loClick, lcControlName)
						=AddProperty(loClick, 'controlName', lcControlName)
						lcMacro = "BindEvent(loContext." + lcControlName + ", 'Click', _screen.foxScript, 'interpret')"
					EndIf
					&lcMacro
				EndIf
			EndFor
		Catch to loEx
			_Screen.FoxScript.throwError('RuntimeError', 'token', this.oCallExpr.oParen, loEx.Message)
		EndTry
		Return .null.
	Endfunc
EndDefine

* ================================================================================== *
* Builtin function Query
* ================================================================================== *
Define Class BuiltinQuery As Callable
	
	Function init
		this.lValidateArity = .f.
	EndFunc

	Function arity
		Return 1
	Endfunc

	Function Call(toInterpreter, toArguments)		
		If Type('toArguments.Item(1)') != 'C'
			_Screen.FoxScript.throwError('RuntimeError', 'token', this.oCallExpr.oParen, "Invalid query content.")
		EndIf
		Local loContext, lnSessionID, lcQuery, lcCursor, loResult
		loContext = _screen.foxScript.oContext.oContext
		If IsNull(loContext)
			_Screen.FoxScript.throwError('RuntimeError', 'token', this.oCallExpr.oParen, "Context is not an object.")
		EndIf
		If Type('loContext.DatasessionID') != 'N'
			_Screen.FoxScript.throwError('RuntimeError', 'token', this.oCallExpr.oParen, "The current context doesn't have a valid data session.")
		EndIf		
		Try			
			lnSessionID = Set("Datasession")
			Set Datasession To (loContext.DataSessionID)
			lcCursor = Sys(2015)
			lcQuery = toArguments.Item(1) + ' into cursor ' + lcCursor
			loResult = Createobject('Collection')

			&lcQuery
			Select (lcCursor)
			Scan
				Scatter name loRow
				loResult.Add(loRow)
			EndScan
			Use in (lcCursor)
			If toArguments.Count == 2 and Type('toArguments.Item(2)') == 'O' and (InList(toArguments.Item(2).Class, 'Runtimefunction', 'Callable') or InList(toArguments.Item(2).ParentClass, 'Runtimefunction', 'Callable'))
				Local loArguments
				loArguments = CreateObject('Collection')
				loArguments.Add(loResult)
				this.prepareCallBack(toArguments.Item(2), toInterpreter, loArguments)
			EndIf			
		Catch to loEx
			_Screen.FoxScript.throwError('RuntimeError', 'token', this.oCallExpr.oParen, loEx.Message)
		EndTry
		Return loResult
	Endfunc

	Hidden function prepareCallBack(toFunction, toInterpreter, toArguments)
		Local loEnvironment, i, loResult
		loEnvironment = Createobject("Environment", toFunction.oClosure)
		loResult = .Null.
		* Zip parameter name with argument values.
		For i = 1 To toFunction.oDeclaration.oParams.Count
			loEnvironment.Define(toFunction.oDeclaration.oParams.Item(i).oName.lexeme, toArguments.Item(i))
		Endfor

		Try
			toInterpreter.executeBlock(toFunction.oDeclaration.oBody, loEnvironment)
		Catch To loEx
			If Type('loEx.Type') == 'C' And loEx.Type == 'ReturnException'
				loResult = loEx.Value
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
		Return loResult
	EndFunc

Enddefine

* ================================================================================== *
* RuntimeFunction class
* ================================================================================== *
Define Class RuntimeFunction As Callable
	oDeclaration = .Null.
	oClosure = .Null.
	isInitializer = .F.

	Function Init(toDeclaration, toClosure, tlIsInitializer)
		This.oDeclaration = toDeclaration
		This.oClosure = toClosure
		This.isInitializer = tlIsInitializer
	Endfunc

	Function Bind(toInstance)
		Local loEnvironment
		loEnvironment = Createobject("Environment", This.oClosure)
		loEnvironment.Define("this", toInstance)
		Return Createobject("RuntimeFunction", This.oDeclaration, loEnvironment, This.isInitializer)
	Endfunc

	Function arity
		Return This.oDeclaration.oParams.Count
	Endfunc

	Function Call(toInterpreter, toArguments)
		Local loEnvironment, i, loResult, lCheckReturn
		loEnvironment = Createobject("Environment", This.oClosure)
		loResult = .Null.
		lCheckReturn = .T.
		* Zip parameter name with argument values.
		For i = 1 To This.oDeclaration.oParams.Count
			loEnvironment.Define(This.oDeclaration.oParams.Item(i).lexeme, toArguments.Item(i))
		Endfor

		Try
			toInterpreter.executeBlock(This.oDeclaration.oBody, loEnvironment)
		Catch To loEx
			If Type('loEx.Type') == 'C' And loEx.Type == 'ReturnException'
				lCheckReturn = .F.
				If This.isInitializer
					loResult = This.oClosure.getAt(0, "this")
				Else
					loResult = loEx.Value
				Endif
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

		If lCheckReturn
			If This.isInitializer
				Return This.oClosure.getAt(0, "this")
			Endif
		Endif
		Return loResult
	Endfunc

Enddefine
* ================================================================================== *
* RuntimeClass class
* ================================================================================== *
Define Class RuntimeClass As Callable
	cName = ''
	oSuperclass = .Null.
	oMethods = .Null.

	Function Init(tcName, toSuperClass, toMethods)
		This.cName = tcName
		This.oSuperclass = toSuperClass
		This.oMethods = toMethods
	Endfunc

	Function findMethod(tcName)
		If This.oMethods.ContainsKey(tcName)
			Return This.oMethods.Get(tcName)
		Endif
		If !Isnull(This.oSuperclass)
			Return This.oSuperclass.findMethod(tcName)
		Endif

		Return .Null.
	Endfunc

	Function Call(toInterpreter, toArguments)
		Local loInstance, loInitializer, loFunction
		loInstance = Createobject('RuntimeInstance', This) && enclosing class environment
		loInitializer = This.findMethod('init')
		If !Isnull(loInitializer)
			loFunction = loInitializer.Bind(loInstance)
			loFunction.Call(toInterpreter, toArguments)
		Endif

		Return loInstance
	Endfunc

	Function arity
		Local loInitializer
		loInitializer = This.findMethod('init')
		If Isnull(loInitializer)
			Return 0
		Endif
		Return loInitializer.arity()
	Endfunc
EndDefine

* ================================================================================== *
* RuntimeInstance class
* ================================================================================== *
Define Class RuntimeInstance As Custom
	oClass = .Null.
	oFields = .Null.

	Function Init(toClass)
		This.oClass = toClass
		This.oFields = Createobject('Dictionary')
	Endfunc

	Function Get(toName)
		If This.oFields.ContainsKey(toName.lexeme)
			Return This.oFields.Get(toName.lexeme)
		Endif

		Local loMethod
		loMethod = This.oClass.findMethod(toName.lexeme)
		If !Isnull(loMethod)
			Return loMethod.Bind(This)
		Endif
		_Screen.FoxScript.throwError('RuntimeError', 'token', toToken, "Undefined property '" + toName.lexeme + "'.")
	Endfunc

	Function Set(toName, toValue)
		This.oFields.put(toName.lexeme, toValue)
	Endfunc
Enddefine

* ================================================================================== *
* RuntimeObject class
* ================================================================================== *
Define Class RuntimeObject As RuntimeInstance
	oClass = .Null.
	oFields = .Null.

	Function Init(toClass)
		DoDefault(toClass)
	Endfunc

	Function Get(toName)
		If toName.lexeme == 'count'
			Return this.oFields.Count
		EndIf
		Return DoDefault(toName)
	Endfunc

EndDefine

* ================================================================================== *
* Context Class
* ================================================================================== *
Define Class ContextContainer as RuntimeInstance
	oContext = .null.
	
	Function init(toContext)
		DoDefault(.null.)
		this.oContext = toContext
	EndFunc

	Function Get(toName)
		Try
			Local lcMacro, loResult
			lcMacro = "this.oContext." + toName.lexeme
			loResult = &lcMacro
		Catch to loEx
			_Screen.FoxScript.throwError('RuntimeError', 'token', toName, loEx.Message)
		EndTry
		Return loResult
	Endfunc

	Function Set(toName, toValue)
		Try
			Local lcMacro
			lcMacro = "this.oContext." + toName.lexeme + " = toValue"
			&lcMacro
		Catch to loEx
			_Screen.FoxScript.throwError('RuntimeError', 'token', toName, loEx.Message)
		EndTry
	Endfunc

EndDefine
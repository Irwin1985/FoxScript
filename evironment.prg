Define Class Environment as Custom
	Hidden oEnclosing
	Hidden oValues
	
	Function init(toEnclosing)
		this.oEnclosing = .null.
		If !IsNull(toEnclosing)
			this.oEnclosing = toEnclosing
		EndIf
		this.oValues = CreateObject('Dictionary')
	EndFunc
	
	Function get(toName)
		If this.oValues.containsKey(toName.lexeme)
			Return oValues.get(toName.lexeme)
		EndIf
		If !IsNull(this.oEnclosing)
			Return this.oEnclosing.get(toName)
		EndIf
		
		_screen.foxScript.runtimeError(toName, "Undefined variable '" + toName.lexeme + "'.")
	EndFunc
	
	Function assign(toName, toValue)
		If this.oValues.containsKey(toName.lexeme)
			this.oValues.put(toName.lexeme, toValue)
			return
		EndIf
		
		If !IsNull(this.oEnclosing)
			this.oEnclosing.assign(toName, toValue)
		EndIf
		
		_screen.foxScript.runtimeError(toName, "Undefined variable '" + toName.lexeme + "'.")		
	EndFunc
	
	Function ancestor(tnDistance)
		Local i, loEnvironment
		loEnvironment = this
		For i = 1 to tnDistance
			loEnvironment = loEnvironment.oEnclosing
		EndFor
		Return loEnvironment
	EndFunc
	
	Function getAt(tnDistance, tcName)
		Local loEnvironment
		loEnvironment = this.ancestor(tnDistance)
		Return loEnvironment.oValues.get(tcName)
	EndFunc
	
	Function assignAt(tndistance, toName, toValue)
		Local loEnvironment
		loEnvironment = this.ancestor(tnDistance)
		loEnvironment.oValues.put(toName.lexeme, toValue)		
	EndFunc
	
	Function toString
		Local lcResult
		lcResult = this.oValues.toString()
		If !IsNull(this.oEnclosing)
			lcResult = lcResult + " -> " + this.oEnclosing.toString()
		EndIf
		
		Return lcResult
	EndFunc
	
EndDefine
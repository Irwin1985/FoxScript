#ifndef CONSTANT_ADDED
	#include "FoxScript.h"
#endif
* -----------------------------------------------
* FoxScript class
Define Class FoxScript As Custom
	lHadError = .f.
	lHadRunTimeError = .f.
	Hidden oIterpreter

	Function Run(tcFileNameOrScript)
		If Empty(tcFileNameOrScript)
			This.runPrompt()
		Else
			This.runFileOrScript(tcFileNameOrScript)
		Endif
	Endfunc

	Function runFileOrScript(tcFileNameOrScript)
		If Lower(Right(tcFileNameOrScript, 8)) == '.fscript'
			tcFileNameOrScript = Filetostr(tcFileNameOrScript)
		Endif
		This.Execute(tcFileNameOrScript)
	Endfunc

	Function runPrompt
		Do form repl
	Endfunc

	Function Execute(tcSource)
		Local loScanner, loTokens, loParser, loResolver, loStatements, llPrintTokens
		loScanner = CreateObject("Scanner", tcSource)
		loTokens = loScanner.scanTokens()
		llPrintTokens = true
		
		* DEBUG
		If llPrintTokens
			For each loToken in loTokens
				? loToken.toString()
			EndFor
		EndIf
		* DEBUG
		loParser = Createobject("Parser", loTokens)
		loStatements = loParser.parse()		

		* Stop if there was a syntax error.
		If This.lHadError
			Return
		Endif
		loResolver = CreateObject("Resolver", this.oInterpreter)
		loResolver.resolve(loStatements)
		* Stop if there was a resolution error.
		If this.lHadError
			Return
		EndIf
		
		This.oInterpreter.interpret(loStatements)
	Endfunc

	Function errorLine(tnLine, tnCol, tcMessage)
		This.reportError(tnLine, tnCol, tcMessage)
	Endfunc

	Function reportError(tnLine, tnCol, tcWhere, tcMessage)
		Messagebox(This.formatError("Parsing", tnLine, tnCol, tcWhere, tcMessage))
		This.lHadError = TRUE
	Endfunc

	Function errorToken(toToken, tcMessage)
		If toToken.Type == TT_EOF
			This.reportError(toToken.Line, toToken.Col, " at end", tcMessage)
		Else
			This.reportError(toToken.Line, toToken.Col, toToken.lexeme, tcMessage)
		Endif
	Endfunc

	Function runtimeError(toError)
		Messagebox(This.formatError("Runtime", toError.Token.Line, toError.Token.Col, toError.Token.lexeme, toError.Message)
		This.lHadRunTimeError = TRUE
	Endfunc

	Function formatError(tcErrorStr, tnLine, tnCol, tcWhere, tcMessage)
		Return "[" + Alltrim(Str(tnLine)) + ":" + Alltrim(Str(tnCol)) + "] - " + tcErrorStr + " error near of `" + tcWhere + "`: " + tcMessage
	Endfunc

EndDefine
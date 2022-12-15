* ================================================================================== *
* FoxScript scripting languaje.
* Version: 0.0.1
* Author: Irwin Rodríguez <rodriguez.irwin@gmail.com>
*
* A tiny scripting language that adds some interesting features inside your
* Visual Foxpro apps.
*
* Example:
* Do FoxScript with 'print("Hello world!");' // print hello world in a messagebox.
*
* ================================================================================== *

* ================================================================================== *
* FoxScript class
* ================================================================================== *
#ifndef CONSTANTS_LOADED
	#include "FoxScript.h"
#endif
Define Class FoxScript As Custom
	lHadError = .F.
	lHadRunTimeError = .F.
	Hidden oInterpreter
	oContext = .null.

	Function Init
		This.oInterpreter = Createobject("Interpreter")
	Endfunc

	Function Run(toContextOrScript, tcFileNameOrScript)
		If Pcount() = 2
			this.registerContext(toContextOrScript)
		Else
			tcFileNameOrScript = toContextOrScript
		Endif

		If Empty(tcFileNameOrScript)
			This.runPrompt()
		Else
			Return This.runFileOrScript(tcFileNameOrScript)
		Endif
	Endfunc

	Function runFileOrScript(tcFileNameOrScript)
		If Lower(Right(tcFileNameOrScript, 8)) == '.fscript'
			tcFileNameOrScript = Filetostr(tcFileNameOrScript)
		Endif
		Return This.Execute(tcFileNameOrScript)
	Endfunc

	Function Execute(tcSource)
		Local loScanner, loTokens, loParser, lcOutput, ;
		loResolver, loStatements, llPrintTokens, llPrintAST

		This.lHadError = false
		This.lHadRunTimeError = false
		
		loScanner = Createobject("Scanner", tcSource)
		loTokens = loScanner.scanTokens()
		llPrintTokens = false
		llPrintAST = false

		* <DEBUG>
		If llPrintTokens
			For Each loToken In loTokens
				? loToken.toString()
			Endfor
		Endif
		* <DEBUG>
		
		loParser = Createobject("Parser", loTokens)
		loStatements = loParser.parse()

		* Stop if there was a syntax error.
		If This.lHadError
			Return .Null.
		Endif

		* <DEBUG>
		If llPrintAST
			Local loASTPrinter
			loASTPrinter = Createobject("ASTPrinter")
			? loASTPrinter.Print(loStatements)
		Endif
		* <DEBUG>
		lcOutput = This.oInterpreter.interpret(loStatements)
		* DEBUG
		clear
		* DEBUG
		Return this.oInterpreter.cInitialization + CRLF + lcOutput + this.oInterpreter.cClassList + CRLF + this.oInterpreter.cFunctionList
	Endfunc

	Function errorLine(tnLine, tnCol, tcMessage)
		This.reportError(tnLine, tnCol, "", tcMessage)
	Endfunc

	Function reportError(tnLine, tnCol, tcWhere, tcMessage)
		Messagebox(This.formatError("Parsing", tnLine, tnCol, tcWhere, tcMessage), 16, "FoxScript Error")
		This.lHadError = TRUE
	Endfunc

	Function errorToken(toToken, tcMessage)
		If toToken.Type == TT_EOF
			This.reportError(toToken.Line, toToken.Col, " at end", tcMessage)
		Else
			This.reportError(toToken.Line, toToken.Col, toToken.lexeme, tcMessage)
		Endif
	Endfunc

	Function runtimeError(toException)
		Messagebox(This.formatError("Runtime", toException.Token.Line, toException.Token.Col, toException.Token.lexeme, toException.Message), 16, "FoxScript Error")
		This.lHadRunTimeError = TRUE
	Endfunc

	Function formatError(tcErrorStr, tnLine, tnCol, tcWhere, tcMessage)
		Return "[" + Alltrim(Str(tnLine)) + ":" + Alltrim(Str(tnCol)) + "] - " + tcErrorStr + " error near of `" + tcWhere + "`: " + tcMessage
	Endfunc

	Function throwError(tcType, tcPropertyName, tvPropertyValue, tcMessage)
		Local oExp
		Try
			Throw
		Catch To oExp
			=AddProperty(oExp, 'type', tcType)
			=AddProperty(oExp, tcPropertyName, tvPropertyValue)
			oExp.Message = tcMessage
			Throw
		Endtry
	Endfunc

	Hidden function registerContext(toContext)
		Local loContext
		loContext = CreateObject("ContextContainer", toContext)
		this.oContext = loContext
		this.oInterpreter.oGlobals.define('_ctx', loContext)
	EndFunc
	
Enddefine
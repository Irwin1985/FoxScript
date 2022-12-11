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
Lparameters toContextOrScript, tcFileNameOrScript

* ================================================================================== *
* Token Type constants
* ================================================================================== *
#Define CONSTANT_ADDED			.T.
#Define CRLF					Chr(13) + Chr(10)
#Define TRUE					.T.
#Define false					.F.
#Define TT_EOF					0
#Define TT_IGNORE				1
#Define TT_SEMICOLON			2
#Define TT_RELATIONAL_OPERATOR	3
#Define TT_EQUALITY_OPERATOR	4
#Define TT_SIMPLE_ASSIGN		5
#Define TT_COMPLEX_ASSIGN		6
#Define TT_NUMBER				7
#Define TT_STRING				8
#Define TT_IDENTIFIER			9
#Define TT_LPAREN				10
#Define TT_RPAREN				11
#Define TT_LBRACKET				12
#Define TT_RBRACKET				13
#Define TT_LBRACE				14
#Define TT_RBRACE				15
#Define TT_DOT					16
#Define TT_COMMA				17
#Define TT_COLON				18
#Define TT_LOGICAL_AND			19
#Define TT_LOGICAL_OR			20
#Define TT_LOGICAL_NOT			21
#Define TT_TERM_OPERATOR		22
#Define TT_FACTOR_OPERATOR		23
#Define TT_ARROW				24

* ================================================================================== *
* Keywords constants
* ================================================================================== *
#Define TT_CONST				100
#Define TT_DO					101
#Define TT_FALSE				102
#Define TT_LET					103
#Define TT_BREAK				104
#Define TT_CONTINUE				105
#Define TT_ELSE					106
#Define TT_IMPORT				107
#Define TT_NEW					108
#Define TT_PUBLIC				109
#Define TT_THIS					110
#Define TT_VAR					111
#Define TT_CASE					112
#Define TT_ENUM					113
#Define TT_FOR					114
#Define TT_IN					115
#Define TT_NULL					116
#Define TT_RETURN				117
#Define TT_THROW				118
#Define TT_CATCH				119
#Define TT_EXPORT				120
#Define TT_FUNCTION				121
#Define TT_SUPER				122
#Define TT_TRY					123
#Define TT_WHILE				124
#Define TT_CLASS				125
#Define TT_IF					126
#Define TT_SWITCH				127
#Define TT_TRUE					128
#Define TT_CONTEXT				129
#Define TT_PRINT				130
#Define TT_EXTENDS				131

* ================================================================================== *
* Token Category constants
* ================================================================================== *
#Define TC_LITERAL		1
#Define TC_IGNORABLE	2
#Define TC_KEYWORD		3
#Define TC_IDENTIFIER	4
#Define TC_ASSIGNMENT	5
#Define TC_GENERIC		6
#Define TC_UNARY		7
#Define TC_ASSIGN		8
#Define TC_LESS			9
#Define TC_LESS_EQ		10
#Define TC_GREATER		11
#Define TC_GREATER_EQ	12
#Define TC_EQUAL		13
#Define TC_BANG			14
#Define TC_NOT_EQ		15
#Define TC_PLUS			16
#Define TC_MINUS		17
#Define TC_MUL			18
#Define TC_DIV			19

If Type('_screen.foxscript') = 'U'
	=AddProperty(_Screen, 'foxscript', .Null.)
Endif

_Screen.foxscript = Createobject("FoxScript")

Do Case
Case Pcount() == 1
	Return _Screen.foxScript.Run(toContextOrScript)
Case Pcount() == 2
	Return _Screen.foxScript.Run(toContextOrScript, tcFileNameOrScript)
Otherwise
	Return .Null.
Endcase

* ================================================================================== *
* FoxScript class
* ================================================================================== *
Define Class FoxScript As Custom
	lHadError = .F.
	lHadRunTimeError = .F.
	oContext = .Null.
	Hidden oInterpreter

	Function Init
		This.oInterpreter = Createobject("Interpreter")
	Endfunc

	Function Run(toContextOrScript, tcFileNameOrScript)
		If Pcount() = 2
			This.oContext = toContextOrScript
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

	Function runPrompt
		Do Form Repl
	Endfunc

	Function Execute(tcSource)
		Local loScanner, loTokens, loParser, loResolver, loStatements, llPrintTokens, llPrintAST
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

		loResolver = Createobject("Resolver", This.oInterpreter)
		loResolver.resolve(loStatements)
		* Stop if there was a resolution error.
		If This.lHadError
			Return .Null.
		Endif

		Return This.oInterpreter.interpret(loStatements)
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

Enddefine

* ================================================================================== *
* Scanner class
* ================================================================================== *
Define Class Scanner As Custom
	Hidden cSource
	Hidden nCursor
	Hidden nTokenCounter
	Hidden nLastToken
	Hidden oRegEx
	Hidden oTokens
	Hidden nLine
	Hidden nCol
	Dimension aSpecs[1]

	Function Init(tcSource)
		With This
			.cSource = tcSource
			.nCursor = 1
			.nTokenCounter = 0
			.nLastToken = 0
			.oTokens = Createobject('Collection')
			* Setting the oRegEx properties
			.oRegEx = Createobject("VBScript.RegExp")
			.oRegEx.IgnoreCase = false
			.oRegEx.Global = TRUE
			.nLine = 1
			.nCol = 1

			Local i, lcPattern
			i = 1
			Dimension .aSpecs[100]
			* -----------------------------------------------------------
			* Whitespace:
			.aSpecs[i] = Createobject("Spec", "^\s+", TT_IGNORE, TC_IGNORABLE)

			* -----------------------------------------------------------
			* Comments:
			* Skip single-line comments
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\/\/.*", TT_IGNORE, TC_IGNORABLE)

			* Skip multi-line comments
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\/\*[\s\S]*?\*\/", TT_IGNORE, TC_IGNORABLE)

			* -----------------------------------------------------------
			* Semicolon:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^;", TT_SEMICOLON, TC_GENERIC)

			* -----------------------------------------------------------
			* Numbers:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\d+[_.\d]*", TT_NUMBER, TC_LITERAL)

			* -----------------------------------------------------------
			* Double quoted string:
			i = i + 1
			lcPattern = '^\"(?:[^\"\\^' + Chr(39) + '\\]|\\.)*\"'
			.aSpecs[i] = Createobject("Spec", lcPattern, TT_STRING, TC_LITERAL)

			* -----------------------------------------------------------
			* Single quoted string:
			i = i + 1
			lcPattern = "^'(?:[^\" + Chr(34) + "\\^'\\]|\\.)*'"
			.aSpecs[i] = Createobject("Spec", lcPattern, TT_STRING, TC_LITERAL)

			* -----------------------------------------------------------
			* Backticked string:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^`[^`]*`", TT_STRING, TC_LITERAL)

			* -----------------------------------------------------------
			* Relational Operators:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^[<>]=?", TT_RELATIONAL_OPERATOR, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^[=!]=", TT_EQUALITY_OPERATOR, TC_GENERIC)

			* -----------------------------------------------------------
			* Logical Operators:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\band\b", TT_LOGICAL_AND, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bor\b", TT_LOGICAL_OR, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^!", TT_LOGICAL_NOT, TC_UNARY)

			* -----------------------------------------------------------
			* Keywords:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bconst\b", TT_CONST, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bdo\b", TT_DO, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bextends\b", TT_EXTENDS, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bfalse\b", TT_FALSE, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\blet\b", TT_LET, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bbreak\b", TT_BREAK, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bcontinue\b", TT_CONTINUE, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\belse\b", TT_ELSE, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bimport\b", TT_IMPORT, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bnew\b", TT_NEW, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bprint\b", TT_PRINT, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bpublic\b", TT_PUBLIC, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bthis\b", TT_THIS, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bvar\b", TT_VAR, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bcase\b", TT_CASE, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\benum\b", TT_ENUM, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bfor\b", TT_FOR, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bin\b", TT_IN, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bnull\b", TT_NULL, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\breturn\b", TT_RETURN, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bthrow\b", TT_THROW, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bcatch\b", TT_CATCH, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bexport\b", TT_EXPORT, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bfn\b", TT_FUNCTION, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bsuper\b", TT_SUPER, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\btry\b", TT_TRY, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bwhile\b", TT_WHILE, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bclass\b", TT_CLASS, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bif\b", TT_IF, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bswitch\b", TT_SWITCH, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\btrue\b", TT_TRUE, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\b_ctx\b", TT_CONTEXT, TC_KEYWORD)

			* -----------------------------------------------------------
			* Arrow symbol:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^=>", TT_ARROW, TC_GENERIC)

			* -----------------------------------------------------------
			* Assignment operators: =, +=, -=, *=, /=
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^=", TT_SIMPLE_ASSIGN, TC_ASSIGNMENT)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^[\+\-\*\/]=", TT_COMPLEX_ASSIGN, TC_ASSIGNMENT)

			* -----------------------------------------------------------
			* Math operators: +, -, *, /
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^[\+\-]", TT_TERM_OPERATOR, TC_UNARY)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^[\*\/]", TT_FACTOR_OPERATOR, TC_GENERIC)

			* -----------------------------------------------------------
			* Identifier
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\w+", TT_IDENTIFIER, TC_IDENTIFIER)

			* -----------------------------------------------------------
			* Symbol and Delimiters:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\(", TT_LPAREN, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\)", TT_RPAREN, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\[", TT_LBRACKET, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\]", TT_RBRACKET, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\{", TT_LBRACE, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\}", TT_RBRACE, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\.", TT_DOT, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^,", TT_COMMA, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^:", TT_COLON, TC_GENERIC)
			* Shrink the array
			Dimension .aSpecs[i]
		endwith
	Endfunc

	Function scanTokens
		Local loToken
		Do While TRUE
			loToken = This.getNextToken()
			If Isnull(loToken)
				Exit
			Endif
			This.oTokens.Add(loToken)
		Enddo
		This.oTokens.Add(Createobject("Token", TT_EOF, TC_GENERIC, "", "", This.nLine, This.nCol))

		Return This.oTokens
	Endfunc

	Hidden Function getNextToken
		If This.nCursor > Len(This.cSource)
			Return .Null.
		Endif
		Local lcInput, i, loMatcher, lcLexeme, ln, loToken
		lcInput = Substr(This.cSource, This.nCursor)
		For Each loSpec In This.aSpecs
			* loSpec = this.aSpecs[i]
			This.oRegEx.Pattern = loSpec.cPattern

			loMatcher = This.oRegEx.Execute(lcInput)
			If Type('loMatcher') != 'O' Or Empty(loMatcher.Count)
				Loop
			Endif
			* Increase cursor to the length of matched string.
			This.nCursor = This.nCursor + loMatcher.Item[0].Length
			lcLexeme = loMatcher.Item[0].Value

			* Count number of lines
			ln = Len(lcLexeme) - Len(Strtran(lcLexeme, Chr(10)))
			This.nLine = This.nLine + ln
			If ln > 0
				This.nCol = 1
			Endif

			* check for the IGNORE token type.
			If loSpec.nType == TT_IGNORE
				This.nCol = This.nCol + Len(lcLexeme) && update column number
				Return This.getNextToken()
			Endif

			* Return the token and value
			Local lvValue, lnCategory
			lnCategory = loSpec.nCategory
			Do Case
			Case loSpec.nType == TT_NUMBER
				lcLexeme = Strtran(lcLexeme, '_')
				lvValue = Val(lcLexeme)
			Case loSpec.nType == TT_TRUE
				lvValue = TRUE
			Case loSpec.nType == TT_FALSE
				lvValue = false
			Case loSpec.nType == TT_NULL
				lvValue = .Null.
			Case loSpec.nType == TT_STRING
				If Left(lcLexeme,1) == '`' && raw string
					lcLexeme = Substr(lcLexeme, 2, Len(lcLexeme)-2)
				Else
					lcLexeme = Substr(lcLexeme, 2, Len(lcLexeme)-2)
					lcLexeme = Strtran(lcLexeme, '\r', Chr(13))
					lcLexeme = Strtran(lcLexeme, '\n', Chr(10))
					lcLexeme = Strtran(lcLexeme, '\t', Chr(9))
					lcLexeme = Strtran(lcLexeme, '\"', '"')
					lcLexeme = Strtran(lcLexeme, '\', "'")
				Endif
				lvValue = lcLexeme
			Case Inlist(loSpec.nType, TT_COMPLEX_ASSIGN, TT_TERM_OPERATOR, TT_FACTOR_OPERATOR, TT_RELATIONAL_OPERATOR, TT_EQUALITY_OPERATOR, TT_LOGICAL_NOT)
				Do Case
				Case Inlist(lcLexeme, "+", "+=")
					lnCategory = TC_PLUS
				Case Inlist(lcLexeme, "-", "-=")
					lnCategory = TC_MINUS
				Case Inlist(lcLexeme, "*", "*=")
					lnCategory = TC_MUL
				Case Inlist(lcLexeme, "/", "/=")
					lnCategory = TC_DIV
				Case lcLexeme == "="
					lnCategory = TC_ASSIGN
				Case lcLexeme == "<"
					lnCategory = TC_LESS
				Case lcLexeme == "<="
					lnCategory = TC_LESS_EQ
				Case lcLexeme == ">"
					lnCategory = TC_GREATER
				Case lcLexeme == ">="
					lnCategory = TC_GREATER_EQ
				Case lcLexeme == "=="
					lnCategory = TC_EQUAL
				Case lcLexeme == "!"
					lnCategory = TC_BANG
				Case lcLexeme == "!="
					lnCategory = TC_NOT_EQ
				Endcase
			Otherwise
				lvValue = lcLexeme
			Endcase
			loToken = Createobject("Token", loSpec.nType, lnCategory, lcLexeme, lvValue, This.nLine, This.nCol)
			This.nCol = This.nCol + Len(lcLexeme)

			Return loToken
		Endfor
		_Screen.FoxScript.errorLine(This.nLine, This.nCol, "Unknown character: " + Substr(lcInput, 1, 1))
		Return .Null.
	Endfunc

Enddefine

* ================================================================================== *
* The Specification class for regular exp.
* ================================================================================== *
Define Class Spec As Custom
	nType = 0
	cPattern = ''
	nCategory = 0
	Function Init(tcPattern, tnType, tnCategory)
		This.nType = tnType
		This.cPattern = tcPattern
		This.nCategory = tnCategory
	Endfunc
Enddefine

* ================================================================================== *
* The Token class
* ================================================================================== *
Define Class Token As Custom
	Type = 0
	category = 0
	lexeme = ''
	literal = .Null.
	Line = 0
	Col = 0

	Function Init(tnType, tnCategory, tcLexeme, tnLiteral, tnLine, tnCol)
		This.Type = tnType
		This.category = tnCategory
		This.lexeme = tcLexeme
		This.literal = tnLiteral
		This.Line = tnLine
		This.Col = tnCol
	Endfunc

	Function toString
		Return foxScriptTokenToStr(This.Type) + "[" + Alltrim(Str(This.Line)) + ":" + Alltrim(Str(This.Col)) + "]<lexeme: '" + This.lexeme + "'>"
	Endfunc
Enddefine

* ================================================================================== *
* helper functions
* ================================================================================== *
Function foxScriptTokenToStr(tnTokenType)
	Do Case
	Case tnTokenType == TT_EOF
		Return "TT_EOF"
	Case tnTokenType == TT_IGNORE
		Return "TT_IGNORE"
	Case tnTokenType == TT_SEMICOLON
		Return "TT_SEMICOLON"
	Case tnTokenType == TT_RELATIONAL_OPERATOR
		Return "TT_RELATIONAL_OPERATOR"
	Case tnTokenType == TT_EQUALITY_OPERATOR
		Return "TT_EQUALITY_OPERATOR"
	Case tnTokenType == TT_EQUALITY_OPERATOR
		Return "TT_EQUALITY_OPERATOR"
	Case tnTokenType == TT_SIMPLE_ASSIGN
		Return "TT_SIMPLE_ASSIGN"
	Case tnTokenType == TT_COMPLEX_ASSIGN
		Return "TT_COMPLEX_ASSIGN"
	Case tnTokenType == TT_NUMBER
		Return "TT_NUMBER"
	Case tnTokenType == TT_STRING
		Return "TT_STRING"
	Case tnTokenType == TT_IDENTIFIER
		Return "TT_IDENTIFIER"
	Case tnTokenType == TT_LPAREN
		Return "TT_LPAREN"
	Case tnTokenType == TT_RPAREN
		Return "TT_RPAREN"
	Case tnTokenType == TT_LBRACKET
		Return "TT_LBRACKET"
	Case tnTokenType == TT_RBRACKET
		Return "TT_RBRACKET"
	Case tnTokenType == TT_LBRACE
		Return "TT_LBRACE"
	Case tnTokenType == TT_RBRACE
		Return "TT_RBRACE"
	Case tnTokenType == TT_DOT
		Return "TT_DOT"
	Case tnTokenType == TT_COMMA
		Return "TT_COMMA"
	Case tnTokenType == TT_COLON
		Return "TT_COLON"
	Case tnTokenType == TT_LOGICAL_AND
		Return "TT_LOGICAL_AND"
	Case tnTokenType == TT_LOGICAL_OR
		Return "TT_LOGICAL_OR"
	Case tnTokenType == TT_LOGICAL_NOT
		Return "TT_LOGICAL_NOT"
	Case tnTokenType == TT_TERM_OPERATOR
		Return "TT_TERM_OPERATOR"
	Case tnTokenType == TT_FACTOR_OPERATOR
		Return "TT_FACTOR_OPERATOR"
	Case tnTokenType == TT_CONST
		Return "TT_CONST"
	Case tnTokenType == TT_DO
		Return "TT_DO"
	Case tnTokenType == TT_FALSE
		Return "TT_FALSE"
	Case tnTokenType == TT_EXTENDS
		Return "TT_EXTENDS"
	Case tnTokenType == TT_LET
		Return "TT_LET"
	Case tnTokenType == TT_BREAK
		Return "TT_BREAK"
	Case tnTokenType == TT_CONTINUE
		Return "TT_CONTINUE"
	Case tnTokenType == TT_ELSE
		Return "TT_ELSE"
	Case tnTokenType == TT_IMPORT
		Return "TT_IMPORT"
	Case tnTokenType == TT_NEW
		Return "TT_NEW"
	Case tnTokenType == TT_PUBLIC
		Return "TT_PUBLIC"
	Case tnTokenType == TT_THIS
		Return "TT_THIS"
	Case tnTokenType == TT_VAR
		Return "TT_VAR"
	Case tnTokenType == TT_CASE
		Return "TT_CASE"
	Case tnTokenType == TT_ENUM
		Return "TT_ENUM"
	Case tnTokenType == TT_FOR
		Return "TT_FOR"
	Case tnTokenType == TT_IN
		Return "TT_IN"
	Case tnTokenType == TT_NULL
		Return "TT_NULL"
	Case tnTokenType == TT_RETURN
		Return "TT_RETURN"
	Case tnTokenType == TT_THROW
		Return "TT_THROW"
	Case tnTokenType == TT_CATCH
		Return "TT_CATCH"
	Case tnTokenType == TT_EXPORT
		Return "TT_EXPORT"
	Case tnTokenType == TT_FUNCTION
		Return "TT_FUNCTION"
	Case tnTokenType == TT_SUPER
		Return "TT_SUPER"
	Case tnTokenType == TT_TRY
		Return "TT_TRY"
	Case tnTokenType == TT_WHILE
		Return "TT_WHILE"
	Case tnTokenType == TT_CLASS
		Return "TT_CLASS"
	Case tnTokenType == TT_IF
		Return "TT_IF"
	Case tnTokenType == TT_PRINT
		Return "TT_PRINT"
	Case tnTokenType == TT_SWITCH
		Return "TT_SWITCH"
	Case tnTokenType == TT_TRUE
		Return "TT_TRUE"
	Case tnTokenType == TT_CONTEXT
		Return "TT_CONTEXT"
	Case tnTokenType == TT_ARROW
		Return "TT_ARROW"
	Otherwise
		Return "UNKNOWN"
	Endcase
Endfunc

* ================================================================================== *
* The Parser Class
* ================================================================================== *
Define Class Parser As Custom
	Hidden oTokens
	Hidden nCurrent
	Hidden previous
	Hidden peek
	Hidden isAtEnd

	Function Init(toTokens)
		This.oTokens = toTokens
		This.nCurrent = 1 && First recognised token
	Endfunc

	Function parse
		Local loStatements
		loStatements = Createobject('Collection')

		Do While !This.isAtEnd
			loStatements.Add(This.declaration())
		Enddo

		Return loStatements
	Endfunc

	Hidden Function declaration
		Local loDeclaration, lSeguir
		loDeclaration = .Null.
		lSeguir = true
		Try
			
			If This.match(TT_CLASS)
				lSeguir = false
				loDeclaration = This.classDeclaration()
			EndIf
			If lSeguir and This.check(TT_FUNCTION)
				Local lnType1
				lnType1 = This.peekNext(1)
				If lnType1 == TT_IDENTIFIER && function declaration
					this.match(TT_FUNCTION)
					lSeguir = false
					loDeclaration = This.functionDeclaration("function")
				EndIf
			EndIf
			if lSeguir and This.match(TT_VAR)
				lSeguir = false
				loDeclaration = This.varDeclaration()
			EndIf
			If lSeguir
				loDeclaration = This.statement()
			EndIf
		Catch To loEx
			If Type('loEx.Type') == 'C'
				This.synchronize()
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
		Return loDeclaration
	Endfunc

	Hidden Function classDeclaration
		Local loName, loSuperClass, loMethods
		loName = This.consume(TT_IDENTIFIER, "Expect class name.")
		loSuperClass = .Null.

		If This.match(TT_EXTENDS) && this class extends from another class (inheritance)
			This.consume(TT_IDENTIFIER, "Expect superclass name.")
			loSuperClass = Createobject("Variable", This.previous)
		Endif

		This.consume(TT_LBRACE, "Expect '{' before class body.")

		loMethods = Createobject('Collection')
		Do While !This.Check(TT_RBRACE) And !This.isAtEnd
			loMethods.Add(This.functionDeclaration("method"))
		Enddo

		This.consume(TT_RBRACE, "Expect '}' after class body.")

		Return Createobject("ClassNode", loName, loSuperClass, loMethods)
	Endfunc

	Hidden Function statement
		If This.match(TT_FOR)
			Return This.forStatement()
		Endif
		If This.match(TT_IF)
			Return This.ifStatement()
		Endif
		If This.match(TT_PRINT)
			Return This.printStatement()
		Endif
		If This.match(TT_RETURN)
			Return This.returnStatement()
		Endif
		If This.match(TT_WHILE)
			Return This.whileStatement()
		Endif
		If This.Check(TT_LBRACE)
			Local lnType1, lnType2, lSeguir
			lnType1 = This.peekNext(1)
			lnType2 = This.peekNext(2)
			lSeguir = TRUE
			If Inlist(lnType1, TT_IDENTIFIER, TT_STRING) And lnType2 == TT_COLON
				lSeguir = false
			Endif
			If lSeguir
				This.advance() && '{'
				Return Createobject("Block", This.blockStatement())
			Endif
		Endif
		Return This.expressionStatement()
	Endfunc
	* TODO(irwin): finish this later...
	Hidden Function forStatement
		Return .Null.
	Endfunc

	Hidden Function ifStatement
		Local loCondition, loThenBranch, loElseBranch
		This.consume(TT_LPAREN, "Expect '(' after 'if'.")
		loCondition = This.Expression()
		This.consume(TT_RPAREN, "Expect ')' after if condition.")
		loThenBranch = This.statement()
		loElseBranch = .Null.
		If This.match(TT_ELSE)
			loElseBranch = This.statement()
		Endif

		Return Createobject("IfNode", loCondition, loThenBranch, loElseBranch)
	Endfunc

	Hidden Function printStatement
		Local loExpression
		loExpression = This.Expression()
		This.consume(TT_SEMICOLON, "Expect ';' after value.")
		Return Createobject("Print", loExpression)
	Endfunc

	Hidden Function returnStatement
		Local loKeyword, loValue
		loKeyword = This.previous
		loValue = .Null.
		If !This.Check(TT_SEMICOLON)
			loValue = This.Expression()
		Endif
		This.consume(TT_SEMICOLON, "Expect ';' after return value.")
		Return Createobject("ReturnNode", loKeyword, loValue)
	Endfunc

	Hidden Function varDeclaration
		Local loName, loInitializer
		loName = This.consume(TT_IDENTIFIER, "Expect variable name.")
		loInitializer = .Null.
		If This.match(TT_SIMPLE_ASSIGN)
			loInitializer = This.Expression()
		Endif
		This.consume(TT_SEMICOLON, "Expect ';' after variable declaration.")

		Return Createobject("Var", loName, loInitializer)
	Endfunc

	Hidden Function whileStatement
		This.consume(TT_LPAREN, "Expect '(' after 'while'.")
		Local loCondition, loBody
		loCondition = This.Expression()
		This.consume(TT_RPAREN, "Expect ')' after condition.")
		loBody = This.statement()

		Return Createobject("WhileNode", loCondition, loBody)
	Endfunc

	Hidden Function expressionStatement
		Local loExpr
		loExpr = This.Expression()
		This.consume(TT_SEMICOLON, "Expect ';' after expression.")
		Return Createobject("Expression", loExpr)
	Endfunc

	Hidden Function functionDeclaration(tcKind)
		Local loName, loParams, loBody
		loName = This.consume(TT_IDENTIFIER, "Expect " + tcKind + " name.")
		This.consume(TT_LPAREN, "Expect '(' after " + tcKind + " name.")

		loParams = Createobject('Collection')
		If !This.Check(TT_RPAREN)
			loParams.Add(This.consume(TT_IDENTIFIER, "Expect parameter name."))
			Do While This.match(TT_COMMA)
				loParams.Add(This.consume(TT_IDENTIFIER, "Expect parameter name."))
			Enddo
		Endif
		This.consume(TT_RPAREN, "Expect ')' after parameters.")
		* Parse Body
		This.consume(TT_LBRACE, "Expect '{' before " + tcKind + " body.")
		loBody = This.blockStatement()

		Return Createobject("FunctionNode", loName, loParams, loBody)
	Endfunc

	Hidden Function blockStatement
		Local loStatements
		loStatements = Createobject('Collection')

		Do While !This.Check(TT_RBRACE) And !This.isAtEnd
			loStatements.Add(This.declaration())
		Enddo

		This.consume(TT_RBRACE, "Expect '}' after block.")

		Return loStatements
	Endfunc

	* ==========================================================
	* Parsing Expression
	Hidden Function Expression
		Return This.assignment()
	Endfunc

	Hidden Function assignment
		Local loExpr
		loExpr = This.logicalOr()

		If This.match(TT_SIMPLE_ASSIGN)
			Local loEquals, loValue
			loEquals = This.previous && catch the equal token
			loValue = This.assignment() && self recursive
			Do Case
			Case loExpr.Class == 'Variable' &&
				Return Createobject("Assign", loExpr.oName, loValue)
			Case loExpr.Class == 'Get'
				Return Createobject("Set", loExpr.oObject, loExpr.oName, loValue)
			Otherwise
				This.parseError(loEquals, "Invalid assignment target.")
			Endcase
		Endif

		Return loExpr
	Endfunc

	Hidden Function logicalOr
		Local loLeft
		loLeft = This.logicalAnd()
		Do While This.match(TT_LOGICAL_OR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.logicalAnd()
			loLeft  = Createobject("Logical", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function logicalAnd
		Local loLeft
		loLeft = This.equality()
		Do While This.match(TT_LOGICAL_AND)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.equality()
			loLeft  = Createobject("Logical", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function equality
		Local loLeft
		loLeft = This.comparison()
		Do While This.match(TT_EQUALITY_OPERATOR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.comparison()
			loLeft  = Createobject("Binary", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function comparison
		Local loLeft
		loLeft = This.Term()
		Do While This.match(TT_RELATIONAL_OPERATOR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.Term()
			loLeft  = Createobject("Binary", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function Term
		Local loLeft
		loLeft = This.factor()
		Do While This.match(TT_TERM_OPERATOR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.factor()
			loLeft  = Createobject("Binary", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function factor
		Local loLeft
		loLeft = This.unary()
		Do While This.match(TT_FACTOR_OPERATOR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.unary()
			loLeft  = Createobject("Binary", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function unary
		If This.match(TT_TERM_OPERATOR, TT_LOGICAL_NOT)
			Return Createobject("Unary", This.previous, This.unary())
		Endif
		Return This.Call()
	Endfunc

	Hidden Function finishCall(toCallee)
		Local loArguments, loParen
		loArguments = Createobject('Collection')
		If !This.Check(TT_RPAREN)
			loArguments.Add(This.Expression())
			Do While This.match(TT_COMMA)
				loArguments.Add(This.Expression())
			Enddo
		Endif
		loParen = This.consume(TT_RPAREN, "Expect ')' after arguments.")
		Return Createobject("Call", toCallee, loParen, loArguments)
	Endfunc

	Hidden Function Call
		Local loExpr, loName
		loExpr = This.Primary()

		Do While TRUE
			Do Case
			Case This.match(TT_LPAREN)
				loExpr = This.finishCall(loExpr)
			Case This.match(TT_DOT)
				loName = This.consume(TT_IDENTIFIER, "Expect property name after '.'.")
				loExpr = Createobject("Get", loExpr, loName)
			Otherwise
				Exit
			Endcase
		Enddo
		Return loExpr
	Endfunc

	Hidden Function Primary
		Do Case
		Case This.match(TT_FALSE)
			Return Createobject("Literal", false)
		Case This.match(TT_TRUE)
			Return Createobject("Literal", TRUE)
		Case This.match(TT_NULL)
			Return Createobject("Literal", .Null.)
		Case This.match(TT_NUMBER, TT_STRING)
			Return Createobject("Literal", This.previous.literal)
		Case This.match(TT_SUPER)
			Local loKeyword, loMethod
			loKeyword = This.previous
			This.consume(TT_DOT, "Expect '.' after 'super'.")
			loMethod = This.consume(TT_IDENTIFIER, "Expect superclass method name.")
			Return Createobject("Super", loKeyword, loMethod)
		Case This.match(TT_THIS)
			Return Createobject("ThisNode", This.previous)
		Case This.match(TT_IDENTIFIER)
			Return Createobject("Variable", This.previous)
		Case This.match(TT_LPAREN)
			Local loExpression
			loExpression = This.Expression()
			This.consume(TT_RPAREN, "Expect ')' after expression.")
			Return Createobject("Grouping", loExpression)
		Case This.match(TT_LBRACKET)
			Return This.parseArrayLiteral()
		Case This.match(TT_LBRACE)
			Return This.parseObject()
		Case This.match(TT_FUNCTION)
			Return This.parseFunctionExpr()
		Otherwise
			This.parseError(This.peek, "Expect expression.")
		Endcase
	Endfunc

	Hidden Function parseArrayLiteral
		Local loKeyword, loElements
		loKeyword = This.previous && token '['
		loElements = Createobject('Dictionary')
		If !This.Check(TT_RBRACKET)
			loElements.Add(This.Expression())
			Do While This.match(TT_COMMA)
				loElements.Add(This.Expression())
			Enddo
		Endif
		This.consume(TT_RBRACKET, "Expect ']' after array elements.")

		Return Createobject("ArrayLiteral", loKeyword, loElements)
	Endfunc

	Hidden Function parseObject
		Local loKeyword, loElements, loProperty, loValue
		loKeyword = This.previous && token '{'
		loElements = Createobject('Dictionary')
		If !This.Check(TT_RBRACE)
			loProperty = This.Expression()
			If !This.checkProperty(loProperty)
				This.parseError(loKeyword, "Invalid property name.")
			Endif
			This.consume(TT_COLON, "Expect ':' after property name.")
			loValue = This.Expression()
			loElements.Add(loValue, This.getPropertyName(loProperty))

			Do While This.match(TT_COMMA)
				loProperty = This.Expression()
				If !This.checkProperty(loProperty)
					This.parseError(loKeyword, "Invalid property name.")
				Endif
				This.consume(TT_COLON, "Expect ':' after property name.")
				loValue = This.Expression()
				loElements.Add(loValue, This.getPropertyName(loProperty))
			Enddo
		Endif
		This.consume(TT_RBRACE, "Expect '}' after array elements.")

		Return Createobject("ObjectLiteral", loKeyword, loElements)
	Endfunc

	Hidden Function parseFunctionExpr
		Local loParams, loBody
		This.consume(TT_LPAREN, "Expect '(' after function name.")

		loParams = Createobject('Collection')
		If !This.Check(TT_RPAREN)
			loParams.Add(This.consume(TT_IDENTIFIER, "Expect parameter name."))
			Do While This.match(TT_COMMA)
				loParams.Add(This.consume(TT_IDENTIFIER, "Expect parameter name."))
			Enddo
		Endif
		This.consume(TT_RPAREN, "Expect ')' after parameters.")
		* Parse Body
		This.consume(TT_LBRACE, "Expect '{' before function body.")
		loBody = This.blockStatement()

		Return Createobject("FunctionExpr", loParams, loBody)
	Endfunc

	Hidden Function checkProperty(toProperty)
		If toProperty.Class = 'Literal'
			Return Type('toProperty.oValue') == 'C'
		Endif
		Return toProperty.Class = 'Variable'
	Endfunc

	Hidden Function getPropertyName(toProperty)
		If toProperty.Class = 'Literal'
			Return toProperty.oValue
		Endif
		Return toProperty.oName.lexeme
	Endfunc
	* ==========================================================

	Hidden Function match(tnType1, tnType2, tnType3)
		If This.Check(tnType1)
			This.advance()
			Return TRUE
		Endif
		If !Empty(tnType2) And This.Check(tnType2)
			This.advance()
			Return TRUE
		Endif
		If !Empty(tnType3) And This.Check(tnType3)
			This.advance()
			Return TRUE
		Endif
		Return false
	Endfunc

	Hidden Function consume(tnType, tcErrorMsg)
		If This.Check(tnType)
			Return This.advance()
		Endif
		This.parseError(This.peek, tcErrorMsg)
	Endfunc

	Hidden Function Check(tnType)
		If This.isAtEnd
			Return false
		Endif
		Return This.peek.Type == tnType
	Endfunc

	Hidden Function advance
		If !This.isAtEnd
			This.nCurrent = This.nCurrent + 1
		Endif
		Return This.previous
	Endfunc

	Hidden Function isAtEnd_access
		Return This.peek.Type == TT_EOF
	Endfunc

	Hidden Function peek_access
		Return This.oTokens.Item(This.nCurrent)
	Endfunc

	Hidden Function peekNext(tnOffset)
		If (This.nCurrent + tnOffset) < This.oTokens.Count
			Return This.oTokens.Item(This.nCurrent+tnOffset).Type
		Endif
		Return TT_EOF
	Endfunc

	Hidden Function previous_access
		Return This.oTokens.Item(This.nCurrent-1)
	Endfunc

	Hidden Procedure parseError(toToken, tcErrorMessage)
		_Screen.FoxScript.errorToken(toToken, tcErrorMessage)
		_Screen.FoxScript.throwError('ParseError', 'token', toToken, tcErrorMessage)
	Endproc

	Hidden Function synchronize
		This.advance()
		Do While !This.isAtEnd
			If This.previous.Type == TT_SEMICOLON
				Return
			Endif
			If Inlist(This.peek.Type, ;
					TT_CLASS, ;
					TT_FUNCTION, ;
					TT_VAR, ;
					TT_LET, ;
					TT_FOR, ;
					TT_IF, ;
					TT_WHILE, ;
					TT_PRINT, ;
					TT_RETURN)
				Return
			Endif
			This.advance()
		Enddo
	Endfunc
Enddefine

* ================================================================================== *
* ASTPrinter class
* ================================================================================== *
Define Class ASTPrinter As Custom

	Function Print(toStmt)
		Local lcOutput
		lcOutput = ''
		For Each loStmt In toStmt
			lcOutput = lcOutput + loStmt.Accept(This) + CRLF
		Endfor
		Return lcOutput
	Endfunc

	Function Evaluate(toExpr)
		Return toExpr.Accept(This)
	Endfunc

	Function visitArrayLiteralExpr(toExpr)
		Local lcOutput, i
		lcOutput = '['
		i = 0
		For Each loElement In toExpr.oElements
			i = i + 1
			If i > 1
				lcOutput = lcOutput + ','
			Endif
			lcOutput = lcOutput + This.Evaluate(loElement)
		Endfor

		lcOutput = lcOutput + ']'
		Return lcOutput
	Endfunc

	Function visitBlockStmt(toStmt)
		Local lcOutput
		lcOutput = '{' + CRLF

		For Each loStmt In toStmt.oStatements
			lcOutput = lcOutput + loStmt.Accept(This) + CRLF
		Endfor
		lcOutput = lcOutput + CRLF + '}'
		Return lcOutput
	Endfunc

	Function visitClassNodeStmt(toStmt)
		Local lcOutput
		lcOutput = 'class ' + toStmt.oName.lexeme
		If !Isnull(toStmt.oSuperclass)
			lcOutput = lcOutput + ' extends ' + toStmt.oSuperclass.oName.lexeme
		Endif

		lcOutput = lcOutput + ' {'
		If !Isnull(toStmt.oMethods)
			For Each loMethod In toStmt.oMethods
				lcOutput = lcOutput + loMethod.Accept(This) + CRLF
			Endfor
		Endif
		lcOutput = lcOutput + ' }'

		Return lcOutput
	Endfunc

	Function visitExpressionStmt(toStmt)
		Return This.Evaluate(toStmt.oExpression) + ';'
	Endfunc

	Function visitFunctionNodeStmt(toStmt)
		Local lcOutput
		lcOutput = "fn " + toStmt.oName.lexeme + "("
		If !Isnull(toStmt.oParams)
			Local i
			i = 0
			For Each loParam In toStmt.oParams
				i = i + 1
				If i > 1
					lcOutput = lcOutput + ','
				Endif
				lcOutput = lcOutput + loParam.lexeme
			Endfor
		Endif
		lcOutput = lcOutput + ')'

		If !Isnull(toStmt.oBody)
			lcOutput = lcOutput + CRLF + '{' + This.Print(toStmt.oBody) + CRLF + '}'
		Endif

		Return lcOutput
	Endfunc

	Function visitGroupingExpr(toExpr)
		Return This.Evaluate(toExpr.oExpression)
	Endfunc

	Function visitIfNodeStmt(toStmt)
		Local lcOutput
		lcOutput = 'if (' + This.Evaluate(toStmt.oCondition) + ')' + CRLF
		If !Isnull(toStmt.oThenBranch)
			lcOutput = lcOutput + toStmt.oThenBranch.Accept(This)
		Endif

		If !Isnull(toStmt.oElseBranch)
			lcOutput = lcOutput + 'else '+ CRLF + toStmt.oElseBranch.Accept(This)
		Endif

		Return lcOutput
	Endfunc

	Function visitLiteralExpr(toExpr)
		Local lcValue
		If Isnull(toExpr.oValue)
			lcValue = "null"
		Endif
		If Type('toExpr.oValue') == 'C'
			lcValue = '"' + toExpr.oValue + '"'
		Else
			lcValue = Transform(toExpr.oValue)
		Endif
		Return lcValue
	Endfunc

	Function visitBinaryExpr(toExpr)
		Return '(' + toExpr.oLeft.Accept(This) + toExpr.oOperator.lexeme + toExpr.oRight.Accept(This) + ')'
	Endfunc

	Function visitGetExpr(toExpr)
		Return toExpr.oObject.Accept(This) + '.' + toExpr.oName.lexeme
	EndFunc
	
	Function visitFunctionExpr(toExpr)
		Local lcOutput
		lcOutput = "fn ("
		If !Isnull(toStmt.oParams)
			Local i
			i = 0
			For Each loParam In toStmt.oParams
				i = i + 1
				If i > 1
					lcOutput = lcOutput + ','
				Endif
				lcOutput = lcOutput + loParam.lexeme
			Endfor
		Endif
		lcOutput = lcOutput + ')'

		If !Isnull(toStmt.oBody)
			lcOutput = lcOutput + CRLF + '{' + This.Print(toStmt.oBody) + CRLF + '}'
		Endif

		Return lcOutput
	EndFunc

	Function visitObjectLiteralExpr(toExpr)
		Local lcOutput, i, lcKey, loValue
		lcOutput = '{'
		i = 0
		For i = 0 To toExpr.oElements.Count
			lcKey = toExpr.oElements.GetKey(i)
			loValue = This.Evaluate(toExpr.oElements.Item(i))
			If i > 1
				lcOutput = lcOutput + ','
			Endif
			lcOutput = lcOutput + lcKey + ':' + loValue
		Endfor

		lcOutput = lcOutput + '}'
		Return lcOutput
	Endfunc

	Function visitPrintStmt(toStmt)
		Return This.Evaluate(toStmt.oExpression) + ';'
	Endfunc

	Function visitReturnNodeStmt(toStmt)
		If !Isnull(toStmt.oValue)
			Return 'return ' + This.Evaluate(toStmt.oValue) + ';'
		Endif
		Return 'return'
	Endfunc

	Function visitSetExpr(toExpr)
		Return toExpr.oObject.Accept(This) + '.' + toExpr.oName.lexeme + ' = ' + This.Evaluate(toExpr.oValue)
	Endfunc

	Function visitThisNodeExpr(toExpr)
		Return 'this'
	Endfunc

	Function visitUnaryExpr(toExpr)
		Return '(' + toExpr.oOperator.lexeme + toExpr.oRight.Accept(This) + ')'
	Endfunc

	Function visitVarStmt(toStmt)
		Local lcOutput
		lcOutput = "var " + toStmt.oName.lexeme
		If !Isnull(toStmt.oInitializer)
			lcOutput = lcOutput + " = " + This.Evaluate(toStmt.oInitializer)
		Endif
		lcOutput = lcOutput + ";"
		Return lcOutput
	Endfunc

	Function visitVariableExpr(toExpr)
		Return toExpr.oName.lexeme
	Endfunc

	Function visitWhileNodeStmt(toStmt)
		Return 'while (' + This.Evaluate(toStmt.oCondition) + ')' + toStmt.oBody.Accept(This)
	Endfunc

Enddefine

* ================================================================================== *
* Resolver Class
* ================================================================================== *
* Function Type Constants
#Define FT_NONE 		0
#Define FT_FUNCTION 	1
#Define FT_INITIALIZER 	2
#Define FT_METHOD 		3

* Class Type Constants
#Define CLT_NONE		0
#Define CLT_CLASS		1
#Define CLT_SUBCLASS	2

Define Class Resolver As Custom
	oInterpreter = .Null.
	oScopes = .Null.
	nCurrentFunction = FT_NONE
	nCurrentClass = CLT_NONE

	Function Init(toInterpreter)
		This.oInterpreter = toInterpreter
		This.oScopes = Createobject("Stack")
	Endfunc

	Function resolve(toStatements)
		For Each loStmt In toStatements
			This.resolveStmt(loStmt)
		Endfor
	Endfunc

	Function visitArrayLiteralExpr(toExpr)
		Return .Null.
	Endfunc

	Function visitBlockStmt(toStmt)
		This.beginScope()
		This.resolve(toStmt.oStatements)
		This.endScope()

		Return .Null.
	Endfunc

	Function visitClassNodeStmt(toStmt)
		Local lnEnclosingClass
		lnEnclosingClass = This.nCurrentClass
		This.nCurrentClass = CLT_CLASS

		This.Declare(toStmt.oName)
		This.Define(toStmt.oName)

		If !Isnull(toStmt.oSuperclass) And toStmt.oName.lexeme == toStmt.oSuperclass.oName.lexeme
			_Screen.FoxScript.errorToken(toStmt.oSuperclass.oName, "A class can't inherit from itself.")
		Endif

		If !Isnull(toStmt.oSuperclass)
			This.nCurrentClass = CLT_SUBCLASS
			This.resolveExpr(toStmt.oSuperclass)
		Endif

		If !Isnull(toStmt.oSuperclass)
			This.beginScope()
			Local loScope
			loScope = This.oScopes.peek()
			loScope.put('super', TRUE)
		Endif

		This.beginScope()
		Local loScope, lnDeclaration
		loScope = This.oScopes.peek()
		loScope.put('this', TRUE)

		For Each loMethod In toStmt.oMethods
			lnDeclaration = FT_METHOD
			If loMethod.oName.lexeme == 'init'
				lnDeclaration = FT_INITIALIZER
			Endif

			This.resolveFunction(loMethod, lnDeclaration)
		Endfor

		This.endScope()

		If !Isnull(toStmt.oSuperclass)
			This.endScope()
		Endif

		This.nCurrentClass = lnEnclosingClass

		Return .Null.
	Endfunc

	Function visitExpressionStmt(toStmt)
		This.resolveExpr(toStmt.oExpression)
		Return .Null.
	Endfunc

	Function visitFunctionNodeStmt(toStmt)
		This.Declare(toStmt.oName)
		This.Define(toStmt.oName)

		This.resolveFunction(toStmt, FT_FUNCTION)

		Return .Null.
	Endfunc

	Function visitIfNodeStmt(toStmt)
		This.resolveExpr(toStmt.oCondition)
		This.resolveStmt(toStmt.oThenBranch)

		If !Isnull(toStmt.oElseBranch)
			This.resolveStmt(toStmt.oElseBranch)
		Endif

		Return .Null.
	Endfunc

	Function visitObjectLiteralExpr(toExpr)
		For each loElement in toExpr.oElements
			this.resolveExpr(loElement)
		EndFor
		Return .null.
	Endfunc

	Function visitPrintStmt(toStmt)
		This.resolveExpr(toStmt.oExpression)
		Return .Null.
	Endfunc

	Function visitReturnNodeStmt(toStmt)
		If This.nCurrentFunction == FT_NONE
			_Screen.FoxScript.errorToken(toStmt.oKeyword, "Can't return from top-level code.")
		Endif

		If !Isnull(toStmt.oValue)
			If This.nCurrentFunction == FT_INITIALIZER
				_Screen.FoxScript.errorToken(toStmt.oKeyword, "Can't return a value from an initializer.")
			Endif

			This.resolveExpr(toStmt.oValue)
		Endif

		Return .Null.
	Endfunc

	Function visitVarStmt(toStmt)
		This.Declare(toStmt.oName)
		If !Isnull(toStmt.oInitializer)
			This.resolveExpr(toStmt.oInitializer)
		Endif
		This.Define(toStmt.oName)

		Return .Null.
	Endfunc

	Function visitWhileNodeStmt(toStmt)
		This.resolveExpr(toStmt.oCondition)
		This.resolveStmt(toStmt.oBody)
		Return .Null.
	Endfunc

	Function visitAssignExpr(toExpr)
		This.resolveExpr(toExpr.oValue)
		This.resolveLocal(toExpr, toExpr.oName)
		Return .Null.
	Endfunc

	Function visitBinaryExpr(toExpr)
		This.resolveExpr(toExpr.oLeft)
		This.resolveExpr(toExpr.oRight)
		Return .Null.
	Endfunc

	Function visitCallExpr(toExpr)
		This.resolveExpr(toExpr.oCallee)
		For Each loArgument In toExpr.oArguments
			This.resolveExpr(loArgument)
		Endfor

		Return .Null.
	Endfunc

	Function visitGetExpr(toExpr)
		This.resolveExpr(toExpr.oObject)
		Return .Null.
	EndFunc
	
	Function visitFunctionExpr(toExpr)
		This.resolveFunction(toExpr, FT_FUNCTION)

		Return .Null.
	Endfunc	

	Function visitGroupingExpr(toExpr)
		This.resolveExpr(toExpr.oExpression)
		Return .Null.
	Endfunc

	Function visitLiteralExpr(toExpr)
		Return .Null.
	Endfunc

	Function visitLogicalExpr(toExpr)
		This.resolveExpr(toExpr.oLeft)
		This.resolveExpr(toExpr.oRight)
		Return .Null.
	Endfunc

	Function visitSetExpr(toExpr)
		This.resolveExpr(toExpr.oValue)
		This.resolveExpr(toExpr.oObject)
		Return .Null.
	Endfunc

	Function visitSuperExpr(toExpr)
		Do Case
		Case This.nCurrentClass == CLT_NONE
			_Screen.FoxScript.errorToken(toExpr.oKeyword, "Can't use 'super' outside of a class.")
		Case This.nCurrentClass != CLT_SUBCLASS
			_Screen.FoxScript.errorToken(toExpr.oKeyword, "Can't use 'super' in a class with no superclass.")
		Endcase
		This.resolveLocal(toExpr, toExpr.oKeyword)

		Return .Null.
	Endfunc

	Function visitThisNodeExpr(toExpr)
		If This.nCurrentClass == CLT_NONE
			_Screen.FoxScript.errorToken(toExpr.oKeyword, "Can't use 'this' outside of a class.")
			Return .Null.
		Endif
		This.resolveLocal(toExpr, toExpr.oKeyword)

		Return .Null.
	Endfunc

	Function visitUnaryExpr(toExpr)
		This.resolveExpr(toExpr.oRight)
		Return .Null.
	Endfunc

	Function visitVariableExpr(toExpr)
		If !This.oScopes.Empty()
			Local loScope, lValue
			loScope = This.oScopes.peek()
			lValue = loScope.Get(toExpr.oName.lexeme)
			If Type('lValue') == 'L' And !lValue
				_Screen.FoxScript.errorToken(toExpr.oName, "Can't read local variable in its own initializer.")
			Endif
			This.resolveLocal(toExpr, toExpr.oName)

			Return .Null.
		Endif
	Endfunc

	Hidden Function resolveStmt(toStmt)
		toStmt.Accept(This)
	Endfunc

	Hidden Function resolveExpr(toExpr)
		toExpr.Accept(This)
	Endfunc

	Hidden Function resolveFunction(toFunction, tnType)
		Local lnEnclosingFunction
		lnEnclosingFunction = This.nCurrentFunction
		This.nCurrentFunction = tnType

		This.beginScope()

		For Each toParam In toFunction.oParams
			This.Declare(toParam)
			This.Define(toParam)
		Endfor
		This.resolve(toFunction.oBody)

		This.endScope()

		This.nCurrentFunction = lnEnclosingFunction
	Endfunc

	Hidden Function beginScope
		This.oScopes.Push(Createobject('Dictionary'))
	Endfunc

	Hidden Function endScope
		This.oScopes.Pop()
	Endfunc

	Hidden Function Declare(toName)
		If This.oScopes.Empty()
			Return
		Endif
		Local loScope
		loScope = This.oScopes.peek()
		If loScope.ContainsKey(toName.lexeme)
			_Screen.FoxScript.errorToken(toName, "Already variable with this name in this scope.")
		Endif
		loScope.put(toName.lexeme, false)
	Endfunc

	Hidden Function Define(toName)
		If This.oScopes.Empty()
			Return
		Endif
		Local loScope
		loScope = This.oScopes.peek()
		loScope.put(toName.lexeme, TRUE)
	Endfunc

	Hidden Function resolveLocal(toExpr, toName)
		Local i, loScope
		* |	  i=5	 |   i=4   |   i=3   |   i=2   |   i=1   |
		* |   5-1    |   5-2   |   5-3   |   5-4   |   5-5   |
		For i = This.oScopes.Size() To 1 Step -1
			loScope = This.oScopes.Get(i)
			If loScope.ContainsKey(toName.lexeme)
				This.oInterpreter.resolve(toExpr, This.oScopes.Size() - i)
				Return
			Endif
		Endfor
	Endfunc
Enddefine

* ================================================================================== *
* Runtime Collection Classes
* ================================================================================== *
Define Class Callable As Custom

	Function arity
		* Abstract
	Endfunc

	Function Call(toInterpreter, toArguments)
		* Abstract
	Endfunc

Enddefine

* ================================================================================== *
* Builtins functions class
* ================================================================================== *
Define Class BuiltinClock As Callable
	Function arity
		Return 0
	Endfunc

	Function Call(toInterpreter, toArguments)
		Return Seconds()
	Endfunc

Enddefine

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
		Endif
		_Screen.FoxScript.throwError('RuntimeError', 'token', toName, "Undefined variable '" + toName.lexeme + "'.")
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
* ArrayLiteral
* ================================================================================== *
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
	oExpression = .Null.
	cHash = ''

	Function Init(toExpression)
		This.oExpression = toExpression
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

	Function Init(toToken, toLeft, toOperator, toRight)
		This.oLeft = toLeft
		This.oOperator = toOperator
		This.oRight = toRight
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitLogicalExpr(This)
	Endfunc
Enddefine

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

* ================================================================================== *
* Interpreter Class
* ================================================================================== *
Define Class Interpreter As Custom
	oGlobals = .Null.
	oEnvironment = .Null.
	oLocals = .Null.

	Function Init
		with this
			.oGlobals = Createobject("Environment", .Null.)
			.oEnvironment = This.oGlobals
			.oLocals = Createobject("Dictionary")
			.oGlobals.Define("clock", Createobject("BuiltinClock"))
		endwith
	Endfunc

	Function interpret(toStatements)
		Local loResult
		Try
			For Each loStatement In toStatements
				loResult = This.Execute(loStatement)
			Endfor
		Catch To loEx
			If Type('loEx.Type') == 'C'
				_Screen.FoxScript.runtimeError(loEx)
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
	Endfunc

	Function Evaluate(toExpr)
		Return toExpr.Accept(This)
	Endfunc

	Function Execute(toStmt)
		Return toStmt.Accept(This)
	Endfunc

	Function resolve(toExpr, tnDepth)
		This.oLocals.put(toExpr.cHash, tnDepth)
	Endfunc

	Function executeBlock(toStatements, toEnvironment)
		Local loPrevious, loResult
		loPrevious = This.oEnvironment
		Try
			This.oEnvironment = toEnvironment
			For Each loStatement In toStatements
				loResult = This.Execute(loStatement)
			Endfor
		Finally
			This.oEnvironment = loPrevious
		Endtry
		Return loResult
	Endfunc

	Function visitArrayLiteralExpr(toExpr)
		Local loArray
		loArray = Createobject('Dictionary')
		For Each loElement In toExpr.oElements
			loArray.Add(This.Evaluate(loElement))
		Endfor

		Return loArray
	Endfunc

	Function visitAnonymousFunctionExpr(toStmt)
		* Create the runtime function object and installs it in the current environment.
		Local loFunction
		loFunction = Createobject("RuntimeFunction", toStmt, This.oEnvironment, false)
		This.oEnvironment.Define(toStmt.oName.lexeme, loFunction)
		Return .Null.
	Endfunc

	Function visitBlockStmt(toStmt)
		This.executeBlock(toStmt.oStatements, Createobject("Environment", This.oEnvironment))
		Return .Null.
	Endfunc

	Function visitClassNodeStmt(toStmt)
		Local loSuperClass, loMethods, loFunction, loClass
		loSuperClass = .Null.
		If !Isnull(toStmt.oSuperclass)
			loSuperClass = This.Evaluate(toStmt.oSuperclass)
			If Type('loSuperclass') != 'O' Or loSuperClass.Class != 'Runtimeclass'
				This.runtimeError(toStmt.oSuperclass.oName, "Superclass must be a class.")
			Endif
		Endif

		This.oEnvironment.Define(toStmt.oName.lexeme, .Null.)
		If !Isnull(toStmt.oSuperclass)
			This.oEnvironment = Createobject('Environment', This.oEnvironment) && enclose the current environment
			This.oEnvironment.Define('super', loSuperClass)
		Endif

		loMethods = Createobject('Dictionary')
		For Each loMethod In toStmt.oMethods
			loFunction = Createobject('RuntimeFunction', loMethod, This.oEnvironment, loMethod.oName.lexeme == 'init')
			loMethods.put(loMethod.oName.lexeme, loFunction)
		Endfor

		loClass = Createobject('RuntimeClass', toStmt.oName.lexeme, loSuperClass, loMethods)

		If !Isnull(loSuperClass)
			This.oEnvironment = This.oEnvironment.oEnclosing && restore the original environment
		Endif

		This.oEnvironment.assign(toStmt.oName, loClass)

		Return .Null.
	Endfunc

	Function visitExpressionStmt(toStmt)
		Return This.Evaluate(toStmt.oExpression)
	Endfunc

	Function visitFunctionNodeStmt(toStmt)
		* Create the runtime function object and installs it in the current environment.
		Local loFunction
		loFunction = Createobject("RuntimeFunction", toStmt, This.oEnvironment, false)
		This.oEnvironment.Define(toStmt.oName.lexeme, loFunction)
		Return .Null.
	Endfunc

	Function visitIfNodeStmt(toStmt)
		If This.isTruthy(This.Evaluate(toStmt.oCondition))
			This.Execute(toStmt.oThenBranch)
		Else
			If !Isnull(toStmt.oElseBranch)
				This.Execute(toStmt.oElseBranch)
			Endif
		Endif
		Return .Null.
	Endfunc

	Function visitObjectLiteralExpr(toExpr)
		Local loInstance
		loInstance = Createobject('RuntimeInstance', .Null.)

		Local i, lcKey, loValue
		For i=1 To toExpr.oElements.Count
			lcKey = toExpr.oElements.GetKey(i)
			loValue = This.Evaluate(toExpr.oElements.Item(i))
			loInstance.oFields.put(lcKey, loValue)
		Endfor

		Return loInstance
	Endfunc

	Function visitPrintStmt(toStmt)
		Local loValue
		loValue = This.Evaluate(toStmt.oExpression)
		Messagebox(This.stringify(loValue))
		Return .Null.
	Endfunc

	Function visitReturnNodeStmt(toStmt)
		Local loValue, oExp
		If !Isnull(toStmt.oValue)
			loValue = This.Evaluate(toStmt.oValue)
		Endif
		_Screen.FoxScript.throwError('ReturnException', 'value', loValue, "")
	Endfunc

	Function visitVarStmt(toStmt)
		Local loValue
		If !Isnull(toStmt.oInitializer)
			loValue = This.Evaluate(toStmt.oInitializer)
		Endif
		This.oEnvironment.Define(toStmt.oName.lexeme, loValue)
		Return loValue
	Endfunc

	Function visitWhileNodeStmt(toStmt)
		Do While This.isTruthy(This.Evaluate(toStmt.oCondition))
			This.Execute(toStmt.oBody)
		Enddo
		Return .Null.
	Endfunc

	Function visitAssignExpr(toExpr)
		Local loValue, lnDistance
		loValue = This.Evaluate(toExpr.oValue)
		lnDistance = This.oLocals.Get(toExpr.cHash)
		If !Isnull(lnDistance)
			This.oEnvironment.assignAt(lnDistance, toExpr.oName, loValue)
		Else
			This.oGlobals.assign(toExpr.oName, loValue)
		Endif

		Return loValue
	Endfunc

	Function visitBinaryExpr(toExpr)
		Local loLeft, loRight
		loLeft = This.Evaluate(toExpr.oLeft)
		loRight = This.Evaluate(toExpr.oRight)

		Do Case
		Case toExpr.oOperator.category == TC_NOT_EQ
			Return !This.isEqual(loLeft, loRight)
		Case toExpr.oOperator.category == TC_EQUAL
			Return This.isEqual(loLeft, loRight)
		Case toExpr.oOperator.category == TC_GREATER
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft > loRight
		Case toExpr.oOperator.category == TC_GREATER_EQ
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft >= loRight
		Case toExpr.oOperator.category == TC_LESS
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft < loRight
		Case toExpr.oOperator.category == TC_LESS_EQ
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft <= loRight
		Case toExpr.oOperator.category == TC_MINUS
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft - loRight
		Case toExpr.oOperator.category == TC_PLUS
			If Type('loLeft') == Type('loRight')
				Return loLeft + loRight
			Endif
			This.runtimeError(toExpr.oOperator, "Operands must be two numbers or two strings.")
		Case toExpr.oOperator.category == TC_DIV
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft / loRight
		Case toExpr.oOperator.category == TC_MUL
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft * loRight
		Endcase
		Return .Null.
	Endfunc

	Function visitCallExpr(toExpr)
		Local loFunction, loArguments
		loFunction = This.Evaluate(toExpr.oCallee)

		loArguments = Createobject('Collection')
		For Each loArgument In toExpr.oArguments
			loArguments.Add(This.Evaluate(loArgument))
		Endfor

		If Type('loFunction') != 'O' Or loFunction.ParentClass != 'Callable'
			This.runtimeError(toExpr.oParen, "Can only call functions and classes.")
		Endif

		If loArguments.Count != loFunction.arity()
			This.runtimeError(toExpr.oParen, ;
				"Expected " + Alltrim(Str(loFunction.arity())) + " arguments but got " + Alltrim(Str(loArguments.Count)) + ".")
		Endif

		Return loFunction.Call(This, loArguments)
	Endfunc

	Function visitGetExpr(toExpr)
		Local loObject
		loObject = This.Evaluate(toExpr.oObject)
		If Type('loObject') == 'O' And loObject.Class == 'Runtimeinstance'
			Return loObject.Get(toExpr.oName)
		Endif
		This.runtimeError(toExpr.oName, "Only instances have properties.")
	Endfunc

	Function visitFunctionExpr(toExpr)
		* Create the runtime function object and installs it in the current environment.
		Local loFunction
		loFunction = Createobject("RuntimeFunction", toExpr, This.oEnvironment, false)
		Return loFunction
	EndFunc
	
	Function visitGroupingExpr(toExpr)
		Return This.Evaluate(toExpr.oExpression)
	Endfunc

	Function visitLiteralExpr(toExpr)
		Return toExpr.oValue
	Endfunc

	Function visitLogicalExpr(toExpr)
		Local loLeft
		loLeft = This.Evaluate(toExpr.oLeft)
		If toExpr.oOperator.Type == TT_OR
			If This.isTruthy(loLeft)
				Return loLeft
			Endif
		Else
			If !This.isTruthy(loLeft)
				Return loLeft
			Endif
		Endif
		Return This.Evaluate(toExpr.oRight)
	Endfunc

	Function visitSetExpr(toExpr)
		Local loObject, loValue
		loObject = This.Evaluate(toExpr.oObject)
		If Type('loObject') != 'O' Or loObject.Class != 'Runtimeinstance'
			This.runtimeError(toExpr.oName, "Only instances have fields.")
		Endif

		loValue = This.Evaluate(toExpr.oValue)
		loObject.Set(toExpr.oName, loValue)

		Return loValue
	Endfunc

	Function visitSuperExpr(toExpr)
		Local lnDistance, loSuperclass, loObject, loMethod

		lnDistance = This.oLocals.Get(toExpr.cHash)
		loSuperclass = This.oEnvironment.getAt(lnDistance, "super")
		loObject = This.oEnvironment.getAt(lnDistance-1, "this")
		loMethod = loSuperclass.findMethod(toExpr.oMethod.lexeme)

		If !Isnull(loMethod)
			This.runtimeError(toExpr.oMethod, "Undefined property '" + toExpr.oMethod.lexeme + "'.")
		Endif

		Return loMethod.Bind(loObject)
	Endfunc

	Function visitThisNodeExpr(toExpr)
		Return This.lookUpVariable(toExpr.oKeyword, toExpr)
	Endfunc

	Function visitUnaryExpr(toExpr)
		Local loRight
		loRight = This.Evaluate(toExpr.oRight)
		Do Case
		Case toExpr.oOperator.category == TC_BANG
			Return !This.isTruthy(loRight)
		Case toExpr.oOperator.category == TT_MINUS
			This.checkNumberOperand(toExpr.oOperator, loRight)
			Return -loRight
		Case toExpr.oOperator.category == TT_PLUS
			Return loRight
		Endcase

		Return .Null.
	Endfunc

	Function visitVariableExpr(toExpr)
		Return This.lookUpVariable(toExpr.oName, toExpr)
	Endfunc

	Function lookUpVariable(toName, toExpr)
		Local lnDistance
		lnDistance = This.oLocals.Get(toExpr.cHash)
		If !Isnull(lnDistance)
			Return This.oEnvironment.getAt(lnDistance, toName.lexeme)
		Else
			Return This.oGlobals.Get(toName)
		Endif
	Endfunc

	Function checkNumberOperand(toOperator, toOperand)
		If Type('toOperand') == 'N'
			Return
		Endif
		This.runtimeError(toOperator, "Operand must be a number.")
	Endfunc

	Function checkNumberOperands(toOperator, toLeft, toRight)
		If Type('toLeft') == 'N' And Type('toRight') == 'N'
			Return
		Endif
		This.runtimeError(toOperator, "Operands must be numbers.")
	Endfunc

	Function isTruthy(toObject)
		If Isnull(toObject)
			Return false
		Endif
		If Type('toObject') == 'L'
			Return toObject
		Endif
		Return TRUE
	Endfunc

	Function isEqual(toA, toB)
		If Isnull(toA) And Isnull(toB)
			Return TRUE
		Endif
		If Isnull(toA)
			Return false
		Endif
		Return toA == toB
	Endfunc

	Function stringify(toObject)
		Return Transform(toObject)
	Endfunc

	Hidden Procedure runtimeError(toToken, tcErrorMessage)
		*_Screen.FoxScript.errorToken(toToken, tcErrorMessage)
		_Screen.FoxScript.throwError('RuntimeError', 'token', toToken, tcErrorMessage)
	Endproc
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
Enddefine
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
* Dictionary Class (internal data structure)
* ================================================================================== *
Define Class Dictionary As Collection
	Function ContainsKey(tcKey)
		Local lnIndex
		If Empty(tcKey)
			Return .F.
		Endif
		Return This.GetKey(tcKey) > 0
	Endfunc

	Function put(tcKey, tvValue)
		If This.ContainsKey(tcKey)
			This.Remove(This.GetKey(tcKey))
		Endif
		This.Add(tvValue, tcKey)
	Endfunc

	Function Get(tvIndexOrKey) As Object
		Do Case
		Case Type('tvIndexOrKey') == 'N'
			If !Between(tvIndexOrKey, 1, This.Count)
				Return .Null.
			Endif
			Return This.Item(tvIndexOrKey)
		Case Type('tvIndexOrKey') == 'C'
			tvIndexOrKey = This.GetKey(tvIndexOrKey)
			If tvIndexOrKey > 0
				Return This.Item(tvIndexOrKey)
			Endif
		Endcase
		Return .Null.
	Endfunc

Enddefine

* ================================================================================== *
* Stack Class (internal data structure)
* ================================================================================== *
Define Class Stack As Collection
	Function Push
		Lparameters tvData As Variant
		Local lcType
		lcType = Type("tvData")
		If lcType != 'U'
			If lcType == 'C'
				Local lnIndex
				lnIndex = This.GetKey(tvData)
				If lnIndex > 0
					This.Remove(lnIndex)
				Endif
				This.Add(tvData, tvData) && same key for future search.
			Else
				This.Add(tvData, Sys(2015)) && random key (it doesn't matter)
			Endif
		Endif
	Endfunc

	Function Pop As Variant
		Local lvData As Variant

		If This.Count > 0
			lvData = This.Item(This.Count)
			This.Remove(This.Count)
		Else
			lvData = .Null.
		Endif
		Return lvData
	Endfunc

	Function peek As Variant
		Local lvData As Variant
		If This.Count > 0
			lvData = This.Item(This.Count)
		Else
			lvData = .Null.
		Endif
		Return lvData
	Endfunc

	Function Empty As Boolean
		Return This.Count == 0
	Endfunc

	Function Get(tvIndexOrKey) As Object
		Do Case
		Case Type('tvIndexOrKey') == 'N'
			If !Between(tvIndexOrKey, 1, This.Count)
				Return .Null.
			Endif
			Return This.Item(tvIndexOrKey)
		Case Type('tvIndexOrKey') == 'C'
			tvIndexOrKey = This.GetKey(tvIndexOrKey)
			If tvIndexOrKey > 0
				Return This.Item(tvIndexOrKey)
			Endif
		Endcase
		Return .Null.
	Endfunc

	Function Size As Integer
		Return This.Count
	Endfunc
Enddefine

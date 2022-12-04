* =========================================================== *
* FoxScript: Scanner
* 2022.12.04
* Irwin Rodriguez <rodriguez.irwin@gmail.com>
* =========================================================== *

loTest = CreateObject("Scanner")
Release loTest

#define TT_IGNORE				1
#define TT_SEMICOLON			2
#define TT_IGNORE				3
#define TT_IGNORE				4
#define TT_RELATIONAL_OPERATOR	5
#define TT_EQUALITY_OPERATOR	6
#define TT_EQUALITY_OPERATOR	7
#define TT_SIMPLE_ASSIGN		8
#define TT_SIMPLE_ASSIGN		9
#define TT_NUMBER				10
#define TT_STRING				11
#define TT_STRING				12
#define TT_IDENTIFIER			13
#define TT_LPAREN				14
#define TT_RPAREN				15
#define TT_LBRACKET				16
#define TT_RBRACKET				17
#define TT_LBRACE				18
#define TT_RBRACE				19
#define TT_DOT					20
#define TT_COMMA				21
#define TT_COLON				22
#define TT_LOGICAL_AND			23
#define TT_LOGICAL_OR			24

* -------------------------------------
* Keywords
#define TT_CONST				25
#define TT_DO					26
#define TT_FALSE				27
#define TT_LET					28
#define TT_BREAK				29
#define TT_CONTINUE				30
#define TT_ELSE					31
#define TT_IMPORT				32
#define TT_NEW					33
#define TT_PUBLIC				34
#define TT_THIS					35
#define TT_VAR					36
#define TT_CASE					37
#define TT_ENUM					38
#define TT_FOR					39
#define TT_IN					40
#define TT_NULL					41
#define TT_RETURN				42
#define TT_THROW				43
#define TT_CATCH				44
#define TT_EXPORT				45
#define TT_FUNCTION				46
#define TT_SUPER				47
#define TT_TRY					48
#define TT_WHILE				49
#define TT_CLASS				50
#define TT_IF					51
#define TT_SWITCH				52
#define TT_TRUE					53

* Keywords
Define Class Scanner As Custom
	cSource = ''
	nCursor = 0
	nTokenCounter = 0
	nLastToken = 0
	oRegEx = .Null.
	
	Dimension aSpecs[1]
	
	Function init(tcSource)
		this.cSource = tcSource
		* Setting the oRegEx properties
		this.oRegEx = CreateObject("VBScript.RegExp")
		this.oRegEx.IgnoreCase = .F.
		this.oRegEx.Global = .T.
		this.nCursor = 1		
		Local i
		i = 1
		        
        * -----------------------------------------------------------
        * Whitespace:
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^[ \t\r\f]+", TT_IGNORE)

	    * -----------------------------------------------------------
	    * NewLine:		
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\n+", TT_SEMICOLON)

        * -----------------------------------------------------------
        * Comments:	
        * Skip single-line comments	
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\/\/.*", TT_IGNORE)

		* Skip multi-line comments
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\/\*[\s\S]*?\*\/", TT_IGNORE)
		
	    * -----------------------------------------------------------
	    * Relational Operators:		
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^[<>]=?", TT_RELATIONAL_OPERATOR)
		
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^[=!]=", TT_EQUALITY_OPERATOR)
		
        * -----------------------------------------------------------
        * Logical Operators:		
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\band\b", TT_LOGICAL_AND)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bor\b", TT_LOGICAL_OR)
		
        * -----------------------------------------------------------
        * Keywords:
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bconst\b", TT_CONST)
		
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bdo\b", TT_DO)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bfalse\b", TT_FALSE)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\blet\b", TT_LET)
        
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bbreak\b", TT_BREAK)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bcontinue\b", TT_CONTINUE)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\belse\b", TT_ELSE)
        
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bimport\b", TT_IMPORT)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bnew\b", TT_NEW)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bpublic\b", TT_PUBLIC)
        
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bthis\b", TT_THIS)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bvar\b", TT_VAR)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bcase\b", TT_CASE)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\benum\b", TT_ENUM)
        
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bfor\b", TT_FOR)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bin\b", TT_IN)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bnull\b", TT_NULL)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\breturn\b", TT_RETURN)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bthrow\b", TT_THROW)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bcatch\b", TT_CATCH)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bexport\b", TT_EXPORT)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bfn\b", TT_FUNCTION)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bsuper\b", TT_SUPER)
		
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\btry\b", TT_TRY)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bwhile\b", TT_WHILE)
		
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bclass\b", TT_CLASS)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bif\b", TT_IF)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\bswitch\b", TT_SWITCH)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\btrue\b", TT_TRUE)

        * -----------------------------------------------------------
        * Assignment operators: =, +=, -=, *=, /=	    
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^=", TT_SIMPLE_ASSIGN)
		
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^[\+\-\*\/]=", TT_SIMPLE_ASSIGN)
		
        * -----------------------------------------------------------
        * Math operators: +, -, *, /
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\d+", TT_NUMBER)
        
        * -----------------------------------------------------------
        * Double quoted string:
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", '^"[^"]*"', TT_STRING)
		* -----------------------------------------------------------
		* Single quoted string:
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^'[^']*'", TT_STRING)
		* -----------------------------------------------------------
		* Identifier
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\w+", TT_IDENTIFIER)        

		* -----------------------------------------------------------
		* Symbol and Delimiters:
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\(", TT_LPAREN)  
		
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\)", TT_RPAREN)
		
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\[", TT_LBRACKET)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\]", TT_RBRACKET)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\{", TT_LBRACE)
		
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\}", TT_RBRACE)

		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^\.", TT_DOT)
		
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^,", TT_COMMA)
		
		i = i + 1
		Dimension this.aSpecs[i]
		this.aSpecs[i] = CreateObject("Tuple", "^:", TT_COLON)
		? "Listo"
	EndFunc
	
	
	Function getNextToken
		
	EndFunc		
		
Enddefine

Define Class Tuple as Custom
	nType = 0
	cPattern = ''
	Function init(tnType, tcPattern)
		this.nType = tnType
		this.cPattern = tcPattern
	EndFunc
enddefine
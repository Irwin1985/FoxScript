Set Procedure To "ASTPrinter" additive
Set Procedure To "Expr" additive
Set Procedure To "FoxScript" additive
Set procedure to "Interpreter" additive
Set Procedure To "Parser" additive
Set Procedure To "Runtime" additive
Set Procedure To "Scanner" ADDITIVE
Set Procedure To "Stmt" additive
Set Procedure To "Structures" additive

If Type('_screen.FoxScript') == 'U'
	_screen.AddProperty('FoxScript', .null.)
EndIf
_screen.FoxScript = CreateObject("FoxScript")

Return
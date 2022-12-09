Set Procedure To "ASTPrinter" additive
Set Procedure To "Environment" additive
Set Procedure To "Expr" additive
Set Procedure To "FoxScript" additive
Set Procedure To "Interpreter" additive
Set Procedure To "Parser" ADDITIVE
Set Procedure To "Resolver" ADDITIVE
Set Procedure To "Scanner" additive
Set Procedure To "Stmt" additive
Set Procedure To "Structures" additive

If Type('_screen.foxscript') = 'U'
	=AddProperty(_screen, 'foxscript', .null.)
EndIf 

* Foxscript handler class
_screen.foxscript = CreateObject("FoxScript")

Return

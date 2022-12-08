Set Procedure To "Expr" additive
Set Procedure To "FoxScript" additive
Set Procedure To "FoxStack" additive
Set Procedure To "Parser" ADDITIVE
Set Procedure To "Scanner" additive
Set Procedure To "Stmt" additive

If Type('_screen.foxscript') = 'U'
	=AddProperty(_screen, 'foxscript', .null.)
EndIf 

* Foxscript handler class
_screen.foxscript = CreateObject("FoxScript")

Return

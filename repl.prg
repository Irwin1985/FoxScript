loRepl = CreateObject("Repl")
loRepl.show()
Read EVENTS

* -----------------------------------------------
* Repl class
Define Class Repl As Form

	DataSession = 2
	Top = 159
	Left = 354
	Height = 347
	Width = 771
*!*		visible = .t.
*!*		Desktop = .F.
*!*		DoCreate = .T.
*!*		AutoCenter = .F.
	Caption = ""
*!*		ControlBox = .T.
	FontName = "MS Sans Serif"
*!*		MaxButton = .F.
*!*		MinButton = .F.
*!*		Icon = "program.ico"
*!*		TitleBar = 1
	BackColor = Rgb(40,44,52)
*!*		Dockable = 1
	editornormalcolor = .F.
	editorcommentcolor = .F.
	editorkeywordcolor = .F.
	editoroperatorcolor = .F.
	editorconstantcolor = .F.
	editorstringcolor = .F.
	editorvariablecolor = .F.
	cerrorcolor = ""
	cforecolor = ""
	cbackcolor = ""
	Prompt = ""
	Version = ""
	ohelp = ""
	cinfocolor = ""
	configfile = ""
	Dimension aList[2]

	Add Object Console As Grid With ;
		ColumnCount = 2, ;
		FontName = "Roboto Mono", ;
		FontSize = 12, ;
		Anchor = 240, ;
		AllowAddNew = .T., ;
		AllowHeaderSizing = .F., ;
		AllowRowSizing = .F., ;
		DeleteMark = .F., ;
		GridLines = 0, ;
		HeaderHeight = 0, ;
		Height = 344, ;
		Highlight = .F., ;
		HighlightRow = .F., ;
		Left = 0, ;
		RecordMark = .F., ;
		RecordSource = "cCommands", ;
		RowHeight = 21, ;
		ScrollBars = 0, ;
		SplitBar = .F., ;
		Top = -2, ;
		Width = 769, ;
		ForeColor = Rgb(220,223,228), ;
		BackColor = Rgb(40,44,52), ;
		GridLineColor = Rgb(40,44,52), ;
		LockColumns = 2, ;
		AllowCellSelection = .T., ;
		Themes = .F., ;
		Name = "console"


	Procedure executecmd
		Lparameters tcCommand
		Replace executed With .T.

		If Empty(tcCommand)
			Return
		Endif

		* Slice commands
		Local lcAction
		Store '' To lcAction

		Do Case
		Case lcAction == 'cls' && clear screen
			Delete From cCommands

		Case lcAction == 'exit' && close console
			Thisform.Release()

		Case lcAction == 'version' && show the version of the console app.
			Thisform.println('Project Manager v' + Thisform.Version, 'I')

		Case lcAction == 'help' && print the cli app help.
			Local lcPrompt
			lcPrompt = Thisform.Prompt
			Thisform.Prompt = ''
			For Each lcItem In Thisform.ohelp
				Thisform.println(lcItem, 'I')
			Endfor
			Thisform.Prompt = lcPrompt

		Case lcAction == 'new'

		Case lcAction == 'build'

		Case lcAction == 'run'
		Otherwise
			Thisform.reportError(lcAction + ' no se reconoce como un comando interno.')
		Endcase

		* Add the next line in the console.
		Thisform.writeNextLine()
	Endproc

	Procedure setDynamicBackColor
		Lparameters tcType
		Do Case
		Case tcType == 'E' && ERROR COLOR
			Return Evaluate(Thisform.cerrorcolor)
		Case tcType == 'N' && NORMAL COLOR
			Return Evaluate(Thisform.cforecolor)
		Case tcType == 'I' && INFORMATION COLOR
			Return Evaluate(Thisform.cinfocolor)
		Otherwise
			Return Rgb(255, 255, 255) && DEFAULT WHITE
		Endcase
	Endproc


	Procedure reportError
		Lparameters tcErrorMsg

		#Define CONST_EXECUTED .T.

		Insert Into cCommands Values(Thisform.Prompt, tcErrorMsg, CONST_EXECUTED, 'E')

		#Undef CONST_EXECUTED
	Endproc


	Procedure println
		Lparameters tcMessage, tcType

		#Define CONST_EXECUTED .T.

		Insert Into cCommands Values(Thisform.Prompt, tcMessage, CONST_EXECUTED, tcType)

		#Undef CONST_EXECUTED
	Endproc


	Procedure writeNextLine
		#Define CONST_EXECUTED .F.

		* Appends a new line and keep the cursor position.
		Insert Into cCommands Values(Thisform.Prompt, '', CONST_EXECUTED, 'N')
		Go Bottom In cCommands

		#Undef CONST_EXECUTED

		Thisform.Console.Refresh()
		Thisform.Console.SetFocus()
	Endproc
	
	Procedure setGrid
		With this.console.column1
			.FontName = "Roboto Mono"
			.FontSize = 12
			.ControlSource = "cCommands.symbol"
			.Enabled = .F.
			.Width = 35
			.Movable = .F.
			.ReadOnly = .T.
			.SelectOnEntry = .F.
			.ForeColor = Rgb(220,223,228)
			.BackColor = Rgb(40,44,52)
			.Name = "Column1"
		EndWith
		
		With this.console.column2
			.FontName = "Roboto Mono"
			.FontSize = 12
			.ControlSource = "cCommands.cmd"
			.Width = 738
			.ForeColor = Rgb(220,223,228)
			.BackColor = Rgb(40,44,52)
			.Name = "Column2"		
		endwith
	
		With this.Console.Column1.header1
			.FontName = "Roboto Mono"
			.FontSize = 12
			.Caption = "Header1"
			.Name = "Header1"
		EndWith

		With this.Console.Column1.text1
			.FontName = "Roboto Mono"
			.FontSize = 12
			.BackStyle = 0
			.BorderStyle = 0
			.Enabled = .F.
			.Margin = 0
			.ReadOnly = .T.
			.SpecialEffect = 1
			.TabStop = .F.
			.Style = 0
			.ForeColor = Rgb(220,223,228)
			.BackColor = Rgb(40,44,52)
			.DisabledBackColor = Rgb(40,44,52)
			.DisabledForeColor = Rgb(220,223,228)
			.SelectedBackColor = Rgb(220,220,220)
			.BorderColor = Rgb(40,44,52)
			.Themes = .F.
			.Name = "Text1"
		endwith

		With this.Console.Column2.header1
			.FontName = "Roboto Mono"
			.FontSize = 12
			.Caption = "Header1"
			.Name = "Header1"
		EndWith

		With this.Console.Column2.text1
			.FontName = "Roboto Mono"
			.FontSize = 12
			.Anchor = 128
			.BackStyle = 0
			.BorderStyle = 0
			.Margin = 0
			.MaxLength = 250
			.SpecialEffect = 1
			.ForeColor = Rgb(220,223,228)
			.BackColor = Rgb(40,44,52)
			.DisabledBackColor = Rgb(40,44,52)
			.SelectedBackColor = Rgb(220,220,220)
			.BorderColor = Rgb(40,44,52)
			.Themes = .F.
			.Name = "Text1"
		EndWith
		
		=BindEvent(this, 'column1Text1When', this.console.column1.text1, 'when')
		=BindEvent(this, 'column2Text1When', this.console.column2.text1, 'when')
		=BindEvent(this, 'column2Text1KeyPress', this.console.column2.text1, 'KeyPress')
	EndProc
	
	function Init
		this.setGrid()
		Local lcForeColor, lcBackColor, lcErrorColor, lcInfoColor
		lcForeColor  = this.getVFPOption("EditorNormalColor")
		lcBackColor  = this.getEditorBgColor()
		lcErrorColor = this.getVFPOption("EditorConstantColor")
		lcInfoColor = this.getVFPOption("EditorStringColor")

		Thisform.cbackcolor  = lcBackColor
		Thisform.cforecolor  = lcForeColor
		Thisform.cerrorcolor = lcErrorColor
		Thisform.cinfocolor = lcInfoColor

		Thisform.Console.BackColor = &lcBackColor
		Thisform.Console.ForeColor = &lcForeColor

		Thisform.Prompt = '>>> '
		Thisform.Version = '0.0.1'

		#Define CONST_TAB Space(2)
		#Define CONST_PAD 8

		* Creates the help dictionary
		Thisform.ohelp = Createobject('Collection')
		With Thisform.ohelp
			.Add('VFP CLI es una herramienta gestionar proyectos no visuales.')
			.Add('')
			.Add('Uso:')
			.Add(CONST_TAB+'[comando] [opciones] [argumentos]')
			.Add('')
			.Add('VFP CLI soporta los siguientes comandos:')
			.Add('')

			* New command
			.Add(CONST_TAB+Padr('new', CONST_PAD, Space(1))+'Crea un proyecto en el directorio actual.')

			* Build command
			.Add(CONST_TAB+Padr('build', CONST_PAD, Space(1))+'Construye la aplicación del proyecto especificado.')

			* Run command
			.Add(CONST_TAB+Padr('run', CONST_PAD, Space(1))+'Construye y ejecuta la aplicación del proyecto especificado.')

			.Add('')
			.Add('Comandos externos:')
			.Add('')
			.Add(CONST_TAB+Padr('cls', CONST_PAD, Space(1))+'Limpia la consola.')
			.Add(CONST_TAB+Padr('version', CONST_PAD, Space(1))+'Muestra la versión de la aplicación.')
			.Add(CONST_TAB+Padr('help', CONST_PAD, Space(1))+'Muestra esta información.')
			.Add(CONST_TAB+Padr('exit', CONST_PAD, Space(1))+'Sale de la aplicación.')

			* New arguments
			.Add('')
			.Add(CONST_TAB+'El comando "new" soporta los siguientes argumentos:')
			.Add(CONST_TAB+CONST_TAB+'projectName                  Nombre del proyecto a crear.')
			.Add(CONST_TAB+CONST_TAB+'-t [exe | app | dll | mtdll] Tipo de aplicación a crear (exe por defecto).')


			* Build arguments
			.Add('')
			.Add(CONST_TAB+'El comando "build" soporta los siguientes argumentos:')
			.Add(CONST_TAB+CONST_TAB+'projectName   Nombre del proyecto a crear.')
			.Add(CONST_TAB+CONST_TAB+'-o [fileName] Nombre del fichero a construir.')

			* Run arguments
			.Add('')
			.Add(CONST_TAB+'El comando "run" soporta los siguientes argumentos:')
			.Add(CONST_TAB+CONST_TAB+'projectName   Nombre del proyecto a ejecutar.')
		Endwith

		Thisform.writeNextLine()
	EndFunc

	Procedure Load
		Create Cursor cCommands (symbol c(5), cmd c(250), executed l, colorType c(1))
	Endproc

	Procedure Unload
		Clear events
		Use In cCommands
	Endproc

	Procedure Resize
		Thisform.Console.Column2.Width = Thisform.Width - Thisform.Console.Column1.Width
	Endproc

	Procedure Console.Init
		This.SetAll("DynamicForeColor","Thisform.setDynamicBackColor(cCommands.colorType)","Column")
	Endproc

	Procedure column1Text1When
		Return .F.
	Endproc

	Procedure column2Text1KeyPress
		Lparameters nKeyCode, nShiftAltCtrl
		If nKeyCode == 13 And !Empty(This.Value) && INTRO
			Thisform.executecmd(Alltrim(This.Value))
		Endif
	Endproc

	Procedure column2Text1When
		Return !cCommands.executed
	Endproc

	* ============================================================== *
	* This sample reads an entry from your Registry
	* ============================================================== *
	* Local lcHLM As String, lcKey As String, lcEntry As String
	* lcHLM	= "HCU"
	* lcKey	= "Software\Microsoft\VisualFoxPro\9.0\Options"
	* lcEntry = "EditorStringColor"
	* this.GetValue(lcHLM, lcKey, lcEntry)
	* ============================================================== *

	&& ======================================================================== &&
	&& Function GetVFPOption
	&& ======================================================================== &&
	Function getVFPOption(tcEntry)
		Local lcHLM As String, lcKey As String
		lcHLM	= "HCU"
		lcKey	= "Software\Microsoft\VisualFoxPro\9.0\Options"

		Return this.parseOptionResult(This.GetValue(lcHLM, lcKey, tcEntry))
	Endfunc
	&& ======================================================================== &&
	&& Function getEditorBgColor
	&& ======================================================================== &&
	Function getEditorBgColor
		Local lcHLM As String, lcKey As String, lcResult
		lcHLM	= "HCU"
		lcKey	= "Software\Microsoft\VisualFoxPro\9.0\Options"
		tcResult = Alltrim(Strextract(Upper(This.GetValue(lcHLM, lcKey, "EditorNormalColor")), 'RGB(', ')'))
		Return 'RGB(' + Getwordnum(tcResult, 4, ',') + ',' + Getwordnum(tcResult, 5, ',') + ',' + Getwordnum(tcResult, 6, ',') + ')'
	Endfunc
	&& ======================================================================== &&
	&& Function parseOptionResult
	&& ======================================================================== &&
	Function parseOptionResult(tcResult)
		Local lcForeColor, lcBackColor

		tcResult = Alltrim(Strextract(Upper(tcResult), 'RGB(', ')'))

		lcForeColor = 'RGB(' + Getwordnum(tcResult, 1, ',') + ',' + Getwordnum(tcResult, 2, ',') + ',' + Getwordnum(tcResult, 3, ',') + ')'
		lcBackColor = 'RGB(' + Getwordnum(tcResult, 4, ',') + ',' + Getwordnum(tcResult, 5, ',') + ',' + Getwordnum(tcResult, 6, ',') + ')'

		this.aList[1] = lcForeColor
		this.aList[2] = lcBackColor

		Return @this.aList
	Endfunc
	&& ======================================================================== &&
	&& Function GetValue
	&& ======================================================================== &&
	Function GetValue As Variant
		Lparameters tcKey As String, tcSubKey As String, tcValue As String
		tcValue = Evl(tcValue, "")

		Local lnKey As Integer, lcSubKey As String, lcValue As String

		#Define HKEY_USERS                  -2147483645
		#Define HKEY_LOCAL_MACHINE          -2147483646
		#Define HKEY_CURRENT_USER           -2147483647
		#Define HKEY_CLASSES_ROOT           -2147483648

		Do Case
		Case m.tcKey == "HCR"
			lnKey = HKEY_CLASSES_ROOT
		Case m.tcKey == "HLM"
			lnKey = HKEY_LOCAL_MACHINE
		Case m.tcKey = "HCU"
			lnKey = HKEY_CURRENT_USER
		Case m.tcKey = "HCR"
			lnKey = HKEY_CLASSES_ROOT
		Otherwise
			lnKey = m.tcKey
		Endcase

		lcSubKey = m.tcSubKey
		lcValue  = m.tcValue

		Return this.ReadFromRegistry(lnKey, lcSubKey, lcValue)
	Endfunc
	&& ======================================================================== &&
	&& Protected Function ReadFromRegistry
	&& ======================================================================== &&
	Function ReadFromRegistry As Variant
		Parameters  tnKey As Integer, tcSubKey As String, tcValue As String

		Declare Integer RegOpenKey 	;
			In 		Win32API 		;
			Integer nHKey			, ;
			String  @cSubKey		, ;
			Integer @nResult

		Declare Integer RegQueryValueEx ;
			In 		Win32API 			;
			Integer nHKey				, ;
			String 	lpszValueName		, ;
			Integer dwReserved			, ;
			Integer @lpdwType			, ;
			String 	@lpbData			, ;
			Integer @lpcbData

		Declare Integer RegCloseKey ;
			In 		Win32API ;
			Integer nHKey

		Local ;
			lnErrCode 		As Integer, ;
			lnKeyHandle		As Integer, ;
			lpdwValueType 	As Variant, ;
			lpbValue      	As Variant, ;
			lpcbValueSize 	As Variant, ;
			lpdwReserved  	As Variant

		lnKeyHandle 	= 0
		lpdwReserved 	= 0
		lpdwValueType 	= 1
		lpbValue 		= ""

		lnErrCode = RegOpenKey(tnKey, tcSubKey, @lnKeyHandle)
		If Empty(lnErrCode)
			lpcbValueSize 	= 1
			lnErrCode 		= RegQueryValueEx(lnKeyHandle, tcValue, lpdwReserved, @lpdwValueType, @lpbValue, @lpcbValueSize)
			lpbValue 		= Space(lpcbValueSize)
			lnErrCode 		= RegQueryValueEx(lnKeyHandle, tcValue, lpdwReserved, @lpdwValueType, @lpbValue, @lpcbValueSize)
			=RegCloseKey(lnKeyHandle)
			If Empty(lnErrCode)
				lpbValue = Left(lpbValue, lpcbValueSize - 1)
			Endif
		Endif
		Clear Dlls RegOpenKey, RegQueryValueEx, RegCloseKey
		Return lpbValue
	Endfunc
EndDefine
Define Class Dictionary as Collection
	function containsKey(tcKey)
		Local lnIndex
		If Empty(tcKey)
			Return .f.
		EndIf
		Return this.GetKey(tcKey) > 0
	EndFunc
	
	Function put(tckey, tvValue)
		If this.containsKey(tcKey)
			this.Remove(this.GetKey(tcKey))
		EndIf
		this.Add(tvValue, tcKey)
	EndFunc
	
    Function Get(tvIndexOrKey) As Object
    	Do case
    	case Type('tvIndexOrKey') == 'N'
	    	If !Between(tvIndexOrKey, 1, this.Count)
	    		Return .null.
	    	EndIf
	    	Return This.Item(tvIndexOrKey)
	    Case Type('tvIndexOrKey') == 'C'
	    	tvIndexOrKey = this.GetKey(tvIndexOrKey)
	    	If tvIndexOrKey > 0
	    		Return this.Item(tvIndexOrKey)
	    	EndIf
	    EndCase
	    Return .null.
    EndFunc
	
EndDefine

&& ======================================================================== &&
&& Stack Class
&& ======================================================================== &&
Define Class Stack As Collection
    Function Push
        lParameters tvData As Variant
        Local lcType
        lcType = Type("tvData")
        If lcType != 'U'
            If lcType == 'C'
            	Local lnIndex
            	lnIndex = this.GetKey(tvData) 
            	If lnIndex > 0
            		this.Remove(lnIndex)
            	EndIf
            	This.Add(tvData, tvData) && same key for future search.
            Else
            	This.Add(tvData, Sys(2015)) && random key (it doesn't matter)
            EndIf
        Endif
    EndFunc

    Function Pop As Variant
        Local lvData As Variant
        
        If this.Count > 0
	        lvData = This.Item(this.Count)
	        this.Remove(this.Count)
	    Else
	    	lvData = .null.
	    EndIf
        Return lvData
    EndFunc

    Function Peek As Variant
        Local lvData As Variant
        If this.Count > 0
	        lvData = This.oStack.Item(this.Count)
	    Else
	    	lvData = .null.
	    EndIf
        Return lvData
    EndFunc

    Function Empty As Boolean
        Return this.Count == 0
    EndFunc

    Function Get(tvIndexOrKey) As Object
    	Do case
    	case Type('tvIndexOrKey') == 'N'
	    	If !Between(tvIndexOrKey, 1, this.Count)
	    		Return .null.
	    	EndIf
	    	Return This.Item(tvIndexOrKey)
	    Case Type('tvIndexOrKey') == 'C'
	    	tvIndexOrKey = this.GetKey(tvIndexOrKey)
	    	If tvIndexOrKey > 0
	    		Return this.Item(tvIndexOrKey)
	    	EndIf
	    EndCase
	    Return .null.
    EndFunc

    Function Size As Integer
        Return this.Count
    EndFunc
EndDefine
/*

  	BaseRoundingEngine.prg
  	André Finken, 2019
  	
  	Wichtig: 
  	Die Instanzierung sollte übergeordnet im SOurce stattfinden, denn der VBAgent braucht
  	eine menge Zeit. Wenn Möglich bereits bei Dienst-Start instanzieren. 
  	
  	Der oTransactionManager kann bereits im Init übergeben werden. Wenn mögich sollte der TxnMgr
  	dort schon übergeben werden.   

*/

CLASS BaseRoundingEngine INHERIT AObject

	PROTECT __aCache                              AS ARRAY
	PROTECT __oTransactionManager                 AS ServerManager 
	PROTECT __lReleaseTransactionManager          AS LOGIC
	PROTECT __oVBAgent                            AS BaseSimpleScriptAgent

	EXPORT  oMsgStack                             AS AStatusStack

	DECLARE ACCESS oTransactionManager
	DECLARE ASSIGN oTransactionManager 
	DECLARE METHOD Round
	DECLARE METHOD GetScript
	DECLARE METHOD CheckScript
	
	DECLARE METHOD __GetScript

METHOD Init( oTransactionManager ) CLASS BaseRoundingEngine

	LOCAL oParameterList          AS AClipboard
     
	SUPER:Init()

	__aCache := {}                  

	__oTransactionManager := NULL_OBJECT    
	__lReleaseTransactionManager := FALSE  
	oParameterList := AClipboard{}
	
	SELF:oMsgStack := AStatusStack{}
		
	if( !IsNil( oTransactionManager ) )
		if( IsInstanceOfUsual( oTransactionManager, #ServerManager ) )
			__oTransactionManager := oTransactionManager
			__lReleaseTransactionManager := TRUE
		else
			AddStatusRecord(oMsgStack, SUBSYSTEM_AMS,, SEVERITY_ERROR,,"Fehler in BaseRoundingEngine: Der initial zugewiesene Transactionsmanager ist Fehlerhaft. Object stammt nicht von ServerManager ab.")			
		endif
	endif

	__oVBAgent := BaseSimpleScriptAgent{}
	__oVBAgent:ScriptContext := #ASimpleContext
	__oVBAgent:ClsIdContext  := @CLSID_SimpleContext
	__oVBAgent:InitScriptEngine( __oTransactionManager ) 
	__oVBAgent:ServerEnvironment := AcceleratedServerEnvironment{NULL_OBJECT} 
	__oVBAgent:ParameterBoard := oParameterList

    
METHOD Destroy() AS VOID PASCAL CLASS BaseRoundingEngine

	if( oMsgStack:IsStatus )
		debugPrint(__ent, __line, "FIN:", "Ausgabe von RoundingEngine:", oMsgStack:GetMessage())
	endif

	__oVBAgent:Release()  
	SELF:oMsgStack:Release()
	
	SUPER:Destroy()
    
ASSIGN oTransactionManager( oTxnMgr AS ServerManager ) AS ServerManager PASCAL CLASS BaseRoundingEngine

if( __oTransactionManager != NULL_OBJECT  )
	AddStatusRecord(oMsgStack, SUBSYSTEM_AMS,, SEVERITY_ERROR,,"Fehler in BaseRoundingEngine: Der Transactionsmanager wurde mehrfach zugewiesen!")
	__oTransactionManager:Release()
	__lReleaseTransactionManager := FALSE
endif

__oTransactionManager := oTxnMgr
return( __oTransactionManager )


ACCESS oTransactionManager AS ServerManager PASCAL CLASS BaseRoundingEngine

if( __oTransactionManager == NULL_OBJECT )
	__oTransactionManager := ServerManager{}
	__lReleaseTransactionManager := TRUE
endif
return( __oTransactionManager )
			
METHOD CheckScript( cScriptKey AS STRING, uValue AS USUAL ) AS LOGIC PASCAL CLASS BaseRoundingEngine
 
	LOCAL cSaveWrapperCode        AS STRING
	LOCAL oKopf, oPos             AS AReadRecord 
 
oKopf := AReadRecord{#Angebot}
oPos  := AReadRecord{#AngebotPos} 
cSaveWrapperCode := __oVBAgent:WrapperCode 
__oVBAgent:WrapperCode := "On Error Resume Next" + CRLF + "If Err.Number <> 0 Then" + CRLF + "MsgBox Err.Number & vbCrLf & Err.Description" + CRLF + "End If" + CRLF + BaseScriptAgent_Placeholder 
SELF:Round( cScriptKey, oKopf, oPos, 12345.123 )
__oVBAgent:WrapperCode := cSaveWrapperCode
oKopf:Release()
oPos:Release() 

return( true )


METHOD Round( cScriptKey AS STRING, oKopfRecord AS AReadRecord, oPosRecord AS AReadRecord, uValue AS USUAL ) AS USUAL PASCAL CLASS BaseRoundingEngine

	LOCAL cScript := ""             AS STRING
	LOCAL lScriptSuccess := FALSE   AS LOGIC
	LOCAL uTempValue                AS USUAL
	
cScript := SELF:__GetScript( cScriptKey ) 

__oVBAgent:ServerEnvironment:Add( #Angebot,     oKopfRecord, TRUE )
__oVBAgent:ServerEnvironment:Add( #AngebotPos,  oPosRecord, TRUE ) 
__oVBAgent:ParameterBoard:@@SET(AClipboardItem{"PosValue", uValue})

uTempValue := __oVBAgent:ExecScript( cScript, FT_UNSPECIFIED, @lScriptSuccess )

if( lScriptSuccess )
	if( IsNumeric( uTempValue ) )
		uValue := uTempValue 
	else
		AddStatusRecord(oMsgStack, SUBSYSTEM_AMS,, SEVERITY_ERROR,,"Fehler in BaseRoundingEngine: Der Rückgabewert ist nicht Numerisch!")		
	endif
else
	__oVBAgent:Status:TransmitTo( SELF:oMsgStack )	
endif

__oVBAgent:ServerEnvironment:Remove( #Angebot )
__oVBAgent:ServerEnvironment:Remove( #AngebotPos )

return( uValue )
             
METHOD GetScript( cScriptKey AS STRING ) AS STRING PASCAL CLASS BaseRoundingEngine
return( SELF:__GetScript( cScriptKey ) )             
             
PROTECT METHOD __GetScript( cScriptKey AS STRING ) AS STRING PASCAL CLASS BaseRoundingEngine

	LOCAL cScript              AS STRING
	LOCAL nFound               AS INT

nFound := aScan( __aCache, { |a| a[1] == cScriptKey })
if( nFound == 0 )
	// TODO: Hier muss das Script nun ermittelt werden und in den Cache geschrieben werden. 
	cScript := 'Set Angebot     = Record("Angebot") ' + CRLF + ;
	           'Set AngebotPos  = Record("AngebotPos")  ' + CRLF + ;	
	           'Set PosValue    = ParamValue("PosValue")   ' + CRLF + ; 
	           'ReturnValue = Round( PosValue,2 )'  

	aadd( __aCache, { cScriptKey, cScript } )
else
	cScript := __aCache[nFound][2] 
endif                                                                                
return( cScript )

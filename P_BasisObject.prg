/*

  	P_BasisObject

  	Ein Object mit AStatusStack und eigenem Transaktionsmanager
  	zur Benutzung oder Vererbung


*/

CLASS P_BasisObject INHERIT AObject


	DECLARE METHOD  Message
	DECLARE ACCESS  lError
	DECLARE ACCESS  oStatusStack
	DECLARE ACCESS  oTransactionManager
	DECLARE METHOD  IsStatusAvailable
	DECLARE METHOD  GetSeverityStatus
	DECLARE METHOD  ShowMessages
	DECLARE METHOD  GetMessageAsString



	PROTECT         __oStatus              AS AStatusStack
	PROTECT         __oTransactionManager  AS Servermanager

METHOD Init( oTransactionManager, oStatusStack ) CLASS P_BasisObject
// P_BasisObject { nil, _stack }

	SUPER:Init()

	//
	// Wurde ein StatusStack übergeben, so den benutzen, sonst neu erstellen
	//
	if( !IsNil( oStatusStack ) .and. IsInstanceOfUsual(oStatusStack, #AStatusStack))
		SELF:__oStatus := oStatusStack
		SELF:__oStatus:AddRef()
	else
		SELF:__oStatus := AStatusStack{}
	endif

	//
	// Wurde ein SeverManager übergeben, so den benutzen, sonst neu erstellen
	//
	if( !IsNil( oTransactionManager ) .and. IsInstanceOfUsual(oTransactionManager, #Servermanager))
		SELF:__oTransactionManager := oTransactionManager
		SELF:__oTransactionManager:AddRef()
	else
		SELF:__oTransactionManager := Servermanager{}
	endif


METHOD Destroy() AS VOID PASCAL CLASS P_BasisObject

	SELF:__oTransactionManager:Release()
	SELF:__oStatus:Release()
	SUPER:Destroy()

METHOD Message( cMessage AS STRING, aParams AS ARRAY, dwSeverity := 1U AS DWORD ) AS VOID PASCAL CLASS P_BasisObject
AddStatusRecord( SELF:oStatusStack, SUBSYSTEM_AMS,, dwSeverity,, MergeTextParams( cMessage, aParams ) )

ACCESS oStatusStack AS AStatusStack PASCAL CLASS P_BasisObject
return( SELF:__oStatus )

ACCESS oTransactionManager AS Servermanager PASCAL CLASS P_BasisObject
return( SELF:__oTransactionManager )

ACCESS lError AS LOGIC PASCAL CLASS P_BasisObject
// Gibt true zurück, wenn im StatusStack min. ein Fehler gemeldet wurde
return( SELF:IsStatusAvailable( SEVERITY_ERROR ) )

METHOD IsStatusAvailable( dwOverSeverity := 0U AS DWORD ) AS LOGIC PASCAL CLASS P_BasisObject
// Gibt true zurück, wenn im StatusStack eine Meldung >= [SEVERITY_NEUTRAL] gefunden wurde
return( SELF:GetSeverityStatus( dwOverSeverity ) )

METHOD GetSeverityStatus( dwSeverity AS DWORD ) AS LOGIC PASCAL CLASS P_BasisObject
// Gibt true zurück, wenn im StatusStack eine Meldung >= [dwSeverity] gefunden wurde
// SEVERITY_NEUTRAL
// SEVERITY_INFORMATION
// SEVERITY_WARNING
// SEVERITY_ERROR
// SEVERITY_FATALERROR

	LOCAL x                        AS DWORD
	LOCAL oRec                     AS AStatusRecord

if( SELF:oStatusStack:IsStatus )
	for x:=1 upto SELF:oStatusStack:RECCOUNT
		oRec := SELF:oStatusStack:RecordRetrieve( x )
		if( oRec:dwSeverity >= dwSeverity )
			oRec:Release()
			**************
			return( true )
			**************
		endif
		oRec:Release()
	next x
endif
return( false )

METHOD ShowMessages( oCommConnect AS ABaseCommConnect, cHeader := "" AS STRING, dwMinSeverityStatus := 2U AS DWORD ) AS LOGIC PASCAL CLASS P_BasisObject
if( SELF:GetSeverityStatus( dwMinSeverityStatus ) )
	oCommConnect:Warn( iif( !Empty(cHeader), cHeader + " : ", "" ) + SELF:GetMessageAsString( dwMinSeverityStatus )	, 0 )
endif
return( true )

METHOD GetMessageAsString( dwMinSeverityStatus := 0U AS DWORD ) AS STRING PASCAL CLASS P_BasisObject

	LOCAL cMessage := ""           AS STRING
	LOCAL x                        AS DWORD
	LOCAL oRec                     AS AStatusRecord

if( SELF:oStatusStack:IsStatus )
	for x:=1 upto SELF:oStatusStack:RECCOUNT
		oRec := SELF:oStatusStack:RecordRetrieve( x )
		if( oRec:dwSeverity >= dwMinSeverityStatus )
			cMessage += oRec:Message +CRLF
		endif
		oRec:Release()
	next x
endif
return( cMessage )





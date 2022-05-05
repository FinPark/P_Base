== TEXTBLOCK P_BaseFileWriteStream

/*
	Diese Klasse ist für das DIREKTE Schreiben in eine ASCII-Datei gedacht. Es soll hier bewusst auf
	Arrays o. Ä. verzichtet werden, um den internen Speicher nicht zu belasten und alles direkt
	auf Platte zu schreiben.
*/
CLASS P_BaseFileWriteStream INHERIT AObject

	//HWS - Open, Close und FetchRow für SEHR große Dateien, die eben nicht
	//einfach als String gelesen werden können!
	DECLARE METHOD OpenAppend
	DECLARE METHOD OpenOverwrite
	DECLARE METHOD OpenCreate
	DECLARE METHOD Close
	
	DECLARE METHOD WriteLine
	DECLARE METHOD Write
	DECLARE METHOD WriteFormatted
	
	DECLARE METHOD _InternalOpen
	
	EXPORT	lUniCode := FALSE				AS LOGIC
	EXPORT	lDosFormat := TRUE				AS LOGIC
	EXPORT	cLineEndings:= CRLF				AS STRING

	DECLARE ACCESS lError
	DECLARE ACCESS oBase

	PROTECT __oBase							AS P_BASE
	PROTECT __hndFile						AS PTR
	PROTECT __cCurrentFileName				AS STRING

METHOD INIT( oP_Base ) CLASS P_BaseFileWriteStream
	SUPER:Init()
	IF( !IsNil( oP_Base ) .AND. IsInstanceOfUsual(oP_Base, #P_Base))
		SELF:__oBase := oP_Base
		SELF:__oBase:AddRef()
	ELSE
		SELF:__oBase := P_Base{}
	ENDIF
	SELF:__cCurrentFileName := ""
	SELF:oBase:dbg("P_BaseFileWriter instanziert")
	
METHOD Destroy() AS VOID PASCAL CLASS P_BaseFileWriteStream
	SELF:Close()
	SELF:__oBase:Release()
	SELF:oBase:dbg("P_BaseFileWriter zerstört.")
	SUPER:Destroy()

METHOD _InternalOpen( cFileName AS STRING, nMode AS INT) AS LOGIC PASCAL CLASS P_BaseFileWriteStream
	LOCAL cPath	AS STRING
	
	// Oha - da will jemand eine Datei öffnen, obwohl eine offen ist!
	IF !Empty(SELF:__cCurrentFileName)
		SELF:oBase:DbgMessage("ACHTUNG - es war noch eine Datei im Zugriff # - die wurde jetzt automatisch geschlossen!",{ SELF:__cCurrentFileName })
		SELF:Close()
	ENDIF

	cPath := SELF:oBase:GetPathNameFromPath(cFileName)
	IF (!SELF:oBase:FolderExists(cPath))
		SELF:oBase:FolderCreate(cPath)
	ENDIF
	
	DO CASE 
	CASE nMode=1 .OR. nMode=3	//OpenCreate - OpenOverwrite
		IF SELF:oBase:FileExists(cFileName)
			SELF:oBase:FileDelete(cFileName)
		ENDIF
		SELF:__hndFile := FCreate2( cFileName, FC_NORMAL)
	CASE nMode=2	//OpenAppend
		IF SELF:oBase:FileExists(cFileName)
			SELF:__hndFile := FOpen2( cFileName, FO_WRITE + FO_EXCLUSIVE)
		ELSE
			SELF:__hndFile := FCreate2( cFileName, FC_NORMAL)
		ENDIF
	ENDCASE
	
	IF (SELF:__hndFile != F_ERROR)
		SELF:__cCurrentFileName := cFileName
	ELSE
		SELF:oBase:MessageFormat("Datei # konnte nicht erzeugt/geöffnet werden! Fehler: #",{cFileName,DosErrString(FError())},PROT_ART_ERROR)
	ENDIF
RETURN SELF:oBase:lError

METHOD WriteFormatted(cString AS STRING, aParams AS ARRAY, nLen:=0 AS INT) AS VOID PASCAL CLASS P_BaseFileWriteStream
	SELF:WriteLine( SELF:oBase:StringFormat(cString,aParams,nLen) )
RETURN

METHOD WriteLine(cString AS STRING) AS VOID PASCAL CLASS P_BaseFileWriteStream
	SELF:Write(cString+CRLF)
	
METHOD Write(cString AS STRING) AS VOID PASCAL CLASS P_BaseFileWriteStream
	LOCAL nBtWritten := 0

	IF !Empty(SELF:__cCurrentFileName)
		nBtWritten := FWrite(SELF:__hndFile,cString)
		IF (nBtWritten < 1)
			SELF:oBase:MessageFormat("In Datei # konnte nicht geschrieben werden! / Error: #",{ SELF:__cCurrentFileName, DosErrString(FError()) },PROT_ART_WARNING)
		ENDIF
	ENDIF
RETURN

METHOD OpenCreate( cFileName AS STRING) AS LOGIC PASCAL CLASS P_BaseFileWriteStream
RETURN SELF:_InternalOpen(cFileName,1)

METHOD OpenOverwrite( cFileName AS STRING) AS LOGIC PASCAL CLASS P_BaseFileWriteStream
RETURN SELF:_InternalOpen(cFileName, 3) 

METHOD OpenAppend( cFileName AS STRING) AS LOGIC PASCAL CLASS P_BaseFileWriteStream
RETURN SELF:_InternalOpen(cFileName, 2)


METHOD Close() AS VOID PASCAL CLASS P_BaseFileWriteStream
	IF (IsPtr(SELF:__hndFile))
		FClose(SELF:__hndFile)
		SELF:oBase:DbgMessage("Close Filehandle: #",{ SELF:__cCurrentFileName } )
	ENDIF
	SELF:__cCurrentFileName := ""
RETURN

ACCESS lError	AS LOGIC PASCAL CLASS P_BaseFileWriteStream
// Wenn ein Fehler protokolliert wurde, wird hier TRUE zurückgegeben
RETURN( SELF:oBase:lError )


ACCESS oBase	AS P_BASE PASCAL CLASS P_BaseFileWriteStream
// Liefert das übergebene oder erstellte oBase-Objekt
RETURN( SELF:__oBase )

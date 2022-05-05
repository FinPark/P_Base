== TEXTBLOCK P_BaseFileReadStream

/*
	Diese Klasse ist für das Einlesen großer Textdateien gedacht, die ein Einlesen in ein
	Array nicht voraussetzt.
	
	Es gibt zwei Möglichkeiten: 
		a.) Fetchen jeweils einer Zeile (wie bei SQL)
		b.) Fetchen der gesamten Datei in Array Elemente
	
	Ascii:Open("Meine Datei")
	DO WHILE ( Ascii:FetchRow(@myRow) )
		MachWasMit(myRow)
	ENDDO
	Ascii:Close()
	
	Ascii:Open("Meine Datei")
	Ascii:Load()			->> oder mit Übergabe eines CodeBlocks, das Ergebnis des Codeblocks wird im Array gespeichert!
	Ascii:Close()
	Ascii:ForEach( { |pos,Elem| DoWhatyouWant(pos,Elem)} )

	oder

	For i:=1 upto Ascii:Count
		cLine := Ascii:GetLine(i)
		DoWhatYouWant(cLine)
	next i
	Ascii:Unload() -> Leert das Array!
	
	
*/
CLASS P_BaseFileReadStream INHERIT AObject

	//HWS - Open, Close und FetchRow für SEHR große Dateien, die eben nicht
	//einfach als String gelesen werden können!
	DECLARE METHOD Open
	DECLARE METHOD FetchRow
	DECLARE METHOD EOF
	DECLARE METHOD Close

	DECLARE METHOD Load
	DECLARE METHOD Unload
	DECLARE METHOD GetRow
	DECLARE METHOD ForEach
	
	DECLARE METHOD _LoadBuffer
	DECLARE METHOD _InternEOF

	EXPORT	lUniCode := FALSE				AS LOGIC
	EXPORT	lDosFormat := TRUE				AS LOGIC
	EXPORT	cLineEndings:= CRLF				AS STRING

	DECLARE ACCESS lError
	DECLARE ACCESS oBase
	DECLARE ACCESS Count

	PROTECT __oBase							AS P_BASE
	PROTECT __hndFile						AS PTR
	PROTECT __cCurrentBuff					AS STRING
	PROTECT __cCurrentFileName				AS STRING
	PROTECT __aLines						AS ARRAY

METHOD INIT( oP_Base ) CLASS P_BaseFileReadStream
	SUPER:Init()
	IF( !IsNil( oP_Base ) .AND. IsInstanceOfUsual(oP_Base, #P_Base))
		SELF:__oBase := oP_Base
		SELF:__oBase:AddRef()
	ELSE
		SELF:__oBase := P_Base{}
	ENDIF
	SELF:__cCurrentFileName := ""
	SELF:__aLines := {}
	SELF:oBase:dbg("P_BaseFileReader instanziert")
	
METHOD Destroy() AS VOID PASCAL CLASS P_BaseFileReadStream
	SELF:Close()
	SELF:__oBase:Release()
	SELF:oBase:dbg("P_BaseFileReader zerstört.")
	SUPER:Destroy()
	
METHOD Open( cFileName AS STRING) AS LOGIC PASCAL CLASS P_BaseFileReadStream
	// Oha - da will jemand eine Datei öffnen, obwohl eine offen ist!
	IF !Empty(SELF:__cCurrentFileName)
		SELF:oBase:DbgMessage("ACHTUNG - es war noch eine Datei im Zugriff # - die wurde jetzt automatisch geschlossen!",{ SELF:__cCurrentFileName })
		SELF:Close()
		SELF:Unload()
	ENDIF

	SELF:__cCurrentBuff := ""
	SELF:__aLines := {}
	
	IF (SELF:oBase:FileExists(cFileName))
		SELF:__hndFile := FOpen2( cFileName, FO_READ + FO_EXCLUSIVE)
		IF !IsPtr(SELF:__hndFile)
			SELF:oBase:MessageFormat("Datei # konnte nicht geöffnet werden!",{cFileName,DosErrString(FError())},PROT_ART_ERROR)
		ELSE
			SELF:__cCurrentFileName := cFileName
		ENDIF
	ELSE
		SELF:oBase:MessageFormat("Die Datei # existiert nicht!",{cFileName},PROT_ART_ERROR)
	ENDIF
RETURN (!SELF:oBase:lError)

METHOD FetchRow() AS STRING PASCAL CLASS P_BaseFileReadStream
	LOCAL cResult := ""			AS STRING
	LOCAL nPos := 0				AS INT
 
//  	SELF:oBase:DbgMessage("FetchRow (START): EOF (#) / BuffLen (#) / nPos (#) / cResultLen (#)",{ SELF:_InternEOF(),Len(SELF:__cCurrentBuff),nPos, Len(cResult) },FALSE,TRUE)

	// Solange laden, bis man den ersten Delimiter (Zeilenumbruch) findet, oder der buffer nicht gefüllt ist
	WHILE (At(SELF:cLineEndings,SELF:__cCurrentBuff) < 1 .OR. Len(SELF:__cCurrentBuff) < 1)
		SELF:_LoadBuffer()
		
		IF Len(SELF:__cCurrentBuff) > 1000000
			SELF:oBase:MessageFormat("Buffer size exceeded > # - no Line-Endings! ",{ Len(SELF:__cCurrentBuff) },PROT_ART_ERROR,TRUE)
			EXIT
		ENDIF
		
		IF (SELF:EOF())
			EXIT
		ENDIF
	ENDDO

 	nPos := At(SELF:cLineEndings,SELF:__cCurrentBuff)
 	IF (nPos > 0)
	 	cResult := Left(SELF:__cCurrentBuff,nPos)
	 	SELF:__cCurrentBuff := SubStr( SELF:__cCurrentBuff, nPos+1, Len(SELF:__cCurrentBuff))
 	ELSE
		IF (Len(SELF:__cCurrentBuff) > 0)
			cResult := SELF:__cCurrentBuff
			SELF:__cCurrentBuff := ""
		ENDIF
 	ENDIF
//  	SELF:oBase:DbgMessage("FetchRow (  END): intern-EOF (#) / extern-EOF (#) / BuffLen (#) / nPos (#) / cResultLen (#)",{ SELF:_InternEOF(),SELF:EOF(), Len(SELF:__cCurrentBuff),nPos, Len(cResult) },FALSE,TRUE)
RETURN cResult

/*
	Lädt weiteren Text in den Buffer
*/
METHOD _LoadBuffer() AS VOID PASCAL CLASS P_BaseFileReadStream
	LOCAL cReadBuff := ""		AS STRING
	LOCAL nReadCnt := 0			AS INT

	IF !SELF:EOF()
		nReadCnt := FRead(SELF:__hndFile, @cReadBuff, 4096)
		IF ( nReadCnt > 0)
			IF (SELF:lDosFormat)
				SELF:__cCurrentBuff += Oem2Ansi( Left(cReadBuff,nReadCnt) )
			ELSE
				SELF:__cCurrentBuff += Left(cReadBuff,nReadCnt)
			ENDIF
		ENDIF 
	ENDIF
RETURN

METHOD _InternEOF() AS LOGIC PASCAL CLASS P_BaseFileReadStream
	LOCAL lResult := TRUE		AS LOGIC
	IF (IsPtr(SELF:__hndFile))
		lResult := FEof(SELF:__hndFile)
	ENDIF 
RETURN lResult
 
METHOD EOF() AS LOGIC PASCAL CLASS P_BaseFileReadStream
	LOCAL lResult := FALSE	AS LOGIC
	
	lResult := (SELF:_InternEOF() .AND. Len(SELF:__cCurrentBuff) < 1)
	IF (lResult)
		SELF:oBase:DbgMessage("Log. Dateiende erreicht. BufferLen: #",{Len(SELF:__cCurrentBuff)} )
	ENDIF
RETURN lResult 

METHOD Close() AS VOID PASCAL CLASS P_BaseFileReadStream
	IF (IsPtr(SELF:__hndFile))
		FClose(SELF:__hndFile)
		SELF:oBase:DbgMessage("Close Filehandle: #",{ SELF:__cCurrentFileName } )
	ENDIF
	SELF:__cCurrentFileName := ""
RETURN

METHOD Load( cb := NIL  AS USUAL ) AS VOID PASCAL CLASS P_BaseFileReadStream
	LOCAL cLine := ""
	LOCAL cNewLine := ""
	
	SELF:Unload()
	IF (IsPtr(SELF:__hndFile))
		FRewind(SELF:__hndFile)
	ENDIF
	
	WHILE !SELF:EOF()
		cLine := SELF:FetchRow()
		cNewLine := cLine
		IF !IsNil(cb)
			IF IsCodeBlock(cb)
				cNewLine := Eval( cb, cLine )
			ENDIF
		ENDIF
		AAdd(SELF:__aLines,cNewLine)
	ENDDO

	IF (IsPtr(SELF:__hndFile))
		FRewind(SELF:__hndFile)
	ENDIF
RETURN

METHOD Unload() AS VOID PASCAL CLASS P_BaseFileReadStream
	SELF:__aLines := {}
RETURN

METHOD GetRow(nPos AS INT) AS STRING PASCAL CLASS P_BaseFileReadStream
	LOCAL cResult := ""
	IF (nPos > 0 .AND. nPos < SELF:Count)
		cResult := SELF:__aLines[nPos]
	ENDIF
RETURN cResult

METHOD ForEach( cb AS CODEBLOCK ) AS VOID PASCAL CLASS P_BaseFileReadStream
	LOCAL X		AS INT
	FOR X:=1 UPTO SELF:Count
		Eval(cb,X,SELF:GetRow(X))
	NEXT X
RETURN

ACCESS lError	AS LOGIC PASCAL CLASS P_BaseFileReadStream
// Wenn ein Fehler protokolliert wurde, wird hier TRUE zurückgegeben
RETURN( SELF:oBase:lError )


ACCESS oBase	AS P_BASE PASCAL CLASS P_BaseFileReadStream
// Liefert das übergebene oder erstellte oBase-Objekt
RETURN( SELF:__oBase )

ACCESS Count	AS INT PASCAL CLASS P_BaseFileReadStream
// Liefert die Zeilen, die mit Load() geladen wurden
RETURN( ALen(SELF:__aLines) )

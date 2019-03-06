TEXTBLOCK == P_Base

DEFINE array_FIELDNAME            := 1
DEFINE array_VALUE                := 2
DEFINE array_CARGO                := 3

// Für StringAlign:
DEFINE sa_left                    := 0
DEFINE sa_middle                  := 1
DEFINE sa_right                   := 2

DEFINE dbg_Line                   := "------------------------------------------------------------------------------"

Function IfNil( uValue AS USUAL, uElseValue AS USUAL ) AS USUAL PASCAL
return( iif( IsNil(uValue), uElseValue, uValue ) )

Function UsualToString( uValue AS USUAL ) AS STRING PASCAL

	LOCAL oBase          AS P_Base
	LOCAL cValue         AS STRING

if( IsString(uValue) )
	cValue := uValue
else
	oBase := P_Base{}
	cValue := oBase:UsualToString( uValue )
	oBase:Release()
endif
return( cValue )

Function StringFormat( cText AS STRING, aItems AS ARRAY, nLen := 0 AS INT ) AS STRING PASCAL

	LOCAL oBase         AS P_Base
	LOCAL cRet := ""    AS STRING

oBase:=P_Base{}
cRet := oBase:StringFormat( cText, aItems, nLen )
oBase:Release()
return( cRet )

Function StringReplace( cString AS STRING, cSearch AS STRING, cReplace AS STRING ) AS STRING PASCAL

	LOCAL oBase         AS P_Base
	LOCAL cRet := ""    AS STRING

oBase:=P_Base{}
cRet := oBase:StringReplace( cString, cSearch, cReplace )
oBase:Release()
return( cRet )

/*

	P_BaseObject

	Ein vererbares Object mit P_Base-Nutzung
	und lokalem Error-Handling.
	-----------------------------------------


*/

CLASS P_BaseObject INHERIT AObject

	DECLARE ACCESS  oBase
	DECLARE ACCESS  lError
	DECLARE METHOD  ShowMessages


	PROTECT __oBase            AS P_Base
	PROTECT __symErrorGuid     AS SYMBOL

METHOD Init( oP_Base ) CLASS P_BaseObject

	SUPER:Init()

	if( !IsNil( oP_Base ) .and. IsInstanceOfUsual(oP_Base, #P_Base))
		SELF:__oBase := oP_Base
		SELF:__oBase:AddRef()
	else
		SELF:__oBase := P_Base{}
	endif

// Einen Fehlermarker nur für dieses Object erzeugen, auch
// wenn p_Base reingereicht wurde
SELF:__symErrorGuid := String2Symbol( NewID() )
SELF:oBase:ResetLocalError( SELF:__symErrorGuid )

METHOD Destroy() AS VOID PASCAL CLASS P_BaseObject

	SELF:oBase:Release()
	SUPER:Destroy()

ACCESS oBase AS P_Base PASCAL CLASS P_BaseObject
return( SELF:__oBase )

ACCESS lError AS LOGIC PASCAL CLASS P_BaseObject
return( SELF:oBase:IsLocalError( SELF:__symErrorGuid ) )

METHOD ShowMessages( oCommConnect AS ABaseCommConnect, cMeldung AS STRING, lAuchWarnungen := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseObject

	LOCAL lReturn             AS LOGIC

if( SELF:oBAse:IsProtocol() )
	if( SELF:oBase:lError .or. ( lAuchWarnungen .and. SELF:oBase:lWarning ) )
		lReturn := SELF:oBase:ShowProtIfError( SELF:oBase:oCommConnect, cMeldung, lAuchWarnungen )
	endif
endif
return( lReturn  )







/*
  	P_Base :

	Funktionsbiblothek für Projekte
	-------------------------------
	André Finken





*/
CLASS P_Base INHERIT AObject

	/* Globale Eigenschaften und Methoden                             */
	/* -------------------------------------------------------------- */
	DECLARE METHOD Integrate

	/* Datenbank */
	DECLARE ACCESS oTransactionManager
	DECLARE ASSIGN oTransactionManager
	DECLARE METHOD BeginTransaction
	DECLARE METHOD EndTransaction
	DECLARE METHOD IsTransactionManager
	DECLARE METHOD TransactionIsolationLevelAsString

	/* Tabellen/Felder Methoden */
	DECLARE METHOD CreateSQLStatement
	DECLARE METHOD CreateCopyStatement
	DECLARE METHOD CreateAServer
	DECLARE METHOD CreateDataSet
	DECLARE METHOD UpdateDataSet
	DECLARE METHOD IsDataSet
	DECLARE METHOD UpdateDataCodeblock
	DECLARE METHOD UpdateFieldsForTable
	DECLARE METHOD GetFieldFromTable
	DECLARE METHOD GetFieldsFromTable
	DECLARE METHOD GetReadRecordFromTable
	DECLARE METHOD GetWriteRecordFromTable
	DECLARE METHOD RecordToTable
	DECLARE METHOD GetRecordID
	DECLARE METHOD CheckFieldsFound
	DECLARE METHOD GetNextPos
	DECLARE METHOD GetBelegNr
	DECLARE METHOD LinkDocument
	DECLARE METHOD SqlTableHasRecords
	DECLARE METHOD GetRowsProcessed

	/* Datensätze sperren */
	DECLARE METHOD RecordLock
	DECLARE METHOD RecordUnlock
	DECLARE METHOD RecordUnlockAll
	DECLARE METHOD RecordLocked
	DECLARE METHOD RecordUpdate
	DECLARE METHOD RecordLockInformation
	DECLARE METHOD IsLockError

	/* Progress */
	DECLARE ACCESS oProgress
	DECLARE ASSIGN oProgress
	DECLARE METHOD ProgressIncrement

	/* Protokoll und Messaging */
	DECLARE METHOD IntegrateMessages
	DECLARE METHOD Message
	DECLARE METHOD MessageFormat
	DECLARE METHOD AddMessageText
	DECLARE METHOD ResetLocalError
	DECLARE METHOD IsLocalError
	DECLARE METHOD IsLocalTransactionLockError
	DECLARE ACCESS lIsTransactionLockError
	DECLARE ACCESS lError
	DECLARE ACCESS lWarning
	DECLARE ACCESS lInformation
	DECLARE ACCESS lAnyMessage
	DECLARE METHOD MessageCount
	DECLARE ACCESS oProtocol
	DECLARE ASSIGN oProtocol
	DECLARE METHOD IsProtocol
	DECLARE METHOD CreateProtocol
	DECLARE METHOD AutoCreateProtocol
//	DECLARE METHOD GetMessages
	DECLARE METHOD AddMessages
	DECLARE METHOD GetMessageMemo
	DECLARE METHOD GetMessageArray
	DECLARE METHOD AddMessageArray
	DECLARE METHOD ShowMessage
	DECLARE METHOD ShowProtIfError
	DECLARE METHOD ShowProt
	DECLARE METHOD MessageOutput
	DECLARE METHOD MessageToStack
	DECLARE METHOD MessageToProtocol
	DECLARE METHOD MessageToPRCStatus
	DECLARE METHOD ProtArtToSeverityStatus

	/* ASCII Filehandling */
	DECLARE METHOD WriteLineUTF8
	DECLARE METHOD WriteLine
	DECLARE METHOD FileWrite
	DECLARE METHOD FileExists
	DECLARE METHOD FileDelete
	DECLARE METHOD FileDeleteIfExists
	DECLARE METHOD FileCopy
	DECLARE METHOD FileMove
	DECLARE METHOD FileRename
	DECLARE METHOD FileReadToArray
	DECLARE METHOD FileReadToString
	DECLARE METHOD FileWriteFromArray
	DECLARE METHOD FileWriteFromString
	DECLARE METHOD GetFileNameFromPath

	/* ASCII Folderhandling */
	DECLARE METHOD GetAmsBinPath
	DECLARE METHOD FolderPrepare
	DECLARE METHOD FolderUnPrepare
	DECLARE METHOD FolderExists
	DECLARE METHOD FolderCreate
	DECLARE METHOD FolderCopy
	DECLARE METHOD GetFilesFromDir
	DECLARE METHOD GetFolderFromDir
	DECLARE METHOD GetPathNameFromPath
	DECLARE METHOD AddPath
	DECLARE METHOD FolderReplace
	DECLARE METHOD BuildFileName

	/* Für komplexere Operationen s. Cargo-Object in Evernote */
	DECLARE METHOD GetCargo
	DECLARE METHOD SetCargo
	DECLARE METHOD IsInCargo
	DECLARE METHOD IsCargoType
	DECLARE ACCESS Cargo

	/* Array Methoden */
	DECLARE METHOD MemoToArray
	DECLARE METHOD ArrayFind
	DECLARE METHOD ArrayGroup
	DECLARE METHOD ArrayFilter
	DECLARE METHOD ArrayValidate
	DECLARE METHOD ArrayChangeDimension
	DECLARE METHOD ArrayCombine
	DECLARE METHOD ArrayCompare
	DECLARE METHOD ArrayKonvert
	DECLARE METHOD ArraySerialize
	DECLARE METHOD __ArraySerialize
	DECLARE METHOD ArrayDeserialize
	DECLARE METHOD __ArrayDeserialize
	DECLARE METHOD ArrayToString
	DECLARE METHOD ArrayStructureToString
	DECLARE METHOD ArrayToDisk
	DECLARE METHOD ArrayFromDisk
	DECLARE METHOD ArrayToSql
	DECLARE METHOD ArrayFromSql
	DECLARE METHOD ArrayFromSqlStatement
	DECLARE METHOD ArrayRecordFromSql
	DECLARE METHOD GetSqlRecord
	DECLARE METHOD ArrayFromRecord
	DECLARE METHOD ArrayToRecord
	DECLARE METHOD ArrayAddIfNotExists
	DECLARE METHOD ArrayToFieldListString
	DECLARE METHOD GetArrayValue
	DECLARE METHOD GetArrayCargo
	DECLARE METHOD SetArrayValue
	DECLARE METHOD TypeOfArrayField
	DECLARE METHOD DeleteArrayValue
	DECLARE METHOD isArrayField
	DECLARE METHOD ArrayProtocolChanges
	DECLARE METHOD SqlToMemo
	DECLARE METHOD KeyToString
	DECLARE METHOD GetConfigArray
	DECLARE METHOD IsInConfigList

   /* String-Konvertierungen und Methoden */
	DECLARE METHOD StringToUsual
	DECLARE METHOD UsualTypeAsString
	DECLARE METHOD UsualToString
	DECLARE METHOD UsualToSymbol
	DECLARE METHOD UsualToNumeric
	DECLARE METHOD UsualInitialValue
	DECLARE METHOD StringToArray
	DECLARE METHOD StringToDisk
	DECLARE METHOD StringToUnicode
	DECLARE METHOD StringToCodeblock
	DECLARE METHOD StringToXml
	DECLARE METHOD StringFromXml
	DECLARE METHOD StringFromUnicode
	DECLARE METHOD StringConvertCodepage
	DECLARE METHOD StringFormat
	DECLARE METHOD StringDecrement
	DECLARE METHOD StringCutLeft
	DECLARE METHOD StringLength
	DECLARE METHOD StringAlign
	DECLARE METHOD StringFixPos
	DECLARE METHOD StringTranslate
	DECLARE METHOD StringRepeat
	DECLARE METHOD StringSeekBackward
	DECLARE METHOD StringDeleteEmptyLines
	DECLARE METHOD StringFindInArray
	DECLARE METHOD StringFind
	DECLARE METHOD StringBetween
	DECLARE METHOD StringReplace
	DECLARE METHOD StringMultiReplace
	DECLARE METHOD StringTemplate
	DECLARE METHOD StringFirstUpper
	DECLARE METHOD StringZero
	DECLARE METHOD StringDate
	DECLARE METHOD StringTime
	DECLARE METHOD StringDuration
	DECLARE METHOD ToDate
	DECLARE METHOD DateAdd

	DECLARE METHOD NumericBuilder
	DECLARE METHOD StringBuilder
	DECLARE METHOD Divide
	DECLARE METHOD HasDecimals
	DECLARE METHOD GetDecimals
	DECLARE METHOD VOTypeToSqlType
	DECLARE METHOD CleanUp
	DECLARE METHOD GetConfigParam
	DECLARE METHOD GetKomplexConfigParameter
	DECLARE METHOD RenameKeyField
	DECLARE METHOD ReleaseIfNotNullObject

	/* Debug und Performance */
	DECLARE METHOD DbgMessage
	DECLARE METHOD DbgArray
	DECLARE METHOD DbgRecord
	DECLARE METHOD DbgObjectTree
	DECLARE METHOD SqlToDebugPrint
	DECLARE ACCESS lDebugMode
	DECLARE ASSIGN lDebugMode
	DECLARE ACCESS dwRunTime
	DECLARE METHOD RunTimeOutput
	DECLARE METHOD ResetRunTime
	DECLARE METHOD Trace
	DECLARE METHOD TraceOut

	EXPORT cbError                            AS CODEBLOCK
	EXPORT oCommConnect                       AS ABaseCommConnect

	/* Interne Eigenschaften und Methoden                             */
	/* -------------------------------------------------------------- */
	DECLARE METHOD dbg
	DECLARE METHOD __MessagesToProtocol
	DECLARE METHOD PrepareCodeblock
	DECLARE METHOD _CheckPath
	DECLARE METHOD __StringTimeHelper

	PROTECT __oTransactionManager             AS Servermanager
	PROTECT __lDestroyTransactionManager      AS LOGIC
  	PROTECT __oProtocol                       AS AMSProtocol
  	PROTECT __lDestroyProtocol                AS LOGIC
  	PROTECT __oProgress                       AS ABaseProgressConnect
  	PROTECT __lDestroyProgress                AS LOGIC
	PROTECT __aMessage                        AS ARRAY
	PROTECT __lDebugMode                      AS LOGIC
	PROTECT __dwStartTime                     AS FLOAT
	PROTECT __lAutoCreateProtocol             AS LOGIC
	PROTECT __cBereich                        AS STRING
	PROTECT __cTitle                          AS STRING
	PROTECT __aCargo                          AS ARRAY
	PROTECT __oIntegratedBaseObject           AS P_Base
	PROTECT __aTrace                          AS ARRAY
	PROTECT __aLocalError                     AS ARRAY
	PROTECT __aLockedRecords                  AS ARRAY
	PROTECT __aCache                          AS ARRAY
	PROTECT __aAddMessageText                 AS ARRAY
	PROTECT __lIsTransactionLockError         AS LOGIC

METHOD Init() CLASS P_Base

	SUPER:Init()


	/* Interne Variablen */
	SELF:__oTransactionManager         := NULL_OBJECT
	SELF:__lDestroyTransactionManager  := FALSE

	SELF:__oProtocol                   := NULL_OBJECT
	SELF:__lDestroyProtocol            := FALSE

	SELF:__oProgress                   := NULL_OBJECT
	SELF:oCommConnect                  := NULL_OBJECT
	SELF:__lDestroyProgress            := FALSE

	SELF:__aMessage                    := {}

	SELF:__lAutoCreateProtocol         := FALSE
	SELF:__cBereich                    := ""
	SELF:__cTitle                      := ""

	SELF:__lDebugMode                  := FALSE
	SELF:__aTrace                      := {}
	SELF:ResetRunTime()

	/* CargoArray für die Methoden GetCargo und SetCargo */
	SELF:__aCargo                      := {}

	/* Über Integrate() kann ein Base-Objekt reingereicht werden. */
	/* Eigenschaften werden von diesem übernommen und Meldungen dorthin weitergereicht */
	SELF:__oIntegratedBaseObject       := NULL_OBJECT

	// Mit LocalError können Teilbereichsfehler abgefragt werden
	// Anders als lError, welcher über die ganze Zeit des P_Base
	// Objektes gesetzt ist
	SELF:__aLocalError                 := {}

	// Mit LockRecord() und UnlockRecord() können Datensätze
	// gesperrt oder wieder freigegben werden.
	SELF:__aLockedRecords              := {}

	// Mit Cache() können Daten gespeichert und Abgerufen werden
	SELF:__aCache                      := {}

	// Über die Methode AddMessageText() können Texte den Meldungen vorangestellt werden
	SELF:__aAddMessageText             := {}

	// Über diesen Access kann abgefragt werden, ob ein Lock-Fehler stattgefunden hat
	SELF:__lIsTransactionLockError     := FALSE

METHOD Destroy() AS VOID PASCAL CLASS P_Base

	SELF:CleanUp()
	SUPER:Destroy()

METHOD Integrate( oBase AS P_BASE ) AS VOID PASCAL CLASS P_Base

	// Eigenschaften eines anderen P_BAse-Objektes werden übernommen
	SELF:oTransactionManager := oBase:oTransactionManager
	SELF:oProgress           := oBase:oProgress
	SELF:oProtocol           := oBase:oProtocol
	SELF:lDebugMode          := oBase:lDebugMode

	// Meldungen werden auch an dieses Objekt weitergegeben
	SELF:__oIntegratedBaseObject := oBase

METHOD CleanUp() AS VOID PASCAL CLASS P_Base
// Aufräumen und ggf. laufende Objecte beenden
//


	/* LockedRecord */
	if( ALen( SELF:__aLockedRecords ) > 0 )
		SELF:dbg( "Es bestanden bei Zerstörung von P_Base noch gelockte Records", TRUE )
		SELF:RecordUnlockAll()
	endif

	/* PROGRESS */
	if( SELF:__lDestroyProgress )
		if( SELF:__oProgress != NULL_OBJECT )
			SELF:__oProgress:Release()
			SELF:__lDestroyProgress := FALSE
			SELF:__oProgress := NULL_OBJECT
			SELF:dbg("Progress zerstört", TRUE)
		endif
	endif

	/* PROTOCOL */
	if( SELF:__lDestroyProtocol )
		if( SELF:__oProtocol != NULL_OBJECT )
			SELF:__oProtocol:Stop(!SELF:lError)
			SELF:__oProtocol:Release()
			SELF:__oProtocol:=NULL_OBJECT
			SELF:__lDestroyProtocol := FALSE
			SELF:dbg(SELF:StringFormat("Protocol # abgeschlossen und oProtocoll zerstört", {iif( SELF:lError, "mit Fehlern", "erfolgreich")} ), TRUE)
		endif
	endif

	/* TRANSACTIONMANAGER */
	if( SELF:__oTransactionManager != NULL_OBJECT )
		if( SELF:__lDestroyTransactionManager )
			// !!! Achtung !!!
			// Transaction nur schliessen, wenn der Transactionsmanager durch
			// P_Base() erstellt wurde. Anderfalls führt das zu einem ODBC
			// Fehler in der Funktionsreihenfolge
			if( SELF:__oTransactionManager:TransactionOpen )
				SELF:dbg("!!! TransactionManager has open Transactions. Transaction positive submittet")
				SELF:__oTransactionManager:endTran(TRUE)
			endif
			//SELF:dbg("TransactionsManager zerstört", TRUE)
		else
			//SELF:dbg("TransactionsManager released", TRUE)
		endif
		/* TransactionManager hat AddRef() bekommen und kann nun released werden */
		SELF:__oTransactionManager:Release()
		SELF:__oTransactionManager := NULL_OBJECT
	endif

ACCESS dwRunTime	AS FLOAT PASCAL CLASS P_Base
// Gibt die Laufzeit des Object, bzw. seit ResetRunTime() in Minuten zurück
return((Seconds()-SELF:__dwStartTime)/60)

METHOD ResetRunTime() AS VOID PASCAL CLASS P_Base
SELF:__dwStartTime := Seconds()

METHOD RunTimeOutput() AS STRING PASCAL CLASS P_Base
// Gibt die Laufzeit in HH:MM:SS als String zurück
return( SELF:StringDuration( SELF:dwRunTime * 60 ) )

ASSIGN lDebugMode( lNewMode AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
if( SELF:__lDebugMode .and. lNewMode == FALSE )
	// Debug war an und wird nun ausgeschaltet
	SELF:dbg(dbg_Line)
	SELF:dbg("P_Base Object Runtime: "+AllTrim(Str3(SELF:dwRunTime,15,5))+" Minutes")
	SELF:dbg(dbg_Line)
	SELF:dbg( SELF:StringAlign( "ENDE P_BASE OBJECT DEBUG", Len(dbg_Line),sa_middle ) )
	SELF:dbg(dbg_Line)
	SELF:__lDebugMode := FALSE
elseif ( !SELF:__lDebugMode .and. lNewMode == TRUE )
	// Debug war aus und wird angeschaltet
	SELF:__lDebugMode := TRUE
	SELF:dbg(dbg_Line)
	SELF:dbg( SELF:StringAlign( "START P_BASE OBJECT DEBUG", Len(dbg_Line),sa_middle ) )
	SELF:dbg(dbg_Line)
elseif ( SELF:__lDebugMode .and. lNewMode == TRUE )
	// Debug war an und wird erneut angeschaltet
	// Nun hier Performanceauswertung schreiben
	SELF:dbg("Marker P_Base Object Runtime: "+AllTrim(Str3(SELF:dwRunTime,15,5))+" Seconds")
else
	// Debugmode war aus und wird ausgeschaltet,
endif
return( SELF:__lDebugMode )

ACCESS lDebugMode AS LOGIC PASCAL CLASS P_Base
return( SELF:__lDebugMode )

ACCESS lIsTransactionLockError AS LOGIC PASCAL CLASS P_Base
// Wenn ein Lock-Error aufgetreten ist, so ist er hier vermerkt.
// Ein BeginTran() setzt diesen Status wieder zurück.
return( __lIsTransactionLockError )

ACCESS lError AS LOGIC PASCAL CLASS P_Base
// Gibt TRUE zurück, wenn im Message-Stack ein PROT_ART_ERROR vorhanden
return( AScan( SELF:__aMessage, { |aM| aM[3] == PROT_ART_ERROR } ) != 0 )

METHOD IsLocalError( symBereich AS SYMBOL ) AS LOGIC PASCAL CLASS P_Base
// Gibt TRUE zurück, wenn seit dem letzten ResetLocalError( symBereich ) ein Fehler aufgetreten ist.
return( IfNil(SELF:GetArrayValue( symBereich, SELF:__aLocalError ), 0 ) < SELF:MessageCount("E") )

METHOD IsLocalTransactionLockError( symBereich AS SYMBOL ) AS LOGIC PASCAL CLASS P_Base
// Überprüft die letzten Fehlermeldungen (für symBereich) und gibt TRUE
// zurück, wenn ein Lock/Deadlock Fehler aufgetreten ist

	LOCAL x, nLocalErrorStart  AS INT
	LOCAL	lLockError := FALSE  AS LOGIC
	LOCAL aErrorMessages       AS ARRAY

if( SELF:IsLocalError( symBereich ) )
	nLocalErrorStart := IfNil(SELF:GetArrayValue( symBereich, SELF:__aLocalError ), 0 )
	aErrorMessages := SELF:GetMessageArray( "E" )
	for x:=nLocalErrorStart upto ALen(aErrorMessages)

		if( SELF:IsLockError( aErrorMessages[x][2] ) )
			lLockError := TRUE
			exit
		endif

	next x
endif
return( lLockError )

METHOD ResetLocalError( symBereich AS SYMBOL ) AS VOID PASCAL CLASS P_Base
// Es wird sich für edn Bereich <symBereich> der Fehlerstand gemerkt. Mit IsLocalError() kann
// abgefragt werden, ob seit dem ResetLocalError ein Fehler aufegtreten ist
SELF:SetArrayValue( symBereich, SELF:MessageCount("E"), SELF:__aLocalError, TRUE )

ACCESS lWarning AS LOGIC PASCAL CLASS P_Base
// Gibt TRUE zurück, wenn im Message-Stack ein PROT_ART_WARNING vorhanden
return( AScan( SELF:__aMessage, { |aM| aM[3] == PROT_ART_WARNING } ) != 0 )

ACCESS lInformation AS LOGIC PASCAL CLASS P_Base
// Gibt TRUE zurück, wenn im Message-Stack ein PROT_ART_INFORMATION vorhanden
return( AScan( SELF:__aMessage, { |aM| aM[3] == PROT_ART_INFORMATION } ) != 0 )

ACCESS lAnyMessage AS LOGIC PASCAL CLASS P_Base
// Gibt TRUE zurück, wenn Meldungen vorhanden sind, egal welcher Schwere
return( ALen(SELF:__aMessage) != 0 )

METHOD MessageCount( cArt := "E" AS STRING ) AS INT PASCAL CLASS P_Base
// Gibt die Anzahl der aufgetretenen Meldungen für den Bereich <cArt> zurück
// Eine Kombination "EWI" ist möglich. E = Error, W = Warnung, I = Information

	LOCAL nError := 0  AS INT

AEval( SELF:__aMessage, { |aM| nError += IIF( aM[3] $ cArt, 1, 0 ) } )
return( nError)

ASSIGN oTransactionManager( oT AS Servermanager ) AS ServerManager PASCAL CLASS P_Base
// Zuweisen eines TransactionsManagers. Mit diesem wird dann gearbeitet
if( SELF:__oTransactionManager == NULL_OBJECT )
	SELF:__lDestroyTransactionManager := FALSE
	SELF:__oTransactionManager := oT
	SELF:__oTransactionManager:AddRef()
	//SELF:dbg( "Transactionsmanager wurde von Aussen reingegeben. Siehe Objekt P_BASE, ACCESS oTransactionManager", TRUE)
else
	SELF:Message( "Transactionsmanager mehrfach zugewiesen. Es wurde versucht, trotz bereits vorhandenem TransactionsManagers, einen neuen zu setzen. Der neue TransactionsManager wird nicht angenommen. Siehe Objekt P_BASE, ASSIGN oTransactionManager", PROT_ART_WARNING, "", TRUE )
endif

ACCESS oTransactionManager AS Servermanager PASCAL CLASS P_Base
// Abfrage des TransactionsManagers. Wenn noch nicht vorhanden, dann wird beim Zugriff
// der TransactionManager aus ServerManager erstellt.
if( SELF:__oTransactionManager == NULL_OBJECT )
	//SELF:dbg( "Transactionsmanager war nicht vorhanden. Transactionsmanager wurde erstellt. Siehe Objekt P_BASE, ACCESS oTransactionManager", TRUE)
	SELF:__lDestroyTransactionManager := TRUE
	SELF:__oTransactionManager := Servermanager{}
endif
return( SELF:__oTransactionManager )

METHOD IsTransactionManager() AS LOGIC PASCAL CLASS P_Base
// Abfrage, ob ein Transactionsmanager initialisiert ist
return( SELF:__oTransactionManager != NULL_OBJECT )

METHOD TransactionIsolationLevelAsString( nTransactionIsolationLevel AS INT ) AS STRING PASCAL CLASS P_Base

	LOCAL cLevel := ""                   AS STRING

do case
case( nTransactionIsolationLevel == SQL_TXN_READ_COMMITTED )
	cLevel := "Read Committed"
case( nTransactionIsolationLevel == SQL_TXN_READ_UNCOMMITTED )
	cLevel := "Read Uncommitted"
case( nTransactionIsolationLevel == SQL_TXN_REPEATABLE_READ )
	cLevel := "Repeatable Read"
case( nTransactionIsolationLevel == SQL_TXN_SERIALIZABLE )
	cLevel := "Serializable"
Otherwise
	cLevel := "(unknown: "+SELF:UsualToString( nTransactionIsolationLevel ) + ")"
endcase

return( cLevel )

METHOD BeginTransaction( lResetLockError := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
// Startet eine Transaction und gibt ggf. Meldungen aus
	LOCAL lSuccess := TRUE   AS LOGIC

/*
if( SELF:oTransactionManager:TransactionOpen )
	SELF:Message( "Fehler beim starten einer neuen Transaktion. Es ist bereits eine offene Transaction vorhanden.", PROT_ART_ERROR, "", TRUE )
	lSuccess := FALSE
else
*/
	// Dieser merker wird auf True gesetzt, wenn ein Lock-Fehler auftritt
	if( lResetLockError )
	   SELF:__lIsTransactionLockError := FALSE
	endif

	SELF:dbg( SELF:StringFormat("Beginne # Transaction mit Isolationslevel #", {iif(SELF:oTransactionManager:TransactionOpen, "vorhandene", "neue"), SELF:TransactionIsolationLevelAsString( SELF:oTransactionManager:TransactionIsolationLevel ) } ) , TRUE)

	// Wenn bereits eine Transaction gestartet wurde,
	// dann wird jetzt nur ein Zähler hochgezählt, keine neue
	// Transaction gestartet.
	if( !SELF:oTransactionManager:BeginTran() )
		 lSuccess := FALSE
		 SELF:MessageFormat( "Fehler beim starten einer neuen Transaktion. # ", { SELF:oTransactionManager:Status:GetMessage() }, PROT_ART_ERROR, TRUE )
	endif
//endif
return( lSuccess )

METHOD EndTransaction( lSuccess AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
// Beendet eine laufende Transaction und gibt ggf. Meldungen aus

SELF:dbg( SELF:StringFormat("Beende Transaction mit #, Isolationslevel #, Es sind # Lock/Deadlock-Fehler aufgetreten.", {lSuccess, SELF:TransactionIsolationLevelAsString( SELF:oTransactionManager:TransactionIsolationLevel ), iif( SELF:__lIsTransactionLockError,"","keine" ) } ) , TRUE)

if( !SELF:oTransactionManager:EndTran( lSuccess ) )
	lSuccess := FALSE
	SELF:MessageFormat( "Fehler beim beenden einer Transaktion. # ", { SELF:oTransactionManager:Status:GetMessage() }, PROT_ART_ERROR, TRUE )
else
	lSuccess := TRUE
endif
return( lSuccess )

METHOD SetCargo( uFieldName AS USUAL, uValue AS USUAL ) AS VOID PASCAL CLASS P_Base
// Bsp: SetCargo( #MEMO, oRecord:FGet(#MEMO_TECH))
SELF:SetArrayValue( uFieldName, uValue, SELF:__aCargo )

METHOD GetCargo( uFieldName AS USUAL ) AS USUAL PASCAL CLASS P_Base
// Wird [uFieldName] nicht gefunden, so wird Nil zurück gegeben
return( SELF:GetArrayValue( uFieldName, SELF:__aCargo ) )

METHOD IsInCargo(  uFieldName AS USUAL ) AS INT PASCAL CLASS P_Base
// Liefert 0, wenn [uFieldName] nicht gefunden wurde
return( SELF:isArrayField( uFieldName, SELF:__aCargo ) )

METHOD IsCargoType( uFieldName AS USUAL, uMyValue AS USUAL ) AS LOGIC PASCAL CLASS P_Base
// Prüft, ob der Typ des Eintrages zu [uFieldName] zu dem Typ der übergebenen
// Variable [uMyValue] passt.
return( SELF:TypeOfArrayField( uFieldName, SELF:__aCargo ) == UsualType( uMyValue ) )

ACCESS Cargo AS ARRAY PASCAL CLASS P_Base
return( SELF:__aCargo )

METHOD DeleteArrayValue( uFieldName AS USUAL, aArray AS ARRAY ) AS ARRAY PASCAL CLASS P_Base
// <uFieldName> kann als Symbol oder als String übergeben werden.
// Durchsucht das Array nach <cFieldName> und löscht den Eintrag

	LOCAL nFound := 0          AS INT

nFound := SELF:isArrayField( uFieldName, aArray )
if( nFound != 0 )
	ADelShrink( aArray, nFound )
else
	SELF:dbg( SELF:StringFormat("Achtung, zu löschendes Element # bei DeleteArrayValue() nicht gefunden", {uFieldName} ) )
endif

return( aArray )


METHOD GetArrayValue( uFieldName AS USUAL, aArray AS ARRAY ) AS USUAL PASCAL CLASS P_Base
// <uFieldName> kann als Symbol oder als String übergeben werden.
// Durchsucht ein Array (Aufbau = { {"Key1", "Value"}, {"Key2", 120} }
// und gibt den Value-Eintrag zurück
// Beispiel: GetArrayValue( "Finken", { {"Meyer", 10}, {"finken", "nix"}, {"Brümmelmeyer", TRUE} } --> "nix"
	LOCAL uFieldValue          AS USUAL
	LOCAL nFound := 0          AS INT

nFound := SELF:isArrayField( uFieldName, aArray )
if( nFound != 0 )
	uFieldValue := aArray[nFound][array_VALUE]
else
	uFieldValue := Nil
endif

return( uFieldValue )

METHOD GetArrayCargo( uFieldName AS USUAL, aArray AS ARRAY ) AS ARRAY PASCAL CLASS P_Base
// Der Aufbau des Arrays ist:
// { {#Key, uValue, {Cargo...}}, {#Key, uValue, {Cargo...}}, ...
// Das Cargo-Array an Position 3 kann alles enthalten und wird einfach
// mitgeschliffen.
//
	LOCAL aReturn            AS ARRAY
	LOCAL nFound             AS INT

aReturn := {}
nFound := SELF:isArrayField( uFieldName, aArray )
if( nFound != 0 )
	aReturn := aArray[nFound][array_CARGO]
endif
return( aReturn )

METHOD SetArrayValue( uFieldName AS USUAL, uValue AS USUAL, aArray AS ARRAY, lAppendMode := FALSE AS LOGIC, aCargo := nil AS ARRAY ) AS ARRAY PASCAL CLASS P_Base
// <uFieldName> kann als Symbol oder als String übergeben werden.
// Durchsucht ein Array (Aufbau = { {"Key1", "Value"}, {"Key2", 120} }
// und setzt den Value-Einrag [2] neu. Wird der Key nicht gefunden,
// so wird das Element angehangen (s. Parameter <lAppendMode>)
// Beispiel: SetArrayValue( "Finken", 100, { {"Meyer", 10}, {"finken", "nix"}, {"Brümmelmeyer", TRUE} } --> { {"Meyer", 10}, {"finken", 100}, {"Brümmelmeyer", TRUE} }

	LOCAL nFound := 0          AS INT

if( aArray == NULL_ARRAY )
	// Es wurde vergessen, dass Array mit := {} zu dimensionieren.
	// Mach ich jetzt hier einfach
	aArray := {}
endif

nFound := SELF:isArrayField( uFieldName, aArray )
if( nFound != 0 )
	aArray[nFound][array_VALUE] := uValue
	if( !IsNil(aCargo) )
		if( ALen(aArray[nFound]) >= array_CARGO )
			aArray[nFound][array_CARGO] := aCargo
		else
			AAdd( aArray[nFound], aCargo )
		endif
	endif
else
	if( lAppendMode )
		aCargo := IfNil(aCargo, {})
		AAdd( aArray, { uFieldName, uValue, aCargo })
	else
		SELF:dbg( SELF:StringFormat("Achtung, Element # bei SetArrayValue() nicht gefunden und <AppendMode> ausgeschaltet.", {uFieldName} ) )
	endif
endif

return( aArray )

METHOD TypeOfArrayField( uFieldName AS USUAL, aArray AS ARRAY ) AS DWord PASCAL CLASS P_Base
return( UsualType( SELF:GetArrayValue( uFieldName, aArray ) ) )

METHOD isArrayField( uFieldName AS USUAL, aArray AS ARRAY ) AS INT PASCAL CLASS P_Base
// <uFieldName> kann als Symbol oder als String übergeben werden.
// Sucht im Array <cFieldName> und gibt die Position zurück, oder 0 wenn nicht gefunden.
if( IsString( uFieldName ) )
	// Wenn der Feldname als String übergeben wurde, dann nun
	// mit Upper() danach suchen um Fehler zu vermeiden.
	uFieldName := Upper(uFieldName)
	return( AScan( aArray, { |a| Upper(a[array_FIELDNAME])  == uFieldName } ) )
endif
return( 	AScan( aArray, { |a| a[array_FIELDNAME]  == uFieldName } ) )

METHOD ArrayStructureToString( aArray AS ARRAY ) AS STRING PASCAL CLASS P_Base

	LOCAL cResult := ""          AS STRING
	LOCAL aResult                AS ARRAY

aResult :=  ATreeList(aArray, "", "" , -1)
AEval(aResult, {|x| cResult += x + CRLF })
return( cResult )

METHOD ArrayToString( aArray AS ARRAY, cSeperator := ";" AS STRING ) AS STRING PASCAL CLASS P_Base
// Ein Array (kann auch Mehrdimensional sein) mit Seperator in einen String schreiben
// Beispiel:
// 		{ "(a)", "", "(c)","3" }   ---> "(a);;(c);3"
// oder ein mehrdimens. Array:
// 		{{1,2,3,"A"}, {1,3,2,"B"}} ---> "1;2;3;A
//                                       1;3;2;B"

	LOCAL cResultString := ""            AS STRING
	LOCAL x                              AS INT

//aEval( aArray, { |uValue| cResultString += iif( isNil(cbCodeblock), uValue, eVal(uValue, cbCodeBlock)) + cSeperator } )

for x:=1 upto ALen(aArray)
	if( UsualType(aArray[x]) == ARRAY )
		// Es handelt sich um ein mehdim. Array
		cResultString += SELF:ArrayToString( aArray[x], cSeperator ) + CRLF
	else
		cResultString += SELF:UsualToString(aArray[x]) + cSeperator
	endif
next x

if( !Empty(cResultString) )
	cResultString := Left( cResultString, Len(cResultString) - Len(cSeperator) )
endif
return( cResultString )

METHOD ArrayToDisk( cFileName AS STRING, aArray AS ARRAY, cDelimiter := ";" AS STRING ) AS LOGIC PASCAL CLASS P_BAse
//
// Schreibt ein ein- oder mehrdimensionales Array auf Platte
// Der Delimiter hat nur für mehrdimensionale Arrays Bedeutung
// ---------------------------------------------------------
// aArray := {"Zeile1","Zeile2",...}  					          --> Zeile1
//                                                               Zeile2
//
//           oder Mehrdimensional
//
//           { {"Zeile1-Spalte1", "Zeile1-Spalte2" },;       --> Zeile1-Spalte1;Zeile1-Spalte2
//             {"Zeile2-Spalte1", "Zeile2-Spalte2" } }	        Zeile2-Spalte1;Zeile2-Spalte2
//
// Bsp: 		 oBase:ArrayToDisk( "\\pc-fin\temp\arraysave.txt", { {1, "Zeile1", today() }, {2,"Zeile2", today()}, {3, "" , today() } } )
//

	LOCAL cString  := ""               AS STRING
	LOCAL x                            AS INT

SELF:ResetLocalError( #BaseArrayToDisk )
if( SELF:FileExists( cFileName ) )
	// Wenn Datei vorhanden, dann löschen
	SELF:FileDelete( cFileName )
endif

for x:=1 upto ALen(aArray )
	if( UsualType( aArray[x] ) == ARRAY )
		//Es handelt sich um ein Array mit mehreren Dimensionen
		cString += SELF:ArrayToString(aArray[x], cDelimiter)
	else
		cString += SELF:UsualToString( aArray[x] )
	endif

	if( x!=ALen(aArray) )
		cString += CRLF
	endif
next x
SELF:FileWriteFromString( cFileName, cString )
return( !SELF:IsLocalError( #BaseArrayToDisk ) )

METHOD FileDeleteIfExists( cFileName AS STRING ) AS LOGIC PASCAL CLASS P_Base
// Löscht die Datei <cFileName>, wenn sie existiert.
	LOCAL lExists := FALSE       AS LOGIC
if( lExists := SELF:FileExists( cFileName ) )
	SELF:FileDelete( cFileNAme )
endif
return( lExists )

METHOD ArrayRecordFromSql( cTableName AS STRING, aColumns := nil REF ARRAY, aDefinition := nil AS ARRAY, cWhere := "" AS STRING, cOrderBy := "" AS STRING ) AS ARRAY PASCAL CLASS P_Base
// Gibt ein Array im Record-Format {{ #NAME, "Inhalt" }, { #NAME, "Inhalt" }} für nur einen Satz zurück.
// Sie hierzu ArrayFromSql mit identischen Parametern.

   LOCAL aFields       AS ARRAY
   LOCAL aReturnRecord AS ARRAY
   LOCAL x             AS INT

aReturnRecord := {}
aFields := SELF:ArrayFromSql( cTablename, @aColumns, aDefinition , cWhere, cOrderBy )
if( !Empty( aFields ) )
	if( ALen(aFields) != ALen(aColumns) )
		SELF:MessageFormat( "Die Feldspaltenanzahl von # passt nicht zu den Feldern in der Anzahl von #. ArrayRecordFromSql auf Tabelle #", { ALen(aColumns), ALen(aFields), cTableName }, PROT_ART_ERROR, TRUE )
	else
		for x:=1 upto ALen(aColumns)
			AAdd( aReturnRecord, { aColumns[x], aFields[x] } )
		next x
	endif
endif
return( aReturnRecord )

METHOD ArrayFromSqlStatement( cStatement AS STRING, aColumns := nil REF ARRAY ) AS ARRAY PASCAL CLASS P_Base
// Es wird ein mehrdim. Array mit den Inhalten zurückgegeben. Wurde [aColumns] nicht übergeben,
// so werden die Spaltennamen als Array von SYMBOL zurückgegeben
// [aColumns] REF-Return:  { #Spalte1, #Spalte2, #Spalte3 }
// Return: {
//         { "Inhalt", 10, 30 },
//         { "Inhalt2", 12, 10 },
//         }
//
	LOCAL oStatement               AS ASqlStatement
	LOCAL aArray                   AS ARRAY
	LOCAL aRowTemp                 AS ARRAY

aArray := {}
oStatement := SELF:oTransactionManager:CreateStmt(cStatement)
if( oStatement:Prepare() .and. oStatement:ExecuteReader() )
	do while (oStatement:Fetch())
		// Da die Spaltennamen bereits in aColumns stecken
		// (reingegeben oder von GetSqlRecord erhalten)
		// werden die Zeilen nur noch  mit den Inhalten benötigt
		aRowTemp := SELF:GetSqlRecord( oStatement, aColumns )
		aEVal( aRowTemp, { |a| aadd( aArray, a[2] ) } )
		aadd( aArray, aRowTemp )
	enddo
else
	SELF:MessageFormat( "Fehler beim Ausführen des Statements <#>. # ", { cStatement, oStatement:Status:GetMessage() }, PROT_ART_ERROR, TRUE )
endif
oStatement:Release()
return( aArray )

METHOD ArrayFromSql( cTableName AS STRING, aColumns := nil REF ARRAY, aDefinition := nil AS ARRAY, cWhere := "" AS STRING, cOrderBy := "" AS STRING ) AS ARRAY PASCAL CLASS P_Base
// Nimmt die Inhalte einer Sql-Tabellen und gibt ein komplettes Array zurück
//
// cTableName         : Name der Sql-Tabelle
// aColumns           : Ein Array mit den Spalten der Sql-Tabelle im Format STRING oder SYMBOL
//                      { "Zeile", "Geburtstag" }
//                      Sollte schon angegeben werden
// aDefinition        : Ein Array mit Definitionen, in welchem Format die Sql-Felder im Array abgelegt werden sollen. Ist aDefinition nicht
//                      angegeben, so werden alle WErte als String abgelegt.
//                      { "STRING", "DATE" }
// cWhere             : Die Where-Bedingung zur Eingrezung der Zeilen. "ZEILE <= 3"
// cOrderBy           : Sortierreihenfolge beim lesen aus der Tabelle "ZEILE, GEBURTSTAG desc"
//
//	Bsp:	aAppendedRecs := oBase:SqlToArray( "FIN", { "Zeile", "Geburtstag" }, { "INT", "DATE" }, "ZEILE <= 3" )

	LOCAL oStatement                      AS ASqlStatement
	LOCAL cStatement  := ""               AS STRING
	LOCAL nCol                            AS INT
	LOCAL aCol                            AS ARRAY
	LOCAL aArray                          AS ARRAY

aArray := {}

cStatement := "SELECT * FROM "+cTableName
if( !Empty( cWhere ) )
	cStatement += " WHERE "+cWhere
endif
if( !Empty( cOrderBy ) )
	cStatement += " ORDER BY "+cOrderBy
endif

oStatement := SELF:oTransactionManager:CreateStmt(cStatement)
IF oStatement:Prepare()                .AND.;
	oStatement:ExecuteReader()

	do while (oStatement:Fetch())

		// Falls es ein oProgress gibt, nun versorgen.
		if( SELF:oProgress != NULL_OBJECT )
			SELF:oProgress:Increment()
		endif

		aCol := SELF:GetSqlRecord( oStatement, @aColumns )

		// Nur die Inhalte, nicht die Spaltennamen und
		// Inhalte als String
		aArray := {}
		for nCol := 1 upto ALen( aCol )
			AAdd( aArray, SELF:UsualToString( aCol[nCol,2] ) )
		next nCol
	enddo

else
	SELF:MessageFormat( "Fehler beim Ausführen des Statements <#>. # ", { cStatement, oStatement:Status:GetMessage() }, PROT_ART_ERROR, TRUE )
endif
oStatement:Release()

aArray := SELF:ArrayKonvert( aArray, aDefinition )

return( aArray )

METHOD GetSqlRecord( oStatement AS ASqlStatement, aColumns := nil REF ARRAY ) AS ARRAY PASCAL CLASS P_Base
// Holt ein Array-Record aus einem SQL-Statment im Format {{#NAME, "Inhalt"}, {#NAME, "Inhalt"}} raus
//
// oStatement : Ein ASqlStatement, welches schon im Status oStatement:Fetch() sein muss
// aColumns   : Ein Array { #NAME, #NAME ... } mit Feldnamen (STRING oder SYMBOL) für die Inhalte die zurückgegeben werden sollen
//              Wird aColumns nicht angegeben, so werden alle Felder (Spaltennamen as SYMBOL) mit ihren Inhalten zurückgegeben.
//
	LOCAL aReturnRecord  AS ARRAY
	LOCAL nCol           AS INT
	LOCAL symField       AS SYMBOL
	LOCAL aFieldInfo     AS ARRAY

aReturnRecord := {}
if( IsNil( aColumns ) )
	aColumns := {}
	for nCol := 1 upto oStatement:FCount
		aFieldInfo := oStatement:GetFieldInfo(nCol)
		AAdd( aReturnRecord, { aFieldInfo[FI_NAME], oStatement:Fget(nCol) } )
		AAdd( aColumns, aFieldInfo[FI_NAME] )
	next nCol
else
	for nCol := 1 upto ALen(aColumns)
		if( UsualType(aColumns[nCol]) == SYMBOL )
			symField := aColumns[nCol]
		else
			symField := String2Symbol( aColumns[nCol] )
		endif
		AAdd( aReturnRecord, { aColumns[nCol], oStatement:FgetN( symField ) } )
	next nCol
endif

return( aReturnRecord )


METHOD ArrayToSql( cTableName AS STRING, aInputArray AS ARRAY, aColumnNames := nil AS ARRAY, aDefinition := nil as ARRAY ) AS VOID PASCAL CLASS P_Base
//
// Legt auf dem Sql-Server eine Tabelle an und schreibt das Array dort rein
// cTableName         : "FIN"
// aInputArray        : Das ein- oder mehrdimensionale Array
// aColumnNames       : Ein Array mit Spaltennamen (Optional). Wenn nicht angegeben werden Spalten im Format Column1, Column2... angelegt
//                      { "Zeile", "Name", "Ort", "Datum" }
// aDefinition        : Ist <aDefinition> nicht angegeben, so werden alle Felder auf dem Sql-Server als varChar(254) angelegt
//                      Anderfalls werden die Spalten mit Sql-Formaten entsprechend den aDefinition-Formaten angelegt
//                      { "INT", "STRING", "STRING", "DATE" }



	LOCAL oCreateTable                AS ASqlStatement
	LOCAL cColumns := ""              AS STRING
	LOCAL cValues := ""               AS STRING
	LOCAL nRow, nCol                  AS INT
	LOCAL aTempDefinition             AS ARRAY
	LOCAL oInsert                     AS ASQLStatement
	LOCAL aArray                      AS ARRAY

//
// Wenn es sich um ein eindimensionales Array handelt,
// dann jetzt in ein mehrdimensionales Array wandeln
//
aArray := {}
if( ALen( aInputArray ) > 0 .and. UsualType(aInputArray[1]) != ARRAY )
	// Das Array ist eindimensional. Nun mehrdimensional machen,
	// damit man besser damit arbeiten kann
	for nRow := 1 upto ALen( aInputArray )
		AAdd( aArray, { aInputArray[nRow] } )
	next nRow
else
	aArray := aInputArray
endif

//
// Wenn keine Definitionen angegeben sind,
// dann jetzt alles mit STRING initialisieren
//
if( IsNil(aDefinition) )
	aDefinition := {}
	if( ALen(aArray) != 0 )
		for nCol := 1 upto ALen(aArray[1])
			AAdd( aDefinition, "STRING" )
		next nCol
	endif
endif

//
// Wenn keine Spalten-Namen angegeben sind,
// dann jetzt welche ausdenken
//
if( IsNil(aColumnNames) )
	// Es ist keine Definition vorganden. Alle Elemente werden mit Name Column1, Column2 etc.
	// und Typ varchar(255) angelegt. Hier die Definition füllen
	aColumnNames := {}
	if( ALen(aArray) != 0 )
		for nCol:=1 to ALen(aArray[1])
			AAdd( aColumnNames, { "Column"+NTrim(nROw) } )
		next nRow
	endif
endif

//
// Aus den <aDefinition> nun ein SQL-Kompatibles aTempDefinition aufbauen
//
// STRING		: varchar(255)
// INT         :
// DATE        :
aTempDefinition := {}
for nCol :=1 upto ALen(aDefinition)
	//                             NAME              STRING                    varchar(254)
	// -------------------------------------------------------------------------------------------------------
	AAdd( aTempDefinition, { aColumnNames[nCol], aDefinition[nCol], SELF:VOTypeToSqlType( aDefinition[nCol] ) })
next nCol

//
// Tabellen-Spalten zusammenbauen
//
for nCol:=1 upto ALen(aTempDefinition)
	//                    [NAME]                            varchar(254)
	cColumns += "["+aTempDefinition[nCol][1] + "] " + aTempDefinition[nCol][3]
	if( nCol!= ALen(aTempDefinition) )
		cColumns += ", "
	endif
next nCol

//
// Tabelle erstellen
//
SELF:dbg( "CREATE TABLE "+cTableName+" ("+cColumns+")" )
oCreateTable := SELF:oTransactionManager:CreateStmt("CREATE TABLE "+cTableName+" ("+cColumns+")")
IF oCreateTable:Prepare()                .AND.;
	oCreateTable:ExecuteBatch()
ELSE
	SELF:MessageFormat( "Fehler beim erstellen der Tabelle # mit den Spalten #. #", { cTableName, cColumns, oCreateTable:Status:GetMessage() }, PROT_ART_ERROR, TRUE )
ENDIF
oCreateTable:Release()

//
// Tabellenspalten aus dem Array einfügen
//
cColumns := ""
for nCol := 1 Upto ALen(aTempDefinition)
	cColumns += "["+aTempDefinition[nCol][1]+"]"
	if( nCol < ALen(aTempDefinition ) )
		cColumns += ", "
	endif
next nCol

self:dbgarray( aArray )
for nRow:=1 upto ALen(aArray)
	// Falls es ein oProgress gibt, nun versorgen.
	if( SELF:oProgress != NULL_OBJECT )
		SELF:oProgress:Increment()
	endif

	cValues  := ""
	// Inhalte vorbereiten
	for nCol := 1 upto ALen( aArray[nRow] )
   	do case
   	case( aTempDefinition[nCol][2] == "STRING")
   		cValues += "'"+aArray[nRow][nCol]+"'"
   	case( aTempDefinition[nCol][2] == "INT" .or. aTempDefinition[nCol][2] == "LONGINT" )
   		cValues += NTrim(aArray[nRow][nCol])
   	case( aTempDefinition[nCol][2] == "REAL8" .or. aTempDefinition[nCol][2] == "FLOAT" )
   		cValues += NTrim(aArray[nRow][nCol])
		case( aTempDefinition[nCol][2] == "DATE" )
			cValues += "convert(datetime, '"+DToC(aArray[nRow][nCol])+"', 104)"
		case( aTempDefinition[nCol][2] == "LOGIC" )
			cValues += if( aArray[nRow][nCol]==TRUE, "1","0" )
		otherwise
			SELF:Messageformat( "Fehler bei Zuordnung Typ # zu Array Zeile # und Spalte #. Der Typ des Array-Elementes ist #", { aTempDefinition[nCol][2], nRow, nCol, TypeString(UsualType(aArray[nRow][nCol])) }, PROT_ART_ERROR, TRUE )
		endcase

		if( nCol < ALen(aArray[nRow]) )
			cValues += ", "
		endif
   next nCol

	SELF:dbg( "Insert into "+cTableName+" ("+ cColumns + ") Values ("+cValues+" )" )
	oInsert := SELF:oTransactionManager:CreateStmt("Insert into "+cTableName+" ("+ cColumns + ") Values ("+cValues+" )")
	IF oInsert:Prepare()
		if( !oInsert:ExecuteBatch() )
			SELF:MessageFormat( "Fehler bei ExecuteBatch(), beim schreiben in Tabelle #. #", { cTableName,  oInsert:Status:GetMessage() }, PROT_ART_ERROR, TRUE )
		endif
	ELSE
		SELF:MessageFormat( "Fehler beim einfügen der Zeile # aus dem Array in die Tabelle #", { nRow, cTableName }, PROT_ART_ERROR, TRUE )
	ENDIF
	oInsert:Release()
next nRow
//
// Wichtig! Die Tabelle ist nun erstellt um muss ggf wieder gelöscht werden
//

METHOD UsualTypeAsString( uVar AS USUAL ) AS STRING PASCAL CLASS P_Base
// Gibt den Datentype von <uVar> als String (zur Ausgabe) zurück
	LOCAL cTyp          AS STRING
do case
	case( UsualType(uVar) == ARRAY )
		cTyp := "ARRAY"
	case( UsualType(uVar) == CODEBLOCK )
		cTyp := "CODEBLOCK"
	case( UsualType(uVar) == DATE )
		cTyp := "DATE"
	case( UsualType(uVar) == FLOAT )
		cTyp := "REAL"
	case( UsualType(uVar) == LOGIC )
		cTyp := "LOGIC"
	case( UsualType(uVar) == LONGINT	)
		cTyp := "INT"
	case( UsualType(uVar) == OBJECT )
		if( uVar == NULL_OBJECT )
			cTyp := "NULL_OBJECT"
		else
			cTyp := "OBJECT"
		endif
	case( UsualType(uVar) == VOID )
		cTyp := "NIL"
	case( UsualType(uVar) == PTR )
		cTyp := "PTR"
	case( UsualType(uVar) == STRING )
		cTyp := "STRING"
	case( UsualType(uVar) == SYMBOL )
		cTyp := "SYMBOL"
	case( isNil( uVar ) )
		cTyp := "NIL"
	case( uVar == NULL_OBJECT )
		cTyp := "NULL_OBJECT"
	otherwise
		cTyp := "UNGÜLTIGER DATENTYP="+ValType(uVar)
endcase
return( cTyp )

METHOD VOTypeToSqlType( cVOType AS STRING ) AS STRING PASCAL CLASS P_Base
//
// Gibt zu einem VO-Typen den entsprechenden SQL-Typen als String zurück
// Bsp: VoTypeToSqlType("LOGIC") --> "tinyint"
//
	LOCAL cSqlType := ""    AS STRING

cVOType := Upper( AllTrim( cVOType ) )
do case
case( cVOType == "STRING" )
	cSqlType := "varchar(254)"
case( cVOType == "INT" )
	cSqlType := "int"
case( cVOType == "LOGIC" )
	cSqlType := "tinyint"
case( cVOType == "MEMO" )
	cSqlType := "text"
case( cVOType == "DATE" )
	cSqlType := "datetime"
otherwise
	cSqlType := "varchar(254)"
endcase

return( cSqlType )

METHOD ArrayProtocolChanges( aValues AS ARRAY, oRecordOrServer AS USUAL, lNoDoku := FALSE AS LOGIC ) AS ARRAY PASCAL CLASS P_Base      // aLen() = 0 --> keine Änderungen vorhanden
// Änderungen an Werten protokollieren.
// Das Array aValues muss dafür einen bestimmten aufbau haben:
//
// aValues [1] symbol   Feldname				#BEZEICH
//         [2] String   Bezeichnung       "Hallo und so"
//         [3] usual    Neuer WErt        "Neu", oder 10, oder 12.12.1014
//         [4] usual    Alter WErt        Optional. Wird wenn nicht vorhanden aus oRecord gelesen, bzw. mit Neuwert besetzt
//
// oRecordOrServer : Ein AServer, AReadRecord order AWriteRecord-Object mit den Tabellenwerten (alt)
//                   Ist oRecordOrServer angegeben, und kann beschrieben werden, so werden die Werte auch zurückgeschrieben
//
// lNoDoku Bedeutet, das keine Änderungen dokumentiert werden sollen, sondern einfach die NeuWerte aus dem Array
//         in den Record zurückgeschrieben werden sollen. Dies ist beim Neuanlegen nötig, da noch keine Änderungen vorhanden
//
// Beispiel:
//              { {#BDE_NR,         "Bde-Nr",                    100339},;
//    				{#BEZEICH,        "Bezeichnung"                "Holundersirup"},;
//    				{#PLAUTZKI,       "Platzki",                   "Hallo", "Holla"},;
//    				{#START_GES,      "Gesamtstart",               CtoD("10.10.2012")},;
//    				{#P_KUN_BEI,      "Kundenbeistellung,          TRUE} }
//
// Rückgabe:
// 				Ein Array im gleichen Format wie aValues, mit dem zusätzlichen Element, ob Änderungen in den Record zurückgeschrieben wurden.
//             Es werden jedoch nur die Elemente mit Änderungen zurückgegeben
//             ist aLen(Rückgabe) = 0, dann sind keine Änderungen vorhanden
//
// 				aValues 	[1] symbol   Feldname				#BEZEICH
//         					[2] String   Bezeichnung       "Hallo und so"
//         					[3] usual    Neuer WErt        "Neu", oder 10, oder 12.12.1014
//         					[4] usual    Alter WErt        Optional. Wird wenn nicht vorhanden aus oRecordgelesen, bzw. mit Neuwert besetzt
//                      [5] logic    true/false        Wenn der neue Wert in den Record geschrieben wurde, dann true, anderfalls false
//

	LOCAL nRow                          AS INT
	LOCAl uValue                        AS USUAL
	LOCAL cMeldung := ""                AS STRING
	LOCAL aReturnValues                 AS ARRAY
	LOCAL lReadOnly := TRUE             AS LOGIC
	// Werden für Konvertierungen benötigt
	LOCAL oRecord                       AS AWriteRecord
	LOCAL oRecRead                      AS AReadRecord
	LOCAL oServer                       AS AServer

oRecord := NULL_OBJECT
if( oRecordOrServer != NULL_OBJECT )
	do case
	case( IsInstanceOfUsual(oRecordOrServer, #AWriteRecord ) )
		oRecord:=oRecordOrServer
		lReadOnly := FALSE
	case( IsInstanceOfUsual(oRecordOrServer, #AReadRecord ) )
		oRecRead := oRecordOrServer
		oRecRead:ExportToWriteRecord( oRecord )
		lReadOnly := TRUE
		oRecRead:Release()
	case( IsInstanceOfUsual(oRecordOrServer, #AServer ) )
		oServer := oRecordOrServer
		oRecord := AWriteRecord{oServer, TRUE}
		lReadOnly := .not. (oServer:InAppendMode .or. oServer:InUpdateMode)
	endcase
endif

aReturnValues := {}
for nRow:=1 upto ALen(aValues)
	if( ALen(aValues[nRow]) ) < 4
		// Es sind nur drei Spalten angegeben. Nun für den "alten WErt"
		// eine vierte Spalte dazubauen und auf oRecord holen
		uValue := aValues[nRow][3] // Erst mal mit dem Neuwert besetzten
		if( oRecord != NULL_OBJECT .and.  !IsNil( oRecord:Fget(aValues[nRow][1]) ) )
			uValue := oRecord:Fget(aValues[nRow][1])
		endif
		AAdd( aValues[nRow], uValue )
	endif

	do case
	case( UsualType(aValues[nRow][3]) == STRING )
		cMeldung := "Der Text"
	case( UsualType(aValues[nRow][3]) == DATE )
		cMeldung := "Das Datum"
	case( IsNumeric(aValues[nRow][3]) )
		cMeldung := "Der Wert"
	case( UsualType(aValues[nRow][3]) == LOGIC )
		cMeldung := "Der Schalter"
	case( UsualType(aValues[nRow][3]) == SYMBOL )
		cMeldung := "Das Symbol"
	otherwise
		cMeldung := "Der Wert"
	endcase
   cMeldung += " des Feldes <#> hat sich von <#> in <#> geändert."

	if( aValues[nRow][3] != aValues[nRow][4] )
		// Neuer Wert ist anders als alter Wert
		AAdd( aReturnValues, { aValues[nRow][1],aValues[nRow][2],aValues[nRow][3],aValues[nRow][4],FALSE } )

		if( oRecord != NULL_OBJECT )
			if( IsNil(oRecord:Fget(aValues[nRow][1])) )
				SELF:MessageFormat( cMeldung + "Das Feld # ist jedoch nicht im Record vorhanden", { aValues[nRow][2], aValues[nRow][4], aValues[nRow][3], aValues[nRow][1] }, PROT_ART_ERROR, TRUE )
			else
				// Wenn ein Record angegeben ist und der Wert gefunden wird,
				// dann jetzt in den Record zurück-schreiben
				if( !lReadOnly )
					oRecord:FPut(aValues[nRow][1], aValues[nRow][3] )
					aReturnValues[ALen(aReturnValues)][5] := TRUE
   			endif

				if( !lNoDoku )
					SELF:MessageFormat( cMeldung , { aValues[nRow][2], aValues[nRow][4], aValues[nRow][3]}, PROT_ART_INFORMATION )
				endif
          endif
		endif
   endif
next nRow

if( ALen(aReturnValues) != 0 .and. !lReadOnly )
	if( IsInstanceOfUsual(oRecordOrServer, #AServer )	)
		// Wenn <oRecordOrServer> == #AServer, dann jetzt das aufgebaute
		// Record <oRecord> auf den Server zurückschreiben
		oRecord:ExportToServer( oServer )
	endif
endif

if( oRecord != NULL_OBJECT .and. !IsInstanceOfUsual(oRecordOrServer, #AWriteRecord ))
	oRecord:Release()
endif
return( aReturnValues )

METHOD ArrayFromRecord( oRecordOrServer AS USUAL, aFields := nil AS ARRAY, aRename := nil AS ARRAY ) AS ARRAY PASCAL CLASS P_Base
//
// Liest einen Record aus und schreibt die Felder und Inhalte in ein Array und gibt dies zurück
// Ist aFields nicht angegeben, werden alle Felder zurückgegeben.
//
// aFields : Optional : { #FELDNAME, #FELDNAME, ... }
// aRename : Optional : {{ #FELDNAME, #WIRDFELDNAME},...

	LOCAL aValues                 AS ARRAY
	LOCAL x                       AS INT
	LOCAL symFieldName            AS SYMBOL
	LOCAl oServer                 AS AServer
	LOCAL oRecord                 AS AReadRecord

aValues := {}
if( IsInstanceOfUsual( oRecordOrServer, #AWriteRecord ) .or. IsInstanceOfUsual( oRecordOrServer, #AServer )  .or. IsInstanceOfUsual( oRecordOrServer, #AReadRecord) )
	if( IsInstanceOfUsual( oRecordOrServer, #AServer ) )
		oServer := oRecordOrServer
		oRecord := AReadRecord{oServer, TRUE}
		debugPrint( "FIN: ", __ENT, __LINE__, "AServer", oRecord:FCount)
	else
		oRecord := oRecordOrServer
	endif

	for x:=1 upto oRecord:FCount
		symFieldName := oRecord:StructInfo:GetFieldName(x)
		if( IsNil(aFields) .or. AScan( aFields, symFieldName ) != 0 )
			// Wenn aktuelles RecordFeld in <aFields> anegeben oder
			// alle Felder gewünscht sind
			AAdd( aValues, { SELF:RenameKeyField(symFieldName, aRename), oRecord:FgetPos(x) } )
		endif
	next x

	oRecord:Release()
	if( oServer != NULL_OBJECT )
		oServer:Release()
	endif
else
	SELF:MessageFormat( "ArrayFromRecord wird mit einem Parameter vom Typ # gerufen, muss aber vom Typ AWriteRecord, AReadRecord oder AServer sein", {ClassName(oRecord)}, PROT_ART_ERROR, TRUE )
endif
return( aValues )

METHOD RenameKeyField( symFieldName AS SYMBOL, aRenameList := nil AS ARRAY ) AS SYMBOL PASCAL CLASS P_Base
// Schaut, ob <symFieldName> in der aRenameList vorhanen ist und gibt ggf. die Übersetzung zurück
// aRenameList : { {#FELD,#NEUFELD},... }

	LOCAL symReturnFieldName      AS SYMBOL
	LOCAL nPos                    AS INT

symReturnFieldName := symFieldName
if( !IsNil(aRenameList) )
	if( (nPos := AScan( aRenameList, { |a| a[1] == symFieldName } )) > 0 )
		symReturnFieldName := aRenameList[nPos][2]
	endif
endif
return( symReturnFieldName )

METHOD ArrayToRecord( aFieldValue AS ARRAY, oRecordOrServer AS USUAL, lErrorIfNotExists := TRUE AS LOGIC, lOnlyNecessary := FALSE) AS LOGIC PASCAL CLASS P_Base
// Schreibt den Inhalt eines Arrays in einen vorhandenen Record (WriteRecord oder AServer) zurück.
// <oRecord> kann entweder ein AWriteRecord oder ein Append/Update-AServer sein
// Aufbau des Arrays:
// 		{ {#FELDNAME, uInhalt, lOnlyNecessary, lAddNumericValue}, {#FELDNAME, uInhalt}, ... }
//
// [lOnlyNecessary]    kann als drittes Array-Element angegeben werden. Wird der Methoden-Parameter lOnlyNecessary gesetzt, so
//                     werden nur Elemente ersetzt, welche in der dritten Dimension ein TRUE haben
// [lAddNumericValue]  Numerische Werte sollen nicht einfach ersetzt, sondern addiert werden.
// 						  Hat ein Element auf der vierten Dimension ein TRUE, so wird es addiert
//
//

	LOCAL x                    AS INT
	LOCAL oError               AS USUAL
	LOCAL oRecord              AS AWriteRecord
	LOCAL oServer              AS AServer
	LOCAL lReplace             AS LOGIC
	LOCAL uValue               AS USUAL

SELF:ResetLocalError( #BaseArrayToRecord )
if( IsInstanceOfUsual( oRecordOrServer, #AWriteRecord ) .or. IsInstanceOfUsual( oRecordOrServer, #AServer ) )

	if( IsInstanceOfUsual( oRecordOrServer, #AServer ) )
		oServer := oRecordOrServer
		oRecord := AWriteRecord{oServer, TRUE}
	else
		oRecord := oRecordOrServer
	endif

	for x:=1 upto ALen(aFieldValue)
		do case
		case( ALen(aFieldValue[x]) < 2 )
			SELF:MessageFormat( "Die <aFieldValue> in Zeile # haben eine Dimension von #, es sollte jedoch eine Dimension von 2 {{#Key, uValue}} sein. Siehe Methode ArrayToRecord()", { x, ALen(aFieldValue[x]) }, PROT_ART_ERROR, TRUE )
		case( UsualType(aFieldValue[x][1]) != SYMBOL )
			SELF:MessageFormat( "Das KeyFeld in Zeile # ist vom Typ #, es sollte jedoch vom Typ SYMBOL sein. Siehe Methode ArrayToRecord()", { x, SELF:UsualTypeAsString(aFieldValue[x]) }, PROT_ART_ERROR, TRUE )
		case( IsNil( oRecord:FGet(aFieldValue[x][1] ) ) )
			// Feld ist nicht in der Struktur
			if( lErrorIfNotExists )
				SELF:MessageFormat( "Feld # in der Tabelle nicht vorhanden. Siehe Methode ArrayToRecord()", { aFieldValue[x][1] }, PROT_ART_ERROR, TRUE )
			endif
		otherwise
			// Alles ok, Felder nun ersetzen
			BEGIN SEQUENCE
				lReplace := !lOnlyNecessary
				if( lOnlyNecessary )
					// Es sollen nur die als Zwingend gekennzeichnetten
					// Felder ersetzt werden
					if( ALen(aFieldValue[x]) > 2 .and. aFieldValue[x][3] )
						lReplace := TRUE
					endif
            endif

				if( lReplace )
					if( UsualType(aFieldValue[x][2]) == SYMBOL )
						// Das Valuefeld ist ein SYMBOL. Hier soll der Inhalt des referenzierten
						// Feldes in das aktuelle Feld übernommen werden
						// Beispiel: { #MENGE_GES, #MENGE_EH } : Es wird das Feld #MENGE_GES aus dem Feld MENGE_EH versorgt
						uValue := oRecord:FGet( aFieldValue[x][2] )
					elseif( ALen(aFieldValue[x]) > 3 .and. aFieldValue[x][4] .and. IsNumeric(oRecord:FGet( aFieldValue[x][1] )) )
						// Numerische Felder sollen nicht einfach ersetzt,
						// sondern sollen zum vorhandenen Wert addiert werden
						if( IsNumeric( oRecord:FGet( aFieldValue[x][1] ) ) .and. IsNumeric( aFieldValue[x][2] ) )
							uValue := oRecord:FGet( aFieldValue[x][1] ) + aFieldValue[x][2]
						else
							SELF:MessageFormat( 'Fehler beim addieren auf Feld "#". Zum Inhalt "#" (Type=#) soll "#" (Type=#) addiert werden. Einer der beiden Werte ist nicht numerisch. Es wird eine Ersetzung durchgeführt.', { aFieldValue[x][1], oRecord:FGet(aFieldValue[x][1]), SELF:UsualTypeAsString(oRecord:FGet(aFieldValue[x][1])), aFieldValue[x][2], SELF:UsualTypeAsString(aFieldValue[x][2]) }, PROT_ART_ERROR, TRUE )
						endif
					else
						uValue := aFieldValue[x][2]
					endif
					// Typen-Test
					if( UsualType(uValue) <> UsualType( oRecord:Fget(aFieldValue[x][1]) ) )
						if(  IsNumeric( oRecord:Fget(aFieldValue[x][1]) ) .and. IsNumeric( uValue ) )
							// Hier braucht nichts getan werden. Da findet eine automatische Typenkonvertierung statt
						else
							SELF:MessageFormat( "Warnung beim schreiben auf das Feld # vom Typ # mit dem Typ:  #.", { aFieldValue[x][1], SELF:UsualTypeAsString(oRecord:Fget(aFieldValue[x][1])), SELF:UsualTypeAsString( uValue ) }, PROT_ART_WARNING, TRUE )
					   endif
					endif

					if( !IsNil( uValue ) )
						oRecord:FPut( aFieldValue[x][1], uValue )
					else
						SELF:MessageFormat( "Fehler beim setzen eines NIL-Wertes auf das Feld # in Methode ArrayToRecord()", { aFieldValue[x][1] }, PROT_ART_ERROR, TRUE )
					endif
				endif
			RECOVER USING oError
				SELF:MessageFormat("Fehler beim schreiben des Feldes # in das #-Objekt: #", { aFieldValue[x][1], ClassName(oRecordOrServer), oError:Description }, PROT_ART_ERROR, TRUE )
			END SEQUENCE
		endcase
	next x

	if( IsInstanceOfUsual(oRecordOrServer, #AServer )	)
		// Wenn <oRecordOrServer> == #AServer, dann jetzt das aufgebaute
		// Record <oRecord> auf den Server zurückschreiben
		BEGIN SEQUENCE
			oRecord:ExportToServer( oRecordOrServer )

		RECOVER USING oError
			SELF:MessageFormat("Fehler beim ExportToServer in das #-Objekt: #", { ClassName(oRecordOrServer), oError:Description }, PROT_ART_ERROR, TRUE )
		END SEQUENCE

		oRecord:Release()
	endif
else
	SELF:MessageFormat( "Fehler bei ArrayToRecord(). Die Übergabe in <oRecordOrServer> sollte vom Type AWriteRecord, oder ein offener AServer (Append-/Updatemodus) sein, ist jedoch vom Typ #.", {ClassName(oRecordOrServer)}, PROT_ART_ERROR, TRUE )
endif
return( !SELF:IsLocalError( #BaseArrayToRecord ) )

METHOD ArrayKonvert( aArray AS ARRAY, aDefines AS ARRAY ) AS ARRAY PASCAL CLASS P_Base
//
// Konvertiert das Array mit den Spalten in die vorgegebenen Formate
// Erlaubt sind "STRING", "INT", "LOGIC", "DATE", "REAL8", "FLOAT"
//
return( SELF:ArrayValidate( aArray, aDefines, nil, TRUE ) )

METHOD ArrayChangeDimension( aArray AS ARRAY, nToDimension AS REAL8 ) AS ARRAY PASCAL CLASS P_Base
	//
	// Konvertierung eines eindimensionalen Arrays im Format { a,b,c,d,e,f,g,h,i,j ...
	// in ein mehrdimensionales Array.
	// <nToDimension> : 2 = {  {a,b}, {c,d}, {e,f}, ... }
	//                : 4 = {  {a,b,c,d}, {e,f,g,h}, ... }

	LOCAL aReturnArray       AS ARRAY
   LOCAL aSubArray          AS ARRAY
   LOCAL x                  AS INT

aReturnArray := {}
if( SELF:HasDecimals(ALen(aArray) / nToDimension ))
	// die Anzahl der Arrayelemente kann nicht grade aufgeteilt werden
	SELF:MessageFormat("Fehler in ArrayChangeDimension() für Dimension #. Array-Elemente können nicht aufgeteilt werden. Array = #", { nToDimension, SELF:ArrayToString( aArray, "," ) }, PROT_ART_ERROR, TRUE )
else
	aSubArray := {}
	for x:=1 upto ALen( aArray )
		AAdd( aSubArray, aArray[x] )

		if( nToDimension <= ALen(aSubArray) )
			AAdd( aReturnArray, AClone(aSubArray) )
			aSubArray := {}
		endif
	next x
endif
return (aReturnArray)

METHOD ArrayValidate( aArray AS ARRAY, aDefines AS ARRAY, aValidates := nil AS ARRAY, lKonvertValues := FALSE AS LOGIC ) AS ARRAY PASCAL CLASS P_Base
// Es wird das gesamte Array auf die Definition hin überprüft.
// Jede Zeile und jede Spalte muss mit der Definition übereinstimmen, sonst werden
// Fehler ausgegeben
//
// aDefines     : { "INT","STRING,"DATE" }
//                Spalte1 vom Typ Integer, Spalte2 vom Typ String etc.
//
// aValidates   : { { |x| x!= 0 }, { |x| !Empty(x) }, nil }
//                Für die Spalten werden die entsprechenden Validierungen ausgeführt
//                und ggf. mit Fehlermeldungen ausgegeben
//
// Bsp: 		      oBase:ArrayValidate( aAppendedRecs, { "INT", "STRING", "DATE" }, { nil, { |x| !Empty(x) }, nil } )
//

	LOCAL nRow, nCol                 AS INT
	LOCAL lError := FALSE            AS LOGIC

if( aDefines != nil )
	for nRow:=1 upto ALen(aArray)
		if( ALen(aArray[nRow]) != ALen(aDefines) )
			SELF:MessageFormat( "Fehler in Zeile # des Arrays bei Validierung. Die Anzahl Spalten (#) im Array passt nicht zur Definition (#)", { nRow, ALen(aArray[nRow]), ALen(aDefines) }, PROT_ART_ERROR, TRUE )
			lError := TRUE
		else
			for nCol:=1 upto ALen(aDefines)
				//
				// Spalten nun auf Typ prüfen und ggf. Meldung machen
				//
				if( TypeString(UsualType(aArray[nRow][nCol])) != Upper(AllTrim(aDefines[nCol])) )
					if( lKonvertValues )
						// Array-Elemente sollen ggf. Konvertiert werden
						// Wird machen hier den Umweg über STRING
						aArray[nRow][nCol] := SELF:StringToUsual( SELF:UsualToString( aArray[nRow][nCol] ), aDefines[nCol] )
					else
						SELF:MessageFormat( "Fehler in Zeile # / Spalte # hat im Array den Typ #, sollte aber per Definition vom Typ # sein.", { nRow, nCol, TypeString(UsualType(aArray[nRow][nCol])), aDefines[nCol] }, PROT_ART_ERROR, TRUE )
						lError := TRUE
					endif
				else
					// Validierungen ok?
					if( !IsNil(aValidates) )
						if( IsCodeBlock(aValidates[nCol]) )
							if( !Eval( aValidates[nCol], aArray[nRow][nCol] )	)
								SELF:MessageFormat( "Fehler bei Validierung in Zeile # / Spalte #: Die Validierung # ergab False", { nRow,nCol, aValidates[nCol] } )
								lError := TRUE
							endif
						endif
					endif
				endif
	      next nCol
		endif
	next x
endif
return( aArray )

METHOD ArrayCombine( aFirstArray AS ARRAY, aSecondArray AS ARRAY ) AS ARRAY PASCAL CLASS P_Base
// Fügt das <aSecondArray> an das <aFirstArray> an und gibt das kombinierte Array zurück
//	LOCAL x     AS INT
AEval( aSecondArray,  { |x| AAdd(aFirstArray,x) }	 )
return( aFirstArray )

METHOD ArrayFromDisk( cFileName AS STRING, cDelimiter := "" AS STRING, lReadEmptyLines := TRUE AS LOGIC, aDefines := nil AS ARRAY ) AS ARRAY PASCAL CLASS P_Base
//
// !!! Achtung !!!
// Jede Zeile (bei mehrdimensionalen Arrays: Spalte) wird als String zurückgegeben, es
// sei denn, es wird ein Format durch <aDefines> erzwungen.
//
// Liest ein ein- oder mehrdimensionales Array von der Platte
// Der Delimiter sollte nur angegeben werden, wenn sich in den Zeilen noch Spalten befinden
// -----------------------------------------------------------------------------------------
//
// Zeile1									--> aArray := {"Zeile1","Zeile2",...}
// Zeile2
//
//           oder Mehrdimensional
//
// Zeile1-Spalte1;Zeile1-Spalte2    -->      { {"Zeile1-Spalte1", "Zeile1-Spalte2" },;
// Zeile2-Spalte1;Zeile2-Spalte2               {"Zeile2-Spalte1", "Zeile2-Spalte2" } }
//
// Bsp: 		aAppendedRecs := oBase:ArrayFromDisk( "\\pc-fin\temp\arraysave.txt", ";" , FALSE, { "INT", "STRING", "DATE" })

//
	LOCAL aRows, aColumns    AS ARRAY
	LOCAL X,Y                AS INT
	LOCAL aFileRows          AS ARRAY


aRows := {}
aFileRows := SELF:FileReadToArray( cFileName, lReadEmptyLines )
SELF:DbgArray( aFileRows, "aFileRows", TRUE )
if( ALen( aFileRows ) != 0 )
	if( Empty(cDelimiter) )
		/* Keine Spalten in der Zeile, daher einfach das eindimensionale Array zurückgeben */
		if( !IsNil(aDefines) .and. ALen(aDefines) != 0 )
			/* Es ist eine Konvertierung auf ein anderen Typ gewünscht */
			for x:=1 upto ALen(aFileRows)
				aFileRows[x] := Self:StringToUsual( aFileRows[x], aDefines[1] )
			next x
		endif
	else
		/* Jede Zeile hat noch Spalten (mit Trenner (cDelimiter) getrennt */
		for x:=1 upto ALen(aFileRows)
			if( !Empty( aFileRows[x] ) )
				/* Leere Zeilen bei Spaltendeefinion nicht mitlesen, egal was <lReadEmptyLines> sagt */
				aColumns := SELF:StringToArray( aFileRows[x], { cDelimiter } )
				if( !IsNil(aDefines) )
					/* Es sind Spaltendefinitionen angegeben */
					if( ALen(aDefines) <= ALen(aColumns) )
						/* Nur wenn mindestens so viele Defines wie Spalten vorhanden sind */
						/* Dann jetzt eine Konvertierung der Spalten durchführen           */
						for y := 1 upto ALen(aDefines)
							if( !IsNil(aDefines[y]) )
								aColumns[y] := SELF:StringToUsual( aColumns[y], aDefines[y] )
							endif
						next y
					endif
				endif
				AAdd( aRows, aColumns )
			endif
		next x
	endif
endif
return( aRows )

METHOD ArrayFind( aArray AS ARRAY, uSearchValue AS USUAL, cbCodeBlockConvert := nil AS USUAL ) AS LOGIC PASCAL CLASS P_Base
// Durchsucht das Array <aArray> nach dem Inhalt <uSearchValue>.
// Es werden nur Elemente geprüft, die vom selben UsualType sind wie <uSearchValue>
// <aArray>             : { "Fub", "Fin", { 1, "UBN } }
// <uSearchValue>       : "FIN"
// [cbCodeBlockConvert] : { |ArrayValue, SearchValue| Upper(AllTrim(ArrayValue)) == SearchValue }     --> Muss TRUE/FALSE ergeben
//
// Für eine einfach String-Suche s. StringFindInArray()
//
	LOCAL lValueFound := FALSE       AS LOGIC
	LOCAL lSubValueFound             AS LOGIC
	LOCAL x,y                        AS INT

cbCodeBlockConvert := SELF:PrepareCodeblock( cbCodeBlockConvert, { |target, search| search == target } )

x:=0
do while( x < ALen(aArray) .and. !lValueFound )
	x++
	//debugPrint( "FIN: ", __ENT, __LINE__, "Find", x, aLen(aArray), UsualType(aArray[x]), uSearchValue, lValueFound )

	if( UsualType( aArray[x] ) == ARRAY )
		// Es handelt sich um ein mehrdim. Array
		for y := 1 upto ALen( aArray[x] )
			// Rekursion ArrayFind()
			lSubValueFound := SELF:ArrayFind( aArray[x], uSearchValue, cbCodeBlockConvert )
			if( lSubValueFound )
				lValueFound := TRUE
				EXIT
			endif
		next y
	elseif( UsualType( uSearchValue ) == UsualType( aArray[x] ) )
		if( Eval( cbCodeBlockConvert, uSearchValue, aArray[x] ) )
			lValueFound := TRUE
		endif
	endif
enddo

return( lValueFound )

METHOD ArrayGroup( aArray AS ARRAY, nElement := 0 AS INT ) AS ARRAY PASCAL CLASS P_Base
// Gibt die Gruppen des übergebenen Arrays zurück
// <nElement> gibt bei einem mehrdimensionalen Array die zu grupperiende Dimension an
//            oder wird bei einem eindimensionalen Array mit 0 übergeben
//
// Beispiel:
// ArrayGroup( {{"FIN",1}, {"MEY",2}, {"FIN",3}}, 1 ) --> { "FIN", "MEY" }

	LOCAL aReturnArray           AS ARRAY
	LOCAL x                      AS INT

aReturnArray := {}
for x := 1 upto ALen(aArray)
	if( nElement == 0 .or. UsualType(aArray[x]) != ARRAY )
		// Eindimensionales Array
		if( AScan(aReturnArray, aArray[x]) == 0 )
			AAdd( aReturnArray, aArray[x] )
		endif
	else
		// Mehrdimensionales Array
		if( AScan(aReturnArray, aArray[x][nElement] ) == 0 )
			AAdd( aReturnArray, aArray[x][nElement] )
		endif
	endif
next x
return( aReturnArray )

PROTECT METHOD ArrayFilter( aArray AS ARRAY, cbCodeBlock AS CODEBLOCK ) AS Array PASCAL CLASS P_Base
// Filtert ein Array und gibt das gefilterte Array zurück
//
// Beispiel:
// FilterArray( {{"FIN",1}, {"MEY",2}, {"FIN",3}}, { |a| a[1]=="FIN" } ) --> {{"FIN",1}, {"FIN",3}}

	LOCAL aReturnArray AS ARRAY
	LOCAL x            AS INT

aReturnArray := {}
for x := 1 upto ALen(aArray)
	if( Eval( cbCodeBlock, aArray[x] ) )
		AAdd( aReturnArray, aArray[x] )
	endif
next x
return( aReturnArray )

METHOD StringToArray( cString AS STRING, aToken AS ARRAY, cbCodeBlock := nil AS USUAL) AS ARRAY PASCAL CLASS P_Base
// Trennt den <cString> anhand der Trenner in <aToken> und
// gibt die einzelnen Strings als Array zurück. Die Trenner werden dabei eleminiert.
// Durch den Codeblock <cbCodeblock> kann jeder Eintrag manipuliert werden
//
// Beispiel:
// StringToArray("123+#456;789+#ABC.EFG", {"+#",";" }) --> {"123","456", "", "789", "ABC.EFG" }
// StringToArray("1;2;3", {";"}, { |a| Val(a) }) --> {1,2,3}

	LOCAL aArray       AS ARRAY
	LOCAL nFound := -1 AS INT
	LOCAL nMin         AS INT
	LOCAL cNextToken   AS STRING
	LOCAL x            AS INT

cbCodeBlock := IfNil(cbCodeBlock, { |a| a })
aArray := {}
do while(nFound != 0)
	nMin       := Len(cString)
	cNextToken := ""

	// Das am nächsten liegende Token finden
	for x:=1 upto ALen(aToken)
		nFound := At( aToken[x], cString )
		if( nFound > 0 .and. nFound <= nMin )
			nMin       := nFound
			cNextToken := aToken[x]
		endif
	next x

	nFound := At( cNextToken, cString )
	if( cNextToken==""  .or. nFound = 0 ) //Empty( cNextToken ) .or. nFound = 0 )
		// Es wurde kein Trenner mehr gefunden
		if( cString != "" ) // !Empty(cString) )
			AAdd( aArray, Eval( cbCodeBlock,cString) )
		endif
		nFound := 0
	else
		AAdd( aArray, Eval( cbCodeBlock, Left(cString, nFound-1)) )
		if( nFound==Len(cString) )
			// Ausnahmesituation: Delimiter letztes Zeichen in der Zeile
			cString := Right( cString, Len(cString) - (nFound-2) - Len(cNextToken))
			if( cString == cNextToken )
				cString := ""
				AAdd( aArray, "" )
			endif
		else
			cString := Right( cString, Len(cString) - (nFound-1) - Len(cNextToken))
		endif
	endif

enddo

return( aArray )

METHOD StringToDisk( cFileName AS STRING, cString AS STRING, lAppend := FALSE AS LOGIC, lUnicode := FALSE AS LOGIC, lDosFormat := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
// Schreibt <cString> in eine Datei auf Platte. Wenn lAppend angegeben ist, dann wird an eine evtl. vorhandene Datei angehängt.
if( !lAppend .and. SELF:FileExists( cFileName ) )
	SELF:FileDelete( cFileName )
endif
return( SELF:FileWriteFromString( cFileName, cString, lUnicode, lDOSFormat ) )

METHOD ShowProtIfError(oCommConnect AS ABaseCommConnect, cMeldung AS STRING, lAuchWarnungen := TRUE AS LOGIC) AS LOGIC PASCAL CLASS P_Base
//
// Wenn im Protokoll einträge stehen, und es sich dabei um Fehler oder Warnungen handelt,
// dann eine Meldung anzeigen. Bei Informationen im Protokoll keine Info bringen
//

	LOCAL lOK := TRUE        AS LOGIC
	LOCAL oStmt              AS ASqlStatement
	LOCAL nAnzahlFehler := 0 AS INT

if( SELF:oProtocol == NULL_OBJECT )
	oCommConnect:Warn("Achtung! Es wird auf ein Protokoll zugegriffen, welches noch nicht erstellt wurde (ShowProtIfError --> SELF:oProtocol ist NULL_OBJECT). Bitte SELF:CreateProtocol() auf dem Base-Object ausführen.", 0)
else
	if( SELF:oProtocol:EntryCount > 0 )
		oStmt := SELF:CreateSQLStatement( "SELECT Count(*) AS Anzahl FROM PROD010 WHERE PROT_NR = ? and (PROT_ART = 'E' "+IIF(lAuchWarnungen, "or PROT_ART = 'W'","")+")", "Fehler bei Anzeige des Protokolls.", TRUE, {SELF:oProtocol:ID})
		if( oStmt != NULL_OBJECT )
			if( oStmt:Fetch() )
			     nAnzahlFehler := oStmt:FGetN(#Anzahl)
			endif
			oStmt:Release()
		endif

		IF( nAnzahlFehler > 0 )
			oCommConnect:Warn(cMeldung+" Es sind "+NTrim(nAnzahlFehler)+" Warnungen/Fehler aufgetreten. Bitte studieren Sie das Protokoll "+SELF:oProtocol:ID, 0)
		ENDIF
	endif
endif
RETURN lOK

METHOD ShowProt( oCommConnect AS ABaseCommConnect, cMeldung AS STRING ) AS LOGIC PASCAL CLASS P_Base
// Eine Meldung anzeigen, egal ob Fehler/Warnungen oder nur Informationen protokolliert wurden.
// Wenn jedoch Warnungen/Fehler protokolliert sind, dann ein Hinweis auf´s Protokoll geben
//

	LOCAL oStmt              AS ASqlStatement
	LOCAL nAnzahlFehler := 0 AS INT

if( SELF:oProtocol == NULL_OBJECT )
	oCommConnect:Warn("Achtung! Es wird auf ein Protokoll zugegriffen, welches noch nicht erstellt wurde (ShowProt --> SELF:oProtocol ist NULL_OBJECT). Bitte SELF:CreateProtocol() auf dem Base-Object ausführen.", 0)
else
	if( SELF:oProtocol:EntryCount > 0 )
		oStmt := SELF:CreateSQLStatement( "SELECT Count(*) AS Anzahl FROM PROD010 WHERE PROT_NR = ? and (PROT_ART = 'E' or PROT_ART = 'W')", "Fehler bei Anzeige des Protokolls.", TRUE, {SELF:oProtocol:ID})

		if( oStmt:Fetch() )
		     nAnzahlFehler := oStmt:FGetN(#Anzahl)
		endif
		oStmt:Release()
	endif
endif

if( nAnzahlFehler == 0 )
	oCommConnect:Inform(cMeldung+"- Funktion erfolgreich abgeschlossen.", 0)
else
	oCommConnect:Warn(cMeldung+"- Es sind "+NTrim(nAnzahlFehler)+" Warnungen/Fehler aufgetreten. Bitte studieren Sie das Protokoll "+SELF:oProtocol:ID, 0)
endif

return( nAnzahlFehler == 0 )

METHOD MessageToStack( oMsgStack AS AStatusStack, cProtArt := "" AS STRING ) AS VOID PASCAL CLASS P_Base
// Übergibt die angefallenen Fehlermeldungen an einen Messagestack
	LOCAL aError           AS ARRAY
	LOCAL X                AS INT

if( oMsgStack != NULL_OBJECT )
	aError := SELF:GetMessageArray( cProtArt )
	for x:=1 upto ALen(aError)
		AddStatusRecord(oMsgStack, SUBSYSTEM_AMS,, SELF:ProtArtToSeverityStatus( aError[x][3] ), , aError[x][2])
	next x
endif

METHOD MessageToPRCStatus( _psStatus AS _AprcStatus, cProtArt := "" ) AS VOID PASCAL CLASS P_Base

	LOCAL aError           AS ARRAY
	LOCAL X                AS INT

aError := SELF:GetMessageArray( cProtArt )
for x:=1 upto ALen(aError)
	FillPrcStatus(_psStatus, prcStatusInformation, prcMsgUserRequest, aError[x][2])
next x
//	FillPrcStatus(_psStatus, prcStatusInformation, prcMsgUserRequest, GetText(TXT_AUFTRAG_NICHT_ZUR_BEARBEITUNG_FREIGEGEBEN_STATUS_UNGLEICH_10))


METHOD MessageOutput( uObject AS USUAL, cProtArt := "" ) AS VOID PASCAL CLASS P_Base
// Gibt Nachrichten an das Empfänger-Object, wenn welche vorhanden sind
// Ausgabe auf dem MessageStack : uObject = AStatusStack
// Ausgabe per FillPrcStatus    : uObject =
// Ausgabe im Protokoll         : uObject = AMSProtocol
// Ausgabe per _oCommConnect    : uObject = ABaseCommConnect
//
// cProtArt : "" = Alle, "E" = Error, "W" = Warnungen, "I" = Information, oder eine Kombination z.B. "IW" für Warnung und Information

debugPrint( "FIN: (MessageOutput (1)) ", __ENT, __LINE__, SELF:UsualTypeAsString( uObject ), UsualType( uObject ) )

do case
case( IsInstanceOfUsual(uObject, #AMSProtocol ) )
	SELF:MessageToProtocol( uObject, cProtArt )

case( IsInstanceOfUsual(uObject, #AStatusStack ) )
	SELF:MessageToStack( uObject, cProtArt )

case( IsInstanceOfUsual(uObject, #ABaseCommConnect ) )
	SELF:ShowMessage( uObject, "Es sind Fehler/Warnungen aufgetreten", cProtArt )

case( UsualType( uObject ) == PTR )
	SELF:MessageToPrcStatus( uObject, cProtArt )

otherwise
	SELF:MessageFormat( "Fehler bei Übergabe der aufgelaufenen Nachrichten an das Object von Typ #. Kein bekanntes Ausgabeobject", { IIF( UsualType(uObject) == OBJECT, ClassName( uObject ), SELF:UsualTypeAsString(uObject)) }, PROT_ART_ERROR )
endcase


METHOD MessageToProtocol( oProtocol AS AMSProtocol, cProtArt := "" AS STRING ) AS VOID PASCAL CLASS P_Base
// Übergibt die angefallenen Fehlermeldungen an ein bestehendes Protocol-Object
	LOCAL aError           AS ARRAY
	LOCAL X                AS INT

if( oProtocol != NULL_OBJECT )
	aError := SELF:GetMessageArray( cProtArt )
	for x:=1 upto ALen(aError)
	   oProtocol:Insert( aError[x][1], aError[x][2], aError[x][3] )
	next x
endif

METHOD ProtArtToSeverityStatus( cProtArt AS STRING ) AS USUAL PASCAL CLASS P_Base
// Wandelt eine Protokoll-Art (E,W,I) in einen Severity-Status
do case
case( cProtArt == PROT_ART_INFORMATION )
	return( SEVERITY_INFORMATION )
case( cProtArt == PROT_ART_WARNING )
	return( SEVERITY_WARNING )
endcase
return( SEVERITY_ERROR )

METHOD KeyToString( aKey AS ARRAY, cbCodeBlock := nil AS USUAL ) AS STRING PASCAL CLASS P_Base
// Gibt die Key/Value-Kombinationen als String zurück
// Default: Key=Value, Key=Value
//
// <aKey>      : {{#KEY, Value},{#KEY, Value}...
// cbCodeBlock	:  { |key, value, keyString, valueString, firstEntry, lastEntry| }

	LOCAL cKey := ""           AS STRING
	LOCAL x                    AS INT

cbCodeBlock := SELF:PrepareCodeblock( cbCodeBlock, { |key, value, keyString, valueString, firstEntry, lastEntry| keyString + "=" + valueString + IIF(lastEntry,"",", ") } )

for x:=1 upto ALen(aKey)
	cKey += Eval( cbCodeblock, aKey[x][1], aKey[x][2], Symbol2String(aKey[x][1]), SELF:UsualToString(aKey[x][2]), x==1, x==ALen(aKey) )
next x
return( cKey )

METHOD IsInConfigList( cValue AS STRING, cBereich AS STRING, cParamName AS STRING, lConvertToUpperCase := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
// Liest aus einer Array-Konfiguration mit <cBereich> und <cParamName> aus und prüft, ob <cValue> in dieser Liste enthalten ist
//
	LOCAL aLIste                AS ARRAY
aListe := SELF:GetConfigArray( cBereich, cParamName, IIF( lConvertToUpperCase, { |x| x := Upper(AllTrim(x)) }, { |x| x := AllTrim(x) } ) )
return( AScanExact( aListe, IIF( lConvertToUpperCase, Upper(cValue), cValue ) ) > 0 )

METHOD GetConfigArray( cBereich AS STRING, cParamName AS STRING, cbCodeBlock := nil AS USUAL ) AS ARRAY PASCAL CLASS P_Base
// Gibt ein String-Array mit den Im AMS-Konfigurationsparameter hinterlegten WErten zurück
// cbCodeBlock: { |cElement| cElement := cElement }

	LOCAL aListe                AS ARRAY

cbCodeBlock := SELF:PrepareCodeBlock( cbCodeBlock, { |cElement| cElement := AllTrim(cElement) } )
aListe := oConfigManager:GetArrayParam( cBereich, cParamName, {} )
AEval( aListe, cbCodeBlock )
return( aListe )

METHOD CreateDataSet( symAlias AS SYMBOL, aFields AS ARRAY, aVorlageKey := nil AS ARRAY ) AS LOGIC PASCAL CLASS P_Base
// Vereinfacht das anlegen von ams-Datensätzen.
// <aVorlageKey> nur übergeben, wenn eine Kopie aus einem vorhandenen Datensatz gemacht werden soll.
//
// symAlias:     (SYMBOL) #STKLAP
// aFields:      (ARRAY) {{ #FIELD, uReplace }, { #FIELD, uReplace } [...] }
// aVorlageKey:  (ARRAY) {{ #FIELD(n), uVorlageKey(n)}, { #FIELD(n), uVorlageKey(n)}, [...] }

	LOCAL oServer         AS AServer

SELF:ResetLocalError( #BaseCreateDataSet )
if( !IsNil( aVorlageKey ) )
	// Datensatz soll aus einer Vorlage erstellt werden
	oServer := SELF:CreateAServer( symAlias, aVorlageKey, TRUE )
else
	oServer := SELF:oTransactionManager:Open( symAlias )
	if( !oServer:BeginAppend() )
		SELF:MessageFormat( "Fehler beim erstellen eines Datensatzes in der Tabelle # mit den Feldern #. #", { symAlias, SELF:ArrayToFieldListString( aFields ), oServer:Status:GetMessage() }, PROT_ART_ERROR, TRUE )
		oServer:Release()
		oServer := NULL_OBJECT
	endif
endif

if( oServer != NULL_OBJECT )
	if( SELF:ArrayToRecord( aFields, oServer ) )

   	if( !oServer:EndAppend("AUTO") )
			SELF:MessageFormat( "Fehler beim erstellen eines Datensatzes in der Tabelle # mit den Feldern #. #", { symAlias, SELF:ArrayToFieldListString( aFields ), oServer:Status:GetMessage() }, PROT_ART_ERROR, TRUE )
		endif
   endif

	oServer:Release()
endif

return( !SELF:IsLocalError( #BaseCreateDataSet ) )

METHOD UpdateDataSet( symTable AS SYMBOL, aKeyFields AS ARRAY, aReplaceFields AS ARRAY, lErrorIfNotExists := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
// Schreibt die Felder <aReplaceFields> in einen vorhandenen Datensatz der Tabelle <symTable> mit dem Schlüssel <aKeyFields>
return( SELF:UpdateFieldsForTable( symTable, aKeyFields, aReplaceFields, lErrorIfNotExists ) )

METHOD IsDataSet( symTable AS SYMBOL, aKeyFields AS ARRAY, lErrorIfNotExists := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
// Gibt TRUE zurück, wenn in der Tabelle <symTable> ein Datensatz zum Key <aKeyFields> existiert
return( SELF:CheckFieldsFound( symTable, Symbol2String( symTable ), aKeyFields, lErrorIfNotExists ) )

METHOD UpdateDataCodeblock( symTable AS SYMBOL, aKeyFields AS ARRAY, cbCodeBlock AS CODEBLOCK, lErrorIfNotExists := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
// Macht ein Update auf einen Datensatz. Hier wird ein Codeblock eingesetzt, somit können WErte besser ersetzt (z.B. addiert, multimliziert etc.) werden.
// <cbCodeBlock> : { |oServer| oServer:FPut( #MENGE, oServer:FGet(#MENGE)+10 ) }
//

	LOCAL oServer             AS AServer
	LOCAL lUpdated := FALSE   AS LOGIC

oServer := SELF:CreateAServer( symTable, aKeyFields, FALSE )
if( oServer != NULL_OBJECT )

	//-------------------------------
	Eval( cbCodeBlock, oServer )
	//-------------------------------

	if( !( lUpdated := oServer:EndUpdate("") ) )
		SELF:MessageFormat( "Fehler in UpdateDataCodeblock( #, {{ # }}) bei EndUpdate(): #", { symTable, SELF:KeyToString( aKeyFields, { |key, value, keyString, valueString, firstEntry, lastEntry| keyString+", "+valueString } ), oServer:Status:GetMessage() }, PROT_ART_ERROR, TRUE )
	endif
	oServer:Release()
endif
return( lUPdated )

METHOD GetWriteRecordFromTable( symTable AS SYMBOL, aKeyFields AS ARRAY, cTableName := "" AS STRING, lErrorIfNotExists := TRUE ) AS AWriteRecord PASCAL CLASS P_Base
// Es wird ein AWRiteRecord aus dem gefundenen Datensatz zurückgegeben. Wurde kein Satz gefunden,
// so wird NULL_OBJECT zurückgegeben

	LOCAL oReadRecord         AS AReadRecord
	LOCAL oWriteRecord        AS AWriteRecord

oWriteRecord := NULL_OBJECT
oReadRecord  := SELF:GetReadRecordFromTable( symTable, aKeyFields, cTableName, lErrorIfNotExists )
if( oReadRecord != NULL_OBJECT )
	if( !oReadRecord:ExportToWriteRecord( oWriteRecord ) )
		SELF:MessageFormat( "Fehler beim Konvertieren eines AReadRecord in einen AWriteRecord : Methode GetWriteRecordFromTable( # ) mit Schlüssel #", { symTable, SELF:KeyToString( aKeyFields, { |key, value, keyString, valueString, firstEntry, lastEntry|  keyString+", "+valueString } ) }, PROT_ART_ERROR, TRUE )
		oWriteRecord:Release()
		oWriteRecord := NULL_OBJECT
	endif
	oReadRecord:Release()
endif
return( oWriteRecord )

METHOD RecordToTable( symTable AS SYMBOL, aKeyFields AS ARRAY, oRecord AS USUAL, lAppendMode := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
// Schreibt einen <oRecord> (AReadRecord oder AWriteRecord) in die Tabelle <symTable> mit dem Key <aKeyFields>
// zurück. [lAppendMode] gibt an, ob der Datensatz angelegt werden soll oder ob ein vorhandener Datensatz
// aktualisiert werden soll

	LOCAL lOK := FALSE        AS LOGIC
	LOCAL aFields             AS ARRAY

aFields := SELF:ArrayFromRecord( oRecord )
if( lAppendMode )
	lOK := SELF:CreateDataSet( symTable, aFields )
else
	lOK := SELF:UpdateDataSet( symTable, aKeyFields, aFields )
endif
return( lOK )

METHOD CreateAServer( symAlias AS SYMBOL, aFields AS ARRAY, lAppendMode := TRUE AS LOGIC ) AS AServer PASCAL CLASS P_Base
//
// Vereinfacht das anlegen/update von ams-Datensätzen per AServer-Object aus einem Vorlagedatensatz.
// symAlias:     (SYMBOL) #STKLAP
// aFields:      (ARRAY) {{ #FIELD(n), uVorlageKey(n)}, { #FIELD(n), uVorlageKey(n)}, [...] }
// lAppendMode:  (LOGIC) TRUE
// Beispiel:
// Hier wird ein AServer zurückgegeben, im BeginAppend, gefüllt mit Daten aus der Vorlage nBdeNr, nPos
// oStklap := oImport:AServerBeginAppend(#STKLAP ,{{#BDE_NR, nBdeNr},{#POS,nPos}}, TRUE)
//
//
     LOCAL oServerSource           AS AServer
     LOCAL oServerTarget           AS AServer
     LOCAL oReadRecord             AS AReadRecord
     LOCAL DIM psCond[9]           AS PTR
     LOCAL x                       AS INT
     LOCAL cMessage := ""          AS STRING

oServerSource := SELF:oTransactionManager:Open(symAlias)
if( !oServerSource:Used )
	SELF:Message("Server "+Symbol2String(symAlias)+" konnte nicht geöffnet werden. "+oServerSource:Status:GetMessage(), PROT_ART_ERROR, "", TRUE)
else
	if( UsualType(aFields[1]) != ARRAY )
		// Keyfelder sind im falschen Format übergeben
		aFields := { aFields }
		SELF:Message( "Achtung! Das Array für die KeyFelder für den CreateAServer()-Zugriff ist nur Eindimensional. Es wird jedoch Mehrdimensional (Format: {{#KEY,VALUE}}) erwartet. Eine automatische Konvertierung auf Mehrdimensional hat stattgefunden.", PROT_ART_ERROR, "", TRUE )
	endif

	for x:=1 to ALen(aFields)
		psCond[x] := BuildCondition(aFields[x][1], SQL_EQUAL, aFields[x][2])
		if( x>1 )
			cMessage += " und "
		endif
		cMessage += SELF:StringFormat( "# = #", { aFields[x][1], aFields[x][2] } )
	next x

	oServerSource:ConditionSet( ALen(aFields), @psCond )
	if( oServerSource:SQLSelect() .and. oServerSource:SQLFetch() )

		if( !lAppendMode )
			// Nur Update des vorhandenen Satzes
			if( oServerSource:beginUpdate() )
				/* Der AServer befindet sich im BeginUpdate-Zustand */
				/* In der rufenden Mehtoden muss noch folgendes gemacht werden: */
				/* oS:EndUpdate("") */
				/* oS:Release()     */

				return( oServerSource )		// <------- ACHTUNG

			else
				SELF:MessageFormat( "Datensatz für Tabelle # mit Inhalt # konnte nicht gesperrt werden. #", { Symbol2String(symAlias), cMessage, oServerSource:Status:GetMessage()}, PROT_ART_ERROR, TRUE )
				oServerSource:Release()
			endif
		else
			oServerTarget := SELF:oTransactionManager:Open(symAlias)
         if( oServerTarget:Used )
				if( oServerTarget:BeginAppend() )
					oReadRecord := AReadRecord{oServerSource}
					oReadRecord:ExportToServer(oServerTarget)
					oReadRecord:Release()

					/* Das Objekt wird in diesem Zustand übergeben */
					/* d.h.:  Neuer Datensatz ist angelegt, Daten aus der Vorlage kopiert */
					/* In der rufenden Methoden muss noch folgendes gemacht werden: */
					/* oS:EndAppend("") */
					/* oS:Release       */

					oServerSource:Release()
					return( oServerTarget )		// <---- ACHTUNG

				else
					SELF:Message( "Datensatz in Tabelle "+Symbol2String(symAlias)+" konnte nicht angelegt werden. "+oServerTarget:Status:GetMessage(), PROT_ART_ERROR, "", TRUE )
				endif
			else
				SELF:MessageFormat("Server für Tabelle # konnte nicht geöffnet werden. #",{Symbol2String(symAlias),oServerSource:Status:GetMessage()}, PROT_ART_ERROR, TRUE)
			endif
			oServerTarget:Release()
		endif
	else
		SELF:MessageFormat( "Vorlage aus Tabelle # mit Inhalt # wurde nicht gefunden. #", {Symbol2String(symAlias), cMessage, oServerSource:Status:GetMessage()}, PROT_ART_ERROR, TRUE )
	endif
endif
oServerSource:Release()
return( NULL_OBJECT )

METHOD UpdateFieldsForTable( symTable AS SYMBOL, aKeyFields AS ARRAY, aReplaceFields AS ARRAY, lErrorIfNotExists := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
// Update von Feldern in einer bestimmten Tabelle
// symTable       : #ANGEBOTPOS
// aKeyFields     : {{ #ANGEBOTSNR, SELF:cAngebot }, { #AUFPOS, SELF:nAngPos }}
// aReplaceFields : {{ #P_ABSTIMM, FALSE}, {#FK, "N"}}
//
// Es wird FALSE zurückgegeben, wenn keine oder nicht alle Ersetzungen durchgeführt werden konnten
//
// Beispiel:
// if( UpdateFieldsForTable( #AUFKOPF, {{#AUFTRAG, cAuftrag}}, {{#BEZEICH, "FINKEN"}} ) )

	LOCAL oServer           AS AServer
	LOCAL lOK := FALSE      AS LOGIC

// Es wird ein AServer im Update-Modus erstellt
oServer := SELF:CreateAServer( symTable, aKeyFields, FALSE )
if( oServer != NULL_OBJECT )
	lOK := SELF:ArrayToRecord( aReplaceFields, oServer )
	if( !oServer:EndUpdate("") )
		SELF:MessageFormat( "Fehler in UpdateFieldsForTable bei EndUpdate(): #", { oServer:Status:GetMessage() }, PROT_ART_ERROR, TRUE )
	endif
	oServer:Release()
endif
return(lOK)

METHOD GetReadRecordFromTable(symTable AS SYMBOL, aKeyFields AS ARRAY, cTableName := "" AS STRING, lErrorIfNotFound := TRUE AS LOGIC) AS AReadRecord PASCAL CLASS P_Base
// Es wird ein Satz in der Tabelle gesucht und der gesamte Record zurückgegeben
// symTabel   : #ARTIKEL
// aKeyFields : {{#ARTIKEL, "1000"}}
//
// Beispiel --> oRec := oBase:GetReadRecordFromTable( #BESTAND, {{ #ARTIKEL, "1000"}, {#LAGER, "102"}} )
//
	LOCAL oServer          AS AServer
	LOCAL x                AS INT
	LOCAL DIM psCond[9]    AS PTR
	LOCAL oRecord          AS AReadRecord
	LOCAl lCancel := FALSE AS LOGIC

oRecord := NULL_OBJECT
oServer := SELF:oTransactionManager:Open(symTable)
if( !oServer:Used )
	SELF:Message("Server "+Symbol2String(symTable)+" konnte nicht geöffnet werden. "+oServer:Status:GetMessage(), PROT_ART_ERROR, "", TRUE)
else
	for x:=1 upto ALen(aKeyFields)
		if( UsualType( aKeyFields[x] ) != ARRAY )
			SELF:MessageFormat( "Fehler bei der Definition der <aKeyFields> für Tabelle #. Das Array muß mehrdimensional sein. Inhalt=# ", { symTable, SELF:ArrayToFieldListString( aKeyFields ) }, PROT_ART_ERROR, TRUE )
			lCancel := TRUE
		else
			psCond[x] := BuildCondition( aKeyFields[x][1], SQL_EQUAL, aKeyFields[x][2] )
		endif
	next x
	if( !lCancel )
		oServer:ConditionSet( ALen(aKeyFields), @psCond )
		if( oServer:SQLSelect() .and. oServer:SQLFetch() )
			oRecord := AReadRecord{oServer, TRUE}
		else
			if( lErrorIfNotFound )
		   	SELF:MessageFormat( "In der Tabelle # wurde der Inhalt # nicht gefunden. ", { iif( Empty(cTableName), symTable, cTableName ), SELF:ArrayToFieldListString( aKeyFields ) }, PROT_ART_ERROR )
	   	endif
		endif
	endif
endif
oServer:Release()

RETURN oRecord // <--- WEnn ein Fehler aufgetreten ist, wird oRecord NULL_OBJECT liefern und ein oRecord:Release() ist  nicht erforderlich

METHOD ArrayAddIfNotExists( aArray AS ARRAY, uValue AS USUAL ) AS VOID PASCAL CLASS P_Base
// Fügt das Element <uValue> an das eindimensionale Array <aArray> an, wenn es noch nicht existiert.

if( AScan( aArray, { |x| UsualType(x) == UsualType( uValue ) .and. x == uValue } ) == 0 )
	AAdd( aArray, uValue )
endif


METHOD ArrayToFieldListString( aArray AS ARRAY, cbCodeBlock := nil AS USUAL, lCRLF := FALSE AS LOGIC ) AS STRING PASCAL CLASS P_Base
// Aus einem Array <aArray> im Format {{#FIELD, Value},{#FIELD, Value}} oder {#FIELD, #FIELD} wird ein
// vorzeigbarer String generiert. Das ist für Feldlisten oder Keyfelder gut zu gebrauchen.
// Durch den optinalen Codeblock [cbCodeblock] kann die Ausgabe noch verändert werder.
// Durch den Parameter [lCRLF] kann nach jedem Field/Value Paar ein Zeilenumbruch generiert werden
// Standardausgabe:
// {{#FIELD, Value},{#FIELD, Value}}  --> Field = "Value", Field = "Value"
// {#FIELD, #FIELD}                   --> Field, Field

	LOCAl cResult := ""            AS STRING
	LOCAL x                        AS INT
	LOCAL lValueAvailable          AS LOGIC
	LOCAL cFieldName, cValue       AS STRING

cbCodeBlock := SELF:PrepareCodeBlock( cbCodeblock, { |cKey, cValue, lValueAvailable, lLastElement| cKey + IIF( lValueAvailable, " = " + CHR(34) + cValue + CHR(34),"" ) + IIF(!lLastElement, ", " , "") } )
for x:=1 to ALen(aArray)
	if( UsualType(aArray[x]) == ARRAY )
		// Ein Array im Format {{#FIELD,uValue}}	wird erwartet
		if( ALen(aArray[x]) >= 2 )
			cFieldName := SELF:UsualToString( aArray[x][1] )
			cValue     := SELF:UsualToString( aArray[x][2] )
			lValueAvailable := TRUE
		endif
	else
		// Ein Array im Format {#FIELD, #FIELD... wird erwaret
		lValueAvailable := FALSE
		cFieldName      := SELF:UsualToString( aArray[x] )
		cValue          := ""
	endif
	cResult += Eval( cbCodeBlock, cFieldName, cValue, lValueAvailable, x==ALen(aArray) )
	cResult += IIF( lCRLF, CRLF, "" )
next x
return( cResult )

METHOD GetFieldFromTable( symTable AS SYMBOL, aKeyFields AS ARRAY, symField AS SYMBOL, lErrorIfEntryNotExits := FALSE AS LOGIC ) AS USUAL CLASS P_Base
// Ist nur noch aus kompatibilitätsgründen hier. Anderfalls GetFieldsFromTable() benutzen
   LOCAL aValues         AS ARRAY
	LOCAL uValue          AS USUAL

uValue  := nil
aValues := SELF:GetFieldsFromTable( symTable, aKeyFields, { symField }, TRUE, lErrorIfEntryNotExits )
if( ALen(aValues) != 0 )
	uValue := aValues[1][2]
endif
return( uValue )

METHOD GetFieldsFromTable( symTable AS SYMBOL, aKeyFields AS ARRAY, aFields AS ARRAY, lErrorIfFieldNotExits := TRUE AS LOGIC, lErrorIfEntryNotExits := FALSE AS LOGIC ) AS ARRAY PASCAL CLASS P_Base
// Es werden die in <aFields> saufgeführten Felder aus der Tabelle <symTable> zurückgegeben
//
// symTable      : #LAGER
// aKeyFields    : {{#ARTIKEL, "10000"}, {#LAGER,"20"}}
// symField      : {#BSTAND_LFD, #MIN_BESTAND}
//
// Beispiel --> aFields := Base:GetFieldFromTable(#ARTIKEL, {{#ARTIKEL,"1000"}},{#BEZEICH})
//
// Wenn kein Wert ermittelt werden konnte wird {} ein leeres Array zurückgegeben

	LOCAL oRecord          AS AReadRecord
	LOCAL aValues          AS ARRAY
	LOCAL x                AS INT

aValues := {}
oRecord := SELF:GetReadRecordFromTable( symTable, aKeyFields )
if( oRecord != NULL_OBJECT )
	for x:=1 upto ALen(aFields)
		do case
		case( UsualType(aFields[x]) != SYMBOL )
			SELF:MessageFormat( "Fehler in <GetFieldsFromTable> für Tabelle #. Das Feld (aFields) # hat den Inhalt # und ist vom Typ #. Erwartet wird ein Symbol. ", { symTable, x, aFields[x], SELF:UsualTypeAsString( aFields[x] ) }, PROT_ART_ERROR, TRUE )
		case( IsNil( oRecord:FGet(aFields[x]) ) )
			if( lErrorIfFieldNotExits )
				SELF:MessageFormat("Das Feld # ist in der Tabelle # nicht vorhanden.", { aFields[x], symTable }, PROT_ART_ERROR, TRUE )
			endif
		otherwise
			AAdd( aValues, { aFields[x], oRecord:FGet(aFields[x]) } )
		endcase
	next x
	oRecord:Release()
else
	if( lErrorIfEntryNotExits )
   	SELF:MessageFormat( "Fehler bei ermitteln des Datensatzes aus der Tabelle # mit dem Schlüssel #. Der Eintrag wurde nicht gefunden. ", { symTable, SELF:ArrayToFieldListString( aKeyFields ) }, PROT_ART_ERROR, TRUE )
	endif
endif
return( aValues )

METHOD CheckFieldsFound( symTable AS SYMBOL, cTableName AS STRING, aKeyFields AS ARRAY, lErrorIfNotFound := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
// Es wird in der Tabelle <symTable> geprüft, ob ein Eintrag für <aKeyFields> existiert.
//
// Aufbau <aKeyFields> : {{#ARTIKEL, "10000"}, {#LAGER,"20"}}
//

	LOCAL oReadRecord         AS AReadRecord
	LOCAL lFound := FALSE	  AS LOGIC

oReadRecord := SELF:GetReadRecordFromTable( symTable, aKeyFields, cTableName, lErrorIfNotfound )
if( oReadRecord != NULL_OBJECT )
	lFound := TRUE
	oReadRecord:Release()
else
	lFound := FALSE
endif

return( lFound )

METHOD GetNextPos( symTable AS SYMBOL, symPosField AS SYMBOL, cWhere AS STRING, nStep := 10 AS INT ) AS INT PASCAL CLASS P_Base
// Es wird die nächste freie Nummer in <cTable> zurückgegeben
//
// Aufbau:	symTable	   : #STKLAK
//  		cPosField	: #AUFPOS
// 			cWhere      : "BDE_NR = 100000"
// 			nStep       : Default 10
//
// Rückgabe:             0 = Nichts gefunden, sonst nächste Position

	LOCAl oStmt         AS ASqlStatement
	LOCAL nNextPos := 0 AS INT

oStmt := SELF:CreateSqlStatement("select top 1 "+Symbol2String(symPosField)+" as POS from {"+Symbol2String(symTable)+"} where "+cWhere+" order by "+Symbol2String(symPosField)+" desc",;
                                   SELF:StringFormat("Fehler beim ermitteln der nächsten freien Positionen (#) für Tabelle #", { symPosField, symTable } ) )
if( oStmt != NULL_OBJECT )
	if( oStmt:Fetch() )
		nNextPos := oStmt:FgetN(#POS)
	endif

	nNextPos += nStep

	oStmt:Release()
endif
return( nNextPos )

METHOD GetBelegNr( cBereich AS STRING, cService AS STRING ) AS INT PASCAL CLASS P_Base
//
// Es wird die nächste Belegnummer zum Bereich <cBereich> zurückgegeben.
// Konnte keine Belegnummer erittelt werden, so wird 0 zurückgegeben
//

	LOCAL fBelegNr          AS REAL8
	LOCAL nBelegNr := 0     AS INT

if( GetBelegNr(SELF:oTransactionManager, cBereich, cService, @fBelegNr)	)
	nBelegNr := Int( fBelegNr )
else
	SELF:MessageFormat( "Aus dem Dienst # konnte die Belegnummer für den Bereich # nicht vergeben werden. #", { cService, cBereich, SELF:oTransactionManager:Status:GetMessage() }, PROT_ART_ERROR )
endif
return( nBelegNr )

METHOD SqlToDebugPrint( cStatement AS STRING ) AS VOID PASCAL CLASS P_Base
//
// Ausgabe eine Sql-Statements als Debugprint
// Es sollte nicht mit Select * gearbeitet werden.
//
// Beispiel: SqlToDebugPrint( "SELECT A,B FROM {TEMP} WHERE A = 1" )
//
debugPrint( SELF:SqlToMemo( cStatement ) )


METHOD SqlToMemo( cStatement as string, lWithHeader := TRUE as logic ) as string pascal class P_Base
//
// Ausgabe eine Sql-Statements als Memo
// Es sollte nicht mit Select * gearbeitet werden.
//
// Beispiel: SqlToMemo( "SELECT A,B FROM {TEMP} WHERE A = 1" )
//

	LOCAL oStmt           AS ASqlStatement
	LOCAL X, nFieldCount  AS INT
	LOCAL cMemo := ""     AS STRING

oStmt := SELF:CreateSqlStatement( cStatement, "Fehler bei Ausgabe in Funktion SqlToDebugPrint (P_Base)"	)
if( oStmt != NULL_OBJECT )
	if( lWithHeader )
		cMemo += Replicate( "-", 80 ) + CRLF
		cMemo += Left(cStatement,80) + CRLF
		cMemo += Replicate( "-", 80 ) + CRLF
	endif

	do while ( oStmt:Fetch() )
		nFieldCount := oStmt:FCount
		for x:=1 upto nFieldCount
			cMemo += SELF:UsualToString( oStmt:Fget(x) ) + "|"
		next x

		cMemo += CRLF
	enddo

	if( lWithHeader )
		cMemo += Replicate( "-", 80 ) + CRLF
	endif
endif
oStmt:Release()
return( cMemo )

METHOD CreateCopyStatement( symAlias AS SYMBOL, cWhere AS STRING, aFieldReplaces AS ARRAY, lDefaultIfCopy := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
// Kopieren einen oder mehrerer Datensätze und führt Feldersetzungen durch
// Man sollte darauf achten, bei den {{#FIELD, cValue}} Kobinationen für Value einen String zu übergeben. Andernfalls erfolgt eine automatische Konvertierung
// <lDefaultIfCopy> Setzt Felder die in der Struktur auf Default gesetzt werden sollen wieder zurück.
// Beispiel:
// 	CreateCopyStatement( #AUFKOPF, "AUFTRAG="+cAuftrag, {{#AUFTRAG, cNeuAuftrag},{#DAT_AEN}} )
//
	LOCAL cSelect := ""      AS STRING
	LOCAL cInsert := ""      AS STRING
	LOCAL aExcludeFields     AS ARRAY
	LOCAL x                  AS INT
	LOCAL oStmt              AS ASqlStatement
	LOCAL lOK := FALSE       AS LOGIC
	LOCAL oBuilder           AS SqlCopyStmtBuilder
	LOCAL uValue             AS USUAL
	LOCAL cValue             AS STRING

aExcludeFields := {}

for x:=1 upto ALen(aFieldReplaces)
	if( IsSymbol(aFieldReplaces[x][1]) )
		aFieldReplaces[x][1] := Symbol2String( aFieldReplaces[x][1] )
	endif
	AAdd(aExcludeFields, String2Symbol(aFieldReplaces[x][1]))
   if( ALen(aFieldReplaces[x]) > 1 )
		if( !Empty(cSelect) )
			cSelect += ", "
			cInsert += ", "
		endif

		cInsert += "["+aFieldReplaces[x][1]+"]"
		uValue := aFieldReplaces[x][2]
		// Values umbauen auf SQL-like: ein String wird 'String' etc.
		do case
		case( IsString( uValue ) )
			cValue := QuoteText( uValue )
		case( IsDate( uValue ) )
			cValue := QuoteText( DToC( uValue ) )
		case( IsNumeric( uValue ) )
			cValue := NTrim(uValue)
		case( IsLogic( uValue ) )
			cValue := IIF( uValue, "1", "0" )
		otherwise
			SELF:MessageFormat( "Mögliche Probleme beim erstellen eines CreateCopyStatement. Der Inahlt # zum Feld # ist vom Typ # und wurde nicht auf SQL-Typ umgestellt.", { uValue, aFieldReplaces[x][1], SELF:UsualTypeAsString( uValue ) }, PROT_ART_WARNING, TRUE )
			cValue := SELF:UsualToString( uValue )
		endcase
      cSelect += cValue
	endif
next x

if( Empty(aExcludeFields) )
     aExcludeFields := NULL_ARRAY
endif

//
// Zum bessere Verständnis CreateCopyStmt() anschauen
// Es gibt dort eine Beschreibung
//

oBuilder               := SqlCopyStmtBuilder{symAlias, symAlias}
oBuilder:Where         := cWhere
oBuilder:FieldNames    := cInsert
oBuilder:FieldValues   := cSelect
oBuilder:ExcludeList   := aExcludeFields
oBuilder:DefaultIfCopy := lDefaultIfCopy

lOK := oBuilder:GetStatement(SELF:oTransactionManager, @oStmt)
if( lOK )
	if( oStmt:ExecDirectBatch() )
		lOK := TRUE
	endif
	oStmt:Release()
endif
oBuilder:Release()

return( lOK )

METHOD SqlTableHasRecords( symTableNAme AS SYMBOL, aKeyFields AS ARRAY, lTableNameRawFormat := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
// Es wird mit "select count(*) from " geprüft, ob es Datensätze zum Key <aKeyFields> in der Tabelle <symTableName>
// gibt. Ist die ANzahl > 0 so wird True zurückgegeben
// [lTableNameRawFormat] gibt an, ob der Tabellenname als #ANGEBOT (false) oder als #ANGD300 (true) übergeben wird

	LOCAL cStatement               AS STRING
	LOCAL oStmt                    AS ASqlStatement
	LOCAL x                        AS INT
	LOCAL lReturn := FALSE         AS LOGIC
	LOCAL cTable                   AS STRING

cTable := IIF( lTableNameRawFormat, Symbol2String( symTableName ), "{"+Symbol2String( symTableName )+"}" )
cStatement := "SELECT Count(*) AS ANZAHL from "+cTable+" WHERE "
for x:=1 upto ALen( aKeyFields )
	cStatement += SELF:UsualToString(aKeyFields[x][1]) + " = ? and "
next x
cStatement := SELF:StringDecrement( cStatement, 5, sa_right )

oStmt := SELF:oTransactionManager:CreateStmt(cStatement)
if( oStmt:Prepare() )
	for x:=1 upto ALen( aKeyFields )
		oStmt:ParamPut(1, aKeyFields[x][2] )
	next x
	if( oStmt:ExecuteReader() )
		if( oStmt:Fetch() )
			lReturn := oStmt:FgetN(#ANZAHL) != 0
		else
			SELF:MessageFormat( "Fehler in SqlTableHasRecords für Tabelle #. Fetch schlug fehl.", { symTableName }, PROT_ART_ERROR, TRUE )
		endif
	else
		SELF:MessageFormat( "Fehler in SqlTableHasRecords für Tabelle #. #", { symTableName, oStmt:Status:GetMessage() }, PROT_ART_ERROR, TRUE )
	endif
else
	SELF:MessageFormat( "Fehler in SqlTableHasRecords für Tabelle #. #", { symTableName, oStmt:Status:GetMessage() }, PROT_ART_ERROR, TRUE )
endif
oStmt:Release()
return( lReturn )

METHOD CreateSqlStatement( cStatement AS STRING, uMeldung AS USUAL, lReader := TRUE AS LOGIC, aFelder := NIL AS USUAL ) AS ASqlStatement PASCAL CLASS P_Base
//
// Parameter:
// 	cStatement     (STRING)     - "SELECT * FROM KAVD000 WHERE AUFTRAG = ? and AUFPOS = ?"
// 	uMeldung       (STRING)     - "Fehler beim lesen aus AUFKOPF" (Kann auch als Array im Format { cMeldung, uField, uField... } übergeben werden
//    lReader        (LOGIC)      - TRUE (Gibt an ob ein Reader oder ein Batch ausgeführt wird (Batch = INSERT, DELETE)
//    aFelder        (ARRAY)      - {"AUF10",10}  für ParamPut ( Wenn in cStatement mit ? gearbeitet wurde )
//
// Rückgabe:
// 	ASqlStatement
//		Dieses muss nach Weitervararbeitung nur noch Released werden
//
	LOCAL oStmt           AS ASqlStatement
	LOCAL X               AS INT
	LOCAL lOK := TRUE     AS LOGIC
	LOCAL aFields         AS ARRAY
	LOCAL cMeldung        AS STRING

oStmt := SELF:oTransactionManager:CreateStmt( cStatement )

/* Prepare */
lOK := oStmt:Prepare()

/* ParamPut */
if( lOK )
	if( aFelder != NIL .and. ALen(aFelder) > 0 )
		for x:=1 upto ALen(aFelder)
			if( !oStmt:ParamPut(x,aFelder[x]) )
				lOK := FALSE
			endif
		next x
	endif
endif

/* Execute */
if( lOK )
	if( SELF:lDebugMode )
		__Trace_SQL_Queries__(TRUE)
	endif

	if( lReader )
		lOK := oStmt:ExecuteReader()
	else
		lOK := oStmt:ExecuteBatch()
	endif

	if( SELF:lDebugMode )
		__Trace_SQL_Queries__(FALSE)
	endif
endif

if( !lOK )
	if( UsualType( uMeldung ) == ARRAY )
		aFields := {}
		for x:=2 upto ALen(uMeldung)
			AAdd( aFields, uMeldung[x] )
		next x
		cMeldung := SELF:StringFormat( uMeldung[1], aFields )

	else
		cMeldung := uMeldung
	endif

	cMeldung += " " + oStmt:Status:GetMessage()

	SELF:Message( cMeldung , PROT_ART_ERROR )
	oStmt:Release()

	oStmt:=NULL_OBJECT
endif
return( oStmt )

METHOD IsLockError( cServerMessage AS STRING ) AS LOGIC PASCAL CLASS P_Base
return( At("42000/1222", cServerMessage) > 0 .OR. At("40001/1205", cServerMessage) > 0 )

ASSIGN oProgress( oP AS ABaseProgressConnect ) AS ABaseProgressConnect PASCAL CLASS P_Base
// Zuweisen eines PROGRESS
if( SELF:__oProgress == NULL_OBJECT )
	SELF:__oProgress := oP
else
	SELF:Message( "Ams-Progress mehrfach zugewiesen. Es wurde versucht, trotz vorhandenem Progress-Object, ein neues zu setzen. Siehe Object P_Base, ASSIGN oProgress", PROT_ART_ERROR, "", TRUE )
endif

ACCESS oProgress AS ABaseProgressConnect PASCAL CLASS P_Base
// Abfragen eines Progress. Ist dieser noch nicht vorhaden,
// so wird er nun neu erstellt
if( SELF:__oProgress == NULL_OBJECT )
	if( SELF:oCommConnect == NULL_OBJECT )
		// Wir haben kein CommConnect von Aussen bekommen
		// Nun ein neuen Progress erstellen, auch wenn der
		// vielleicht nicht sichtbar wird.
		SELF:__oProgress := ABaseProgressConnect{}
		SELF:dbg("Progress erstellt (ABaseProgressConnect - Ist evtl. nicht sichtbar, da kein oCommConnect vorhanden)")
	else
		SELF:__oProgress := SELF:oCommConnect:GetProgress()
		SELF:dbg("Progress aus (CommConnect) erstellt")
	endif
	SELF:__oProgress:Setup(1,50)
	SELF:__oProgress:Abortable := FALSE
	SELF:__oProgress:Title     := ""
	SELF:__lDestroyProgress  := TRUE
	SELF:__oProgress:ShowAlways()
endif
return( SELF:__oProgress )

METHOD ProgressIncrement( cDisplayText := "" AS STRING ) AS VOID PASCAL CLASS P_Base
if( !Empty( cDisplayText ) )
	SELF:oProgress:DisplayText := cDisplayText
endif
if( SELF:oProgress:Value >= 80 .and. SELF:__lDestroyProgress )
	// Wenn wir den Progress erstellt haben (mit 100 initialisiert)
	// und diese 100 erreicht sind, dann wieder von Vorne anfangen,
	// damit sich der Balken auch weiter bewegt.
	SELF:oProgress:Value := 1
endif
SELF:oProgress:Increment()

// ----------------------------------------------------------------------------------
//
// Protocol und Message
//
// ----------------------------------------------------------------------------------

METHOD Message( cMeldung AS STRING, cArt := "" AS STRING, cHeader := "" AS STRING, lAppendHistory := FALSE AS LOGIC ) AS VOID PASCAL CLASS P_Base
//
// Parameter:
//		cMeldung   (STRING)     "Hier der Header. Hier der Body..."
//		cArt       (ART)        PROT_ART_INFORMATION
//
// Es wird eine Nachricht auf den Nachrichten-Stack gepackt.
// if ein oProtocol vorhaden, so wird auch das Protocol versorgt
// Zudem kann ein Codeblock (siehe SELF:cbError) benutzt werden:
// cb  := { |header,meldung,art| oProt:Insert(header,meldung,art) }
//

	LOCAL cAddMessageText := ""             AS STRING


// prüfen, ob in der Meldung ein Lock-Fehler gemeldet wurde
if( At("42000/1222", cMeldung) > 0 .OR. At("40001/1205", cMeldung) > 0 )
	// 42000/1222 : Lock request time out period exceeded
	// 40001/1205 : Transaction (Process ID) was deadlocked on resources with another process and has been chosen as the deadlock victim
	SELF:__lIsTransactionLockError := TRUE
endif

if( ALen( SELF:__aAddMessageText ) != 0 )
	cMeldung := SELF:ArrayToString( SELF:__aAddMessageText, ". " ) + ". " + cMeldung
endif

if( Empty(cArt) )
	cArt := PROT_ART_INFORMATION
endif

if( Empty(cHeader) )
	/* Header wurde nicht übergeben, jetzt aus der Meldung generieren */
	cHeader := Left(cMeldung,120)
endif

if( lAppendHistory )
	// Es soll der Method-Stack mit an die Meldung angehängt werden.
	cMeldung += CRLF + CRLF + CallHistory()
endif

if( SELF:__oProtocol != NULL_OBJECT )
	SELF:__oProtocol:Insert( cHeader, cMeldung, cArt )
endif

if( SELF:cbError != NULL_CODEBLOCK )
	/* Es kann ein Codeblock für das Abfangen von Meldungen */
	/* benutzt werden: */
	/* cb  := { |header,meldung,art| oProt:Insert(header,meldung,art) } */
	Eval( SELF:cbError,cHeader, cMeldung, cArt)
	//SELF:cbError:Eval(cHeader, cMeldung, cArt)
endif

AAdd( SELF:__aMessage, { cHeader, cMeldung, cArt } )

if( SELF:__oIntegratedBaseObject != NULL_OBJECT )
	// Meldungen an dieses Objekt weitergeben
	SELF:__oIntegratedBaseObject:Message( cHeader, cMeldung, cArt )
else
	// Dbg-Meldungen nur einmal ausgeben. Das macht dann
	// das integrierte Objekt
	SELF:dbg( cArt + " - " + cHeader + " - " + cMeldung, IIF( cArt $ "EW", TRUE, FALSE ) )
endif

METHOD AddMessageText( symType AS SYMBOL, cText := "" AS STRING ) AS VOID PASCAL CLASS P_Base
// Mit dieser Methode können Texte den Meldungen vorangestellt werden.
// <symType> #add  : Fügt einen Text hinzu
//           #del  : Löscht den zueletzt hinzugefügten Text
//           #kill : Löscht alle Text
//
// Beispiel:
// AddMessageText( #add, "Angebot" )
// AddMessageText( #add, "Position" )
// Message( "HALLO", PROT_ART_ERROR ) --> Ergibt "Angebot. Position. Hallo" als Meldung
// AddMessageText( #del ) -> Löscht den letzten Eintrag wieder raus
//
do case
case( InList( symType, #add, #put, #ins, #insert ) )
	AAdd( SELF:__aAddMessageText, cText )
case( InList( symType, #del, #delete ) )
	if( ALen( SELF:__aAddMessageText ) > 0 )
		ADelShrink( SELF:__aAddMessageText, ALen( SELF:__aAddMessageText ) )
	endif
case( InList( #kill, #empty ) )
	SELF:__aAddMessageText := {}
endcase


METHOD MessageFormat( cMeldung AS STRING, aItems AS ARRAY, cArt := "" AS STRING, lAppendHistory := FALSE AS LOGIC ) AS VOID PASCAL CLASS P_Base
SELF:Message( SELF:StringFormat( cMeldung, aItems ), cArt, "", lAppendHistory )


METHOD IntegrateMessages( oBaseObject AS P_BASE ) AS VOID PASCAL CLASS P_Base
//
// Methode zum schnellen integrieren von Nachrichten
// aus einem anderen P_Base Object
//
// oBase := P_Base{}
// oBase:Message( "Hallo" )
// 	oView := P_Base{}
//    oView:Message("Hier in anderem Object ")
// 	oBase:IntegrateMessages( oView )
//		oView:Release()
// oBase:Message( "Ende" )
// oBase:Release()
SELF:AddMessages( oBaseObject:GetMessageArray() )

METHOD AddMessages( aMessages AS ARRAY ) AS VOID PASCAL CLASS P_Base
// Aufbau des Arrays: {{"Header","Description", <ProtArt>},...}
// Über SELF:Message laufen lassen, damit die Meldung auch ins Protocol kommen, fals vorhanden

	LOCAL X               AS INT

for x:=1 upto ALen( aMessages )
	SELF:Message( aMessages[x][2], aMessages[x][3], aMessages[x][1] )
next x

METHOD GetMessageArray( cArt := "" AS STRING, nMaxCount := 0 AS INT ) AS ARRAY PASCAL CLASS P_Base
//
// Parameter
// 	cArt      (STRING)    LEER, PROT_ART_INFORMATION, PROT_ART_WARNING, PROT_ART_ERROR oder eine Kombination (z.B. "WE" für Warnungen und Error)
//    nMaxCount (INT)       0 = Alle, sonst max. Anzahl zu Meldungen die zurückgegeben werden sollen
//
// Beschreibung:
// 	Es werden alle Nachrichten der Art (cArt) als Array zurückgegeben.
// 	Ist cArt = Leer, dann werden alle Nachrichten zurückgegeben
//
// Rückgabe:
// 	Aufbau: {{1,2,3},{1,2,3}}
// 	[1] Header
// 	[2] Body
// 	[3] Art (PROT_ART_INFORMATION, PROT_ART_WARNING, PROT_ART_ERROR)
//

	LOCAL aReturnMessage  AS ARRAY
	LOCAL X               AS INT

aReturnMessage := {}
if( nMaxCount == 0 .or. nMaxCount > ALen(SELF:__aMessage))
	// Wenn nMaxCount nicht angegeben, dann alle Meldungen zurückgeben
	nMaxCount := ALen(SELF:__aMessage)
endif

for x:=1 upto ALen(SELF:__aMessage)
	if( ALen(aReturnMessage) < nMaxCount )
		if( Empty(cArt) .or. SELF:__aMessage[x][3] $ cArt )
			AAdd( aReturnMessage, SELF:__aMessage[x] )
		endif
	endif
next x

return( aReturnMessage )

METHOD AddMessageArray( cHeader AS STRING,  aArray AS ARRAY ) AS VOID PASCAL CLASS P_Base
//
// Parameter:
//		cHeader: Ist dieser Parameter ungleich "" (leer), dann wird er als Überschrift für den Meldungsbody verwendet-
// 	aArray:	entweder {"Meldung", "Meldung", etc.) oder {{ "Meldung, PROT_ART_ERROR}, {"Meldung", PROT_ART_WARNING}, etc.
//
//	Beschreibung:
//		Ein fremdes Fehlerarray (z.B. aus ASCImport-Object oder VoXmlReader-Object) wird dem
//    P_Base-MeldungsArray hinzugefügt
//
	LOCAL x                AS INT

for x:=1 upto ALen(aArray)
	do case
	case( UsualType(aArray[x]) == STRING )
		SELF:Message( IIF(!Empty(cHeader), cHeader + "." +aArray[x], aArray[x]), PROT_ART_ERROR )
		//AAdd( SELF:__aMessage, { IIF(!Empty(cHeader), cHeader, aArray[x]), aArray[x], PROT_ART_ERROR } )
	case( UsualType(aArray[x]) == ARRAY )
		SELF:Message( IIF(!Empty(cHeader), cHeader + "." +aArray[x][1], aArray[x][1]), aArray[x][2])
		//AAdd( SELF:__aMessage, { IIF(!Empty(cHeader), cHeader, aArray[x][1]), aArray[x][1], aArray[x][2] } )
	endcase
next x

METHOD ShowMessage( oCommConnect AS ABaseCommConnect, cHeader := "" AS STRING, cArt := "" AS STRING, nMaxMessages := 3 AS INT ) AS VOID PASCAL CLASS P_Base
//
// Paramater:
//		oCommConnect   (ABaseCommConnect) - Zur Ausgabe
//    cHeader        (STRING)           - Falls noch eine zusätzliche Meldung ausgegeben werden soll
// 	cArt           (STRING)           - LEER, PROT_ART_INFORMATION, PROT_ART_WARNING, PROT_ART_ERROR  oder eine Kombination (z.B. "WI" für Warnungen und Information)
//    nMaxMessages   (INT)              - Anzahl der anzuzeigenden Nachrichten
//
// Beschreibung:
//		Die gesammelten Nachrichten werden - abhängig von <cArt> im Fehler-/Warnungs- oder Informationfenster
//    ausgegeben.
//

	LOCAL cMessage     AS STRING

if( !Empty( cHeader ) )
	cHeader += CRLF
endif

cMessage := SELF:GetMessageMemo(cArt, nMaxMessages)
if( !Empty(cMessage) )
	do case
	case PROT_ART_ERROR $ cArt
		oCommConnect:ErrorMsg(cHeader + cMessage, 0)
	case PROT_ART_WARNING $ cArt
		oCommConnect:Warn(cHeader + cMessage, 0)
	otherwise
		oCommConnect:Inform(cHeader + cMessage, 0)
   endcase
endif


METHOD GetMessageMemo( cArt := "" AS STRING, nMaxCount := 0 ) AS STRING PASCAL CLASS P_Base
//
// Parameter
// 	cArt      (STRING)    LEER, PROT_ART_INFORMATION, PROT_ART_WARNING, PROT_ART_ERROR
// 	nMaxCount (INT)       0 = Alle, sonst Anzahl der maximalen Meldungen
//
// Beschreibung:
// 	Es werden alle Nachrichten der Art (cArt) als STRING zurückgegeben.
// 	Ist cArt = Leer, dann werden alle Nachrichten zurückgegeben
//
// Rückgabe:
//		Ein Memo mit den Bodies der Nachrichten im Format
//			Body1 + CRLF
//			Body2 + CRLF...
//
	LOCAL cMemo      AS STRING
	LOCAL aMessage   AS ARRAY

cMemo := ""
aMessage := SELF:GetMessageArray(cArt, nMaxCount)
AEval( aMessage, { |a| cMemo += a[2] + CRLF + CRLF } )
return( cMemo )

ASSIGN oProtocol( oP AS AMSProtocol ) AS AMSProtocol PASCAL CLASS P_Base
// Protocol zuweisen. Ist ein Protocol zugewiesen, so werden alle Fehlermeldungen
// zusätzlich ins Protocol geschrieben
if( SELF:__oProtocol == NULL_OBJECT )
	SELF:__oProtocol := oP
	// Nun alle bereits aufgelaufenen Nachrichten
	// in das grade zugewiesene Protokoll übernehmen
	SELF:__MessagesToProtocol()

else
	SELF:Message( "Ams-Protokoll mehrfach zugewiesen. Es wurde versucht, trotz vorhandenem Protocol-Object, ein neues zu setzen. Siehe Object P_Base, ASSIGN oProtocol", PROT_ART_ERROR,"", TRUE )
endif

ACCESS oProtocol AS AMSProtocol PASCAL CLASS P_Base
// Abfrage des Protocols. Es wird nicht automatisch ein Neues erstellt.
// Siehe dazu CreateProtocol() und <lAutoCreate>
if( SELF:__oProtocol == NULL_OBJECT )
	if( SELF:__lAutoCreateProtocol )
		// Es wurde bereits SELF:CreateProtocol() gerufen,
		// dass Protocol soll jedoch erst jetzt - beim ersten
		// Zugriff - erstellt werden
		SELF:CreateProtocol( SELF:__cBereich, SELF:__cTitle, FALSE )
	else
		SELF:Message("Protokoll nicht vorhanden. Es wird versucht, ein nicht vorhandenes Protokoll abzufragen. Siehe Object P_Base, ACCESS oProtocol", PROT_ART_WARNING,"", TRUE )
	endif
endif
return( SELF:__oProtocol )

METHOD IsProtocol() AS LOGIC PASCAL CLASS P_Base
// Abfrage, ob ein Protocol aktv ist, oder nicht
return( SELF:__oProtocol != NULL_OBJECT )

METHOD AutoCreateProtocol( cBereich AS STRING, cTitle AS STRING ) AS VOID PASCAL CLASS P_Base
SELF:CreateProtocol( cBereich, cTitle, TRUE )

METHOD CreateProtocol( cBereich AS STRING, cTitle AS STRING, lAutoCreate := FALSE AS LOGIC ) AS VOID PASCAL CLASS P_Base
// Erstellt ein neues Protocol, auf das mit SELF:oProtocol zugegriffen werde kann
// Sind bis hierhin Meldungen aufgetreten, so werden diese nachträgelich ins Protocol geschrieben

if( lAutoCreate )
	// Das Protokoll soll nicht sofort erstellt werden,
	// sondern erst, wenn das erste Mal auf SELF:oProtocol
	// zugegriffen wird.
	SELF:__lAutoCreateProtocol := TRUE
	SELF:__cBereich := cBereich
	SELF:__cTitle   := cTitle
else
	if( SELF:__oProtocol == NULL_OBJECT )
		SELF:__oProtocol := AMSProtocol{}
		SELF:__oProtocol:DelayedCreation := TRUE
		SELF:__oProtocol:Start( cBereich, cTitle )
		SELF:__lDestroyProtocol := TRUE

		// Alle aufgelaufenen Nachrichten nun in das
		// Protokoll schreiben
		SELF:__MessagesToProtocol()
	else
		SELF:Message( "Ams-Protokoll mehrfach zugewiesen. Es wurde versucht, trotz vorhandenem Protocol-Object, ein neues zu erstellen. Siehe Object P_Base, Method CreateProtocol", PROT_ART_ERROR, "", TRUE )
	endif
endif

PROTECT METHOD __MessagesToProtocol() AS VOID PASCAL CLASS P_Base

	LOCAL x            AS INT

if( ALen(SELF:__aMessage) != 0 )
	/* Es haben sich bis hier hin schon Meldungen angesammelt.
		Diese nun auch ins Protokoll schreiben                   */
	for x:=1 upto ALen(SELF:__aMessage)
		SELF:__oProtocol:Insert( SELF:__aMessage[x][1], SELF:__aMessage[x][2], SELF:__aMessage[x][3] )
	next x
endif

METHOD StringFormat( cText AS STRING, aItems AS ARRAY , nLen := 0 AS INT) AS STRING PASCAL CLASS P_Base
//
// Parameter:
// 	cText     (STRING)    - "Hier begint nun die # Suche am # für # und weiter #"
//		aItems    (ARRAY)     - {3, CToD("10.10.2011"), "mich und meine Freunde",3.14}
//    nLen      (INT)       - Bringt den String danach auf die angegebene Länge.
//
// Beispiel:
// 	StringFormat("Hier begint nun die # Suche am # für # und weiter #", {3, CToD("10.10.2011"), "mich und meine Freunde",3.14})
//
     LOCAL X        AS INT
     LOCAL cReplace AS STRING

for x:=1 upto ALen(aItems)
	cReplace := SELF:UsualToString( aItems[x] )
   cText := StrTran(cText,"#",cReplace, 1, 1)
next x
if( nLen != 0 )
	cText := SELF:StringAlign( cText, nLen )
endif
return( cText )

METHOD StringToUsual( cString AS STRING, cTyp AS STRING ) AS USUAL PASCAL CLASS P_Base

	LOCAL uValue                   AS USUAL

cTyp := Upper( AllTrim(cTyp) )
do case
case( cTyp == "STRING" )
	uValue := cString
case( cTyp == "LOGIC" )
	uValue := iif( Upper(cString) == "TRUE", TRUE, FALSE )
case( cTyp == "SYMBOL" )
	uValue := String2Symbol(cString)
case( cTyp == "DATE" )
	uValue := CToD(cString)
case( cTyp == "INT" .or. cTyp == "LONGINT" )
	uValue := INT(Val(cString))
case( cTyp == "FLOAT" .or. Left(cTyp,4) == "REAL" )
	uValue := Val(cString)
case( cTyp == "NULL_OBJECT" )
	uValue := NULL_OBJECT
case( cTyp == "OBJECT" )
	/* Hier keine Wandlung möglich, also NULL_OBJECT */
	uValue := NULL_OBJECT
case( cTyp == "NIL" )
	uValue := nil
otherwise
	/* Keine Konvertierung möglich */
	uValue := ""
endcase
return( uValue )

METHOD UsualToSymbol( uValue AS USUAL ) AS SYMBOL PASCAL CLASS P_Base
return( IIF( IsSymbol(uValue), uValue, String2Symbol( SELF:StringTranslate( SELF:UsualToString( uValue ), {{" ",""},{"#",""}} ) ) ) )

METHOD UsualToNumeric( uValue AS USUAL ) AS REAL8 PASCAL CLASS P_Base
return( IIF( IsNumeric(uValue), uValue, Val(SELF:UsualToString(uValue)) ) )

METHOD UsualToString( uValue AS USUAL ) AS STRING PASCAL CLASS P_Base
//
// Wandet den Inhalt einer Variable beliebigen Typs
// in einen String
//
// STRING   --> STRING
// LOGIC    --> STRING 			TRUE 			--> "TRUE"
// SYMBOL   --> STRING        #TYP 			--> "TYP"
// LONGINT  --> STRING        123  			--> "123"
// FLOAT    --> STRING        1,25 			--> "1.25"
// DATE     --> STRING        10.10.2012  --> "10.10.2012"
//
	LOCAL cValue := ""       AS STRING
	LOCAL y                  AS INT
	LOCAL dwType             AS DWORD

dwType := UsualType( uValue )
do case
case( dwType == STRING )
    cValue := AllTrim(uValue)
case( dwType == LOGIC )
    cValue := iif( uValue, "TRUE", "FALSE" )
case( dwType == SYMBOL )
    cValue := Symbol2String(uValue)
case( dwType == LONGINT )
    cValue := AllTrim(Str3(uValue,150,0))
case( dwType == FLOAT )
	if( int(uValue) = uValue )
		cValue := NTrim(Int(uValue))
	else
	   cValue := AllTrim(Str3(uValue,150,10))
	    for y := Len(cValue) downto 1
			if( Right(cValue,1) == "0" .or. Right(cValue,1) == "." )
	    		cValue := SELF:StringDecrement( cValue )
			else
				EXIT
	    	endif
	    next
	 endif
case( dwType == DATE )
    cValue := DToC(uValue)
case( dwType == ARRAY )
	cValue := SELF:ArrayStructureToString( uValue )
case( IsNil(uValue) )
	 cValue := ""
case( dwType == OBJECT )
	/* Object kann nur als NULL_OBJECT zurückgegeben werden */
	cValue := "NULL_OBJECT"
otherwise
    /* Keine Konvertierung möglich */
    cValue := ""
	SELF:MessageFormat( "Achtung, Konvertierungsfehler in UsualToString Die Variable vom Typ # konnte nicht auf STRING konvertiert werden. Der Varablentyp wird nicht behandelt", { SELF:UsualTypeAsString( uValue ) }, PROT_ART_ERROR, TRUE )
endcase
return( cValue )

METHOD UsualInitialValue( uValue AS USUAL ) AS USUAL PASCAL CLASS P_Base

	LOCAL dwType             AS DWORD

dwType := UsualType( uValue )
do case
case( dwType == STRING )
    uValue := ""
case( dwType == LOGIC )
    uValue := FALSE
case( dwType == SYMBOL )
    uValue := String2Symbol( NewID() )
case( dwType == LONGINT .or. dwType == FLOAT)
	uValue := 0
case( dwType == DATE )
	uValue := CToD( "  .  .  ")
case( IsNil(uValue) )
	//uValue := ""
case( dwType == ARRAY )
	uValue := {}
otherwise
    /* Keine Konvertierung möglich */
	SELF:MessageFormat( "Achtung, Konvertierungsfehler in UsualInitialValue. Die Variable vom Typ # konnte nicht auf Initialwert konvertiert werden. Der Varablentyp wird nicht behandelt", { SELF:UsualTypeAsString( uValue ) }, PROT_ART_ERROR, TRUE )
endcase
return( uValue )


METHOD StringToUnicode( cString AS STRING ) AS STRING PASCAL CLASS P_Base
// Konvertiert einen String in das Unicode-Format (UTF8)
return( DynWide2Utf( VO2DynWideString(cString) )	 )

METHOD StringFromUnicode( cString AS STRING ) AS STRING PASCAL CLASS P_Base
// Konvertiert einen String in das Unicode-Format (UTF8)
return( SELF:StringConvertCodePage( cString, CP_UTF8, CP_ACP) )

METHOD StringConvertCodepage(cString AS STRING, dwCodePageFrom AS DWORD, dwCodePageTo AS DWORD) AS STRING PASCAL CLASS P_Base
// Konvertieren eines Strings von einer Codepage <dwCodePageFrom> in eine Andere <dwCodePageTo>
	LOCAL DIM szBuffer[1024]  AS BYTE
	LOCAL lAlloc := FALSE     AS LOGIC
	LOCAL dwLen               AS DWORD
	LOCAL dwBufLen            AS DWORD
	LOCAL pString := NULL     AS PTR

	dwLen    := SLen(cString)
	dwBufLen := MultiByteToWideChar(dwCodePageFrom, 0, PTR(_CAST, cString), dwLen, NULL, 0) << 1

	IF pString = NULL
		IF dwBufLen > 1024
			pString := MemAlloc(dwBufLen)
			lAlloc := TRUE
		ELSE
			pString := @szBuffer
		ENDIF
	ENDIF

	IF (dwLen := MultiByteToWideChar(dwCodePageFrom, 0, PTR(_CAST, cString), dwLen, pString, dwBufLen) << 1) > 0
		cString := Mem2String(pString, dwLen)
	ELSE
		cString := NULL_STRING
	ENDIF

	IF lAlloc
		MemFree(pString)
	ENDIF

	lAlloc := FALSE
	pString := NULL

	dwLen    := SLen(cString) >> 1
	dwBufLen := WideCharToMultiByte(dwCodePageTo, 0, PTR(_CAST, cString), dwLen, NULL, 0, NULL, NULL)

	IF pString = NULL
		IF dwBufLen > 1024
			pString := MemAlloc(dwBufLen)
			lAlloc := TRUE
		ELSE
			pString := @szBuffer
		ENDIF
	ENDIF

	IF (dwLen := WideCharToMultiByte(dwCodePageTo, 0, PTR(_CAST, cString), dwLen, pString, dwBufLen, NULL, NULL)) > 0
		cString := Mem2String(pString, dwLen)
	ELSE
		cString := NULL_STRING
	ENDIF

	IF lAlloc
		MemFree(pString)
	ENDIF

RETURN cString


METHOD StringFindInArray( aArray AS ARRAY, cString AS STRING ) AS LOGIC PASCAL CLASS P_Base
// Eine einfache Suche des String im Array (Kann mehrdimensional sein). Alle Werte werden mit Upper() und AllTrim() verglichen
return( SELF:ArrayFind( aArray, Upper(AllTrim(cString)), { |search,target| search = Upper(AllTrim(target)) } ) )


METHOD StringDecrement( cString AS STRING, nLen := 1 AS INT, nAlignment := 2 ) AS STRING PASCAL CLASS P_Base
// Verkleinert den String <cString> um <nLen> Stellen
// sa_right   : schneidet rechts ab (Vorschlag)
// sa_left    : schneidet links ab

	LOCAL cReturnString := ""    AS STRING

cReturnString := cString
if( nLen > Len(cString) )
	SELF:MessageFormat( "Fehler in Funktion <StringDecrement> : Der String # mit Länge # soll um # Stellen gekürzt werden. Die Kürzung ist größer als die Länge des Strings.", { cString, Len(cString), nLen }, PROT_ART_ERROR, TRUE )
else
	do case
	case( nAlignment == sa_right )
		cReturnString := Left( cReturnString, Len(cReturnString) - nLen )
	case( nAlignment == sa_left )
		cReturnString := Right( cReturnString, Len(cReturnString) - nLen )
	endcase
endif

return( cReturnString )

METHOD StringCutLeft( cString AS STRING, nLenCutLeft AS INT ) AS STRING PASCAL CLASS P_Base
// Schneidet link <nLenCutLeft> stellen von <cString> weg.
// Beispiel: StringCutLeft( "Hallo André", 5 ) --> " André"
return( SELF:StringDecrement( cString, nLenCutLeft, sa_Left ) )

METHOD StringLength( cString AS STRING, nLength AS INT ) AS STRING PASCAL CLASS P_Base
// Bringt den übergebenen <cString> auf die Länge <nLength>. Entweder durch abschneiden, oder durch anfügen von Spaces
if( Len(cString) > nLength )
	cString := Left( cString, nLength )
elseif( Len(cString) < nLength )
	cString += SELF:StringRepeat( nLength - Len(cString), " " )
endif
return( cString )

METHOD StringAlign( cString AS STRING, nSize AS INT, nAlignment := 0 AS INT ) AS STRING PASCAL CLASS P_Base
// Bringt den übergebenen <cString> durch anfügen von Spaces, bzw. abschneiden auf die
// vorgegebene Größe <nSize>
// StringAlign( "12345678901234567890", 15 )           --> "123456789012345"
// StringAlign( "12345678901234567890", 30 )           --> "12345678901234567890          "
// StringAlign( "12345678901234567890", 30, sa_right ) --> "          12345678901234567890"

	LOCAL cReturnString            AS STRING

if( nSize < Len(cString ))
	// Es muss etwas abgeschnitten werden
	do case
	case( nAlignment == sa_left )
		// "123456789012345"
		cReturnString := Left( cString, nSize )
	case( nAlignment == sa_right )
		// "678901234567890"
		cReturnString := Right( cString, nSize )
	case( nAlignment == sa_middle )
		// "345678901234567"
		cReturnString := SubStr( cString, (Len(cString)/2)-(nSize/2), nSize )
	endcase
else
	// Mit Space auffüllen und einordnen
	do case
	case( nAlignment == sa_left )
		// "1234567890          "
		cReturnString := cString + Replicate( " ", nSize - Len(cString) )
	case( nAlignment == sa_right )
		// "          1234567890"
		cReturnString := Replicate( " ", nSize - Len(cString) ) + cString
	case( nAlignment == sa_middle )
		// "     1234567890     "
		cReturnString := Replicate( " ", (nSize - Len(cString))/2 ) + cString + Replicate( " ", (nSize - Len(cString))/2 )
	endcase
endif
return( cReturnString )

METHOD StringFixPos( cString AS STRING, cVar AS STRING, nPosition AS INT, nAlignment := 0 AS INT ) AS STRING PASCAL CLASS P_Base
// Setzt einen String an bestimmter Position in einem String ab                                      |
// Beispiele:
// StringFixPos( cString, "<Fin>", 20, sa_left )         --> Dies ist das Haus vo<Fin>kolaus und so weiter
// StringFixPos( cString, "<Fin>", 20, sa_right )        --> Dies ist das Ha<Fin>om Nikolaus und so weiter
// StringFixPos( cString, "<Fin>", 20, sa_middle )       --> Dies ist das Haus <Fin>Nikolaus und so weiter
// StringFixPos( "1234567890", "<Fin>", 20, sa_left )    --> 1234567890          <Fin>
// StringFixPos( "1234567890", "<Fin>", 20, sa_right )   --> 1234567890     <Fin>
// StringFixPos( "1234567890", "<Fin>", 20, sa_middle )  --> 1234567890        <Fin>

	LOCAL cReturnString := ""             AS STRING
	LOCAL nVarStartPosition               AS INT

if( (nPosition + Len(cVar)) - Len(cString) > 0 )
	// Wenn nötig, den String erstmal vergrößern
	cString += Replicate( " ", (nPosition + Len(cVar)) - Len(cString) )
endif

do case
case( nAlignment == sa_left )
	//                       |
	// "123456789012345678901Finken890"
	nVarStartPosition := nPosition
case( nAlignment == sa_right )
	//                       |
	// "1234567890123456Finken34567890"
	nVarStartPosition := nPosition - Len(cVar)
case( nAlignment == sa_middle )
	//                       |
	// "1234567890123456789Finken67890"
	nVarStartPosition := nPosition - (Len(cVar)/2)
endcase

if( nVarStartPosition > 0 )
	cReturnString := Left( cString, nVarStartPosition ) + cVar
	if( nVarStartPosition + Len(cVar) <= Len(cString) )
		// Nur, wenn der String vorher größer war
		cReturnString += SubStr( cString, nVarStartPosition + Len(cVar), Len(cString) - (nVarStartPosition + Len(cVar)) )
	endif
else
//	debugPrint( "FIN: ", __ENT, __LINE__, "ERROR, nVarStartPosition <= 0 --> ", nVarStartPosition)
endif
return( cReturnString )

METHOD StringTranslate( cString AS STRING, aReplaces AS ARRAY ) AS STRING PASCAL CLASS P_Base
// Ersetzen verschiendener Zeichen durch andere Zeichen
// StringTranslate( "Höllo maine Freünde", { {"ö","o"}, {"ai","ei"}, {"ü", "u"}, {" ",""}, {CRLF, ""} } ) ---> HollomeineFreunde"
AEval( aReplaces, { |a| cString := StrTran(cString, a[1], a[2]) })
return(cString)

METHOD StringFind( cString AS STRING, cFind AS STRING, nStartPos := 0 AS DWord ) AS INT PASCAL CLASS P_Base
// Sucht einen Teilstring in einem String und gibt die Position zurück. 0, wenn nicht gefunden
return( At3( cFind, cString, nStartPos ) )

METHOD StringBetween( cString AS STRING, cStart AS STRING, cEnd AS STRING ) AS STRING PASCAL CLASS P_Base
// Gibt einen String zwischen zwei Vorkommen zurück. Wurde nichts gefunden, so wird ein leerer String "" zurückgegeben
// StringFind( "[DAT:01]", "[DAT:", "]" ) --> "01"

	LOCAL nStart, nEnd     AS INT
	LOCAL cResult := ""    AS STRING

if( nStart := SELF:StringFind( cString, cStart ) ) != 0
	if( nEnd := SELF:StringFind( cString, cEnd, nStart + Len( cStart ) ) ) != 0
		cResult := SubStr3( cString, nStart + Len( cStart ), nEnd - ( nStart  + Len(cStart) ) )
	endif
endif
return( cResult )

METHOD StringReplace( cString AS STRING, cFind AS STRING, cReplace AS STRING ) AS STRING PASCAL CLASS P_Base
// Suchen und Ersetzen eines Teil-Strings innerhalb eines Strings
return( SELF:StringTranslate( cString, {{ cFind, cReplace }} ) )

METHOD StringMultiReplace( cString AS STRING, aReplaces AS ARRAY ) AS STRING PASCAL CLASS P_Base
// aReplaces := {{ "a","b" }, { "1", "2" }}
// Sucht die Vorkommen (1) und ersetzt diese durch (2)
return( SELF:StringTranslate( cString, aReplaces ) )

METHOD StringTemplate( cMemo AS STRING, aRecordSet AS ARRAY, aDelimiter := NIL AS ARRAY ) AS STRING PASCAL CLASS P_Base
// Ersetzen von Platzhaltern in einem Memo mit den Inhalten aus aRecordSet
// StringTemplate( "Hallo {NAME}. Treffen am {DATUM}", { {#NAME, "Finki"}, {#DATUM, CTOD("19.12.70")} } ) ---> "Hallo Finki, Treffen am 19.12.70"

	LOCAL X            AS INT
	LOCAL aReplaces    AS ARRAY

if( aDelimiter == NIL )
	aDelimiter := { "{","}" }
endif

aReplaces := {}
for x:=1 upto ALen(aRecordSet)
	AAdd( aReplaces, { aDelimiter[1]+Symbol2String(aRecordSet[x][1])+aDelimiter[2], SELF:UsualToString(aRecordSet[x][2]) })
next x
return( SELF:StringTranslate( cMemo, aReplaces ) )

METHOD StringFirstUpper( cString AS STRING ) AS STRING PASCAL CLASS P_Base
// Wandelt einen String in Gross/Kleinschreibung. Der erste Buchstabe jedes Wortes wird groß, alle anderen klein zurückgegeben -> "lOWer und sohN" wird "Lower Und Sohn"
	LOCAL aStrings     AS ARRAY
	LOCAL x            AS INT
aStrings := SELF:StringToArray( cString, { " " } )
for x:=1 upto ALen(aStrings)
	aStrings[x] := IIF( Len(aStrings[x])>0, Upper(Left(aStrings[x],1)) + Lower(Right(aStrings[x], Len(aStrings[x])-1)), aStrings[x] )
next x
return( SELF:ArrayToString( aStrings, " ") )

METHOD StringZero( nValue, nLen, nDec ) AS STRING PASCAL CLASS P_Base
//Ersetzt alle " " durch "0"
return( SELF:StringReplace( Str3( nValue,nLen,nDec), " ","0" ) )

METHOD ArraySerialize( aArray AS ARRAY) AS STRING PASCAL CLASS P_Base
/*

	Wandeln eines Arrays (egal welchen Aufbau und wieviel Dimensionen) in eine XML-Struktur,
	welche das Array komplett beschreibt (mit Type). So kann es in einem String oder in SQL abgelegt
	werden und mit ArrayDeserialize wieder in ein Array gewandelt werden.

	-------------------------------------------------------------------------------------------------------------------------
	Beispiel :
	aArray := {{"Finken", True, 1, 12.5}, {"Meyer", False, 2, 12.3}, #MEY, #UHU, FALSE, {"a","b","c", {1,2,3,nil,NULL_OBJECT}}}
	oBase := P_Base{}
	cXml := oBase:ArraySerialize(aArray)
	debugPrint( "FIN: ", __ENT, __LINE__, cXml )
	aArray2 := oBase:ArrayDeserialize(cXml)
	debugprintArray(aArray)
	debugPrint( "FIN: ", __ENT, __LINE__, "Compare: ", oBase:ArrayCompare( aArray, aArray ) ) -> Liefert TRUE
	oBase:Release()
	-------------------------------------------------------------------------------------------------------------------------
	 <Root>
		<Structure Type="ARRAY" ORDER="1" DEPT="1" INDEX="1" GUID="{4FED4BA9-2CB9-49B8-AA23-C6B9EF8DEA15}">
			<Element Type="STRING" ORDER="2" DEPT="2" INDEX="1" GUID="{E9CCF467-5E3A-49C0-BC11-D8FA853C8988}">Finken</Element>
			<Element Type="LOGIC" ORDER="3" DEPT="2" INDEX="2" GUID="{E9CCF467-5E3A-49C0-BC11-D8FA853C8988}">TRUE</Element>
			<Element Type="INT" ORDER="4" DEPT="2" INDEX="3" GUID="{E9CCF467-5E3A-49C0-BC11-D8FA853C8988}">1</Element>
			<Element Type="REAL" ORDER="5" DEPT="2" INDEX="4" GUID="{E9CCF467-5E3A-49C0-BC11-D8FA853C8988}">12.5</Element>
		</Structure>
		<Structure Type="ARRAY" ORDER="6" DEPT="1" INDEX="1" GUID="{4FED4BA9-2CB9-49B8-AA23-C6B9EF8DEA15}">
			<Element Type="STRING" ORDER="7" DEPT="2" INDEX="1" GUID="{F2115865-6738-4317-8866-DAAB5FD8AA18}">Meyer</Element>
			<Element Type="LOGIC" ORDER="8" DEPT="2" INDEX="2" GUID="{F2115865-6738-4317-8866-DAAB5FD8AA18}">FALSE</Element>
			<Element Type="INT" ORDER="9" DEPT="2" INDEX="3" GUID="{F2115865-6738-4317-8866-DAAB5FD8AA18}">2</Element>
			<Element Type="REAL" ORDER="10" DEPT="2" INDEX="4" GUID="{F2115865-6738-4317-8866-DAAB5FD8AA18}">12.3</Element>
		</Structure>
		<Element Type="SYMBOL" ORDER="11" DEPT="1" INDEX="3" GUID="{4FED4BA9-2CB9-49B8-AA23-C6B9EF8DEA15}">MEY</Element>
		<Element Type="SYMBOL" ORDER="12" DEPT="1" INDEX="4" GUID="{4FED4BA9-2CB9-49B8-AA23-C6B9EF8DEA15}">UHU</Element>
		<Element Type="LOGIC" ORDER="13" DEPT="1" INDEX="5" GUID="{4FED4BA9-2CB9-49B8-AA23-C6B9EF8DEA15}">FALSE</Element>
		<Structure Type="ARRAY" ORDER="14" DEPT="1" INDEX="1" GUID="{4FED4BA9-2CB9-49B8-AA23-C6B9EF8DEA15}">
			<Element Type="STRING" ORDER="15" DEPT="2" INDEX="1" GUID="{08D33CB7-A2E7-4E87-BFAE-DBA051F1C6BE}">a</Element>
			<Element Type="STRING" ORDER="16" DEPT="2" INDEX="2" GUID="{08D33CB7-A2E7-4E87-BFAE-DBA051F1C6BE}">b</Element>
			<Element Type="STRING" ORDER="17" DEPT="2" INDEX="3" GUID="{08D33CB7-A2E7-4E87-BFAE-DBA051F1C6BE}">c</Element>
			<Structure Type="ARRAY" ORDER="18" DEPT="2" INDEX="6" GUID="{08D33CB7-A2E7-4E87-BFAE-DBA051F1C6BE}">
				<Element Type="INT" ORDER="19" DEPT="3" INDEX="1" GUID="{22734FA8-A71F-49AA-928C-1933BDD0389F}">1</Element>
				<Element Type="INT" ORDER="20" DEPT="3" INDEX="2" GUID="{22734FA8-A71F-49AA-928C-1933BDD0389F}">2</Element>
				<Element Type="INT" ORDER="21" DEPT="3" INDEX="3" GUID="{22734FA8-A71F-49AA-928C-1933BDD0389F}">3</Element>
				<Element Type="NIL" ORDER="22" DEPT="3" INDEX="4" GUID="{22734FA8-A71F-49AA-928C-1933BDD0389F}"></Element>
				<Element Type="NULL_OBJECT" ORDER="23" DEPT="3" INDEX="5" GUID="{22734FA8-A71F-49AA-928C-1933BDD0389F}"></Element>
			</Structure>
		</Structure>
	</Root>
*/
	LOCAL nOrder       AS INT

nOrder := 0
return( "<Root>" + CRLF + SELF:__ArraySerialize( aArray, 0, @nOrder,1, NewID() ) + "</Root>"+CRLF )

PROTECT METHOD __ArraySerialize( aArray AS ARRAY, nDept AS INT, nOrder REF INT, nIndex AS INT, cGuid AS STRING ) AS STRING PASCAL CLASS P_Base

 	LOCAL cXML := ""          AS STRING
	LOCAL x                   AS INT

nDept += 1
nOrder += 1

for x:= 1 upto aLen(aArray)
	do case
	case( UsualType(aArray[x]) == ARRAY )
		cXml += '<Structure Type="ARRAY" ORDER="'+NTrim(nOrder)+'" DEPT="'+NTrim(nDept)+'" INDEX="'+NTrim(nIndex)+'" GUID="'+cGuid+'">'+CRLF
		cXML += SELF:__ArraySerialize( aArray[x], nDept, @nOrder, x, NewID() )
		cXml += "</Structure>"+CRLF
	otherwise
		// <Element Type="STRING">Meyer</Element>
		cXml += '	<Element Type="' + SELF:UsualTypeAsString( aArray[x] ) + '"' +;
		' ORDER="'+NTrim(nOrder)+'" DEPT="'+NTrim(nDept)+'" INDEX="'+NTrim(x)+'" GUID="'+cGuid+'">' +;
			 SELF:StringToXml(SELF:UsualToString( aArray[x] )) +;
			 '</Element>' + CRLF
		nOrder++
	endcase
next x
return( cXML )

METHOD ArrayDeserialize( cXml AS STRING ) AS ARRAY PASCAL CLASS P_Base
// Geht nur mit XML, welches über ArrayToXml erzeugt wurde.
// Es werden auch die ursprünglichen Typen (STRING; INT; LOGIC; etc.) wiederhergestellt

	LOCAL aArray                AS ARRAY
	LOCAL oStmt                 AS ASqlStatement
	LOCAL cNodeName, cNodeType  AS STRING
	LOCAL cNodeValue, cGuid     AS STRING
	LOCAL nOrder, nDept, nIndex AS INT
	LOCAL aXML                  AS ARRAY

aXML := {}
oStmt := SELF:CreateSqlStatement( "select RowNumber, NodeName, NodeType, XPath, Value from [ufn_XmlToTable] ('"+cXml+"')" , "Fehler beim wandeln von XML-Daten via SQL")
if( oStmt != NULL_OBJECT )
	// Skip <Root>
	oStmt:Fetch()
	while( oStmt:Fetch() )
		cNodeName  := oStmt:FGetN(#NodeName)
		cNodeValue := SELF:StringFromXml(oStmt:FGetN(#Value))
		oStmt:Fetch()
		cNodeType := oStmt:FGetN(#Value)
		oStmt:Fetch()
		nOrder := Val(oStmt:FGetN(#Value))
		oStmt:Fetch()
		nDept := Val(oStmt:FGetN(#Value))
		oStmt:Fetch()
		nIndex := Val(oStmt:FGetN(#Value))
		oStmt:Fetch()
		cGuid := oStmt:FGetN(#Value)
		aadd( aXml, { cNodeName, cNodeType, cNodeValue, nOrder, nDept, nIndex, cGuid })
	enddo
	oStmt:Release()

    aXml := aSort( aXml,,, { |a,b| a[4] <= b[4] } )
	debugprintarray( aXml )
    /*
    	Wir haben nun fogenden Aufbau:
    	{{Name, Type, Value, Order, Dept, Index, Guid},...

		 01:A[23]
		   |__ 01:A[7]
		   |     |__ 01:C[9] = "Structure"
		   |     |__ 02:C[5] = "ARRAY"
		   |     |__ 03:C[0] = ""
		   |     |__ 04:N = 1
		   |     |__ 05:N = 1
		   |     |__ 06:N = 1
		   |     |__ 07:C[38] = "{4FED4BA9-2CB9-49B8-AA23-C6B9EF8DEA15}"
		   |__ 01:A[7]
		   |     |__ 01:C[7] = "Element"
		   |     |__ 02:C[6] = "STRING"
		   |     |__ 03:C[6] = "Finken"
		   |     |__ 04:N = 2
		   |     |__ 05:N = 2
		   |     |__ 06:N = 1
		   |     |__ 07:C[38] = "{E9CCF467-5E3A-49C0-BC11-D8FA853C8988}"
		   |__ 01:A[7]
		   |     |__ 01:C[7] = "Element"
		   |     |__ 02:C[5] = "LOGIC"
		   |     |__ 03:C[4] = "TRUE"
		   |     |__ 04:N = 3
		   |     |__ 05:N = 2
		   |     |__ 06:N = 2
		   |     |__ 07:C[38] = "{E9CCF467-5E3A-49C0-BC11-D8FA853C8988}"
		   |__ 01:A[7]
		   |     |__ 01:C[7] = "Element"
		   |     |__ 02:C[3] = "INT"
		   |     |__ 03:C[1] = "1"
		   |     |__ 04:N = 4
		   |     |__ 05:N = 2
		   |     |__ 06:N = 3
		   |     |__ 07:C[38] = "{E9CCF467-5E3A-49C0-BC11-D8FA853C8988}"
	*/

	aArray := SELF:__ArrayDeserialize( 1, aXml )
endif

return( aArray )

METHOD __ArrayDeserialize( nOrder AS INT, aXml AS ARRAY) AS ARRAY PASCAL CLASS P_Base

	LOCAL _NodeName_, _NodeType_, _NodeValue_, _NodeOrder_, _NodeDept_, _NodeIndex_, _NodeGuid_ AS INT
	LOCAL aArray          AS ARRAY
	LOCAL aContent        AS ARRAY
    LOCAL x := 0          AS INT
    LOCAL cGuid           AS STRING

	/* Konstanten */
	_NodeName_  := 1
	_NodeType_  := 2
	_NodeValue_ := 3
	_NodeOrder_ := 4
	_NodeDept_  := 5
	_NodeIndex_ := 6
	_NodeGuid_  := 7

aArray := {}

if( nOrder <= aLen( aXml ) )
	cGuid := aXml[nOrder][_NodeGuid_]
	aContent := {}
	/* Alle passenden Einträge zur Guid */
	for x:=1 upto aLen(aXml)
		if( aXml[x][_NodeGuid_] == cGuid )
			aadd( aContent, aXml[x] )
		endif
	next x

	/* Nur wenn min. 1 Element in der Struktur ist */
	if( aLen( aContent ) >= 1 )
		aSize( aArray, aLen(aContent) )

		for x:=1 upto aLen(aContent)
			if( aContent[x][_NodeName_] == "Structure" )
				aArray[x] := SELF:__ArrayDeserialize( aContent[x][_NodeOrder_]+1, aXml )
			else
				aArray[x] := SELF:StringToUsual( SELF:StringFromXml(aContent[x][_NodeValue_]), aContent[x][_NodeType_] )
			endif
		next x
	endif
endif

return( aArray )

METHOD StringToXml( cString AS STRING ) AS STRING PASCAL CLASS P_Base
// Nimmt die Sonderzeichen aus dem String und mach XML-Konforme Ausdrücke davon
return( SELF:StringTranslate( cString, {{"&", "&amp;"},{"'", "&apos;"},{"<", "&lt;"}, {">","&gt;"}, {CHR(34), "&quot;"}, {"Ä","&#196;"}, {"Ö", "&#214;"}, {"Ü", "&#220;"}, {"ä", "&#228"}, {"ö","&#246;"}, {"ü","&#252;"}, {"ß","&#223;"}} ) )

METHOD StringFromXml( cXmlString AS STRING ) AS STRING PASCAL CLASS P_Base
// Wandelt die XML-Konformen Ausdrücke wieder zurück
return( SELF:StringTranslate( cXmlString, {{"&amp;","&"},{"&apos;","'"},{"&lt;","<"}, {"&gt;",">"}, {"&quot;", CHR(34)}, {"&#196;", "Ä"}, {"&#214;","Ö"}, {"&#220;","Ü"}, {"&#228","ä"}, {"&#246;","ö"}, {"&#252;","ü"}, {"&#223;","ß"}} ) )

METHOD StringRepeat( nRepeatTimes AS INT, cString AS STRING ) AS STRING PASCAL CLASS P_Base
// Dupliziert den <cString> <nRepeatTimes> und gibt den neuen String zurück
return( Replicate( cString, nRepeatTimes ) )

METHOD StringSeekBackward( cString AS STRING, cSearchValue AS STRING, nOffset AS INT, lCaseSensitive := FALSE AS LOGIC ) AS INT PASCAL CLASS P_Base
// Sucht im <cString> von der Position <nOffset> Rückwärts nach <cSearchValue> und gibt
// die Position zurück. Wird <cSearchString nicht gefunden, wird 0 zurück gegeben.
// Mit <lCaseSensitive> kann angegeben werden, ob Groß-/Kleinschreibung berücksichtigt werden soll.

if( !lCaseSensitive )
	cString      := Upper( cString )
	cSearchValue := Upper( cSearchValue )
endif
return( RAt(cSearchValue, Left(cString,nOffset)) )

METHOD StringDeleteEmptyLines( cString AS STRING, lStart := TRUE AS LOGIC, lEnd := TRUE AS LOGIC, lEveryLine := FALSE AS LOGIC ) AS STRING PASCAL CLASS P_Base
// Befreit einen mehrzeiligen String von Leerzeilen
// [lStart]     : TRUE : Löscht Leerzeilen am Anfang des Strings
// [lEnde]      : TRUE : Löscht Leerzeilen am Ende des Strings
// [lEveryLine] : FALSE: Löscht auch Zeilen in der Mitte des Strings, welche von befüllten Zeilen umgeben sind

	LOCAL nRow                     AS INT
	LOCAL aString                  AS ARRAY
	LOCAL lFirstCharFound := FALSE AS LOGIC
	LOCAL aResultString            AS ARRAY
	LOCAL lDeleteLine              AS LOGIC

if( !Empty(cString) )
	aResultString := {}
	aString := SELF:MemoToArray( cString )
	for nRow := 1 upto ALen( aString )
		lDeleteLine := FALSE
		if( !Empty( aString[nRow] ) )
			lFirstCharFound := TRUE
		endif

		if( Empty( aString[nRow] ) )
			do case
			case( lEveryLine )
				// Jede leere Zeile löschen
				lDeleteLine := TRUE
			case( lStart .and. !lFirstCharFound )
				// Der String soll am Anfang von leeren Zeilen befreit werden
				lDeleteLine := TRUE
			endcase
		endif

		if( !lDeleteLine )
			AAdd( aResultString, aString[nRow] )
		endif
	next nRow

	if( lEnd )
		// Leere Zeilen am Ende löschen
		lFirstCharFound := FALSE
		for nRow := ALen(aResultString) downto 1
			if( !Empty( aResultString[nRow] ) )
				exit
			endif
			ADel( aResultString, nRow )
			ASize( aResultString, ALen(aResultString) - 1)
	  	next nRow
	endif

	cString := SELF:ArrayToString( aResultString, CRLF )
endif
return( cString )

METHOD MemoToArray( cMemo AS STRING ) AS ARRAY PASCAL CLASS P_Base

	LOCAL aArray            AS ARRAY
	LOCAL x                 AS INT

aArray := {}
for x:=1 upto MLCount(cMemo, 254)
	AAdd( aArray, MemoLine(cMemo, 254, x)	)
next x
return( aArray )


METHOD DbgMessage( cMessage AS STRING, aValues := nil AS ARRAY, lFrame := FALSE AS LOGIC ) AS VOID PASCAL CLASS P_Base

if( !IsNil(aValues) )
	cMessage := SELF:StringFormat( cMessage, aValues )
endif

if( lFrame )
	SELF:dbg(dbg_Line)
endif
SELF:dbg(cMessage)
if( lFrame )
	SELF:dbg(dbg_Line)
endif

METHOD DbgArray( aArray AS ARRAY, cHeader := "" AS STRING, lFrame := FALSE AS LOGIC ) AS VOID PASCAL CLASS P_Base

lFrame := !Empty(cHeader)

if( lFrame )
	SELF:dbg(dbg_Line)
endif
if( !Empty( cHeader ) )
	SELF:dbg( SELF:StringAlign( cHeader, Len(dbg_Line), sa_middle ) )
	if( lFrame )
		SELF:dbg(dbg_Line)
	endif
endif

SELF:dbg(aArray)

if( lFrame )
	SELF:dbg(dbg_Line)
endif

METHOD DbgRecord( oRecord AS USUAL, cHeader := "" AS STRING, lFrame := FALSE AS LOGIC ) AS VOID PASCAL CLASS P_Base

	LOCAL aArray            AS ARRAY
	LOCAL x                 AS INT

aArray := SELF:ArrayFromRecord( oRecord )
for x:=1 upto ALen(aArray)
	// Hier noch den Typ von Value mit ausgeben
	AAdd( aArray[x], SELF:UsualTypeAsString( aArray[x][2] ) )
next x
SELF:DbgArray( aArray, cHeader, lFrame )

METHOD dbg( uMessage as Usual, lDisplayAnyway := FALSE AS LOGIC ) AS VOID PASCAL CLASS P_Base

if( SELF:lDebugMode .or. lDisplayAnyway )
	if( UsualType(uMessage) == ARRAY )
		debugPrintArray(uMessage)
	else
		debugPrint( "FIN: (P_Base) ", __ENT, __LINE__, uMessage )
	endif
endif

//
// Verzeichnis-Handling
//
METHOD GetAmsBinPath() AS STRING PASCAL CLASS P_Base
// Gibt den Pfad zum ams-Bin-Verzeichnis zurück
// Format: "p:\bearbeitung\lhr32\bin\"

	LOCAL DIM pszFilePath[MAX_PATH]  AS BYTE
	LOCAL cFilePath                  AS STRING

cFilePath := ""
if( GetModuleFileName(_GetInst(), @pszFilePath, MAX_PATH) != 0 )
	cFilePath := Psz2String( @pszFilePath )
	cFilePath := SELF:StringReplace( Lower(cFilePAth), "backend.dll", "" )
	cFilePath := SELF:FolderPrepare( cFilePath )
else
	SELF:MessageFormat( "Fehler bei der Ermittlung des ams-Verzeichnisses. _GetInst() = #", { _GetInst() }, PROT_ART_ERROR, TRUE )
endif
return( cFilePath )

METHOD FolderPrepare( cPath AS STRING ) AS STRING PASCAL CLASS P_Base
// Add a backslash at the end of the folderame, if not exists

	LOCAL oPath       AS AFileSpec

oPath := AFileSpec{cPath}
cPath := oPath:FullPath
if( Right(cPath,1) != "\" )
	cPath += "\"
endif
oPath:Release()
RETURN cPath

METHOD FolderUnPrepare( cFolder AS STRING) AS STRING PASCAL CLASS P_Base
// Take away the backslashat the end of the Foldername, if exists
cFolder := AllTrim(cFolder )
IF( Right(cFolder,1) == "\" )
     cFolder := SubStr( cFolder, 1, Len(cFolder)-1)
ENDIF
RETURN cFolder

METHOD FolderExists( cFolder AS STRING ) AS LOGIC PASCAL CLASS P_Base
// Check, if the folder exists
RETURN (DirChange(String2Psz(SELF:FolderUnPrepare(cFolder))) == 0)

METHOD FolderCreate( cFolder AS STRING ) AS LOGIC PASCAL CLASS P_Base
// Create Folder
  LOCAL lFolderCreated := TRUE      AS LOGIC
  LOCAL siCode                       AS SHORTINT

siCode := DirMake(String2Psz(cFolder))
IF( siCode != 0 )
     SELF:Message(SELF:StringFormat("Das Verzeichnis # kann nicht erstellt werden. DOS-Fehler: #", {AllTrim(cFolder),DosErrString(siCode)}), PROT_ART_ERROR)
     lFolderCreated := FALSE
ENDIF
RETURN lFolderCreated

METHOD GetFilesFromDir( cPath AS STRING, cPattern := "*.*" AS STRING, lSubDirectories := FALSE AS LOGIC, cbCodeBlock := nil AS USUAL ) AS ARRAY PASCAL CLASS P_Base
// Ermittelt alle Dateien aus dem <cPath>, welche dem Suchpattern <cPattern> entsprechen und gibt diese im Format { "D:\PATH\FILENAME.EXT", "D:\PATH\FILENAME.EXT", .. } als Array zurück
// Verzeichnisse werden nicht zurückgegeben. Bei [lSubDirectories] = TRUE werden auch Unterverzeichnisse durchsucht
// StandardCodeblock: { |cFileName, cAttribute, nSize, dDate, cTime| TRUE }
	LOCAL aFiles, aTemp      AS ARRAY
	LOCAL aSubFiles          AS ARRAY
	LOCAL x                  AS INT

aFiles := {}
cPath := SELF:FolderPrepare( cPath )
if( SELF:FolderExists( cPath ) )
	cbCodeBlock := SELF:PrepareCodeBlock( cbCodeBlock, { |cFileName, cAttribute, nSize, dDate, cTime| TRUE } )
	aTemp := Directory( cPath + cPattern, "D" )

	for x:=1 upto ALen( aTemp )
		if( Left( aTemp[x][F_NAME],1 ) = "." )
			// Hier nix tun, sind "." und ".." Verzeichnisse
		elseif( At("D", aTemp[x][F_ATTR]) > 0  )
			// Verzeichnisse werden weiter unten abgehandelt
		else
			//
			// Dateien
			//
			if( Eval( cbCodeBlock, aTemp[x][F_NAME], aTemp[x][F_ATTR], aTemp[x][F_SIZE], aTemp[x][F_DATE], aTemp[x][F_TIME] ) )
				AAdd( aFiles, cPath + aTemp[x][F_NAME] )
			endif
		endif
	next x

	if( lSubDirectories )
		//
		// Verzeichnisse
		//
		aTemp := SELF:GetFolderFromDir( cPath, TRUE )
		for x:=1 upto ALen( aTemp )
			aSubFiles := SELF:GetFilesFromDir( aTemp[x], cPattern, TRUE, cbCodeBlock )
			aFiles := SELF:ArrayCombine( aFiles, aSubFiles )
		next x
	endif
else
	SELF:MessageFormat( "Der Ordner # existiert nicht. Es wurden keine Dateien ermittelt. ", {AllTrim(cPath)}, PROT_ART_ERROR)
endif
return( aFiles )

METHOD GetFolderFromDir( cPath AS STRING, lSubDirectories := FALSE ) AS ARRAY PASCAL CLASS P_Base

	LOCAL aFolder, aTemp      AS ARRAY
	LOCAL aSubFolder          AS ARRAY
	LOCAL x                   AS INT

aFolder := {}
if( SELF:FolderExists( cPath ) )
	cPath := SELF:FolderPrepare( cPath )
	aTemp := Directory( cPath + "*.*", "D" )
	for x:=1 upto ALen(aTemp)
		if( At("D", aTemp[x, F_ATTR]) > 0 )
			if( Left( aTemp[x][F_NAME],1 ) != "." )
				// NUr wenn es ein Verzeichnis ist
	         AAdd( aFolder, cPath + aTemp[x][F_NAME] )

	         if( lSubDirectories )
	         	aSubFolder := SELF:GetFolderFromDir( cPath + aTemp[x][F_NAME], lSubDirectories )
	         	aFolder := SELF:ArrayCombine( aFolder, aSubFolder )
	         endif
			endif
		endif
	next x
else
	SELF:MessageFormat( "Der Ordner # existiert nicht. Es wurden keine Dateien ermittelt. ", {AllTrim(cPath)}, PROT_ART_ERROR)
endif
return( aFolder )


METHOD FolderCopy( cSourceFolder AS STRING, cTargetFolder AS STRING, lCopyRecursive AS LOGIC, cbCodeblock := nil AS USUAL ) AS LOGIC PASCAL CLASS P_Base
// Kopiert alle Dateien eines Verzeichnisses (und deren Unterverzeichnisse <lCopyRecursive) in ein neues Verzeichnis
// Druch den Codeblock kann eingeschränkt werden, welche Dateien und Verzeichnisse kopiert werden sollen. Gibt der Codeblock TRUE zurück, wird die Datei kopiert.
// Standard: Alle kopieren : { |cFileName, lDirectory, cAttribute, nSize, dDate, cTime| TRUE }
     LOCAL lSuccess := TRUE          AS LOGIC
     LOCAL aFiles                    AS ARRAY
     LOCAL x                         AS INT
     LOCAL cFile                     AS STRING
     LOCAL lDir                      AS LOGIC

IF( SELF:FolderExists( cSourceFolder ) )

     IF( !SELF:FolderExists( cTargetFolder ))
          lSuccess := SELF:FolderCreate( cTargetFolder)
     ELSE
			 SELF:Message( SELF:StringFormat( "Das Zielverzeichnis # existiert bereits. Kopieren des Verzeichnisses # nach # wurde abgebrochen.",{cTargetfolder,cSourceFolder, cTargetfolder}), PROT_ART_ERROR )
          lSuccess := FALSE
     ENDIF

     IF( lSuccess )
   		cbCodeblock := SELF:PrepareCodeblock( cbCodeblock, { |cFileName, lDirectory, cAttribute, nSize, dDate, cTime| TRUE} )
			debugPrint( "FIN: ", __ENT, __LINE__, "vorher", cSourcefolder)
	       cSourceFolder := SELF:FolderPrepare( cSourceFolder )
          cTargetFolder := SELF:FolderPrepare( cTargetFolder )
			debugPrint( "FIN: ", __ENT, __LINE__, "nachher", cSourcefolder)

          aFiles := Directory( cSourceFolder + "*.*", "D" )
          FOR x:=1 UPTO ALen( aFiles )

           cFile := aFiles[x, F_NAME]
           lDir  := At("D", aFiles[x, F_ATTR]) > 0

               IF( lDir .and. Left(cFile,1) = "." )
                    // Don´t copy this: "." or ".."
               ELSE
                    IF( lDir )
                         IF( lCopyRecursive )
									if( Eval( cbCodeblock, aFiles[x][F_NAME], lDir, aFiles[x][F_ATTR], aFiles[x][F_SIZE], aFiles[x][F_DATE], aFiles[x][F_TIME] ) )
										debugPrint( "FIN: ", __ENT, __LINE__, "Foldercopy", cSourceFolder, cFile)
	                         	lSuccess := SELF:FolderCopy( cSourceFolder + cFile, cTargetFolder + cFile, lCopyRecursive )
									endif
                         ENDIF
                    ELSE
								if( Eval( cbCodeblock, aFiles[x][F_NAME], lDir, aFiles[x][F_ATTR], aFiles[x][F_SIZE], aFiles[x][F_DATE], aFiles[x][F_TIME] ) )
	                   		lSuccess := SELF:FileCopy( cSourceFolder + cFile, cTargetFolder + cFile )
	                   	endif
                    ENDIF
             ENDIF

               // QUICK EXIT
               // ----------
             IF( !lSuccess )
             	debugPrint( "FIN: ", __ENT, __LINE__, "ACHTUNG! QUICK EXIT")
               return( FALSE )
             ENDIF

          NEXT x
     ENDIF
ELSE
	lSuccess := FALSE
	SELF:MessageFormat( "Der Ordner # existiert nicht und kann nicht kopiert werden. ", {AllTrim(cSourceFolder)}, PROT_ART_ERROR)
ENDIF

RETURN ( lSuccess )

METHOD FileWrite( cFileName AS STRING, cString AS STRING ) AS LOGIC PASCAL CLASS P_Base

     LOCAL lExported := FALSE       AS LOGIC
     LOCAL hFWrite                  AS PTR

IF( SELF:FileExists( cFileName ) )
     hfWrite := FOpen2(cFileName, FO_READWRITE)
     FSeek3(hfWrite, 0, FS_END) // Append
ELSE
     hfWrite := FCreate(cFileName)
ENDIF

IF( hfWrite != f_ERROR )
     FWriteLine(hfWrite, cString)
     lExported := TRUE
     FClose( hfWrite )
ENDIF
RETURN lExported

METHOD FileExists( cFileName AS STRING ) AS LOGIC PASCAL CLASS P_Base
RETURN File(cFileName)

METHOD FileDelete( cFileName AS STRING ) AS LOGIC PASCAL CLASS P_Base

     LOCAL lDeleted := FALSE      AS LOGIC

IF( SELF:FileExists( cFileName ) )
	 DeleteFile( String2Psz(cFileName) )
    IF( SELF:FileExists( cFileName ) )
			SELF:MessageFormat( "Die Datei # kann nicht gelöscht werden: #", {cFileName, DosErrString(FError())}, PROT_ART_WARNING )
		ELSE
		lDeleted := TRUE
    ENDIF
ELSE
	SELF:MessageFormat( "Die Datei # wurde nicht gefunden und kann nicht gelöscht werden", {cFileName}, PROT_ART_WARNING )
ENDIF

RETURN lDeleted

METHOD FileCopy( cSourceFile AS STRING, cTargetFile AS STRING ) AS LOGIC PASCAL CLASS P_Base

     LOCAL lCopied := FALSE     AS LOGIC
     LOCAL cTargetPath          AS STRING

cTargetPath := SELF:GetPathNameFromPath(cTargetFile)
FErase( cTargetFile ) // String2Psz(cTargetFile) )
if( SELF:FolderExists( cTargetPath ) )
	IF( SELF:FileExists( cSourceFile ) )
			debugPrint( "FIN: ", __ENT, __LINE__, "TARGET", cTargetFile)
	     lCopied := FCopy( cSourceFile, cTargetFile )
	     IF( !lCopied )
				SELF:MessageFormat( "Die Datei # konnte nicht kopiert werden. Fehler: #", {cSourceFile, DosErrString(FError())}, PROT_ART_WARNING )
	     ENDIF
	ELSE
			SELF:MessageFormat( "Die Datei # wurde nicht gefunden und konnte somit nicht kopiert werden. Fehler: #", {cSourceFile, DosErrString(FError())}, PROT_ART_WARNING )
	ENDIF
else
	SELF:MessageFormat( "Das Zielverzeichnis # ist nicht vorhanden", { cTargetPath }, PROT_ART_ERROR )
endif
RETURN lCopied

METHOD FileMove( cSourceFile AS STRING, cTargetFile AS STRING) AS LOGIC PASCAL CLASS P_Base

     LOCAL lMoved := FALSE     AS LOGIC

if(lMoved := SELF:FileCopy( cSourceFile, cTargetFile ))
	lMoved := SELF:FileDelete( cSourceFile )
endif
return( lMoved )

METHOD FileRename( cOldFileName AS STRING, cNewFileName AS STRING ) AS LOGIC PASCAL CLASS P_Base
return( SELF:FileMove( cOldFileName, cNewFileName ) )

METHOD FileReadToArray( cFileName AS STRING, lReadEmptyLines := TRUE AS LOGIC, lUnicode := FALSE AS LOGIC,  lDOSFormat := TRUE AS LOGIC ) AS ARRAY PASCAL CLASS P_Base
// List die Datei <FileName> und gibt jede Zeile als Array zurück
// <cFileName>       : Verzeichnis und Dateiname
// [lReadEmptyLines] : Bei True werden auch Leerzeilen mitgelesen. Bei False werden diese ausgelassen
// [lUnicode]        : Wenn es sich um eine Unicode-DAtei handelt, kann automatisch eine Umwandlung stattfinden
return(SELF:StringToArray( SELF:FileReadToString( cFileName, lReadEmptyLines, lUnicode, lDOSFormat ), {CRLF} ))

METHOD FileReadToString( cFileName AS STRING, lReadEmptyLines := TRUE AS LOGIC, lUnicode := FALSE AS LOGIC, lDOSFormat := TRUE AS LOGIC) AS STRING PASCAL CLASS P_Base
// List die Datei <FileName> und gibt den Inhalt als String zurück.
// <cFileName>       : Verzeichnis und Dateiname
// [lReadEmptyLines] : Bei True werden auch Leerzeilen mitgelesen. Bei False werden diese ausgelassen
// [lUnicode]        : Wenn es sich um eine Unicode-DAtei handelt, kann automatisch eine Umwandlung stattfinden

	LOCAL hFRead             AS PTR
	LOCAL cString := ""      AS STRING
	LOCAL cLine              AS STRING
	LOCAL nLen               AS INT

hFRead := FOpen2(cFileName, FO_READ + FO_EXCLUSIVE)
IF( hfRead != f_ERROR )
   do while( !FEof(hfRead) )
		cLine := ""
		nLen := FRead(hfRead, @cLine, 4096)
		if( nLen>0 )
			cString += Left(cLine,nLen)
      endif
   enddo
	FClose( hfRead )

	if( lDOSFormat )
		cString := Oem2Ansi( cString )
	endif
else
	SELF:MessageFormat( "Fehler beim importieren der Datei #. Unicode-Format=#, DOS-Format=#. Fehler: #", {cFileName, lUnicode, lDOSFormat, DosErrString(FError())} , PROT_ART_ERROR)
endif
return( cString )

METHOD FileWriteFromArray( cFileName AS STRING, aRows AS ARRAY, lUnicode := FALSE AS LOGIC, lDOSFormat := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
return( SELF:FileWriteFromString( cFileName, SELF:ArrayToString( aRows, CRLF ), lUnicode, lDOSFormat ) )

METHOD FileWriteFromString( cFileName AS STRING, cString AS STRING, lUnicode := FALSE AS LOGIC, lDOSFormat := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_Base
// Exportiert einen String <cString> in die Datei <cFileName>. Wird [lUnicode] mit True angegeben, so findet vorher
// für den String eine Konvertierung in UniCode statt
   LOCAL hFWrite                  AS PTR
	LOCAL pszLine                  AS PSZ
	LOCAL lExported := FALSE       AS LOGIC

IF( SELF:FileExists( cFileName ) )
	SELF:MessageFormat( "Fehler beim exportieren der Datei #. Die Datei # existiert bereits am Zielort.", { cFileName, cFileName }, PROT_ART_ERROR )
ELSE
   hfWrite := FCreate(cFileName)
   IF( hfWrite != f_ERROR )
		pszLine := String2Psz( IIF( lUnicode, SELF:StringToUnicode( cString ), cString) )
		FWrite4( hfWrite, pszLine, PszLen(pszLine), lDOSFormat )
    	FClose( hfWrite )
		lExported := TRUE
    ELSE
		SELF:MessageFormat( "Fehler beim exportieren der Datei #. Fehler: #" , {cFileName, DosErrString(FError())}, PROT_ART_ERROR )
    ENDIF
endif
return(lExported)

METHOD WriteLine( hfWrite AS PTR, cLine AS STRING, lDOSFormat := TRUE AS LOGIC, lAddCRLF := TRUE ) AS DWORD PASCAL CLASS P_Base
// Write back a <cLine> string to an open File <hfWrite> in Ansi(Dos)-Format.
// If <lAnsiFormat> is False, OEM-Format will be written

     LOCAL pszLine             AS PSZ

PSZLine := String2Psz( cLine + IIF( lAddCRLF, CRLF,"" ) )
RETURN FWrite4(hfWrite, PSZLine, PszLen(PSZLine), lDosFormat )

METHOD WriteLineUTF8( hfWrite AS PTR, cLine AS STRING, lAnsiFormat := TRUE AS LOGIC, lAddCRLF := TRUE  ) AS DWord PASCAL CLASS P_Base
// Einen normalen VO-String in einer UTF-8 Datei ausgeben (Umlaute)
return( SELF:WriteLine(hfWrite, DynWide2Utf(VO2DynWideString(cLine)), lAnsiFormat, lAddCRLF ) )

METHOD GetFileNameFromPath( cPathAndFileName AS STRING, cbCodeblock := nil AS USUAL ) AS STRING PASCAL CLASS P_Base
// c:\temp\finken.xml --> finken.xml
// c:\finken.xml      --> finken.xml

	LOCAL oFile           AS AFileSpec
	LOCAL cFileName       AS STRING

cbCodeblock := SELF:PrepareCodeblock( cbCodeblock, { |Drive, Path, FileName, Extension, FullPath| FileName + Extension } )
SELF:_CheckPath( cPathAndFileName )
oFile:=aFileSpec{cPathAndFileName}
cFileName := Eval(cbCodeBlock, oFile:Drive, oFile:Path, oFile:FileName, oFile:Extension, oFile:FullPath )
oFile:Release()
return( cFileName )

METHOD GetPathNameFromPath( cPathAndFileName AS STRING, cbCodeblock := nil AS USUAL ) AS STRING PASCAL CLASS P_Base
// c:\temp\finken.xml --> c:\temp\
// c:\finken.xml      --> c:\
	LOCAL oFile           AS AFileSpec
	LOCAL cPathName       AS STRING

cbCodeblock := SELF:PrepareCodeblock( cbCodeblock, { |Drive, Path, FileName, Extension, FullPath| Drive + Path } )
SELF:_CheckPath( cPathAndFileName )
oFile:=aFileSpec{cPathAndFileName}
cPathName := Eval(cbCodeBlock, oFile:Drive, oFile:Path, oFile:FileName, oFile:Extension, oFile:FullPath )
//cPathName := eVal( cbCodeBlock, oFile )
oFile:Release()
return( cPathName )


METHOD AddPath( cPathAndFileName AS STRING, cAddPath AS STRING, cbCodeblock := nil AS USUAL ) AS STRING PASCAL CLASS P_BASE
// Fügt einen Pfad an einen vorhandenen Pfad an.
// AddPath( "c:\temp\finken.xml", "Archiv" ) --> "c:\temp\Archiv\finken.xml"
// AddPath( "c:\temp\", "Archiv" ) --> "c:\temp\Archiv\"
// Standardrückgabe: { |Drive, Path, FileName, Extension, FullPath| Drive + Path + FileName + Extension }

	LOCAL oFile           AS AFileSpec
	LOCAL cPathName       AS STRING

cbCodeblock := SELF:PrepareCodeblock( cbCodeblock, { |Drive, Path, FileName, Extension, FullPath| Drive + Path + FileName + Extension } )
SELF:_CheckPath( cPathAndFileName )
SELF:_CheckPath( cAddPath )
oFile:=aFileSpec{cPathAndFileName}
oFile:AppendToPath( cAddPath )
cPathName := Eval(cbCodeBlock, oFile:Drive, oFile:Path, oFile:FileName, oFile:Extension, oFile:FullPath )
oFile:Release()
return( cPathName )

PROTECT METHOD _CheckPath( cPath AS STRING ) AS LOGIC PASCAL CLASS P_Base
if( SELF:StringFind( cPath, " " ) != 0 )
	SELF:MessageFormat( "Achtung, im Pfad/Dateinamen # sind Leerzeichen vorhanden. Evtl. findet keine korrekte Ausführung statt", { cPath }, PROT_ART_WARNING )
	return( false )
endif
return( true )

METHOD BuildFileName( cFileName AS STRING, cbCodeBlock AS CODEBLOCK ) AS STRING PASCAL CLASS P_Base
// Baut den Dateinamen <FileName> mit <cbCodeblock> zusammen und gibt diesen zurück
// BuildFileName( "c:\temp\finken.xml", { |D,P,F,E,FP| D + P + "NeuerDateiname" + E } ) --> c:\temp\NeuerDateiname.xml
// Standardübergabe: { |Drive, Path, FileName, Extension, FullPath| Drive + Path + FileName + Extension }

	LOCAL oFile           AS AFileSpec

cbCodeblock := SELF:PrepareCodeblock( cbCodeblock, { |Drive, Path, FileName, Extension, FullPath| Drive + Path + FileName + Extension } )
SELF:_CheckPath( cFileName )
oFile:=aFileSpec{cFileName}
cFileName := Eval(cbCodeBlock, oFile:Drive, oFile:Path, oFile:FileName, oFile:Extension, oFile:FullPath )
oFile:Release()
return( cFileName )

METHOD FolderReplace( cFileName AS STRING, cNewPath AS STRING, cbCodeblock := nil AS USUAL ) AS STRING PASCAL CLASS P_Base
// Ersetzt den Pfad des Dateinamens <cFileName> mit <cNewPath>.
// Hier kann man durch den Codeblock noch ne Menge schmutziger Sachen machen

	LOCAL oFile           AS AFileSpec

cbCodeblock := SELF:PrepareCodeblock( cbCodeblock, { |Drive, Path, FileName, Extension, FullPath| Drive + Path + FileName + Extension } )
SELF:_CheckPath( cFileName )
oFile:=aFileSpec{cFileName}
oFile:Path := cNewPath
cFileName := Eval(cbCodeBlock, oFile:Drive, oFile:Path, oFile:FileName, oFile:Extension, oFile:FullPath )
oFile:Release()
return( cFileName )

METHOD DbgObjectTree( oObject AS OBJECT, cHeader := "" AS STRING ) AS VOID PASCAL CLASS P_Base
SELF:DbgArray( OOPTree( oObject ), cHeader )

METHOD PrepareCodeBlock( cbCodeblock AS USUAL, cbCodeBlockStandard AS USUAL) AS CODEBLOCK PASCAL CLASS P_Base
// Prüft einen Codeblock, setzt ggf. einen Standardcodeblock ein und wird Meldungen raus bevor der ganze VO zu Grunde geht :)
	LOCAL lOK := TRUE

cbCodeBlock := IfNil( cbCodeBlock, cbCodeBlockStandard )
if( IsNil(cbCodeblock) )
	SELF:MessageFormat( "Übergebener Codeblock ist nil! Format = #.", { SELF:UsualTypeAsString(cbCodeblock) }, PROT_ART_ERROR, TRUE )
elseif( ValType(cbCodeblock) != "B" )
	SELF:MessageFormat( "Übergebener Codeblock hat falsches Format! Format = #.", { SELF:UsualTypeAsString(cbCodeblock) }, PROT_ART_ERROR, TRUE )
elseif( cbCodeBlock == NULL_CODEBLOCK )
	SELF:MessageFormat( "Übergebener Codeblock ist NULL_CODEBLOCK! Format = #.", { SELF:UsualTypeAsString(cbCodeblock) }, PROT_ART_ERROR, TRUE )
endif
return( cbCodeblock )

METHOD Divide( nValue AS REAL8, nDivideValue AS REAL8, lReportDivideByZero := FALSE AS LOGIC ) AS REAL8 PASCAL CLASS P_Base
// Es wird versucht <nValue> durch <nDivideValue> zu teilen
if( lReportDivideByZero .and. nDivideValue == 0 )
	SELF:MessageFormat("Fehler (Teilung durch Null / Divide by Zero) bei Operation #/#. Der Teiler kann nicht Null sein. Es wurde 1 angenommen.", { nValue, nDivideValue }, PROT_ART_WARNING, TRUE )
endif
return( nValue / IIF( nDivideValue == 0 , 1 , nDivideValue) )

METHOD HasDecimals( nValue AS REAL8 ) AS LOGIC PASCAL CLASS P_Base
// Prüft, ob der übergebene Wert stellen nach dem Komma hat
return( nValue <> Integer(nValue) )

METHOD GetDecimals( nValue AS REAL8 ) AS REAL8 PASCAL CLASS P_Base
return( nValue - Integer(nValue) )

METHOD GetKomplexConfigParameter( cBereich AS STRING, cParameterName AS STRING, aFieldList AS ARRAY, cDivider := "," AS STRING) AS ARRAY PASCAL CLASS P_Base
// Ermittelt aus einem StringParameter eine Feldliste und gibt diese als Einzelwerte im Array zurück
// Aufbau <aFieldList> {{ symField, cbConvertValue, cbCheckValue }}
//        {{#BDE_NR, { |cWert| Val(cWert) }, { |uWert| uWert != 0 }}}
//
// Rückgabe
//        { uValue, uValue, uValue... }

	LOCAL aValueList            AS ARRAY
   LOCAL cParameterValue := "" AS STRING
	LOCAL cFieldList := ""      AS STRING
	LOCAL x                     AS INT

aValueList := {}
cFieldList := SELF:ArrayToFieldListString( aFieldList, { |k,v,l1,lLastEntry| k + IIF( lLastEntry, cDivider + " ", "" ) } )
cParameterValue := oConfigManager:GetStringParam(cBereich, cParameterName, "")
if( Empty( cParameterValue ) )
	SELF:MessageFormat( 'Fehler in Konfigurationsparameter "#" aus dem Bereich "#". Der Konfigurationsparameter ist nicht gesetzt. Erwartet werden die Feldinhalte #', { cParameterName, cBereich, cFieldList }, PROT_ART_ERROR )
else
	aValueList := SELF:StringtoArray( cParameterValue, { cDivider } )
	if( ALen(aValueList) != ALen(aFieldList) )
		SELF:MessageFormat( 'Fehler in Konfigurationsparameter "#" aus dem Bereich "#". Erwartet werden # Feldinhalte: #. Es sind jedoch # Feldinhalte gesetzt.', { cParameterName, cBereich, ALen(aFieldList), cFieldList, ALen(aValueList) }, PROT_ART_ERROR )
	else
		for x:=1 upto ALen(aFieldList)
			if( ALen(aFieldList[x]) >= 2 .and. aFieldList[x][2] != nil )
				// Konvertierung der Werte, wenn gefordert
				aValueList[x] := Eval( aFieldList[x][2], aValueList[x] )
			endif

			if( ALen(aFieldList[x]) >= 3 .and. aFieldList[x][3] != nil )
				if( !Eval(aFieldList[x][3], aValueList[x] ) )
					SELF:MessageFormat( 'Fehler in Konfigurationsparameter "#" aus dem Bereich "#". Die Validierung für das Feld # mit dem Inhalt "#" schlug fehl. Erwartet werden # Feldinhalte: #.' , { cParameterName, cBereich, aFieldList[x][1], aValueList[x], ALen(aFieldList), cFieldList }, PROT_ART_ERROR )
				endif
         endif
		next x
	endif
endif
return( aValueList )

METHOD GetConfigParam( dwParameterTyp AS DWORD, cBereich AS STRING, cParameterName AS STRING, uInitValue AS USUAL, symTable AS SYMBOL, symKeyField AS SYMBOL, cTableName := "" AS STRING ) AS USUAL PASCAL CLASS P_Base
//
//  Liest den Konfigurationsparameter aus und prüft ihn gleich auf vorhandensein in der Schlüsseltabelle
//  Beispiel:
//  SELF:oBase:GetConfigParam( STRING, "", "KOSTENART", "",  #KOSTENART, #KOSTENART, "Kostenarten")

	LOCAL uReturnValue             AS USUAL

uReturnValue := uInitValue

SELF:ResetLocalError( #BaseGetConfigParam )
do case
case( dwParameterTyp == STRING )
	uReturnValue := oConfigManager:GetStringParam(cBereich, cParameterName, uInitValue	)
case( dwParameterTyp == LOGIC )
	uReturnValue := oConfigManager:GetLogicParam(cBereich, cParameterName, uInitValue	)
case( dwParameterTyp == INT )
	uReturnValue := oConfigManager:GetINTParam(cBereich, cParameterName, uInitValue	)
case( dwParameterTyp == FLOAT )
	uReturnValue := oConfigManager:GetFloatParam(cBereich, cParameterName, uInitValue	)
otherwise
	SELF:MessageFormat( "Fehler beim auslesen von Konfigurationsparameter #, # vom Typ #. Der Typ ist nicth bekannt. ", { cBereich, cParameterName, dwParameterTyp }, PROT_ART_ERROR )
endcase
if( !SELF:IsLocalError( #BaseGetConfigParam ) )
	SELF:AddMessageText( #add, "Konfigurationsarameter "+cBereich+", "+cParameterName )
	SELF:CheckFieldsFound( symTable, iif( Empty(cTableName), Symbol2String(symTable), cTableName ), {{ symKeyField, uReturnValue }} )
	SELF:AddMessageText( #del )
endif
return( uReturnValue )


METHOD StringDate( dDate AS DATE, cbCodeBlock := nil AS USUAL ) AS STRING PASCAL CLASS P_Base
// Gibt das übergebene Datum <dDate> in einem bestimmten Format zurück
// [cbCodeblock] = { |TT,T, MM,M,MMMM, JJJJ,JJ| }
// TT   : 01
// T    : 1
// MM   : 08
// M    : 8
// MMMM : August
// JJ   : 96
// JJJJ : 1996
// Standard: TT.MM.JJJJ

	LOCAL cDateString := ""                  AS STRING
	LOCAL aMonth                             AS ARRAY
	LOCAL cTag, cMonat, cJahr                AS STRING

cDateString := DToS(dDate)
cTag        := Right(cDateString,2)
cMonat      := SubStr(cDateString,5,2)
cJahr       := Left(cDateString,4)
aMonth      := SELF:StringToArray( "Januar,Februar,März,April,Mai,Juni,Juli,August,September,Oktober,November,Dezember", {","} )
cbCodeBlock := SELF:PrepareCodeBlock( cbCodeblock, { |TT,T, MM,M,MMMM, JJJJ,JJ| TT+"."+MM+"."+JJJJ } )
cDateString := Eval( cbCodeblock, cTag, SELF:StringReplace(cTag,"0",""),cMonat, SELF:StringReplace(cMonat,"0",""), if( Val(cMonat) > 0 , aMonth[Val(cMonat)],"" ), cJahr, Right(cJahr,2) )
return( cDateString )

METHOD StringDuration( nDurationSecs AS REAL8, cbCodeBlock := nil AS USUAL ) AS STRING PASCAL CLASS P_Base
// Gibt die übergebene Zeit in Minuten in einem bestimmten Format zurück
// Ohne Übergabe Codeblock gilt der folgende Codeblock, bei dem die Stunde nur ausgegeben wird, wenn ungleich Null
// [cbCodeblock] = { |HH,MM,SS,H,M,S,nH,nM,nS| HH +":" + MM+":" + SS }
// HH    : Stunden, zweistellig, ggf. mit führender Null : 02
// H     : Stunden, ohne führende Null : 2
// nH    : Stunden, numerisch
// MM    : Minuten zweistellig, ggf. mit führender Null : 09
// M     : Minuten, ohne führende Null : 9
// nM    : Minuten, numerisch
// SS    : Sekunden zweistellig, ggf. mit führender Null : 09
// S     : Sekunden, ohne führende Null : 9
// nS    : Sekunden, numerisch
//
// { |HH,MM,SS,H,M,S,nH,nM,nS| iif(nH!=0, HH+":","") + iif( nM != 0,MM+":","") + SS + iif(nH!=0," Stunden", iif(nM!=0," Minuten", " Sekunden") }
// Mit diesem Codeblock wird "HH:MM:SS Stunden" gezeigt, wenn Stunden != 0.
// Es wird "MM:SS Minuten" gezeigt, wenn Minuten !=0. Anderfalls wird "SS Sekunden" gezeigt.

	LOCAL nDuration AS DWORD
	LOCAL nSeconds  AS DWORD
	LOCAL nMinutes  AS DWORD
	LOCAL nHours    AS DWORD

	nDuration := DWord( nDurationSecs )

	nSeconds := MOD(nDuration, 60)
	nMinutes := MOD(Int(nDuration / 60), 60)
	nHours   := Int(nDuration / 3600)

cbCodeBlock := SELF:PrepareCodeBlock( cbCodeblock, { |HH,MM,SS,H,M,S,nH,nM,nS| iif(nH!=0, HH+":","") + iif( nM != 0,MM+":","") + SS + iif(nH!=0," Stunden", iif(nM!=0," Minuten", " Sekunden") ) } )
return( Eval( cbCodeBlock, StrZero(nHours,2,0), StrZero(nMinutes,2,0), StrZero(nSeconds,2,0), Str3( nHours,2,0 ), Str3( nMinutes,2,0), Str3( nSeconds,2,0), nHours, nMinutes, nSeconds ) )

METHOD StringTime( uInputTime AS USUAL, cbCodeBlock := nil AS USUAL, cFormat := "" AS STRING ) AS STRING PASCAL CLASS P_Base
// Gibt einen ZeitString formatiert zurück. Damit läßt sich jedoch noch viel mehr machen :)
// uInputTime : STRING    - StringTime( "12:13 Uhr und 13 Sekunden", nil, "HH:MM Uhr und SS Sekunden" ) -> Baut aus diesen Vorgaben einen String wie in cbCodeblock beschrieben
//            : CODEBLOCK - StringTime( { |h,m,s| h := 10, m := 3 } ) -> "10:03 Uhr"
//
// cbCodeblock: { |HH,MM,SS| HH + ":" + MM + " Uhr" }
//              Dient zur Formatierung des Rückgabestrings, oder zum auslesen der Stunden, Minuten und Sekunden
//              { |HH,MM,SS| nHH := HH, nMM := 00, "" } Holt aus der reingegebenen Zeit die Stunde auf eine lokale Variable. Der Rückgabestring wird auf leer gesetzt.

	LOCAL nHH := 0,nMM := 0,nSS := 0       AS INT
	LOCAL cTime := ""                      AS STRING
	LOCAL nPos := 0                        AS INT

do case
case( UsualType( uInputTime ) == STRING )
	if( Empty( cFormat ) )
		SELF:MessageFormat( "Fehler bei StringTime(#). Zu dem Parameter von Typ STRING muss ein Format übergeben werden", { uInputTime }, PROT_ART_ERROR, TRUE )
	else
		nHH := SELF:__StringTimeHelper( "HH", uInputTime, cFormat )
		nMM := SELF:__StringTimeHelper( "MM", uInputTime, cFormat )
		nSS := SELF:__StringTimeHelper( "SS", uInputTime, cFormat )
	endif
case( UsualType( uInputTime ) == CODEBLOCK )
	Eval( uInputTime, @nHH, @nMM, @nSS )
endcase

cbCodeBlock := SELF:PrepareCodeBlock( cbCodeBlock, { |HH,MM,SS| HH + ":" + MM + " Uhr" } )
cTime := Eval( cbCodeBlock, nHH,nMM,nSS )
return( cTime )

PROTECT METHOD __StringTimeHelper( cArt AS STRING, cTime AS STRING, cFormat AS STRING ) AS INT PASCAL CLASS P_Base

	LOCAL nReturn := 0              AS INT
	LOCAL nPos := 0                 AS INT

nPos := SELF:StringFind( cFormat, cArt )
if( nPos != 0 )
	if( Len(cTime) >= nPos+2 )
		nReturn := Val( SubStr3( cTime, nPos, 2 ) )
	else
	endif
endif
return( nReturn )

METHOD StringToCodeblock( cCodeblockString AS STRING ) AS CODEBLOCK PASCAL CLASS P_Base
// Wandelt einen String in einen Codeblock um. Der String muss folgenden Aufbau haben: "{ |a,b| 1==1, a==b }"

	LOCAL cbCodeblock                AS CODEBLOCK
	LOCAL oError                     AS USUAL

BEGIN SEQUENCE
	cbCodeblock := &cCodeblockString
RECOVER USING oError
	SELF:MessageFormat("StringToCodeblock(#) : Fehler beim Konvertieren eines Strings in einen Codeblock : #", { cCodeblockString, oError:Description }, PROT_ART_ERROR, TRUE )
	cbCodeblock := NULL_CODEBLOCK
END SEQUENCE

return( cbCodeblock )

METHOD ToDate( cMask AS STRING, cStringDate AS STRING ) AS DATE PASCAL CLASS P_Base
// Konvertieren des cStringDate per cMask in ein DATE-Datum
// Beispiel:
// 	cMask        :  "MM/TT/JJJJ"
// 	cStringDate  :  "10/31/2014"
//
	LOCAL nTT, nMM, nJJ          AS INT
	LOCAL nJahrStellen := 2      AS INT
	LOCAL dDate                  AS DATE
	LOCAL oError                 AS USUAL

dDate := NULL_DATE
nTT := SELF:StringFind( cMask, "TT" )
nMM := SELF:StringFind( cMask, "MM" )
nJJ := SELF:StringFind( cMask, "JJ" )
if( SELF:StringFind( cMask, "JJJJ" ) != 0 )
	nJahrStellen := 4
endif
if( nTT == 0 .or. nMM == 0 .or. nJJ == 0 )
	SELF:MessageFormat( "Fehler bei Datumsermittlung nach Maske # aus String #. TT=#, MM=#, JJ=#", {cMask, cStringDate, nTT, nMM, nJJ}, PROT_ART_ERROR )
else
	BEGIN SEQUENCE
   	dDate := CToD(SubStr(cStringDate,nTT,2)+"."+SubStr(cStringDate,nMM,2)+"."+SubStr(cStringDate,nJJ,nJahrStellen))
	RECOVER USING oError
		SELF:MessageFormat("Fehler beim Konvertieren des String # per Maske # in ein Datum: #", { cStringDate, cMask, oError:Description }, PROT_ART_ERROR )
	END SEQUENCE
endif
return( dDate )

METHOD GetRecordID( symTable AS SYMBOL, aKeyFields AS ARRAY, lErrorIfEntryNotExits := FALSE AS LOGIC ) AS USUAL PASCAL CLASS P_Base
// Es wird die RecordID des Datensatzes zu <aKeyFields> aus der Tabelle <symTable> als String zurückgegeben
// symTable      : #LAGER
// aKeyFields    : {{#ARTIKEL, "10000"}, {#LAGER,"20"}}
// Wird der Eintrag nicht gefunden, wird ein Empty-String "" zurückgegeben
	LOCAL uValue                    AS USUAL
uValue := SELF:GetFieldFromTable( symTable, aKeyFields, #RECORDID, lErrorIfEntryNotExits )
return( IIF( IsNil(uValue), "", uValue ) )



METHOD DateAdd( dDate AS DATE, nZeitWert AS REAL8, symZeitWertTyp AS SYMBOL, lUseBetriebstageKalender := FALSE AS LOGIC ) AS DATE PASCAL CLASS P_Base
// Vereinfachte Benutzung der Klassen ADateTimeStamp und AWorkTimeStamp zum addieren von Zeitwerten
// <dDate>        : Datum
// <nZeitWert>    : Ein Zeitwert in <symZeitWertTyp>
// <symZeitWErtTyp> : #STUNDEN, #MINUTEN, #SEKUNDEN, #TAGE, #WOCHEN, #JAHRE

	LOCAL dReturn            AS DATE
	LOCAL nTage := 0         AS REAL8
	LOCAL iTage := 0         AS INT
	LOCAL oADate             AS ADateTimeStamp
	LOCAL oWDate             AS AWorkTimeStamp

dReturn := dDate

do case
case( symZeitWertTyp == #JAHRE )
	nTage := nZeitWErt * 365
case( symZeitWertTyp == #WOCHEN )
	nTage := nZeitWErt * 7
case( symZeitWertTyp == #TAGE )
	nTage := nZeitWErt
case( symZeitWertTyp == #STUNDEN )
	nTage := nZeitWErt / 24
case( symZeitWertTyp == #MINUTEN )
	nTage := (nZeitWErt / 60) / 24
case( symZeitWertTyp == #SEKUNDEN )
	nTage := (nZeitWErt / 60 / 60 ) / 24
otherwise
	SELF:MessageFormat( "Fehler beim addieren des Zeitwertes # # zum Datum #. Es wurde das Eingangsdatum verwendet.", { nZeitWErt, symZeitWertTyp, dDate }, PROT_ART_ERROR )
endcase

if( nTage > 0 )
	iTage := int(nTage)
	if( lUseBetriebstageKalender )
		oWDate := AWorkTimeStamp{ dDate }
		oWDate:AddWorkDays( iTage )
		dReturn := oWDate:DateVar
		oWDate:Release()
	else
		oADate := ADateTimeStamp{dDate}
		oADate:Day += iTage
		dReturn := oADate:DateVar
		oADate:Release()
	endif
endif
return( dReturn )

METHOD Trace( uValue1 := nil AS USUAL, uValue2 := nil AS USUAL ) AS VOID PASCAL CLASS P_Base
// Merkt sich den aktuellen Ort und die Zeilennummer. Kann mit TraceOut() ausgewertet werden
AAdd( SELF:__aTrace, { __ENT, __LINE, uValue1, uValue2 } )

METHOD TraceOut( uHeader := nil AS USUAL ) AS STRING PASCAL CLASS P_Base
// Die mit Trace() gemerkten Modulpunkte werden nun ins Debugfester ausgegegeben und als ein String
// zurückgegben.

	LOCAL cOutput                  AS STRING
	LOCAL cHeader := ""            AS STRING

cHeader := SELF:UsualToString(IfNil( uHeader, "" ))
cOutput := SELF:StringRepeat(	Len(cHeader), "-" ) + CRLF + cHeader + CRLF + SELF:StringRepeat(	Len(cHeader), "-" ) + CRLF
AEval( SELF:__aTrace, { |a| cOutput += a[1] + " | " + a[2]  + UsualToString( a[3] ) + " | "+ UsualToString( a[4] ) + CRLF } )
SELF:dbg( cOutput, TRUE )
SELF:__aTrace :={}
return( cOutput )


METHOD LinkDocument( cFileName AS STRING, symStructure AS SYMBOL, StructureRecordID AS STRING, cDocumentCaption AS STRING ) AS STRING PASCAL CLASS P_Base
//
// Verlinken einer externen Datei <cFileName> mit einer ams-Struktur
// <symStructure>       : #REBUCHK
// <StructureRecordID>  : Die RecordID des Rechnungseingangsbuch-Kopfes
//
// Return:
// Es wird die DokumentenID zurückgegeben. Wird diese Leer zurückgegeben, so ist ein Fehler aufgetreten
//
	LOCAL cDocID := ""      AS STRING
	LOCAL cStatus := ""     AS STRING

if( DocMgr_InsertExternalDocument(SELF:oTransactionManager, "AUTO", "", cFileName, @cDocID, @cStatus) )
	DocMgr_LinkDocWithRecord(SELF:oTransactionManager, cDocID, symStructure, StructureRecordID, "AUTO", FALSE, @cStatus)
endif

if( !Empty( cStatus ) )
	SELF:MessageFormat("Fehler beim anlegen des externen Dokumentes #: #", { cFileName, cStatus }, PROT_ART_ERROR )
endif

return( cDocID )

METHOD StringBuilder( aParam AS ARRAY, cbDivider := nil AS CODEBLOCK, cbSubDivider := nil AS CODEBLOCK ) AS STRING PASCAL CLASS P_Base
/*
	cbDivider    = { |paramAsString, paramTypeAsString, lLastElement| "ResultString" }
	cbSubDivider = { |aSubArrayAsString| "ResultString" }

	Beispiel:
	aArray := {"Fin", 1, 2, 3, #FIN, {"a", "b","c"},4}
	cbDivider := { |paramAsString, paramTypeAsString, lLastElement| iif( paramTypeAsString=="STRING", '"'+paramAsString+'"', iif( paramTypeAsString=="SYMBOL", "#"+paramAsString, paramAsString) )+iif(lLastElement, "", ",") }
	cbSubDivider := { |subAsString| "{" + subAsString + "}" }
	SELF:StringBuilder( aArray, cbDivider, cbSubDivider ) --> '"Fin", 1, 2, 3,#FIN ,{"a", "b", "c"},4'

*/
	LOCAl x                AS INT
	LOCAL cString := ""    AS STRING
	LOCAL cSubString       AS STRING

for x:=1 upto aLen(aParam)

	/* SubElement-Divider */
	if( UsualType(aParam[x]) == ARRAY )
		cSubString := SELF:StringBuilder( aParam[x], cbDivider, cbSubDivider )
		if( !IsNil(cbSubDivider) )
			cSubString := eVal( cbSubDivider, cSubString )
		endif
	else
		cSubString := SELF:UsualToString( aParam[x] )
	endif

	/* Element-Divider */
	if( !IsNil(cbDivider) )
		cSubString := eVal( cbDivider, cSubString, SELF:UsualTypeAsString( aParam[x] ), x==aLen(aParam) )
	endif

	cString += cSubString
next x

return( cString )

METHOD NumericBuilder ( nValue AS REAL8, cbCodeBlock AS CODEBLOCK ) AS REAL8 PASCAL CLASS P_Base
// Hilft beim Aufbau eines numerischen WErtes.
// <nValue>      : Der numerische Wert
// <cbCodeblock> : { | nPreDecimals, nPostDecimals, nNumberOfPreDecimals, nNumberOfPostDecimals, lHasDecimals, lIsPositive | nPreDecimals := 10, nPostDecimals = 20 } ---> 10.20
//    Parameter am Beispiel der <nValue> 250.123
// 	nPreDecimals           : Die Vorkommastellen als Integer   --> 250
//    nPostDecimals          : Die Nachkommastellen als Real8    --> 123
//    nNumberOfPreDecimals   : Anzahl der Vorkommastellen        --> 3
//    nNumberOfPostDecimals  : Anzahl der Nachkommastellen       --> 3
//    lHasDecimals           : Sind Nachkommatsellen vorhanden?  --> Ja
//    lIsPositive            : Ist die Gesamtzahl positiv oder negativ
//
// Beispiel für ein Codeblock, welcher Vor- und Nachkommastellen verändert:
// { | nPreDecimals, nPostDecimals, lHasDecimals, lIsPositive | nPreDecimals := Integer( nAngebotPos ), nPostDecimals = nHauptbaugruppe }

		LOCAL nPreDecimals  := 0         AS LONGINT
		LOCAL nPostDecimals := 0         AS LONGINT
		LOCAL nNumberOfPreDecimals       AS INT
		LOCAL nNumberOfPostDecimals := 0 AS INT
		LOCAL aValue                     AS ARRAY

aValue := SELF:StringToArray( NTrim(nValue), { "." } )
nPreDecimals  := Val( aValue[1] )
nNumberOfPreDecimals := Len( aValue[1] )
if( ALen( aValue ) > 1 )
	// Es sind nachkommastellen vorhanden
	nNumberOfPostDecimals := Len( aValue[2] )
	nPOstDecimals := Val( aValue[2] )
endif
Eval( cbCodeBlock, @nPreDecimals, @nPostDecimals, nNumberOfPreDecimals, nNumberOfPostDecimals, nPostDecimals > 0, nValue >= 0 )
nValue := nPreDecimals + Val( "0."+AllTrim(Str3(nPostDecimals,120,0)) )
debugPrint( "FIN: ", __ENT, __LINE__, "Builder:", nPreDecimals, nPostDecimals, nValue)
return( nValue )

METHOD ArrayCompare( aArray1 AS ARRAY, aArray2 AS ARRAY ) AS LOGIC PASCAL CLASS P_Base
// Prüft, ob zwei unterschiedliche Arrays den gleichen Inhalt haben.
// Funktioniert auch mit Arrays in Arrays

	LOCAL x                      AS INT

if( ALen( aArray1 ) != ALen( aArray2 ) )
	return( false )
else
	for x:=1 upto ALen( aArray1 )
		if( UsualType( aArray1[x] ) == ARRAY .and. UsualType( aArray2[x] ) == ARRAY )
			if( !SELF:ArrayCompare( aArray1[x], aArray2[x] ) )
				return( false )
			endif
		else
     		if( UsualType( aArray1[x] ) != UsualType( aArray2[x] ) )  .or. ( aArray1[x] != aArray2[x] )
     			return( false )
     		endif
		endif
	next x
endif
return( true )

METHOD RecordLock( symTable AS SYMBOL, aKeyFields AS ARRAY ) AS LOGIC PASCAL CLASS P_Base
// Einen Datensatz in Tabelle <symTable> sperren mit dem Key <aKeyFields> sperren.
// Dieser Datensatz kann mit RecordUnlock() wieder freigegeben werden oder auch
// mit RecordUpdate() verändert werden.
// <aKeyFields> : {{ #ANGEBOTSNR, "1012" }, { #AUFPOS, 10 }}

	LOCAL oServer           AS AServer

SELF:ResetLocalError( #BaseLockRecord )
if( !SELF:RecordLocked( symTable, aKeyFields ) )
	oServer := SELF:CreateAServer( symTable, aKeyFields, FALSE )
	if( oServer != NULL_OBJECT )
		// Satzinformation und oServer merken, damit er beim Unlock wieder
		// released oder beim RecordUpdate beschrieben werden kann
		AAdd( SELF:__aLockedRecords, { symTable, aKeyFields, oServer, FALSE } )
	else
		SELF:MessageFormat( "Fehler beim Lock der Tabelle #, Datensatz: #. #", { symTable, SELF:KeyToString( aKeyFields ), oServer:Status:GetMessage() }, PROT_ART_ERROR )
	endif
endif
return( !SELF:IsLocalError( #BaseLockRecord ) )

METHOD RecordUnlock( symTable AS SYMBOL, aKeyFields AS ARRAY ) AS LOGIC PASCAL CLASS P_Base
// EInen mit RecordLock() gesperrten Datensatz wieder freigeben.
// Wurde mit RecordUpdate() in den Datensatz geschrieben, so wird ein EndUpdate()
// gemacht. Anderfalls ein CancelUpdate()

	LOCAL nRow                AS INT
	LOCAL lUnlocked := FALSE  AS LOGIC
	LOCAL oServer             AS AServer

nRow := SELF:RecordLockInformation( symTable, aKeyFields )
if( nRow != 0 )
	oServer := SELF:__aLockedRecords[nRow][3]
	if( SELF:__aLockedRecords[nRow][4] )
		// Datensatz wurde durch RecordUpdate() verändert
		// Nun EndUpdate() machen
		lUnlocked := oServer:EndUpdate("Base")
	else
		// Datensatz mit CancelUpdate() freilassen
		lUnlocked := oServer:CancelUpdate()
	endif
	if( !lUnlocked )
		SELF:MessageFormat( "Fehler beim Unlock für Tabelle # mit Key #. #", { symTable, SELF:KeyToString( aKeyFields ), oServer:Status:GetMessage() }, PROT_ART_ERROR )
	endif
	oServer:Release()
	ADelShrink( SELF:__aLockedRecords, nRow )
else
	SELF:MessageFormat( "RecordUnlock( # ) mit dem Key # nicht möglich. Der Satz wurde nicht mit RecordLock() gesperrt.", { symTable, SELF:KeyToString( aKeyFields ) }, PROT_ART_ERROR, TRUE )
endif
return( lUnlocked )

MEthod RecordUnlockAll() AS LOGIC PASCAL CLASS P_Base

	LOCAL nRow               AS INT
	LOCAL lUnlocked := TRUE  AS LOGIC

for nRow := 1 upto ALen( SELF:__aLockedRecords )
	if( !SELF:RecordUnlock( SELF:__aLockedRecords[nRow][1], SELF:__aLockedRecords[nRow][2] ) )
		lUnlocked := FALSE
	endif
next
return( lUnlocked )

METHOD RecordUpdate( symTable AS SYMBOL, aKeyFields AS ARRAY, aUpdateFields AS ARRAY ) AS LOGIC PASCAL CLASS P_Base
// Ein gesperrter Record wird mit Feldern <aUpdateFields> aktualisiert.

	LOCAL nRow            AS INT
	LOCAL lOk := FALSE    AS LOGIC
	LOCAL oServer         AS AServer

nRow := SELF:RecordLockInformation( symTable, aKeyFields )
if( nRow != 0 )
	oServer := SELF:__aLockedRecords[nRow][3]
	// Es werden nun die Felder in den Sever geschrieben.
	// Bei Fehlern wird eine Meldung generiert.
	SELF:ArrayToRecord( aUpdateFields, oServer )
endif
return( lOK )

METHOD RecordLocked( symTable AS SYMBOL, aKeyFields AS ARRAY ) AS LOGIC PASCAL CLASS P_Base
// Gibt True zurück wenn bereits ein Lock zu dem Datensatzinformationen besteht.
return( SELF:RecordLockInformation( symTable, aKeyFields ) != 0  )

METHOD RecordLockInformation( symTable AS SYMBOL, aKeyFields AS ARRAY ) AS INT PASCAL CLASS P_Base
// Gibt die Zeilennummer im Array __aLockedRecords zurück, zu der der Eintrag <symTable> und <aKeyFields> passt

	LOCAL nRow := 0              AS INT
	LOCAL x                      AS INT

for x:=1 upto ALen( SELF:__aLockedRecords )
	if( SELF:__aLockedRecords[x][1] == symTable )
		if( SELF:ArrayCompare( SELF:__aLockedRecords[x][2], aKeyFields ) )
			nRow := x
			exit
		endif
	endif
next x

return( nRow )

METHOD ReleaseIfNotNullObject( oObject AS AObject ) AS VOID PASCAL CLASS P_Base
if( oObject != NULL_OBJECT )
	oObject:Release()
endif

METHOD GetRowsProcessed( oStatement AS ASqlStatement ) AS INT PASCAL CLASS P_Base

	LOCAL dwRowsProcessed         AS DWord

if( !oStatement:GetRowsProcessed( @dwRowsProcessed ) )
	SELF:Message( "Fehler beim ermitteln der verarbeiteten Zeilen (GetRowsProcessed) : "+ oStatement:Status:GetMessage(), PROT_ART_ERROR )
endif
return( int(dwRowsProcessed) )

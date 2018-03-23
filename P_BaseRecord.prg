/*

	P_BaseRecord |

	Stellt ein Objekt mit Record-Eigenschaften zur Verfügung
	Autor: André Finken

	symKey / uValue - Kombination mit zusätzlichen Attributen
	[lRequired]   : Zwingende Ersetzung (z.B. Keyfelder)
	[lReplace]    : Soll beim zurückschreiben in den Record mit geschrieben werden
	[uCargo]      : kann Alles sein und wird einfach mitgeschliffen

	Die Array-Spalten sollten auf jeden Fall über die Konstanten rec_ angesprochen werden
*/

FUNCTION GetBaseRecordFromTable( symTable AS SYMBOL, aKey AS ARRAY, oBase AS P_Base, lErrorIfNotExist := TRUE AS LOGIC ) AS P_BaseRecord PASCAL

	LOCAL oBaseRecord           AS P_BaseRecord

oBaseRecord := P_BaseRecord{ oBase }
if( !oBaseRecord:ImportFromTable( symTable, aKey ) )
	oBaseRecord:Release()
	oBaseRecord:=NULL_OBJECT
endif
return( oBaseRecord )





static define rec_Key            := 1
static define rec_Value          := 2
static define rec_Required       := 3
static define rec_Replace        := 4
static define rec_PlaceHolder1   := 5
static define rec_PlaceHolder2   := 6
static define rec_PlaceHolder3   := 7
static define rec_Cargo          := 8

static define recDoc_Deleted     := #Deleted
static define recDoc_Modified    := #Modified
static define recDoc_UnModified  := #UnModified
static define recDoc_Added       := #Added

static define recDoc_Type        := 1
static define recDoc_Key         := 2
static define recDoc_OldValue    := 3
static define recDoc_NewValue    := 4
static define recDoc_Message     := 5
static define recDoc_RecordArray := 6

CLASS P_BaseRecord                 INHERIT AOBJECT


	DECLARE ACCESS  oBase
	EXPORT          aRecord         AS ARRAY
   EXPORT          aRecordInit     AS ARRAY
   DECLARE METHOD  Pos
	DECLARE METHOD  Scan
	DECLARE METHOD  Clone
	DECLARE METHOD  Exists
	DECLARE METHOD  Manipulate
   DECLARE METHOD  Get
	DECLARE METHOD  GetCargo
	DECLARE METHOD  GetColumnValue
	DECLARE METHOD  GetRowValue
   DECLARE METHOD  Del
   DECLARE METHOD  Count
   DECLARE METHOD  Rename
   DECLARE METHOD  RenameList
   DECLARE METHOD  Kill
   DECLARE METHOD  Put
   DECLARE METHOD  PutColumnValue
	DECLARE METHOD  Len
	DECLARE METHOD  Increment

	DECLARE METHOD  IsRequired
	DECLARE METHOD  SetRequired
	DECLARE METHOD  IsReplace
	DECLARE METHOD  SetReplace
	DECLARE ACCESS  lTrackChanges
	DECLARE ASSIGN  lTrackChanges
	DECLARE METHOD  GetChanges
	DECLARE METHOD  GetChangesAsString
	DECLARE METHOD  IsModified
	DECLARE METHOD  HasChanges

	DECLARE METHOD  FieldsInTable
	DECLARE METHOD  FieldsNotInTable
	DECLARE METHOD  IsFieldInTable
	DECLARE METHOD  MarkFieldsIfNotInTable
	DECLARE METHOD  RemoveFieldsIfNotInTable

   DECLARE METHOD  ChangeDocumentation
   DECLARE METHOD  ChangeDocumentationAsString
	DECLARE METHOD  GetDiffRecord
	DECLARE METHOD  GetEqualRecord

	DECLARE METHOD  TableDataRead
	DECLARE METHOD  TableDataUpdate
	DECLARE METHOD  TableDataAppend
	DECLARE METHOD  TableDataEmptyRecord

	DECLARE METHOD  ImportFromArray
	DECLARE METHOD  ImportFromRecord
	DECLARE METHOD  ImportFromTable
	DECLARE METHOD  ImportFromSql
	DECLARE METHOD  ImportFromBaseRecord
	DECLARE METHOD  ImportOnlyIfNotExists
	DECLARE METHOD  ImportFromString
	DECLARE METHOD  ImportFromStringComplex

	DECLARE METHOD  ExportToRecord
	DECLARE METHOD  ExportToArray
	DECLARE METHOD  ExportToArrayComplex
	DECLARE METHOD  ExportToTable
	DECLARE METHOD  ExportToString
	DECLARE METHOD  ExportToFile
	DECLARE METHOD  ExportToSql
	DECLARE METHOD  ExportToBaseRecord

	DECLARE METHOD  UnitTest
	DECLARE METHOD  Debug

	/* Interne Methoden */
	DECLARE METHOD  __Get
	DECLARE METHOD  __Put
	PROTECT         __oBase          AS P_Base
	PROTECT         __oChangeTracker AS P_BaseRecord
	PROTECT         __lTrackChanges  AS LOGIC


METHOD Init( oP_Base, oBezug ) CLASS P_BaseRecord
// [oBezug] kann Array, AReadRecord, AWriteRecord oder AServer sein
//          wird ein symbol übergeben (z.B. #ME) wird versucht ein
//          leeren Record aus der Strktur #ME zu erstellen

	SUPER:Init()

	if( !IsNil( oP_Base ) .and. IsInstanceOfUsual(oP_Base, #P_Base))
		SELF:__oBase := oP_Base
		SELF:__oBase:AddRef()
	else
		SELF:__oBase := P_Base{}
	endif

	SELF:aRecord       := {}

	//  [1] KEY      ( Symbol ) : Wird zwingend gesetzt, bzw. hier egal
	//  [2] VALUE    ( USUAL )  : Wird zwingend gesetzt, bzw. hier egal
	//  [3] Required ( Logic )  : Soll dieser Value zwingend alles erseten?
	//  [4] Replace  ( Logic )  : Soll diese Key/Value-Kombination beim ersetzen im Record mit berücksichtigt werden?
	//  [5] PlaceHolder
	//  [6] PlaceHolder
	//  [7] PlaceHolder
	//  [8] Cargo    ( Usual )  : Kann Alles sein und wird einfach mitgeschliffen.
	//                    [1]  [2]   [3]    [4]  [5]  [6]  [7]  [8]
	SELF:aRecordInit := { nil, nil, FALSE, TRUE, nil, nil, nil, nil }
	SELF:__oChangeTracker := NULL_OBJECT
	SELF:__lTrackChanges  := FALSE

	if( oBezug != nil )
		do case
		case( IsInstanceOfUsual(oBezug, #P_BaseRecord) )
			// Aus eine BaseRecord importieren
      	SELF:ImportFromBaseRecord( oBezug )
		case( IsArray(oBezug) )
			// Aus einem Array importieren
			SELF:ImportFromArray( oBezug )
		case( IsSymbol( oBezug ) )
			// Aus einer ams-Struktur erstellen
			SELF:TableDataEmptyRecord( oBezug )
		otherwise
			// Es wird davon ausgegangen, dass ein AREadRecord, AWriteRecord oder ein AServer übergeben wurde
			SELF:ImportFromRecord( oBezug )
		endcase
	endif


METHOD Destroy() AS VOID PASCAL CLASS P_BaseRecord

	if( SELF:__oChangeTracker != NULL_OBJECT )
		SELF:__oChangeTracker:Release()
	endif
	SELF:oBase:Release()
	SUPER:Destroy()

METHOD UnitTest() AS VOID PASCAL CLASS P_BaseRecord

	LOCAL oRec     AS P_BaseRecord
	LOCAL oFirst   AS P_BaseRecord
	LOCAL oSecond  AS P_BaseRecord
	LOCAL oDiff    AS P_BaseRecord
	LOCAL oReadRec AS AReadRecord

oRec := P_BaseRecord{}
oRec:ImportFromTable( #ME, {{ #ME, "ST" }} )
oRec:Put( #ARTIKEL, "NULL" )
ORec:Put( #BEZEICH, "Neue Bezeichnung" )
oRec:Put( #MAUSCHL, "Quatsch" )
oRec:ImportFromTable( #ARTIKEL, {{ #ARTIKEL, "1000" }}, nil, nil, true )
oRec:MarkFieldsIfNotInTable( #ME )
debugPrintArray( oRec:aRecord )
oDiff := oRec:GetChanges()
debugPrint( "FIN: ", __ENT, __LINE__, "Hier die Veränderungen")
debugPrintArray( oDiff:aRecord )
oDiff:Release()
debugPrint( "FIN: ", __ENT, __LINE__, "Hier wieder nur vohandenen Felder zu #AUFPOS")
oRec:RemoveFieldsIfNotInTable( #AUFPOS )
debugPrintArray( oRec:aRecord )
oRec:Release()

oRec := P_BaseRecord{ SELF:oBase }
oRec:aRecordInit[rec_Required] := TRUE
oRec:ImportFromTable( #AUFPOS, {{#AUFTRAG, "1000"}, {#AUFPOS,10}} )

oRec:Manipulate( { |a| a[rec_Required] := TRUE } )
oRec:Put( #AUFTRAG, "1125", TRUE )
oRec:Put( #AUFPOS,  10    , TRUE )
if( oRec:Scan( { |a| a[rec_Key]==#AUFPOS .and. a[rec_Required] } ) != 0 )
	oRec:Put( #AUFPOS, 20 )
endif

/*
oRec:Debug()
debugPrint( "FIN: ", __ENT, __LINE__, oRec:GetColumnValue( #AUFTRAG, rec_Value ), oRec:GetRowValue( 1, rec_Key ) )
debugPrint( "FIN: ", __ENT, __LINE__, "Export String:", oRec:ExportToString({ | cKey, cValue, a | cKey + ";" +cValue + CRLF    }))
*/
oRec:Release()

// Unterschiede in Auftragskopf und Positionen herausfinden
oFirst := P_BaseRecord{ SELF:oBase }
oFirst:ImportFromTable( #AUFKOPF, {{#AUFTRAG, "1000"}} )

oReadRec := SELF:oBase:GetReadRecordFromTable( #AUFPOS,  {{#AUFTRAG, "1000"}, {#AUFPOS,10}}, "Auftragsposition" )
oSecond  := P_BaseRecord{ SELF:oBase, oReadRec }

oRec := P_BaseRecord{ SELF:oBase }
debugPrint( "FIN: ", __ENT, __LINE__, "Gleich")
oRec:GetDiffRecord( oFirst, oSecond, TRUE, FALSE )
oRec:Debug()
oRec:Release()

// Leeren Record aus Strukur erstellen, aus Struktur füllen, verändern und neu erstellen
oRec := P_BaseRecord{}
oRec:TableDataEmptyRecord( #ME )
oRec:TableDataRead( #ME, {{ #ME, "ST" }} )
oRec:Put( #BEZEICH, "Finken" )
oRec:Put( #ME, "FI" )
oRec:TableDataAppend( #ME )
oRec:Release()


oSecond:Release()
oFirst:Release()
oReadRec:Release()

METHOD TableDataRead( symTable AS SYMBOL, aKey AS ARRAY ) AS LOGIC PASCAL CLASS P_BaseRecord
// Es wird ein P_BaseRecord aus einem vorandenen Datensatz erstellt und zurückgegeben
// Ab der Rückgabe werden Veränderungen am P_BaseRecord berücksichtigt

	LOCAL lOK := FALSE                AS LOGIC

SELF:Kill()
if( SELF:ImportFromTable( symTable, aKey ) )
	SELF:Manipulate( { |aField| aField[rec_Required] := TRUE, aField[rec_Replace] := TRUE } )

	SELF:lTrackChanges := TRUE
	lOK := TRUE
endif
return( lOK )

METHOD TableDataUpdate( symTable AS SYMBOL, aKey AS ARRAY, lWriteOnlyChanges := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseRecord
// Es werden die Änderungen am <oBaseRecord> in die Tabelle <symTable> geschrieben.
// Ist [lWriteAll] gesetzt, so werden alle Felder (die lReplace = TRUE haben) in die Tabelle zurückgeschrieben
// Die Änderungen werden zurückgesetzt und sind nach dem TableDataUpdate wieder leer.

	LOCAL oChangeSet        AS P_BaseRecord
	LOCAL lReturn := FALSE  AS LOGIC

if( !lWriteOnlyChanges )
	lReturn := SELF:ExportToTable( symTable, aKey, FALSE )
else
	oChangeSet := SELF:GetChanges( TRUE )
	if( oChangeSet:Count() != 0 )
		lReturn := oChangeSet:ExportToTable( symTable, aKey, FALSE )
	endif
	oChangeSet:Release()
endif

if( lReturn )
	SELF:lTrackChanges := FALSE
	SELF:lTrackChanges := TRUE
endif

return( lReturn )

METHOD TableDataAppend( symTable AS SYMBOL ) AS LOGIC PASCAL CLASS P_BaseRecord
// Es werden die WErte aus <oBaseRecord> in die Tabelle <symTable> als neuer Datensatz mit dem
// Key <aKey> geschrieben.
return( SELF:oBase:CreateDataSet( symTable, SELF:ExportToArray( TRUE, FALSE,TRUE, FALSE ) ) )

METHOD TableDataEmptyRecord( symTable AS SYMBOL ) AS LOGIC PASCAL CLASS P_BaseRecord
// Es wird ein leerer P_BaseRecord mit den Feldern der Tabelle <symTable> erzeugt.
// Alle Änderungen werden nach Rückgabe getracked

	LOCAL lOK                AS LOGIC
	LOCAL oRecord            AS AWriteRecord

SELF:Kill()
oRecord := AWriteRecord{ symTable }
lOK := SELF:ImportFromRecord( oRecord )
oRecord:Release()

SELF:Manipulate( { |aField| aField[rec_Required] := TRUE, aField[rec_Replace] := TRUE } )
SELF:lTrackChanges := TRUE
return( TRUE )

METHOD Manipulate( cbCodeBlock AS CODEBLOCK ) AS VOID PASCAL CLASS P_BaseRecord
// cbCodeBlock : { |a| a[rec_Value] := iif(a[rec_Key] == #FIELD,10, a[rec_Value]), a[rec_Required] := TRUE }
AEval( SELF:aRecord, cbCodeBlock )

METHOD Scan( cbCodeBlock AS CODEBLOCK ) AS INT PASCAL CLASS P_BaseRecord
// cbCodeBlock : { |a| a[rec_Key] == #FIELD }
return( AScan( SELF:aRecord, cbCodeBlock ) )

METHOD Clone() AS P_BaseRecord PASCAL CLASS P_BaseRecord
// Es wird eine komplette Kopie des P_BaseRecords, inklusive der gemerkten Änderungen (TrackChanges) zurückgegeben

	LOCAL oCloneRecord         AS P_BaseRecord

oCloneRecord := P_BaseRecord{ SELF:oBase, SELF }
if( SELF:lTrackChanges )
	// Unser BaseRecord (SELF) hatt trackingChanges.
	// Da wir einen Clone wollen, müssen diese Informationen nun
	// auch übertragen werden
	oCloneRecord:lTrackChanges := TRUE
	oCloneRecord:ImportFromBaseRecord( SELF:GetChanges() )
endif

return( oCloneRecord )

ACCESS oBase AS P_Base PASCAL CLASS P_BaseRecord
return( SELF:__oBase )

METHOD Pos( symKey AS SYMBOL ) AS INT PASCAL CLASS P_BaseRecord
return( SELF:Scan( { |a| a[rec_Key] == symKey }) )

METHOD Exists( symKey AS SYMBOL ) AS LOGIC PASCAL CLASS P_BaseRecord
return( SELF:Pos( symKey ) != 0 )

METHOD Count() AS INT PASCAL CLASS P_BaseRecord
return( ALen(SELF:aRecord) )

METHOD Get( symKey AS SYMBOL, nField := nil AS USUAL, lErrorIfNotFound := TRUE AS LOGIC ) AS USUAL PASCAL CLASS P_BaseRecord
return( SELF:__Get( symKey, IfNil(nField, rec_Value), lErrorIfNotFound ) )

METHOD GetCargo( symKey AS SYMBOL ) AS USUAL PASCAL CLASS P_BaseRecord
return( SELF:__Get( symKey, rec_Cargo ) )

METHOD GetColumnValue( symKey AS SYMBOL, nColumn AS INT ) AS USUAL PASCAL CLASS P_BaseRecord
return( SELF:__GET(symKey, nColumn, TRUE )	)

METHOD GetRowValue( nRow AS INT, nColumn AS INT ) AS USUAL PASCAL CLASS P_BaseRecord
if( nRow < 1 .or. nRow > ALen(SELF:aRecord) )
	SELF:oBase:MessageFormat( "P_BaseRecord:GetRowValue[#,#] : Zugriff ausserhalb des Bereichs. Die Zeile # existiert nicht. Max-Zeilen: #", { nRow, nColumn, nRow, ALen(SELF:aRecord) }, PROT_ART_ERROR, TRUE )
else
	if( nColumn <= 1 .or. nColumn > ALen(SELF:aRecord[nRow]))
		SELF:oBase:MessageFormat( "P_BaseRecord:GetRowValue[#,#] : Zugriff ausserhalb des Bereichs. Die Spalte # in Zeile # existiert nicht. Max-Spalten: #", { nRow, nColumn, nColumn, nRow, ALen(SELF:aRecord[nRow]) }, PROT_ART_ERROR, TRUE )
	else
		//
		// EXIT
		//
		return( SELF:aRecord[nRow, nColumn] )
	endif
endif
return( nil )

METHOD __Get( symKey AS SYMBOL, nItem AS INT, lErrorIfNotFound := TRUE AS LOGIC ) AS USUAL PASCAL CLASS P_BaseRecord

	LOCAL uValue         AS USUAL
	LOCAL nFound         AS INT

uValue := nil
nFound := SELF:Pos( symKey )
if( nFound != 0 )
	if( ALen(SELF:aRecord[nFound]) >= nItem )
		uValue := SELF:aRecord[nFound][nItem]
	else
		SELF:oBase:MessageFormat( "P_BaseRecord:__Get(#,#) Zugriff auf ungültiges Feld Nr. #. Es stehen nur # Felder zur Verfügung", { symKey, nItem, nItem, ALen(SELF:aRecord) } , PROT_ART_ERROR, TRUE )
	endif
else
	if( lErrorIfNotFound )
		SELF:oBase:MessageFormat( "P_BaseRecord:Get(#) : Feldname in Liste nicht gefunden.", { symKey }, PROT_ART_ERROR, TRUE )
	endif
endif
return( uValue )

METHOD IsRequired( symKey AS SYMBOL ) AS LOGIC PASCAL CLASS P_BaseRecord
return( SELF:__Get( symKey, rec_Required ) )

METHOD SetRequired( symKey AS SYMBOL, lValue AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseRecord
return( SELF:__Put( symKey, rec_Required, lValue ) )

METHOD SetReplace( symKey AS SYMBOL, lValue AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseRecord
return( SELF:__Put( symKey, rec_Replace, lValue ) )

METHOD IsReplace( symKey AS SYMBOL ) AS LOGIC PASCAL CLASS P_BaseRecord
return( SELF:__Get( symKey, rec_Replace ) )

PROTECT METHOD __Put( symKey AS SYMBOL, nColumn AS INT, uValue AS USUAL ) AS LOGIC PASCAL CLASS P_BaseRecord

	LOCAL nPos          AS INT

nPos := SELF:Pos( symKey )
if( nPos != 0 )
	SELF:aRecord[nPos][nColumn] := uValue
	return( true )
else
	return( false )
endif

METHOD Put( symKey AS SYMBOL, uValue AS USUAL, lRequired := nil AS USUAL, lReplace := nil AS USUAL, uCargo := Nil AS USUAL ) AS INT PASCAL CLASS P_BaseRecord

	LOCAL nFound := 0 AS INT
	LOCAL aInit       AS ARRAY

nFound := SELF:Pos( symKey )
if( nFound != 0 )
	// Wert bereits vorhanden
	SELF:aRecord[nFound][rec_Value]    := uValue
	SELF:aRecord[nFound][rec_Required] := IfNil(lRequired, SELF:aRecord[nFound][rec_Required])
	SELF:aRecord[nFound][rec_Replace]  := IfNil(lReplace,  SELF:aRecord[nFound][rec_Replace])
	SELF:aRecord[nFound][rec_Cargo]    := IfNil(uCargo,    SELF:aRecord[nFound][rec_Cargo])
else
	aInit := AClone( SELF:aRecordInit )
	aInit[rec_Key]      := symKey
	aInit[rec_Value]    := uValue
	aInit[rec_Required] := IfNil(lRequired, aInit[rec_Required])
	aInit[rec_Replace]  := IfNil(lReplace,  aInit[rec_Replace])
	aInit[rec_Cargo]    := IfNil(uCargo,    aInit[rec_Cargo])
	AAdd( SELF:aRecord, aInit )
	nFound := ALen(SELF:aRecord)
endif
return( nFound )

METHOD Increment( symKey AS SYMBOL, uIncrementValue AS USUAL ) AS USUAL PASCAL CLASS P_BaseRecord

	LOCAL uValue 	   AS USUAL

uValue := SELF:Get( symKey )
if( !IsNil(uValue) )
	uValue := uValue + uIncrementValue
else
	uValue := uIncrementValue
endif
SELF:Put( symKey, uValue, TRUE )
return( uValue )


METHOD FieldsNotInTable( symTable AS SYMBOL ) AS P_BaseRecord PASCAL CLASS P_BaseRecord
// Gibt die Liste der Felder aus dem aktuellen BaseRecord als neuen Record zurück,
// die nicht in der Tabelle <symTable> vorhanden sind

	LOCAL oTableRecord         AS P_BaseRecord
	LOCAL oDifferences         AS P_BaseRecord
	LOCAL x                    AS INT


oTableRecord := P_BaseRecord{ SELF:oBase }
oDifferences := NULL_OBJECT
if( oTableRecord:TableDataEmptyRecord( symTable ) )

	oDifferences := P_BaseRecord{ SELF:oBase }
	for x:=1 upto SELF:Count()
		if( !oTableRecord:Exists( SELF:aRecord[x][rec_Key] ) )
			oDifferences:Put( SELF:aRecord[x][rec_Key] , SELF:aRecord[x][rec_Value] )
		endif
	next x

endif
oTableRecord:Release()
return( oDifferences )

METHOD FieldsInTable( symTable AS SYMBOL ) AS P_BaseRecord PASCAL CLASS P_BaseRecord
// Gibt die Liste der Felder aus dem aktuellen BaseRecord als neuen Record zurück,
// die in der Tabelle <symTable> vorhanden sind

	LOCAL oTableRecord         AS P_BaseRecord
	LOCAL oEquals              AS P_BaseRecord
	LOCAL x                    AS INT

oTableRecord := P_BaseRecord{ SELF:oBase }
oEquals := NULL_OBJECT
if( oTableRecord:TableDataEmptyRecord( symTable ) )

	oEquals := P_BaseRecord{ SELF:oBase }
	for x:=1 upto SELF:Count()
		if( oTableRecord:Exists( SELF:aRecord[x][rec_Key] ) )
			oEquals:Put( SELF:aRecord[x][rec_Key] , SELF:aRecord[x][rec_Value] )
		endif
	next x
endif
oTableRecord:Release()
return( oEquals )

METHOD IsFieldInTable( symTable AS SYMBOL, symFieldName AS SYMBOL ) AS LOGIC PASCAL CLASS P_BaseRecord
// Prüft, ob das Feld <symFieldName> in der Tabelle <symTable> vorhanden ist

	LOCAL oEquals         AS P_BaseRecord
	LOCAl lReturn         AS LOGIC

oEquals := SELF:FieldsInTable( symTable )
lReturn := oEquals:Exists( symFieldName )
oEquals:Release()
return( lReturn )

METHOD MarkFieldsIfNotInTable( symTable AS SYMBOL ) AS VOID PASCAL CLASS P_BAseRecord
// Alle Felder im aktuellen Record, die nicht in der Tabelle <symTable> vorhanden sind
// werden als Replace=False gekennzeichnet. Alle Anderen als Replace=True

	LOCAL oTable              AS P_BaseRecord

oTable := SELF:FieldsInTable( symTable )
if( oTable != NULL_OBJECT )
	AEval( SELF:aRecord, { |a| a[rec_Replace] := oTable:Exists( a[rec_key] ) } )
endif
oTable:Release()

METHOD RemoveFieldsIfNotInTable( symTable AS SYMBOL ) AS VOID PASCAL CLASS P_BaseRecord
// Es werden alle Felder aus dem aktuellen Record entfernt, die nicht in der
// Tabelle <symTable> vorhanden ind

	LOCAL oTable              AS P_BaseRecord
	LOCAL oSelf               AS P_BaseRecord
	LOCAL aRemove             AS ARRAY
	LOCAL x                   AS INT

aRemove := {}
oTable := P_BaseRecord{ SELF:oBase }
if( oTable:TableDataEmptyRecord( symTable ) )
	for x:=1 upto SELF:Count()
		// Felder die in symTable> nicht existstieren nun merken
		if( !oTable:Exists( SELF:aRecord[x][rec_Key] ) )
			AAdd( aRemove, SELF:aRecord[x][rec_Key] )
		endif
	next x

	// Alle gemerkten Felder nun löschen
	oSelf := SELF
	AEval( aRemove, { |symKey| oSelf:Del( symKey ) } )
endif
oTable:Release()


METHOD IsModified( symKey AS SYMBOL ) AS LOGIC PASCAL CLASS P_BaseRecord
// Gibt zurück, ob sich der Inhalt von <symKey> seit dem einschalten von <lTracChanges> geändert hat
	LOCAl oDiff                AS P_BaseRecord
	LOCAL isModified := FALSE  AS LOGIC

oDiff := SELF:GetChanges()
isModified := oDiff:Exists( symKey )
oDiff:Release()
return( isModified )

METHOD HasChanges() AS LOGIC PASCAL CLASS P_BaseRecord
// Gibt TRUE zurück, wenn es Änderungen am aktuellen BaseRecord gibt

	LOCAL lHasChanges         AS LOGIC
	LOCAL oDiff               AS P_BaseRecord

oDiff := SELF:GetChanges()
lHasChanges := oDiff:Count() > 0
oDiff:Release()
return( lHasChanges )


METHOD Rename( symKey AS SYMBOL, symKeyNew AS SYMBOL ) AS VOID PASCAL CLASS P_BaseRecord
// Benennt einen Key um. Bestand das Feld mit dem neuen Key bereits, so wird nur Value übertragen
SELF:Put( symKeyNew, SELF:Get(symKey) )
SELF:Del( symKey )

METHOD RenameList( aMapping AS ARRAY ) AS VOID PASCAL CLASS P_BaseRecord
// Es wird eine Liste von Feldern umbenannt
// <aMapping> : {{ #OLDFIELDNAME, #NEWFIELDNAME }, [...]
	LOCAL x        AS INT

for x:=1 upto ALen(aMapping)
	SELF:Rename( aMapping[x][1], aMapping[x][2] )
next x

METHOD PutColumnValue( symKEy AS SYMBOL, nColumn AS INT, uColumnValue AS USUAL) AS INT PASCAL CLASS P_BaseRecord
	LOCAL nPos       AS INT
if( nPos := SELF:Pos(symKey) ) != 0
   SELF:aRecord[nPos][nColumn] := uColumnValue
endif
return( nPos )

METHOD Del( symKey AS SYMBOL, lErrorIfNotFound := TRUE AS LOGIC ) AS VOID PASCAL CLASS P_BaseRecord

	LOCAL nFound := 0 AS INT

nFound := SELF:Pos( symKey )
if( nFound != 0 )
	ADelShrink( SELF:aRecord, nFound )
else
	if( lErrorIfNotFound )
		SELF:oBase:MessageFormat( "P_BaseRecord:Del(#) : Feldname in Liste nicht gefunden.", { symKey }, PROT_ART_ERROR, TRUE )
	endif
endif

METHOD Kill() AS VOID PASCAL CLASS P_BaseRecord
SELF:aRecord := {}
SELF:lTrackChanges := FALSE

METHOD Len() AS INT PASCAL CLASS P_BaseRecord
return( ALen( SELF:aRecord ) )

ASSIGN lTrackChanges( lTrackChangesLocal AS LOGIC) AS LOGIC PASCAL CLASS P_BaseRecord
// Sollen Änderungen mitgetracked werden? Wird der Schalter auf TRUE gestellt, so werden die aktuellen
// Inhalte zwischengespeichert und die Veränderungen können mit GetChanges() abgerufen werden.
// Ein erneutes Setzen änderung nicht.

	do case
	case( lTrackChangesLocal .and. !SELF:__lTrackChanges )
		// Tracking an
		SELF:__oChangeTracker := P_BaseRecord{ SELF:oBase }
		SELF:__oChangeTracker:aRecord := AClone(SELF:aRecord)

	case( !lTrackChangesLocal .and. SELF:__oChangeTracker != NULL_OBJECT )
		// Tracking aus
		SELF:__oChangeTracker:Release()

	case( lTrackChangesLocal )
		// Tracking an, war aber schon an
		SELF:oBase:Message("Das Tracking (lTrackChanges) in P_BaseRecord wurde angeschaltet, obwohl es bereits angeschaltet war. Es gelten die Änderungen ab dem ersten anschalten" , PROT_ART_WARNING )
	case( !lTrackChangesLocal )
		// Tracking aus, war aber aus

	endcase
	SELF:__lTrackChanges := lTrackChangesLocal
return( SELF:__lTrackChanges )

ACCESS lTrackChanges AS LOGIC PASCAL CLASS P_BaseRecord
return( SELF:__lTrackChanges )

METHOD GetChanges( lOnlyReplace := FALSE AS LOGIC ) AS P_BaseRecord PASCAL CLASS P_BaseRecord
// Es werden die seit SELF:lTrackChanges := TRUE gemerkten Änderungen als P_BaseRecord zurückgegeben
//
	LOCAL oDiffRecord      AS P_BaseRecord
   LOCAL aDoc             AS ARRAY

oDiffRecord:=P_BaseRecord{ SELF:oBase }
if( SELF:__oChangeTracker != NULL_OBJECT )
	aDoc := SELF:ChangeDocumentation( SELF:__oChangeTracker, lOnlyReplace, TRUE )
	AEval( aDoc, { |a| oDiffRecord:ImportFromArray( { a[recDoc_RecordArray] }, TRUE ) } )
endif
return( oDiffRecord )

METHOD GetChangesAsString( lOnlyReplace := FALSE AS LOGIC ) AS STRING PASCAL CLASS P_BaseRecord

	LOCAL oDiffRecord      AS P_BaseRecord
   LOCAL aDoc             AS ARRAY
   LOCAL cResult := ""    AS STRING

if( SELF:__oChangeTracker != NULL_OBJECT )
	oDiffRecord:=P_BaseRecord{ SELF:oBase }
	aDoc := SELF:ChangeDocumentation( SELF:__oChangeTracker, lOnlyReplace, TRUE )
	AEval( aDoc, { |a| cResult += a[recDoc_Message] + CRLF } )
	oDiffRecord:Release()
endif
return( cResult )

METHOD ExportToBaseRecord() AS P_BaseRecord PASCAL CLASS P_BaseRecord
return( SELF:Clone() )

METHOD ImportFromArray( aArray AS ARRAY, lExactStructure := FALSE AS LOGIC, aRename := nil AS ARRAY, lOnlyIfExists := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseRecord
// [lExactStructure] : Bei TRUE wird die gleiche Struktur erwartet wie in SELF:aRecord (s. SELF:aRecordInit)
//

	LOCAL oError        AS USUAL
	LOCAL x             AS INT
	LOCAL symKey        AS SYMBOL
	LOCAL nPos          AS INT

SELF:oBase:ResetLocalError( #BaseRecordImportFromArray )
for x:=1 upto ALen(aArray)
	do case
	case( ALen(aArray[x]) < 2 )
		SELF:oBase:MessageFormat( "P_BaseRecord:ImportFromArray -> Zeile # hat weniger als 2 Spalten. Key / Value Kombination nicht möglich.", { x }, PROT_ART_ERROR, TRUE )
	case( !IsSymbol( aArray[x][rec_Key] ) )
		SELF:oBase:MessageFormat( "P_BaseRecord:ImportFromArray -> Zeile #, KeyFeld (Nummer #) mit Inhalt # hat weniger als 2 Spalten. Key / Value Kombination nicht möglich.", { x, rec_Key, aArray[x][rec_Key] }, PROT_ART_ERROR, TRUE )
	otherwise
		symKey := aArray[x][rec_Key]
		if( !lOnlyIfExists .or. SELF:Exists( symKey ) )
			nPos   := SELF:Put( SELF:oBase:RenameKeyField(symKey, aRename), aArray[x][rec_Value] )

			if( lExactStructure )
				BEGIN SEQUENCE
	         	SELF:aRecord[nPos][rec_Required] := aArray[x][rec_Required]
	         	SELF:aRecord[nPos][rec_Replace]  := aArray[x][rec_Replace]
	         	SELF:aRecord[nPos][rec_Cargo]    := IIF( ALen(aArray[x]) >= rec_Cargo, aArray[x][rec_Cargo], SELF:aRecordInit[rec_Cargo] )

				RECOVER USING oError
					SELF:oBase:MessageFormat("P_BaseRecord:ImportFromArray -> Zeile #, Key  # : Fehler beim befüllen des Arrays: #", { x, symKey, oError:Description }, PROT_ART_ERROR, TRUE )
				END SEQUENCE
			endif
		endif
	endcase
next x
return( !SELF:oBase:IsLocalError( #BaseRecordImportFromArray ) )

METHOD ImportFromBaseRecord( oBaseRecord AS P_BaseRecord, aRename := nil AS ARRAY, lOnlyIfExists := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseRecord
// Es wird von einem BaseRecord importiert.
	LOCAL aArray          AS ARRAY

aArray := SELF:ExportToArray()
return( SELF:ImportFromArray( aArray, TRUE, aRename, lOnlyIfExists ) )

METHOD ImportOnlyIfNotExists( oRecord AS P_BaseRecord ) AS VOID PASCAL CLASS P_BaseRecord
// Importiert aus einem BaseRecord nur die Felder, die nicht im aktuellen Record vorhanden sind

	LOCAL x              AS INT

for x:=1 upto oRecord:Count()
	if( !SELF:Exists( oRecord:aRecord[x][rec_key] ) )
		SELF:Put( oRecord:aRecord[x][rec_key], oRecord:aRecord[x][rec_Value], oRecord:aRecord[x][rec_Replace], oRecord:aRecord[x][rec_Required], oRecord:aRecord[x][rec_Cargo]  )
	endif
next x


METHOD ImportFromTable( symTable AS SYMBOL, aKeyFields AS ARRAY, aFields := nil AS ARRAY, aRename := nil AS ARRAY, lOnlyIfExists := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseRecord
//
// Liest die Feld/Inhalt Kombinationen der Tabelle <symTable> aus und füllt damit das interne Array auf
// aFields : Nur diese Felder übernehmen - Optional : { #FELDNAME, #FELDNAME, ... }
// aRename : übernommene Felder ummappen/umbenennen : {{#FELDAUSTABELLE,#NEUERFELDNAME},...
//
	LOCAL oRec          AS AReadRecord
	LOCAL lOK := FALSE  AS LOGIC

oRec := SELF:oBase:GetReadRecordFromTable( symTable, aKeyFields, Symbol2String( symTable ), TRUE )
if( oRec != NULL_OBJECT )
	lOK := SELF:ImportFromRecord( oRec, aFields, aRename, lOnlyIfExists )
	oRec:Release()
endif
return( lOK )

METHOD ImportFromRecord( oRecord AS USUAL, aFields := nil AS ARRAY, aRename := nil AS ARRAY, lOnlyIfExists := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseRecord
// Liest einen Record aus. Ist aFields nicht angegeben, werden alle Felder zurückgegeben.
// aFields : Optional : { #FELDNAME, #FELDNAME, ... }
// aRename : übernommene Felder ummappen/umbenennen : {{#FELDAUSTABELLE,#NEUERFELDNAME},...

	LOCAL aTemp                    AS ARRAY

aTemp := SELF:oBase:ArrayFromRecord( oRecord, aFields, aRename )
return( SELF:ImportFromArray( aTemp, FALSE, nil, lOnlyIfExists  ) )

METHOD ImportFromSql( oStatement AS ASqlStatement, aRename := nil AS ARRAY, lOnlyIfExists := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseRecord
// Importiert aus einem Sql-Statement.
// <oStatement> muss ausgeführt sein (ExecuteReader), so dass jede Zeile mit einem Fetch gelesen werden kann.
// [aRename]  : übernommene Felder ummappen/umbenennen : {{#FELDAUSTABELLE,#NEUERFELDNAME},...
// [lOnlyIfExists] : TRUE, es werden nur Felder aus SQL übernommen die schon im aktuellen Record sind

	LOCAL symKey            AS SYMBOL
	LOCAL aFields           AS ARRAY
	LOCAL x                 AS INT
	LOCAL aColumns          AS ARRAY

aColumns := nil
aFields := SELF:oBase:GetSqlRecord( oStatement, @aColumns )
for x:=1 upto ALen( aFields )
	symKey := SELF:oBase:RenameKeyField(aFields[x][1], aRename)
	if( !lOnlyIfExists .or. SELF:Exists( symKey ) )
		SELF:Put( symKey, aFields[x][2] )
	endif
next x
return( ALen(aFields) != 0 )


METHOD ImportFromString( cString AS STRING, cDelimiter := ";" AS STRING, lOnlyIfExists := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseRecord
// Importiert aus einem String im Format "Key;Value"+CRLF+"Key;Value"
// Siehe auch ExportToString().
// Für eine koplexere Methode zum importieren aus einem String s. ImportFromStringComplex()

	LOCAL aRows, aRow                AS ARRAY
	LOCAl lReturn := FALSE           AS LOGIC
	LOCAL x                          AS INT

SELF:oBase:ResetLocalError( #BaseRecordImportFromString )
aRows := SELF:oBase:StringToArray( 	cString, { CRLF } )
for x:=1 upto ALen(aRow)
	aRow := SELF:oBase:StringToArray( aRows[x], { cDelimiter } )
	if( ALen(aRow) >= 2 )
		if( !lOnlyIfExists .or. SELF:Exists( String2Symbol( aRow[1] ) ) )
			SELF:Put( String2Symbol( aRow[1] ), aRow[2] )
		endif
	else
		SELF:oBase:MessageFormat( "Fehler in ImportFromString() in Zeile #. Die Anzahl Spalten sollte Zwei sein, ist jedoch nur #. String = #", { x, ALen(aRow), cString }, PROT_ART_ERROR, TRUE )
	endif
next x
return( SELF:oBase:IsLocalError( #BaseRecordImportFromString ) )

METHOD ImportFromStringComplex( uHeader AS USUAL, cRow AS STRING, cDelimiter := "," AS STRING, lErrorIfNoContend := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseRecord
// Importiert in einen vorhandenen BaseRecord aus einer String-Zeile.
// Der <uHeader> (die Spaltennamen) können sowohl als ARRAY wie auch als STRING übergeben werden.
// Beispiel:
// 	uHeader : Array  : { #ME, "DIVISOR" "BEZEICH", #USER }
// 	cRow    : String : "ST, 1, Hier die Bezeichnung, FIN"
//
// Ergibt ein BaseRecord #ME = "ST", #DIVISOR = "1", #BEZEICH = "Hier die Bezeichnung", #USER = "FIN"
// Wird uHeader als String übergeben, so muss der gleiche Delimiter und die gleiche Anzahl Spalten wie in cRow verwendet werden

	LOCAL aRow, aHeader                 AS ARRAY
	LOCAL lReturn := FALSE              AS LOGIC
	LOCAL x                             AS INT

SELF:oBase:ResetLocalError( #BaseRecordImportFromStringComplex )
aRow := SELF:oBase:StringToArray( cRow, {cDelimiter} )
if( ALen( aRow ) != 0 )
	do case
	case( UsualType( uHeader ) == ARRAY )
		aHeader := uHeader
	case( UsualType( uHeader ) == STRING )
		aHeader := SELF:oBase:StringToArray( uHeader, {cDelimiter} )
	otherwise
		SELF:oBase:MessageFormat( "Fehler bei ImportFromString(). Der Paramater <uHeader> ist vom Typ # und wird nicht unterstützt.", { SELF:oBase:UsualTypeAsString( uHeader ) }, PROT_ART_ERROR, TRUE )
	endcase

	if( !SELF:oBase:IsLocalError( #BaseRecordImportFromString ) )
		if( ALen( aHeader ) != ALen( aRow ) )
			SELF:oBase:MessageFormat( "Fehler bei ImportFromString(). Der Header hat # Spalten und die Zeile hat # Spalten. Zeile = #", { ALen( aHeader), ALen( aRow ), cRow }, PROT_ART_ERROR, TRUE )
		else

			for x:=1 upto ALen( aHeader )
				do case
				case( UsualType( aHeader[x] ) == STRING )
					SELF:Put( String2Symbol( aHeader[x] ), aRow[x] )
				case( UsualType( aHeader[x] ) == SYMBOL )
					SELF:Put( aHeader[x],aRow[x] )
				otherwise
					SELF:OBase:MessageFormat( "Fehler bei ImportFromString(). Die Spalte # mit Namen # wurde als Typ # übergeben. Akzeptiert werden nur STRING und SYMBOL.", { x, aHeader[x], SELF:oBase:UsualTypeASString( aHeader[x] ) }, PROT_ART_ERROR, TRUE )
				endcase
			next x

		endif
	endif
	if( lErrorIfNoContend )
		SELF:oBase:MessageFormat( "Fehler bei ImportFromString(), die Zeile # hat keinen Inhalt", { cRow }, PROT_ART_ERROR, TRUE )
	endif
endif
return( SELF:oBase:IsLocalError( #BaseRecordImportFromStringComplex ) )

METHOD ExportToRecord( oRecord AS USUAL, lOnlyRequired := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseRecord
// Exportiert das P_BaseRecord-Array auf den Record <oRecord> vom Typ AREadRecord, AWriteRecord oder AServer
// Es werden nur die Felder exportiert, welche <lReplace> TRUE haben.
// [lOnlyRequired] gibt an, ob nur die mit <rec_Required>=TRUE gekennzeichnetten Einträge exportiert werden sollen
return( SELF:oBase:ArrayToRecord(  SELF:ExportToArray( TRUE, lOnlyRequired, TRUE ), oRecord ) )

METHOD ExportToArray( lOnlyKeyValue := FALSE AS LOGIC, lOnlyRequired := FALSE AS LOGIC, lOnlyReplace := FALSE AS LOGIC, lKeyFieldAsString := FALSE AS LOGIC ) AS ARRAY PASCAL CLASS P_BaseRecord
// Exportiert das P_BaseRecord-Array auf ein neues Array und gibt dieses zurück
// [lOnlyKeyValue] bei TRUE wird nur ein zwei-Dim Array zurückgegeben {{#Key, uValue}, {#Key, uValue}..
// 					 Anderfalls wird das Array komplett zurückgegeben
// [lOnlyRequired] gibt an, ob nur die mit <rec_Required>=TRUE gekennzeichnetten Einträge exportiert werden sollen
// [lOnlyReplace]  gibt an, ob nur die mit <rec_Replace>=TRUE gekennzeichnetten Einträge exportiert werden sollen
// [lKeyFieldAsString] Wir true übergeben, so wird der Key-Begriff nicht als Symbol sondern als String zurückgegeben

	LOCAL aReturn             AS ARRAY
	LOCAL aTemp               AS ARRAY
	LOCAL x                   AS INT

aReturn := {}
for x:=1 upto ALen(SELF:aRecord)
	if( SELF:aRecord[x][rec_Required] .or. !lOnlyRequired ) .and. ( SELF:aRecord[x][rec_Replace] .or. !lOnlyReplace )
		aTemp := AClone(aRecord[x])
		if( lKeyFieldAsString )
			// Keyfeld soll als String, nicht al Symbol übergeben werden
   		aTemp[rec_Key] := Symbol2String( aTemp[rec_Key] )
		endif
		if( lOnlyKeyValue )
			// Es soll nur die Key/Value-Kombination übergeben werden
			aTemp := { aTemp[rec_Key], aTemp[rec_Value] }
		endif

		AAdd( aReturn, aTemp )
	endif
next x
return( aReturn )

METHOD ExportToArrayComplex( cbCodeblock ) AS ARRAY PASCAL CLASS P_BaseRecord
// Gibt ein Array aus dem aktuellen zurück. Dieses Array kann über den Codeblock selbst gestaltet werden
// { | aExportArray, nRow, symKey, uValue, lReplace, lRequired, uCargo | aadd( aExportArray, { Symbol2String( symKey ), uValue } ) }

	LOCAL aReturn          AS ARRAY
	LOCAL X                AS INT

aReturn := {}
for x:=1 upto SELF:Count()
	Eval( cbCodeblock, @aReturn, x,  SELF:aRecord[x][rec_Key], SELF:aRecord[x][rec_Value], SELF:aRecord[x][rec_Replace], SELF:aRecord[x][rec_Required], SELF:aRecord[x][rec_Cargo] )
next x
return( aReturn )

METHOD ExportToTable( symTable AS SYMBOL, aKeyFields AS ARRAY, lAppendmode := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseRecord
// Exportiert das CargoArray in einen vorhandenen Datensatz
// Felder aus dem Record die in der Zieltabelle nicht existieren, werden nicht berücksichtigt
//
// symTable       : #ANGEBOTPOS
// aKeyFields     : {{ #ANGEBOTSNR, SELF:cAngebot }, { #AUFPOS, SELF:nAngPos }}
// aReplaceFields : {{ #P_ABSTIMM, FALSE}}
if( lAppendMode )
	return( SELF:oBase:CreateDataSet( symTable, SELF:ExportToArray( TRUE, FALSE,TRUE, FALSE ) ) )
else
	return( SELF:oBase:UpdateFieldsForTable( symTable, aKeyFields, SELF:ExportToArray( TRUE, FALSE,TRUE, FALSE ) ) )
endif

METHOD ExportToString( cbCodeblock := nil AS USUAL ) AS STRING PASCAL CLASS P_BaseRecord
// Gibt einen String im Format "Key;Value" je Zeile zurück
// Codeblock: { |cKey, cValue, aArrayRow| }
	LOCAL cString := ""              AS STRING
	LOCAL x                          AS INT

cbCodeBlock := SELF:oBase:PrepareCodeblock( cbCodeblock, { |cKey, cValue, a | AllTrim(cKey) + ";" + AllTrim(cValue) + CRLF } )
for x:=1 upto ALen( SELF:aRecord )
	cString += Eval( cbCodeblock, Symbol2String(SELF:aRecord[x][rec_Key]), SELF:oBase:UsualToString(SELF:aRecord[x][rec_Value]), SELF:aRecord[x] )
next x
return( cString )

METHOD ExportToFile( cFileName AS STRING,  cbCodeblock := nil AS USUAL, lUnicode := FALSE AS LOGIC, lDOSFormat := FALSE AS LOGIC) AS LOGIC PASCAL CLASS P_BaseRecord
// Schreibt das Array auf Platte
return( SELF:oBase:FileWriteFromString( cFileName, SELF:ExportToString( cbCodeBlock ), lUnicode, lDOSFormat ) )

METHOD ExportToSql( cTableName AS STRING, lOnlyRequired := FALSE AS LOGIC, lOnlyReplace := TRUE AS LOGIC ) AS VOID PASCAL CLASS P_BaseRecord
// Exportiert die Feld/Werte-Paare in eine Sql-Tabelle mit dem Namen <cTableName>
	LOCAL aValues, aColumns, aDefinition     AS ARRAY
	LOCAl x                                  AS INT

aValues     := {}
aColumns    := {}
aDefinition := {}
for x:=1 upto SELF:Len()
	if( !lOnlyReplace .or. SELF:aRecord[x][rec_Replace] )
		if( !lOnlyRequired .or. SELF:aRecord[x][rec_Required] )
			AAdd( aColumns,    SELF:aRecord[x][rec_Key] )
			AAdd( aValues,     SELF:aRecord[x][rec_Value] )
			AAdd( aDefinition, SELF:oBase:VOTypeToSqlType( SELF:oBase:UsualTypeAsString( SELF:aRecord[x][rec_Value] ) ) )
		endif
	endif
next x
SELF:oBase:ArrayToSql( cTableName, aValues, aColumns )

METHOD ChangeDocumentation( oOtherrecord AS P_BaseRecord, lOnlyReplace := FALSE AS LOGIC, lOnlyModifiedFields := FALSE AS LOGIC ) AS ARRAY PASCAL CLASS P_BaseRecord
// <oOtherRecord> ist ein P_BaseRecord mit gleichem oder ähnlichem Aufbau und wird als das Ältere zum Vergleich herangezogen
// Bei <lOnlyModifiedFields> werden nur die veränderten Felder zurückgegeben
//
// Es wird ein Array mit folgenden Spalten zurückgegeben
// { {symType, symKey, uOldValue, uNewValue, cMessage, aRecordArray }, ... }
//
// Hier die Konstaten zum ansprechen der Spalten:
// { {recDoc_Type, recDoc_Key, recDoc_OldValue, recDoc_NewValue, recDoc_Message, recDoc_Array }, ... }
//
// symType:
// recDoc_Deleted    := #Deleted
// recDoc_Modified   := #Modified
// recDoc_UnModified := #UnModified
// recDoc_Added      := #Added

	LOCAL aDoc                AS ARRAY
	LOCAL aOldRecord          AS ARRAY
	LOCAL nPos                AS INT
	LOCAl x                   AS INT

aDoc := {}
aOldRecord := oOtherrecord:ExportToArray(, , lOnlyReplace )

// gelöschte, unveränderte und geänderte prüfen
for x:=1 upto ALen( aOldRecord )
	if( nPos := SELF:Pos( aOldRecord[x][rec_Key] ) ) != 0
		if( aOldRecord[x][rec_Value] == SELF:aRecord[nPos][rec_Value] )
			if( !lOnlyModifiedFields )
				AAdd( aDoc, { recDoc_UnModified, aOldRecord[x][rec_key], aOldRecord[x][rec_Value], SELF:aRecord[nPos][rec_Value], SELF:oBase:StringFormat( 'Das Feld # hat unverändert den Inhalt "#"', { aOldRecord[x][rec_key], aOldRecord[x][rec_Value] } ),aOldRecord[x] } )
			endif
		else
			AAdd( aDoc, { recDoc_Modified, aOldRecord[x][rec_key], aOldRecord[x][rec_Value], SELF:aRecord[nPos][rec_Value], SELF:oBase:StringFormat( 'Der Inhalt des Feldes # wurde von "#" auf "#" geändert', { aOldRecord[x][rec_key], aOldRecord[x][rec_Value], SELF:aRecord[nPos][rec_Value] } ), SELF:aRecord[nPos] } )
		endif
	else
		// Diesen Key gibt es jetzt nicht mehr
		AAdd( aDoc, { recDoc_Deleted, aOldRecord[x][rec_key], aOldRecord[x][rec_Value], nil, SELF:oBase:StringFormat( 'Das Feld # mit Inhalt "#" wurde gelöscht', { aOldRecord[x][rec_key], aOldRecord[x][rec_Value] } ), aOldRecord[x] } )
	endif
next x

// neue herausfinden
for x:=1 upto ALen(SELF:aRecord)
	if( oOtherrecord:Pos(SELF:aRecord[x][rec_Key] ) == 0  )
		AAdd( aDoc, { recDoc_Added, SELF:aRecord[x][rec_key], nil, SELF:aRecord[x][rec_Value], SELF:oBase:StringFormat( 'Das Feld # mit Inhalt "#" wurde hinzugefügt', { SELF:aRecord[x][rec_key], SELF:aRecord[x][rec_Value] } ), SELF:aRecord[x] } )
	endif
next x
return( aDoc )

METHOD ChangeDocumentationAsString( oOtherrecord AS P_BaseRecord, lOnlyReplace := FALSE AS LOGIC, lOnlyModifiedFields := FALSE AS LOGIC ) AS STRING PASCAL CLASS P_BaseRecord

	LOCAl aArray           AS ARRAY
	LOCAL cResult := ""    AS STRING

aArray := SELF:ChangeDocumentation( oOtherrecord, lOnlyReplace, lOnlyModifiedFields )
AEval( aArray, { |a| cResult += a[recDoc_Message]+CRLF } )
return( cResult )

METHOD Debug AS VOID PASCAL CLASS P_BaseRecord
SELF:oBAse:lDebugMode := TRUE
SELF:__oBase:DbgArray( SELF:aRecord, "Debug P_BaseRecord Inhalte", TRUE )

METHOD GetDiffRecord( oFirstRecord AS P_BaseRecord, oSecondRecord AS P_BaseRecord, lGetDifferentKeys := TRUE AS LOGIC, lGetDifferentValues := TRUE AS LOGIC, lOnlyReplace := FALSE AS LOGIC ) AS VOID PASCAL CLASS P_BaseRecord
// Liefert ein neues P_BaseRecord mit
// <oFirstRecord>        Der Erste Record wird gegen den <oSecondRecord> geprüft und das Ergebnis
//                       wird in den aktuellen Record (SELF) geschrieben
// [lGetDifferentKeys]   Sollen die Keys auf Ungleichheit überprüft werden
// [lGetDifferentValues] Sollen die Values auf Ungleichheit überprüft werden
// [lOnlyReplace]        TRUE = Es werden nur die Einträge mit Replace = True berücksichtigt
//
// Beispiel:
// [lGetDifferentKeys] = TRUE, [lGetDifferentValues] = FALSE --> Es werden nur die Keys überprüft und die ungleichen Keys als Record zurückgegeben
// [lGetDifferentKeys] = FALSE, [lGetDifferentValues] = TRUE --> Ungeleiche Keys werden überlesen, bei gleichen Keys werden die Values überprüft

   LOCAL aDoc             AS ARRAY
   LOCAL x                AS INT

SELF:Kill()

aDoc := oFirstRecord:ChangeDocumentation( oSecondRecord, lOnlyReplace )
for x:=1 upto ALen( aDoc )
	if( lGetDifferentKeys .and. (aDoc[x][recDoc_Type] == recDoc_Added .or. aDoc[x][recDoc_Type] == recDoc_Deleted) )
      // Da die Keys überprüft werden sollen,
		// nur die, welche dazugekommen sind oder gelöscht wurden
		SELF:ImportFromArray( { aDoc[x][recDoc_RecordArray] }, TRUE )
	endif

	if(  lGetDifferentValues .and. aDoc[x][recDoc_Type] == recDoc_Modified )
		// Es sollen auch die Values überprüft werden. Nun die
		// unterschiedlichen Values zurückgeben
		SELF:ImportFromArray( { aDoc[x][recDoc_RecordArray] }, TRUE )
	endif
next x

METHOD GetEqualRecord( oFirstRecord AS P_BaseRecord, oSecondRecord AS P_BaseRecord, lGetDifferentKeys := TRUE AS LOGIC, lGetDifferentValues := TRUE AS LOGIC, lOnlyReplace := FALSE AS LOGIC ) AS VOID PASCAL CLASS P_BaseRecord
// Liefert ein neues P_BaseRecord mit
// <oFirstRecord>        Der Erste Record wird gegen den <oSecondRecord> geprüft und das Ergebnis
//                       wird in den aktuellen Record (SELF) geschrieben
// [lGetDifferentKeys]   Sollen die Keys auf Ungleichheit überprüft werden
// [lGetDifferentValues] Sollen die Values auf Ungleichheit überprüft werden
// [lOnlyReplace]        TRUE = Es werden nur die Einträge mit Replace = True berücksichtigt
//
// Beispiel:
// [lGetDifferentKeys] = TRUE, [lGetDifferentValues] = FALSE --> Es werden nur die Keys überprüft und die ungleichen Keys als Record zurückgegeben
// [lGetDifferentKeys] = FALSE, [lGetDifferentValues] = TRUE --> Ungeleiche Keys werden überlesen, bei gleichen Keys werden die Values überprüft

   LOCAL aDoc             AS ARRAY
   LOCAL x                AS INT

SELF:Kill()

aDoc := oFirstRecord:ChangeDocumentation( oSecondRecord, lOnlyReplace )
for x:=1 upto ALen( aDoc )
	if( lGetDifferentKeys .and. aDoc[x][recDoc_Type] != recDoc_Added .and. aDoc[x][recDoc_Type] != recDoc_Deleted )
		// Es sollen auch Keys berücksichtigt werden. Wenn der Key schon vorher da war nun zurückgeben
		SELF:ImportFromArray( { aDoc[x][recDoc_RecordArray] }, TRUE )
	endif

	if(  lGetDifferentValues .and. aDoc[x][recDoc_Type] == recDoc_UnModified )
		// Es sollen auch die Values überprüft werden. Nun die
		// unterschiedlichen Values zurückgeben
		SELF:ImportFromArray( { aDoc[x][recDoc_RecordArray] }, TRUE )
	endif
next x


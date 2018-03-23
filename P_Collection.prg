function P_Collection_UnitTest() AS VOID PASCAL

	LOCAL oCollection             AS P_Collection
	LOCAL oChange                 AS P_Collection
	LOCAL oSubCollection          AS P_Collection
	LOCAl x                       AS INT
	LOCAL aChanges                AS ARRAY

oCollection := P_Collection{}
oCollection:Add( "Finken", 10)
oCollection:Add( "Finken", 20)
oCollection:Add( "Finken", 30)
for x:=1 upto 10
	oCollection:Add( x, "Nummer "+NTrim(x))
next x
oCollection:Set( "Finken", 50 )
oCollection:Set( 5, "Neue Nummer 5")
oSubCollection := oCollection:GetN( "Finken" )
debugPrint( "FIN: ", __ENT, __LINE__, "SubCollection", oSubCollection:ExportToString())
oSubCollection:Release()
debugPrint( "FIN: ", __ENT, __LINE__, "Error", oCollection:lError, oCollection:oBase:GetMessageMemo())
oCollection:Release()

oCollection := P_Collection{}
oCollection:Dictionary := TRUE
oCollection:Add( #DAT_NEU, TODAY() )
oCollection:Add( #AUFTRAG, "Finken" )

oSubCollection := P_Collection{ oCollection:oBase }
oSubCollection:Add(#START, 10)
oSubCollection:Add(#ENDE, 50 )
oSubCollection:Add(#Modified, FALSE )
oChange := oSubCollection:GetCloneCollection()
oCollection:Add( #AUFPOS, 10.10, oChange )
oChange:Release()
oChange := oSubCollection:GetCloneCollection()
oCollection:Add( #BEZEICH, "Finkens Bezeichnung", oChange )
oChange:Release()
oSubCollection:Release()
oChange := oCollection:GetCloneCollection()
oChange:Set(#BEZEICH, "Geänderte Bezeichnung" )
oChange:Add(#KLASSE, "10")
oChange:Del(#DAT_NEU)

debugPrint( "FIN: ", __ENT, __LINE__, "oCollection", oCollection:ExportToString())
debugPrint( "FIN: ", __ENT, __LINE__, "oChange    ", oChange:ExportToString())
debugPrint( "FIN: ", __ENT, __LINE__, "Sub", oChange:GetSome(#Aufpos, #ENDE):Value)
aChanges := oChange:DocumentChanges( oCollection )
for x:=1 upto aLen(aChanges)
	debugPrint( "FIN: ", __ENT, __LINE__, aChanges[x][4] )
next x
oChange:Release()

oCollection:ExportToXML("\\pc-fin\temp\oCollection.xml", TRUE )
debugPrint( "FIN: ", __ENT, __LINE__, "Error", oCollection:lError, oCollection:oBase:GetMessageMemo())
oCollection:Release()

oCollection := P_Collection{}
oCollection:Dictionary := TRUE
oCollection:ImportFromXml("\\pc-fin\temp\oCollection.xml")
debugPrint( "FIN: ", __ENT, __LINE__, "XML", oCollection:ExportToString())
debugPrint( "FIN: ", __ENT, __LINE__, "XML-Sub", oCollection:GetSome(#AUFPOS):oChildCollection:ExportToString())
oCollection:ExportToXML( "\\pc-fin\temp\oCollectionNeu.xml", TRUE )
oCollection:Release()
//
// P_Collection : Eine Collection oder ein Dictionary-Object
// Autor: André Finken
//
//
//


CLASS P_Collection INHERIT AObject

	DECLARE ACCESS oBase                                      // Ein oBase-Object (Erstellt oder reingereicht) das an jede Instanz vererbt wird. Somit ...
	DECLARE ACCESS lError                                     // ...werden auch Protokolleinträge überall zugänglich und ein einheitlicher lError-Status
	DECLARE ACCESS Dictionary                                 // Gibt an ob die P_Collection uniqueKeys (TRUE) benutzt
	DECLARE ASSIGN Dictionary
	DECLARE METHOD Exists                                     // Prüft die Existenz eines Keys
	DECLARE METHOD Count                                      // Gibt die Anzahl der Einträge zurück
	DECLARE METHOD Add                                        // Fügt einen Eintrag hinzu (Key, Value). Existsiert der Eintrag bereits, gibt es einen Fehler
	DECLARE METHOD AddIfNotExists                             // Fügt einen Eintrag hinzu, wenn er nicht existiert.
	DECLARE METHOD AddAuto                                    // Fügt einen Eintrag mit AutoKey (numerisch) hinzu
	DECLARE METHOD AddCollection                              // Fügt eine ganze Collection an SELF an. Dictionary wird berücksichtigt
	DECLARE METHOD AddCollectionNode                          // Fügt eine vorhandene P_CollectionNode der P_Collection hinzu
	DECLARE METHOD Set                                        // Setzt ein Value für ein vorhandenen Key neu. Gibt es den Key nicht gibts ein Fehler
	DECLARE METHOD SetIfExists                                // Setzt ein Value für ein vorhandenen Key neu, wenn dieser existstiert
	DECLARE METHOD SetOrAdd                                   // Setzt ein Value-Wert. Existiert der Key nicht, so wird er neu angelegt
	DECLARE METHOD Del                                        // Löscht einen Key
	DECLARE METHOD DelCollectionNode                          // Löscht eine spezifische P_CollectionNode
	DECLARE METHOD Get                                        // Gibt die P_CollectionNode an einer spezifischen Stelle zurück
	DECLARE METHOD GetN                                       // Gibt eine P_Collection zurück mit den geforderten Einträgen (Key oder Codeblock)
	DECLARE METHOD GetSome                                    // Gibt den ersten auffindbaren P_CollectionNode zu einem Key oder Codeblock zurück
	DECLARE METHOD GetCollection                              // s. auch GETN. Es wird eine neue P_Collection mit dem Inhalt der Suche zurückgegeben
	DECLARE METHOD ForEach                                    // Arbeitet in einem Codeblock alle Einträge der aktuellen Collection durch
	DECLARE METHOD Sort                                       // Sortiert die aktuelle Collection nach Key oder mit einem Codeblock

	DECLARE METHOD GetDiffCollection                          // Gibt die Differenz der aktuellen Collectionen auf Basis einer weiteren Collection als P_Collectoin zurück
	DECLARE METHOD GetGroupCollection                         // Gruppiert die Collection nach Key und gibt eine neue P_Collection mit den Keys zurück (auf Value ist dann eine P_Collection mit den verweisen)
	DECLARE METHOD GetEmptyCollection                         // Gibt eine leere Kopie der aktuellen Collection zurück
	DECLARE METHOD GetCopyCollection                          // Kopiert die aktuelle Collection (mit Verweisen) und gibt die zurück
	DECLARE METHOD GetCloneCollection								 // Cloned die aktuelle Collection (auch alle enthaltenen P_CollectionNode) und gibt das neue Objekt zurück
	DECLARE METHOD DocumentChanges                            // Dokumentiert veränderungen zweier P_Collection

	DECLARE METHOD Sum                                        // Gibt die Summe aller Values zurück
	DECLARE METHOD Min                                        // Gibt den Min-Wert aller Values zurück
	DECLARE METHOD Max                                        // Gibt den Max-Wert aller Values zurück
	DECLARE METHOD Average                                    // Gibt den Durchschnittswert aller Values zurück

	/* Export */
	DECLARE METHOD ExportToArray                              // Exportiert die aktuelle P_Collection in ein Array
	DECLARE METHOD ExportToString                             // Exportiert die aktuelle P_Collection in einen String
	DECLARE METHOD ExportToRecord                             // Schreibt die aktuelle P_Collection, wenn das Feld auf Key (#Symbol) im Record vorhanden ist, in den Record zurück
	DECLARE METHOD ExportToXML                                // Exportiert die aktuelle P_Collection (und alle auf oChildCollection angehängten) in eine XML-Datei zum späteren Import

	/* Import */
	DECLARE METHOD ImportFromArray                            // Importiert aus einem Array in die aktuelle Collection
	DECLARE METHOD ImportFromRecordOrServer                   // Füllt die aktuelle Collection aus einem ARead-, AWriteRecord oder aus einem AServer als #Key, uValue
	DECLARE METHOD ImportFromTable                            // Füllt die aktuelle Collection aus einer Tabelle als #Key, Value
	DECLARE METHOD ImportFromString                           // Importiert aus einem String in die aktuelle Collection
	DECLARE METHOD ImportFromXML                              // Importiert eine mit ExportToXML exportierte P_Collection in die aktuelle Collection

	/* Interne Methoden */
	DECLARE METHOD __GetCollection
	DECLARE METHOD __HelpExportXml
	DECLARE METHOD __HelpImportXml
	DECLARE METHOD __ConvertToUsual

	PROTECT __oBase                   AS P_Base
	PROTECT __aListe                  AS ARRAY
	PROTECT __lUniqueKey              AS LOGIC

METHOD Init( oBase ) CLASS P_Collection

	SUPER:Init()

	if( !IsNil( oBase ) .and. IsInstanceOfUsual(oBase, #P_Base))
		SELF:__oBase := oBase
		SELF:__oBase:AddRef()
	else
		SELF:__oBase := P_Base{}
	endif

	SELF:__aListe := {}
	SELF:__lUniqueKey := FALSE

METHOD Destroy() AS VOID PASCAL CLASS P_Collection

	LOCAL x                    AS INT
	LOCAL oNode                AS P_CollectionNode

for x:=1 upto alen( SELF:__aListe )
	oNode := SELF:__aListe[x]
	oNode:Release()
next x

SELF:__oBase:Release()
SUPER:Destroy()

ACCESS oBase AS P_Base PASCAL CLASS P_Collection
return( SELF:__oBase )

ACCESS lError AS LOGIC PASCAL CLASS P_Collection
return( SELF:__oBase:lError )

ACCESS Dictionary AS LOGIC PASCAL CLASS P_Collection
return( SELF:__lUniqueKey )

ASSIGN Dictionary( lNewValue AS LOGIC ) AS LOGIC PASCAL CLASS P_Collection
SELF:__lUniqueKey := lNewValue
return( SELF:__lUniqueKey )

METHOD AddCollectionNode( oNode AS P_CollectionNode, oChildCollection := nil AS USUAL ) AS VOID PASCAL CLASS P_Collection
// Fügt eine P_CollectionNode an die P_Collection an.

if( SELF:Dictionary .and. SELF:Exists( {|o,b| ValType(o:Key) == ValType(oNode:Key) .and. o:Key == oNode:Key } ) )
	SELF:oBase:MessageFormat( 'Eindeutigkeit verletzt in # beim einfügen von Key="#" und Value="#". Ein Eintrag mit Key="#" ist bereits vorhanden. Der Eintrag wird nicht hinzugefügt.', { __ENT, oNode:Key, oNode:Value, oNode:Key }, PROT_ART_ERROR )
else
	oNode:AddRef()
	if( oChildCollection != nil )
		oNode:oChildCollection := oChildCollection
	endif
	aadd( SELF:__aListe, oNode )
endif

METHOD Add( uKey AS USUAL, uValue AS USUAL, oChildCollection := nil AS USUAL ) AS VOID PASCAL CLASS P_Collection
// Fügt eine P_CollectionNode mit <uKey> und <uValue> an
	LOCAL oNode           AS P_CollectionNode

oNode := P_CollectionNode{ uKey, uValue }
SELF:AddCollectionNode( oNode, oChildCollection )
oNode:Release()

METHOD AddAuto( uValue AS USUAL ) AS INT PASCAL CLASS P_Collection
// Es wird eine Key-Nummer ermittelt und eine neue P_CollectionNode mit Key=Nummer und Value=uValue angehängt
	LOCAL nID := 0      AS INT
SELF:ForEach( { |oNode,oBase,cKey,cValue, nValue| IIF(IsNumeric(oNode:Key) .and. oNode:Key > nID, nID := oNode:Key, FALSE ) } )
nID += 1
SELF:Add( nId, uValue )
return( nID )

METHOD AddIfNotExists( uKey AS USUAL, uValue AS USUAL, oChildCollection := nil AS USUAL ) AS VOID PASCAL CLASS P_Collection
// Fügt uKey an, wenn er nicht schon vorhanden ist
if( !SELF:Exists(uKey) )
	SELF:Add( uKey, uValue, oChildCollection )
endif

METHOD AddCollection( oCollection AS P_Collection ) AS VOID PASCAL CLASS P_Collection
// Fügt eine komplette P_Collection hinzu.
// Wichtig: die Nodes sind die gleichen wie in <oCollection>
	LOCAL x                 AS INT

for x:=1 upto oCollection:Count()
	SELF:AddCollectionNode( oCollection:Get(x) )
next x

METHOD Set( uKey AS USUAL, uValue AS USUAL ) AS VOID PASCAL CLASS P_Collection
// Setzt für einen vorhanden Key <uKey> die uValue um
	LOCAL oCollection          AS P_Collection
	LOCAL oNode                AS P_CollectionNode
	LOCAl x                    AS INT

oCollection := SELF:GetCollection( uKey )
if( oCollection:Count() == 0 )
	SELF:oBase:MessageFormat( 'Fehler in # beim setzen von Key="#" und Value"#". Der Key "#" wurde in der Collection nicht gefunden. Anderfalls SetOrAdd() benutzen.', { __ENT, uKey,uValue,uKey }, PROT_ART_ERROR )
else
	for x:=1 upto oCollection:Count()
		oNode := oCollection:Get(x)
		oNode:Value := uValue
	next x
endif
oCollection:Release()

METHOD SetIfExists( uKey AS USUAL, uValue AS USUAL ) AS VOID PASCAL CLASS P_Collection
// Setzt uValue, wenn der Key existiert
if( SELF:Exists( uKey ) )
	SELF:Set( uKey, uValue )
endif

METHOD SetOrAdd( uKey AS USUAL, uValue AS USUAL ) AS VOID PASCAL CLASS P_Collection
// Setzt uValue, wenn der Key existiert, sonst wird ein neuer Satz angehängt
if( SELF:Exists( uKey ) )
	SELF:Set(uKey,uValue)
else
	SELF:Add(uKey,uValue)
endif

METHOD Count() AS INT PASCAL CLASS P_Collection
// Gibt die Anzahl der enthaltenen P_CollectionNodes zurück
return( aLen( SELF:__aListe ) )

METHOD Exists( uKeyOrCodeblock AS USUAL ) AS LOGIC PASCAL CLASS P_Collection
// Prüft auf vorhandensein eines Keys in der aktuellen P_Collection
// Es kann auch ein Codeblock im Format { |oNode,oBase| TRUE } übergeben werden
// [uKeyOrCodeblock] : USUAL Key
//                   : CODEBLOCK : { |oNode,oBase| TRUE }

	LOCAL lExists := FALSE       AS LOGIC
	LOCAL oCollection            AS P_Collection

oCollection	:= SELF:GetCollection( uKeyOrCodeblock )
lExists := oCollection:Count() > 0
oCollection:Release()
return( lExists )

METHOD DelCollectionNode( oNode AS P_CollectionNode ) AS VOID PASCAL CLASS P_Collection
// Löscht die <oNode> aus den P_CollectionNode der aktuellen Collection
	LOCAL x         AS INT

for x:=1 upto aLen( SELF:__aListe )
	if( SELF:__aListe[x] == oNode )
		oNode:Release()
		aDelShrink( SELF:__aListe, x )
		EXIT
	endif
next x


METHOD Del( uKey AS USUAL ) AS VOID PASCAL CLASS P_Collection
// Löscht alles Knoten mit <uKey> aus der aktuellen Collection
	LOCAL oCollection      AS P_Collection
	LOCAL x                AS INT

oCollection := SELF:GetN(uKey)
for x:=1 upto oCollection:Count()
	// Die Node in SELF löschen, damit die for/next weiter funktioniert
	SELF:DelCollectionNode( oCollection:Get(x) )
next x
oCollection:Release()

METHOD Sort( cbCodeblock := nil AS USUAL ) AS VOID PASCAL CLASS P_Collection
// Sortiert die aktuelle Collection. Wird nichts übergeben, so wird der Key aufsteigend sortiert.
// [cbCodeblock] : { |oNode1,oNode2| oNode1:Key <= oNode2:Key }

cbCodeblock := SELF:oBase:PrepareCodeblock( cbCodeblock, { |x,y| x:Key <= y:Key } )
ASort( SELF:__aListe,,, cbCodeblock )


METHOD GetCollection( uKeyOrCodeBlock := nil AS USUAL ) AS P_Collection PASCAL CLASS P_Collection
// Gibt eine Collection zum Suchbegriff zurück
// [uKeyOrCodeblock] : USUAL     : KEY
//                   : CODEBLOCK : { |oNode,oBase| TRUE }
// !!! Achtung !!! Release() der zurückgegebenen Collection
	LOCAL oCollection         AS P_Collection
	LOCAL cbCodeblock         AS CODEBLOCK

cbCodeblock := IIF( UsualType( uKeyOrCodeBlock ) == CODEBLOCK, uKeyOrCodeBlock, { |oNode, oBase| ValType(oNode:Key) == ValType(uKeyOrCodeBlock) .and. oNode:Key == uKeyOrCodeBlock } )
oCollection := SELF:__GetCollection( cbCodeblock )
return( oCollection )

METHOD GetGroupCollection() AS P_Collection PASCAL CLASS P_Collection
// Gibt eine neue P_Collection zurück. Die Knoten verweisen immer noch auf SELF
// Es werden alle doppelten Keys gruppiert. Auf VALUE wird eine P_Collection mit den einzelnen Einträgen gespeichert
// !!! Achtung !!! Release() der zurückgegebenen Collection
	LOCAL oCollection            AS P_Collection
	LOCAL oDupplicates           AS P_Collection
	LOCAl oNode                  AS P_CollectionNode
	LOCAL x                      AS INT

oCollection := SELF:GetEmptyCollection()
oCollection:Dictionary := TRUE
for x:=1 upto SELF:Count()
	oNode := SELF:Get(x)
	if( !oCollection:Exists( {|o,b| ValType(o:Key) == ValType(oNode:Key) .and. o:Key == oNode:Key } ) )
		oDupplicates := SELF:GetN( oNode:Key )
		oCollection:Add( oNode:Key, oDupplicates )
		oDupplicates:Release()
	endif
next x
return( oCollection )

METHOD GetDiffCollection( oBaseCollection AS P_Collection, nType AS INT ) AS P_Collection PASCAL CLASS P_Collection
// Gibt eine Collection mit den Differenzen raus. Verglichen wird, was sich in SELF zu oBaseCollection verändert hat
// Wurde in SELF ein knoten gelöscht, so wird dieser nicht mit zurückgegeben. Eigentlich selbstsprechend
// <oBaseCollection> Die P_Collection, so zu sagen die alte Collection
// <nType> : 1  -> Es werden alle Veränderungen zurückgegeben
//           2  -> Es werden die nicht veränderten zurückgegeben
// !!! Achtung !!! Die P_Collection muss released werden

	LOCal oDiff                 AS P_Collection
	LOCAL x                     AS INT
	LOCAL lExists               AS LOGIC
	LOCAL oOtherNode            AS P_CollectionNode

oDiff := SELF:GetEmptyCollection()
for x:=1 upto SELF:Count()
	oOtherNode := SELF:Get(x)
	lExists := oBaseCollection:Exists( {|oNode| ValType(oNode:Key) == ValType(oOtherNode:Key) .and.;
																ValType(oNode:Value) == ValType(oOtherNode:Value) .and.;
																oNode:Key == oOtherNode:Key .and.;
																oNode:Value == oOtherNode:Value } )

	if( nType == 1 .and. !lExists ) .or. ( nType == 2 .and. lExists )
		oDiff:AddCollectionNode( oOtherNode )
	endif
next x
return( oDiff )

METHOD DocumentChanges( oOtherCollection AS P_Collection ) AS ARRAY PASCAL CLASS P_Collection
// Es findet eine Änderungsdokumentation statt. <SELF> ist die gegenüber <oOtherCollection> veränderte Collection
// Ein mehrdimensionales Array mit folgenden Informationen wird zurückgegeben:
// #Type  : #INS  - Wurde in SELF hinzugefügt       : {#INS, oSelfNode,   NULL_OBJECT, "Inserttext"   }
//          #DEL  - Wurde in SELF gelöscht          : {#DEL, NULL_OBJECT, oOtherNode , "Löschttext"   }
//          #MOD  - Wurde verändert                 : {#MOD, oSelfNode,   oOtherNode , "Änderungstext }
//
	LOCAL aChanges                AS ARRAY
	LOCAL oSelfNode               AS P_CollectionNode
	LOCAL oOtherNode              AS P_CollectionNode
	LOCAL oDiff                   AS P_Collection
	LOCAL x                       AS INT

aChanges := {}
// Neu #INS und Veränderte #MOD prüfen
for x:=1 upto SELF:Count()
	oSelfNode := SELF:Get(x)
	oDiff := oOtherCollection:GetN( oSelfNode:Key )
	do case
	case( oDiff:Count() == 0 )
		// oSelfNode ist neu dazugekommen
		aadd( aChanges, { #INS, oSelfNode, NULL_OBJECT, SELF:oBase:StringFormat( 'Neu: Key="#" und Value="#" sind neu hinzugekommen', { oSelfNode:Key, oSelfNode:Value } ) } )
	case( oDiff:Count() > 1 )
		// Es sind mehrere gleiche Keys vorhanden
		// Eine genaue Prfung ist nicht möglich
		aadd( aChanges, { #MOD, oSelfNode, oDiff:Get(1), SELF:oBase:StringFormat( 'Geändert: Key="#" und Value="#" wurden verändert, sind jedoch nicht zuordenbar. SELF:Dictionary = #, Other:Dictionary = #', { oSelfNode:Key, oSelfNode:Value, SELF:Dictionary, oOtherCollection:Dictionary } )  } )
	otherwise
		// oSelfNode ist in oOtherCollection ein Mal vorhanden
		// Nun prüfen ob Veränderung stattgefunden hat
		oOtherNode := oDiff:Get(1)
		if .not. ( ValType(oSelfNode:Value) == ValType(oOtherNode:Value) .and. oSelfNode:Value == oOtherNode:Value )
			aadd( aChanges, { #MOD, oSelfNode, oOtherNode, SELF:oBase:StringFormat( 'Geändert: Key="#", von "#" in "#" geändert.', { oSelfNode:Key, oOtherNode:Value, oSelfNode:Value } ) } )
		endif
	endcase
	oDiff:Release()
next x

// Gelöschte #DEL prüfen
for x:=1 upto oOtherCollection:Count()
	oOtherNode := oOtherCollection:Get(x)
	if( !SELF:Exists( oOtherNode:Key ) )
   	aadd( aChanges, { #DEL, NULL_OBJECT, oOtherNode, SELF:oBase:StringFormat( 'Gelöscht: Key="#" und Value="#" wurden gelöscht.', { oOtherNode:Key, oOtherNode:Value } ) } )
	endif
next x

return( aChanges )

METHOD Get( nEntry AS INT ) AS P_CollectionNode PASCAL CLASS P_Collection
// Gibt die P_CollectionNode einer bestimmten Position zurück
if( nEntry > 0 .and. nEntry <= SELF:Count() )
	return( SELF:__aListe[nEntry] )
endif
return( NULL_OBJECT )

METHOD GetN( uKey AS USUAL ) AS P_Collection PASCAL CLASS P_Collection
// s. GetCollection()
// !!! Achtung !!! Die P_Collection muss released werden
return( SELF:GetCollection( uKey ) )

METHOD GetSome( uKey AS USUAL, uChildCollectionKey := nil AS USUAL ) AS P_CollectionNode PASCAL CLASS P_Collection
// Es wird eine P_CollectionNode zum Key <uKey> zurückgegeben. Sollte es doppelte Keys geben, so wird der erste Eintrag zurückgegeben
// Sollte der Eintrag nicht existieren, so wird es einen Absturz beim Aufruf von GetSome() geben. Dieser ist aber durch eine Warnung
// dokumentiert.
// <uKey>                : Es wird die erste gefundene Node zum Key zurückgegeben
// [uChildCollectionKey] : Es wird aus der P_CollectionNode zu <uKey> die oChildCollection durchsucht und die P_CollectionNode zu [uChildCollectionKey] zurückgegben

	LOCAL oCollection           AS P_Collection
	LOCAL oNode                 AS P_CollectionNode

oCollection := SELF:GetCollection(uKey)
if( oCollection:Count() > 0 )
	oNode := oCollection:Get(1)
	if( oCollection:Count() > 1 )
		SELF:oBase:MessageFormat( 'Warnung bei Zugriff in # aus Knoten mit Key="#". Es wurden mehrere Einträge zu dem Key gefunden. Es wird Value="#" zurückgegeben', { __ENT, uKey, oNode:Value }, PROT_ART_WARNING )
	endif

	if( !IsNil( uChildCollectionKey ) )
		// Es soll ein Eintrag aus der oChildCollection genommen werden
		oNode := oNode:oChildCollection:GetSome( uChildCollectionKey )
	endif
else
	SELF:oBase:MessageFormat( 'Warnung bei Zugriff in # aus Knoten mit Key="#". Der Key wurde nicht gefunden und es wird # zurückgegeben', { __ENT, uKey, oNode }, PROT_ART_WARNING )
endif
oCollection:Release()
return( oNode )

METHOD ForEach( cbCodeblock AS CODEBLOCK, uSearchKeyOrCodeblock := nil AS USUAL ) AS VOID PASCAL CLASS P_Collection
// Geht jede P_CollectionNode in der aktuellen Collection durch und führt den Codeblock <cbCodeblock> aus.
// Um den Codeblock einfacher zu halten, kann ein Key oder Codeblock in <uSearchKeyOrCodeblock> im Format { |oNode,oBase| TRUE } übergeben werden.
// <cbCodeblock>           := { |oNode,oBase,cKey,cValue, nValue| }
// [uSearchKeyOrCodeblock] := { |oNode,oBase| TRUE }
		LOCAl x         AS INT
		LOCAL cbSearch  AS CODEBLOCK

do case
case( isNil( uSearchKeyOrCodeblock ) )
	cbSearch := { |oNode, oBase| TRUE }
case( UsualType( uSearchKeyOrCodeblock ) == CODEBLOCK )
	cbSearch := uSearchKeyOrCodeblock
otherwise
	cbSearch := { |oNode, oBase| ValType(oNode:Key) == ValType( uSearchKeyOrCodeblock ) .and. oNode:Key == uSearchKeyOrCodeblock }
endcase

for x:=1 upto SELF:Count()
	if( eVal( cbSearch, SELF:Get(x), SELF:oBase ) )
		eVal( cbCodeblock, SELF:Get(x), SELF:oBase, SELF:oBase:UsualToString( SELF:Get(x):Key ), SELF:oBase:UsualToString( SELF:Get(x):Value ), SELF:oBase:UsualToNumeric(SELF:Get(x):Value) )
	endif
next x

METHOD Sum( uKeyOrCodeBlock := nil AS USUAL ) AS REAL8 PASCAL CLASS P_Collection
// Gibt die Summe der Values zum Key zurück
// Wird ein Codeblock angegeben, so kann die Suche noch verfeinert werden
// Codeblock: { |oNode, oBase| TRUE / FALSE }
	LOCAL nSum := 0         AS REAL8

SELF:ForEach( { |oNode,oBase,cKey,cValue, nValue| nSum += nValue }, uKeyOrCodeBlock )
return( nSum )

METHOD Average( uKeyOrCodeBlock := nil AS USUAL ) AS REAL8 PASCAL CLASS P_Collection
// Gibt den Durchschnitt der Values zum Key zurück
// Wird ein Codeblock angegeben, so kann die Suche noch verfeinert werden
// Codeblock: { |oNode, oBase| TRUE / FALSE }

	LOCAL nAnzahl, nSum         AS REAL8

nSum    := 0
nAnzahl := 0
SELF:ForEach( { |oNode,oBase,cKey,cValue, nValue| nSum += nValue, nAnzahl += 1 }, uKeyOrCodeBlock )
return( nSum / IIF( nAnzahl == 0, 1 , nAnzahl ) )

METHOD Min( uKeyOrCodeBlock := nil AS USUAL ) AS REAL8 PASCAL CLASS P_Collection
// Gibt den kleinsten Wert der Values zum Key zurück
// Wird ein Codeblock angegeben, so kann die Suche noch verfeinert werden
// Codeblock: { |oNode, oBase| TRUE / FALSE }

	LOCAL nMin              AS REAL8

nMin := -9999.99999
SELF:ForEach( { |oNode, oBase, cKey, cValue, nValue| nMin := IIF( nMin == -9999.99999, nValue, Min(nMin, nValue) ) }, uKeyOrCodeBlock )
return( nMin )

METHOD Max( uKeyOrCodeBlock := nil AS USUAL ) AS REAL8 PASCAL CLASS P_Collection
// Gibt den größten Wert der Values zum Key zurück
// Wird ein Codeblock angegeben, so kann die Suche noch verfeinert werden
// Codeblock: { |oNode, oBase| TRUE / FALSE }

	LOCAL nMax              AS REAL8

nMax := -9999.99999
SELF:ForEach( { |oNode, oBase, cKey, cValue, nValue| nMax := IIF( nMax == -9999.99999, nValue, Max(nMax, nValue) ) }, uKeyOrCodeBlock )
return( nMax )

METHOD __GetCollection( cbCodeBlock AS CODEBLOCK ) AS P_Collection PASCAL CLASS P_Collection

	LOCAL oCollection             AS P_Collection
	LOCAL x                       AS INT

oCollection := P_Collection{ SELF:oBase }
oCollection:Dictionary := SELF:Dictionary
for x:=1 upto aLen( SELF:__aListe )
	if( eVal(cbCodeBlock, SELF:__aListe[x] ) )
		oCollection:AddCollectionNode( SELF:__aListe[x] )
	endif
next x
return( oCollection )

METHOD GetEmptyCollection() AS P_Collection PASCAL CLASS P_Collection
// Kopiert SELF (mit oBase etc.) ohne Einträge und gibt die leere P_Collection zurück
// !!! Achtung !!! Die P_Collection muss released werden
return( SELF:__GetCollection( { |oNode, oBase| FALSE } ) )

METHOD GetCopyCollection( cbCodeblock := nil AS USUAL ) AS P_Collection PASCAL CLASS P_Collection
// Kopiert SELF (mit oBase etc.) und allen Einträgen und gibt diese zurück
// Dies ist keine echte Kopie (s. GetCloneCollection), sondern es wird noch auf die P_CollectionNode(s) von SELF verwiesen
// !!! Achtung !!! Die P_Collection muss released werden
cbCodeBlock := SELF:oBase:PrepareCodeblock( cbCodeblock, { |oNode, oBase| TRUE } )
return( SELF:__GetCollection( cbCodeblock ) )

METHOD GetCloneCollection( cbCodeblock := nil AS USUAL ) AS P_Collection PASCAL CLASS P_Collection
// Hier wird eine echte Kopie von SELF erzeugt. Es wird auch jede P_CollectionNode kopiert
// Es entsteht so inhaltlich ein komplett neues Object
// !!! Achtung !!! Die P_Collection muss released werden
	LOCAL oCollection            AS P_Collection
	LOCAL oNode                  AS P_CollectionNode
	LOCAL x                      AS INT

cbCodeBlock := SELF:oBase:PrepareCodeblock( cbCodeblock, { |oNode, oBase| TRUE } )
oCollection := SELF:GetEmptyCollection()
for x:=1 upto SELF:Count()
	if( EVal( cbCodeblock, SELF:Get(x), SELF:oBase ) )
		oNode := SELF:Get(x):Clone()
		oCollection:AddCollectionNode( oNode )
		oNode:Release()
	endif
next x
return( oCollection )

METHOD ExportToArray( cbSearch := nil AS USUAL, cbKeyConvert := nil AS USUAL, cbValueConvert := nil AS USUAL ) AS ARRAY PASCAL CLASS P_Collection
// Exportiert SELF in ein Array im Format {{Key,Value},{Key,Value},...}
// [cbSearch]       : Grenzt die zu übergebenden Nodes ein           : { |oNode, oBase| TRUE }
// [cbKeyConvert]   : Konvertiert den Key in ein anderes Format      : { |oNode, oBase| oNode:Key }
// [cbValueConvert] : Konvertiert den Value in ein anderes Format    : { |oNode, oBase| oNode:Value }

	LOCAL aResult               AS ARRAY
	LOCAL x                     AS INT

cbSearch      := SELF:oBase:PrepareCodeblock( cbSearch, { |oNode, oBase| TRUE } )
cbKeyConvert  := SELF:oBase:PrepareCodeblock( cbKeyConvert, { |oNode, oBase| oNode:Key } )
cbValueConvert:= SELF:oBase:PrepareCodeblock( cbValueConvert, { |oNode, oBase| oNode:Value } )
aResult := {}

for x:=1 upto SELF:Count()
	if( eVal( cbSearch, SELF:Get(x), SELF:oBase ) )
		aadd( aResult, { eVal( cbKeyConvert, SELF:Get(x), SELF:oBase ), eVal( cbValueConvert, SELF:Get(x), SELF:oBase ) } )
	endif
next x
return( aResult )

METHOD ExportToRecord( oRecordOrServer AS USUAL, cbSearch := nil AS USUAL, cbKeyConvert := nil AS USUAL, cbValueConvert := nil AS USUAL ) AS LOGIC PASCAL CLASS P_Collection
// Exportiert SELF in einen offenen AServer oder einen AWriteRecord
// [cbSearch]       : Grenzt die zu übergebenden Nodes ein           : { |oNode, oBase| TRUE }
// [cbKeyConvert]   : Konvertiert den Key in ein anderes Format      : { |oNode, oBase| oNode:Key }
// [cbValueConvert] : Konvertiert den Value in ein anderes Format    : { |oNode, oBase| oNode:Value }
cbKeyConvert := SELF:oBase:PrepareCodeblock( cbKeyConvert, { |oNode, oBase| oBase:UsualToSymbol( oNode:Key ) } )
return( SELF:oBase:ArrayToRecord( SELF:ExportToArray( cbSearch, cbKeyConvert, cbValueConvert ), oRecordOrServer, TRUE ) )

METHOD ExportToString( cbSearch := nil AS USUAL, cbConvert := nil AS USUAL) AS STRING PASCAL CLASS P_Collection
// Exportiert SELF in einen String im Format "Key, Value "+CRLF+"Key, Value"
// [cbSearch]       : Grenzt die zu übergebenden Nodes ein           : { |oNode, oBase| TRUE }
// [cbConvert]      : Konvertiert den Key in ein anderes Format      : { |oNode, oBase| oNode:Key + oNode:Value }

	LOCAL cResult               AS STRING
	LOCAL x                     AS INT

cbSearch      := SELF:oBase:PrepareCodeblock( cbSearch, { |oNode, oBase| TRUE } )
cbConvert     := SELF:oBase:PrepareCodeblock( cbConvert, { |oNode, oBase| oBase:UsualToString(oNode:Key) + ", " + oBase:UsualToString( oNode:Value ) +CRLF } )
cResult       := ""
for x:=1 upto SELF:Count()
	if( eVal( cbSearch, SELF:Get(x), SELF:oBase ) )
		cResult += eVal( cbConvert, SELF:Get(x), SELF:oBase )
	endif
next x
return( cResult )

METHOD ImportFromArray( aArray AS ARRAY, cbSearch := nil AS USUAL, cbKeyConvert := nil AS USUAL, cbValueConvert := nil AS USUAL ) AS VOID PASCAL CLASS P_Collection
// Es werden Einträge aus dem <aArray> als P_CollectionNodes zu SELF hinzugefügt
// Welche Elemente aus dem Array wohin kommen, kann durch die Codeblöcke selbst bestimmt werden
// [cbSearch]       : Grenzt die zu übergebenden Nodes ein           : { |aArrayElement, oBase| aLen(aArrayElement) == 2  }
// [cbKeyConvert]   : Konvertiert den Key in ein anderes Format      : { |aArrayElement, oBase| aArrayElement[1] }
// [cbValueConvert] : Konvertiert den Value in ein anderes Format    : { |aArrayElement, oBase| aArrayElement[2] }
	LOCAL x             AS INT
	LOCAL oNode         AS P_CollectionNode

cbSearch      := SELF:oBase:PrepareCodeblock( cbSearch, { |aArrayElement, oBase| aLen(aArrayElement) == 2  } )
cbKeyConvert  := SELF:oBase:PrepareCodeblock( cbKeyConvert, { |aArrayElement, oBase| aArrayElement[1] } )
cbValueConvert:= SELF:oBase:PrepareCodeblock( cbValueConvert, { |aArrayElement, oBase| aArrayElement[2] } )
for x:= 1 upto aLen( aArray )
	if( eVal( cbSearch, aArray[x], SELF:oBase ) )
		oNode := P_CollectionNode{}
		oNode:Key   := eVal( cbKeyConvert, aArray[x], SELF:oBase )
		oNode:Value := eVal( cbValueConvert, aArray[x], SELF:oBase )
		SELF:AddCollectionNode( oNode )
		oNode:Release()
	endif
next x

METHOD ImportFromRecordOrServer( oRecordOrServer AS USUAL, cbSearch := nil AS USUAL, cbKeyConvert := nil AS USUAL, cbValueConvert := nil AS USUAL ) AS LOGIC PASCAL CLASS P_Collection
// Es werden Einträge aus dem <oRecordOrServer> (AServer, AREadRecord oder AWriteRecord) als P_CollectionNodes zu SELF hinzugefügt
// Der Record wird erst in ein Key,Value-Array konvertiert, was den Inhalt der Codeblöcke erklärt
// Welche Elemente aus dem Array wohin kommen, kann durch die Codeblöcke selbst bestimmt werden
// [cbSearch]       : Grenzt die zu übergebenden Nodes ein           : { |aArrayElement, oBase| TRUE }
// [cbKeyConvert]   : Konvertiert den Key in ein anderes Format      : { |aArrayElement, oBase| aArrayElement[1] }
// [cbValueConvert] : Konvertiert den Value in ein anderes Format    : { |aArrayElement, oBase| aArrayElement[2] }

	LOCAL oRecord              AS AReadRecord
	LOCAL oServer              AS AServer
	LOCAL lError := FALSE      AS LOGIC
	LOCAL aArray               AS ARRAY

do case
case( IsInstanceOfUsual( oRecordOrServer, #AServer ) )
	oServer := oRecordOrServer
	oRecord := AReadRecord{oServer, TRUE}
case( IsInstanceOfUsual( oRecordOrServer, #AReadRecord ) )
	oRecord := oRecordOrServer
otherwise
	lError := TRUE
endcase

if( !lError )
	aArray := SELF:oBase:ArrayFromRecord( oRecord )
	SELF:ImportFromArray( aArray, cbSearch, cbKeyConvert, cbValueConvert )

	if( IsInstanceOfUsual( oRecordOrServer, #AServer ) )
		oRecord:Release()
	endif
endif

return( !lError )

METHOD ImportFromTable( symTable AS SYMBOL, aKeyFields AS ARRAY, cTableName AS STRING, cbSearch := nil AS USUAL, cbKeyConvert := nil AS USUAL, cbValueConvert := nil AS USUAL  ) AS LOGIC PASCAL CLASS P_Collection
// Es wird ein ReadRecord aus dem Table <symTable> mit <aKeyFields> {{#AUFTRAG, "FIN"},{#AUFPOS,10}} gezogen.
// Der Record wird erst in ein Key,Value-Array konvertiert, was den Inhalt der Codeblöcke erklärt
// Welche Elemente aus dem Array wohin kommen, kann durch die Codeblöcke selbst bestimmt werden
// [cbSearch]       : Grenzt die zu übergebenden Nodes ein           : { |aArrayElement, oBase| TRUE }
// [cbKeyConvert]   : Konvertiert den Key in ein anderes Format      : { |aArrayElement, oBase| aArrayElement[1] }
// [cbValueConvert] : Konvertiert den Value in ein anderes Format    : { |aArrayElement, oBase| aArrayElement[2] }

	LOCAL oRecord         AS AReadRecord
	LOCAl lOK := TRUE     AS LOGIC

oRecord := SELF:oBase:GetReadRecordFromTable( symTable, aKeyFields, cTableName )
if( oRecord != NULL_OBJECT )
	lOK := SELF:ImportFromRecordOrServer( oRecord, cbSearch, cbKeyConvert, cbValueConvert )
	oRecord:Release()
else
	lOK := FALSE
endif
return( lOK )

METHOD ImportFromString( cString AS STRING, cPairDelimiter := ";" AS STRING, cKeyValueDelimiter := "," AS STRING, cbSearch := nil AS USUAL, cbKeyConvert := nil AS USUAL, cbValueConvert := nil AS USUAL  ) AS VOID PASCAL CLASS P_Collection
// Importiert Key,Value-Paare aus einem String und fügt diese an SELF an.
// <String>         : Der String, z.B. im Format "Key1,Value;Key2,Value"
// [cPairDelimiter] : Trennt die Key/Value Paare ab
// [cKeyValueDei..] : Trennt Key von Value ab
// [cbSearch]       : Grenzt die zu übergebenden Nodes ein           : { |aArrayElement, oBase| TRUE }
// [cbKeyConvert]   : Konvertiert den Key in ein anderes Format      : { |aArrayElement, oBase| aArrayElement[1] }
// [cbValueConvert] : Konvertiert den Value in ein anderes Format    : { |aArrayElement, oBase| aArrayElement[2] }

	LOCAL aArray           AS ARRAY
	LOCAl aKeyValue        AS ARRAY
	LOCAL aResults         AS ARRAY
	LOCAl x                AS INT
	LOCAL lOK := TRUE      AS LOGIC

cbSearch      := SELF:oBase:PrepareCodeblock( cbSearch, { |aArrayElement, oBase| aLen(aArrayElement) == 2  } )
cbKeyConvert  := SELF:oBase:PrepareCodeblock( cbKeyConvert, { |aArrayElement, oBase| aArrayElement[1] } )
cbValueConvert:= SELF:oBase:PrepareCodeblock( cbValueConvert, { |aArrayElement, oBase| aArrayElement[2] } )
aResults := {}
aArray   := SELF:oBase:StringToArray( cString, {cPairDelimiter} )
for x:=1 upto aLen(aArray)
	aKeyValue := SELF:oBase:StringToArray( aArray[x], {cKeyValueDelimiter} )
	if( aLen(aKeyValue) == 2 )
		if( eVal( cbSearch, aKeyValue, SELF:oBase ) )
			aadd( aResults, { aKeyValue[1], aKeyValue[2] } )
		else
         lOK := FALSE
         SELF:oBase:MessageFormat( "Fehler beim #. Kein Key/Value-Paar in Zeile # gefunden. Syntax im String: Key#Value#Key#Value#", { __ENT, x, cKeyValueDelimiter, cPairDelimiter, cKeyValueDelimiter, cPairDelimiter }, PROT_ART_ERROR )
		endif
	endif
next x
SELF:ImportFromArray( aResults, cbSearch, cbKeyConvert, cbValueConvert )

METHOD ExportToXML( cFileName AS STRING, lOverwrite := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_Collection
// Es erfolgt ein Export der Collection (und aller Subcollection) in eine Xml-Datei mit dem Aufbau:
// <Collection>
//   <DAT_NEU KeyType="#" ValueType="D">
//     <Value>26.09.2013</Value>
//   </DAT_NEU>
//   <AUFTRAG KeyType="#" ValueType="C">
//     <Value>Finken</Value>
//  </AUFTRAG>
//
// KeyType und ValueType geben dabei den Datentype an

	LOCAL oFile             AS P_BaseXml
	LOCAL oRoot             AS P_XmlNode
	LOCAL lOK := FALSE      AS LOGIC

oFile := P_BaseXml{nil, SELF:oBase}
oFile:cTabulator := SPACE(2)

oFile:XML:AddNode( oFile:Xml:nNewID, "root" )
oRoot := oFile:XML:GetNode( oFile:XML:nActID )
SELF:__HelpExportXml( SELF, oRoot )
lOK := oFile:ExportToFile( cFileName, lOverwrite )
oFile:Release()

return( lOK )

PROTECT METHOD __HelpExportXml( oCollection AS P_Collection, oXml AS P_XmlNode ) AS VOID PASCAL CLASS P_Collection

	LOCAL x                 AS INT
	LOCAL oXmlCollection    AS P_XmlNode
	LOCAL oXmlNode          AS P_XmlNode
	LOCAL oNode             AS P_CollectionNode

oXml:AddNode( oXml:nNewID, "Dictionary", SELF:oBase:UsualToString(SELF:Dictionary) )
oXml:AddNode( oXml:nNewID, "Collection" )
oXmlCollection := oXml:GetNode( oXml:nActID )
for x:=1 upto oCollection:Count()
	oNode := oCollection:Get(x)
	oXmlCollection:AddNode( oXml:nNewID, SELF:oBase:UsualToString( oNode:Key ) )
	oXmlNode := oXmlCollection:GetNode( oXml:nActID )
	oXmlNode:AddAttribute( oXml:nNewID, "KeyType", ValType( oNode:Key ) )
	oXmlNode:AddAttribute( oXml:nNewID, "ValueType", ValType( oNode:Value ) )
	oXmlNode:AddNode( oXml:nNewID, "Value", SELF:oBase:UsualToString( oNode:Value ) )
	if( oNode:oChildCollection != NULL_OBJECT )
		SELF:__HelpExportXML( oNode:oChildCollection, oXmlNode )
	endif
next x

METHOD ImportFromXML( cFileName AS STRING ) AS LOGIC PASCAL CLASS P_Collection
// Importiert in SELF aus einer XML-Datei im Format:
// (Diese Datei sollte mit SELF:ExportToXML() erzeugt worden sein)
//
// <Collection>
//   <DAT_NEU KeyType="#" ValueType="D">
//     <Value>26.09.2013</Value>
//   </DAT_NEU>
//   <AUFTRAG KeyType="#" ValueType="C">
//     <Value>Finken</Value>
//  </AUFTRAG>
//
// KeyType und ValueType geben dabei den Datentype an und es erfolgt eine
// Konvertierung von Key und Value in die ursprünglichen Datentypen

	LOCAL oXmlFile          AS P_BaseXML
	LOCAL oRoot             AS P_XmlNode
	LOCAl lOK := TRUE       AS LOGIC

oXmlFile:=P_BaseXML{nil, SELF:oBase}
oXmlFile:ImportFromFile( cFileName )
if( !oXmlFile:lError )
	oRoot := oXmlFile:XML:GetNodeByName("root")
	SELF:__HelpImportXml( SELF, oRoot)
endif
oXmlFile:Release()

PROTECT METHOD __HelpImportXml( oCollection AS P_Collection, oXml AS P_XmlNode ) AS VOID PASCAL CLASS P_Collection

//	LOCAL aSubNodes         AS ARRAY
	LOCAL oSubCollection    AS P_Collection
	LOCAL oXmlNode          AS P_XmlNode
	LOCAL oMain             AS P_XmlNode
	LOCAL x                 AS INT
	LOCAL uValue            AS USUAL
	LOCAL uKey              AS USUAL
	LOCAL aNodes            AS ARRAY

oMain := oXml:GetNodeByName("Dictionary")
if( oMain != NULL_OBJECT )
	SELF:Dictionary := if( oMain:cValue == "TRUE", TRUE, FALSE )
	oMain := oXml:GetNodeByName("Collection")
	aNodes := oMain:aChilds
	if( aLen(aNodes) != 0 )
		for x:=1 upto aleN(aNodes)
			oXmlNode := aNodes[x]
			uKey   := SELF:__ConvertToUsual( oXmlNode:GetAttributeValue("KeyType"), oXmlNode:cName )
			uValue := SELF:__ConvertToUsual( oXmlNode:GetAttributeValue("ValueType"), oXmlNode:GetNodeByName( "Value" ):cValue )
			oCollection:Add( uKey, uValue )
			debugPrint( "FIN: ", __ENT, __LINE__, "Import -> ", uKey, UsualType(uKey), uValue, usualType(uValue), oXmlNode:GetAttributeValue("KeyType"), oXmlNode:GetAttributeValue("ValueType") )

			if( oXmlNode:GetNodeByName("Dictionary") != NULL_OBJECT )
				oSubCollection := oCollection:GetEmptyCollection()
				SELF:__HelpImportXml( oSubCollection, oXmlNode )
				oCollection:GetSome( uKey ):oChildCollection := oSubCollection
				oSubCollection:Release()
			endif
		next x
	endif
endif

PROTECT METHOD __ConvertToUsual( cValType AS STRING, uValue AS USUAL ) AS USUAL PASCAL CLASS P_Collection
do case
case( cValType == "C" )
	// STRING: Nix zu tun da eh schon String
case( cValType == "D" )
	// DATE:
	uValue := CTOD(uValue)
case( cValType == "L" )
	uValue := IIF( uValue == "FALSE", FALSE, TRUE )
case( cValType == "#" )
	uValue := String2Symbol( uValue )
case( cValType == "N" )
	// INT oder REAL8
	uValue := Val( uValue )
otherwise
	// Konnte nicht konvertiert werden
	SELF:oBase:MessageFormat( "Fehler in # bei der Konvertierung von # vom Typ # in den Typ #", { __ENT, uValue, SELF:oBase:UsualTypeAsString(uValue), cValType }, PROT_ART_ERROR )
endcase
return( uValue )




//
// CollectionNode
// Auto: Finken
//
// Ein Key, Value Objekt
// oChildCollection  : Hier kann eine P_Collection() angehängt werden
// oObject           : Hier kann alles mögliche angehängt werden
//
CLASS P_CollectionNode INHERIT AObject

	EXPORT         Key                AS USUAL
	EXPORT         Value              AS USUAL

	DECLARE ACCESS oChildCollection
	DECLARE ASSIGN oChildCollection

	DECLARE METHOD HasChildCollection
	DECLARE METHOD Clone
	DECLARE METHOD Copy

	/* Interne Methoden */
	PROTECT        __oChildCollection AS P_Collection

METHOD Init( uKey, uValue ) CLASS P_CollectionNode

	SUPER:Init()

	SELF:Key   := uKey
	SELF:Value := uValue

	SELF:__oChildCollection := NULL_OBJECT

METHOD Destroy() AS VOID PASCAL CLASS P_CollectionNode

	if( SELF:__oChildCollection != NULL_OBJECT )
		 SELF:__oChildCollection:Release()
	endif

	SUPER:Destroy()

METHOD HasChildCollection() AS LOGIC PASCAL CLASS P_CollectionNode
return( SELF:__oChildCollection != NULL_OBJECT )

METHOD Copy() AS P_CollectionNode PASCAL CLASS P_CollectionNode
SELF:AddRef()
return(SELF)

METHOD Clone() AS P_CollectionNode PASCAL CLASS P_CollectionNode

	LOCAL oNode         AS P_CollectionNode
	LOCAL oCollection   AS P_Collection

oNode := P_CollectionNode{ SELF:Key, SELF:Value }
if( SELF:oChildCollection != NULL_OBJECT )
	oCollection := SELF:oChildCollection:GetCloneCollection()
	oNode:oChildCollection := oCollection
	oCollection:Release()
endif
return( oNode )

ACCESS oChildCollection AS P_Collection PASCAL CLASS P_CollectionNode
return( SELF:__oChildCollection )

ASSIGN oChildCollection( oCollection AS P_Collection ) AS P_Collection PASCAL CLASS P_CollectionNode
if( oCollection != NULL_OBJECT )
	oCollection:AddRef()
else
	SELF:__oChildCollection:Release()
endif
return( SELF:__oChildCollection := oCollection )

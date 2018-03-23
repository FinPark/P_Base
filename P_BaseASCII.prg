//
// P_BaseASCIICsv      : Import mit Trennzechen
// P_BaseASCIIFixedPos : Import mit festen Positionen
// P_BaseASCII         : Import ohne Spaltendefinitionen
//

//
// P_BaseASCII
//
function P_BaseASCII_UnitTest( oCommConnect AS ABaseCommConnect ) as void pascal

	LOCAL oASCII               AS P_BaseASCII

oASCII := P_BaseASCII{}
oASCII:ReadFromFile( "\\pc-fin\temp\ASCIMport\Info-Datei.csv" )
oASCII:oBase:AutoCreateProtocol( "SNT", "Import ASCII-Datei" )
if( !oASCII:lError )
	if( oASCII:Count() == 0 )
		oASCII:oBase:MessageFormat( "Die ASCII-Datei # hat keinen Inhalt. ", { "(Die von oben)" }, PROT_ART_ERROR )
	else
		oASCII:InsertRow(5, "Meier")
		oASCII:ManipulateRow( { |nRow, cRow| cRow := P_BaseASCII_UnitTest_RowHandling(oASCII, nRow,cRow) } )
	endif

	oASCII:Documentation( "\\pc-fin\temp\ASCIMport\ASCII_Dokumentation.txt" )
	oASCII:WriteToFile("\\pc-fin\temp\ASCIMport\Info-Datei_ASCII.csv")
endif
oASCII:oBase:ShowProtIfError( oCommConnect, "Achtung, es sind beim importieren Fehler/Warnungen aufgetreten.", TRUE )
oASCII:Release()

function P_BaseASCII_UnitTest_RowHandling( oASCII AS P_BaseASCII, nRow AS INT, cLine AS STRING ) AS STRING PASCAL

if( Left(cLine,5) == "Meier" )
	cLine := "finken und Meier"
endif

if( oASCII:oBase:StringFind( cLine, "Werkbank-Handbesen" ) != 0 )
	cLine := oASCII:oBase:StringReplace( cLine, "Westphal GmbH", "Finken GBR" )
endif
return( cLine )

//
// P_BaseASCIICsv
//
function P_BaseASCIICsv_UnitTest( oCommConnect AS ABaseCommConnect ) AS VOID PASCAL

	LOCAl oFile             AS P_BaseASCIICsv

oFile := P_BaseASCIICsv{}
oFile:oBase:AutoCreateProtocol( "SNT", "FixedPosImport" )
oFile:lFirstRowGotHeader := TRUE
oFile:cSeparator := ";"
if( oFile:ReadFromFile( "\\pc-fin\temp\ASCImport\Info-Datei.csv") )
	debugPrint( "FIN: ", __ENT, __LINE__, "oFile:Count", oFile:Count())
	if( oFile:Count() != 0 )
		oFile:ManipulateAll( { |r,c,name,value| P_BaseASCIICsv_UnitTest_Function(oFile,r,c,name,value) } )
		debugPrintArray( oFile:aRows )
		oFile:WriteToFile( "\\pc-fin\temp\ASCImport\Info-Datei_Export.csv" )
		oFile:Documentation( "\\pc-fin\temp\ASCIMport\CSV_Dokumentation.txt" )
	endif
else
	debugPrint( "FIN: ", __ENT, __LINE__, "ReadFromFile liefert false")
endif
oFile:oBase:ShowProtIfError( oCommConnect, "Achtung, es sind beim importieren Fehler/Warnungen aufgetreten.", TRUE )
oFile:Release()

function P_BaseASCIICsv_UnitTest_Function( oFile AS P_BaseASCIICsv,nRow AS INT, nColumn AS INT, symColumnName AS SYMBOL, cValue AS STRING ) AS STRING PASCAL
do case
case symColumnName == #ArtikelnummerLieferant .and. Left(oFile:FgetN( nRow, #Artikelnummer ),6) == "135450"
	cValue := "Finken"
case symColumnName == #Standort
	cValue := Upper(cValue)
endcase
return( cValue )

//
// P_BaseASCIIFixedPos
//
function P_BaseASCIIFixedPos_UnitTest( oCommConnect AS ABaseCommConnect ) AS VOID PASCAL

	LOCAL oFile            AS P_BaseASCIIFixedPos

oFile := P_BaseASCIIFixedPos{}
oFile:oBase:AutoCreateProtocol( "SNT", "FixedPosImport" )
oFile:aHeader := { { #Name, 1, 20 },;
                   { #Alter, 21, 3 },;
                   { #Bezeichnung, 24,25 } }
if( oFile:ReadFromFile( "\\pc-fin\temp\ASCImport\FixedPosImport.txt", 2  ) )
	oFile:ManipulateAll( { |r,c,name,value| iif( name==#bezeichnung .and. AllTrim(value) = "Standardentwickler", "Standardentwicklung", value ) } )
	oFile:Documentation( "\\pc-fin\temp\ASCIMport\FixedPos_Dokumentation.txt" )
	oFile:WriteToFile( "\\pc-fin\temp\ASCImport\FixedPosExport.txt" )
endif
oFile:oBase:ShowProtIfError( oCommConnect, "Achtung, es sind beim importieren Fehler/Warnungen aufgetreten.", TRUE )
oFile:Release()






/* Aufbau Header */
DEFINE fp_Name          := 1
DEFINE fp_Position      := 2
DEFINE fp_Len           := 3

/*

   P_BaseASCIICsv : Importieren / Exportieren von ASCII-Dateien
						  mit Trennern für die Spalten (Tab, ";", "," etc.)
   -------------------------------------------------------------

	Abgeleitet von P_BaseASCIIFixedPos(), abgeleitet von P_BaseASCII()

*/
CLASS P_BaseASCIICsv INHERIT P_BASEASCIIFixedPos
/*
   Headeraufbau: { #Name1, #Name2,... }
*/

   EXPORT cSeparator                    AS STRING

	/* Interne Methoden */
	DECLARE METHOD __AutoGenerateHeader


METHOD Init( oP_Base ) CLASS P_BaseASCIICsv

	SUPER:Init( oP_Base )

	SELF:lFirstRowGotHeader  := TRUE
	SELF:cSeparator          := ";"

PROTECT METHOD __DocumentationHeader() AS STRING PASCAL CLASS P_BaseASCIICsv

	LOCAL cDoc := ""          AS STRING
	LOCAL cValue := ""        AS STRING
	LOCAl x                   AS INT

cDoc += "Erste Zeile besitzt Header-Informationen: "+IIF( SELF:lFirstRowGotHeader, "Ja","Nein" ) + CRLF
cDoc += "Seperator = "+SELF:cSeparator + CRLF
cDoc += "Header-Deklaration = { "
for x:=1 upto ALen(SELF:aHeader)
	cDoc += IIF( x!=1,", ","" ) + "#" + SELF:oBase:StringFirstUpper(SELF:aHeader[x])
next x
cDoc += "}" + CRLF
cDoc += CRLF
for x:=1 upto ALen( SELF:aHeader )
	cValue := CHR(34) + IIF( SELF:IsColumn( 1, x ), SELF:FGet( 1,x ),"" ) + CHR(34)
	cDoc += SELF:oBase:StringFormat( "[#] # = #", { SELF:oBAse:StringZero( x,3,0 ), SELF:oBase:StringFirstUpper( SELF:__GetHeaderName(x) ), cValue} ) + CRLF
next x
cDoc += CRLF
cDoc += "* Inhalte sind nur Beispieldaten" + CRLF

return( cDoc )

METHOD OnReadRow( nRow AS INT, cRow AS STRING ) AS STRING PASCAL CLASS P_BaseASCIICsv
// OnReadRow wird beim importieren jeder Zeile gerufen und kann vererbt werden um dort einzugreifen

	LOCAL aColumns                 AS ARRAY
	LOCAL x                        AS INT

if( Empty( cRow ) )
	cRow := NULL_STRING
elseif( ALen( SELF:aImportTranslations ) != 0 )
	// Es sollen Übersetzungen durchgeführt werden
	cRow := SELF:oBase:StringTranslate( cRow, SELF:aImportTranslations )
endif

SELF:oBase:ProgressIncrement()

if( cRow != NULL_STRING )
	if( nRow == 1 )
		if( SELF:lFirstRowGotHeader )
			// Der Header ist in der Datei definiert.
			aColumns := SELF:oBase:StringToArray( cRow, { SELF:cSeparator } )
			SELF:aHeader := {}
			for x:=1 upto ALen(aColumns)
				if( !Empty( aColumns[x] ) .or. x < ALen(aColumns) )
					AAdd( SELF:aHeader, SELF:oBase:StringTranslate( Upper(aColumns[x]), SELF:aTranslateHeaderChars ) )
				endif
			next x
			cRow := NULL_STRING
		elseif( ALen(SELF:aHeader) == 0 )
			// Es ist weder ein HEader angegeben, noch lässt er sich aus der Datei ermitteln.
			SELF:__AutoGenerateHeader(ALen(SELF:oBase:StringToArray( cRow, { SELF:cSeparator } ) ))
		endif
	endif
endif
return( cRow )

METHOD CheckRows() AS LOGIC PASCAL CLASS P_BaseASCIICsv
// Diese Methode ist für abgeleitete Klassen interessant, weil eine Prüfung der Zeilen stattfinden kann
	LOCAL x                   AS INT
	LOCAL aColumns            AS ARRAY
	LOCAL lOK := TRUE         AS LOGIC

for x:=1 upto SELF:Count()
	aColumns := SELF:oBase:StringToArray( SELF:aRows[x], { SELF:cSeparator } )
	do case
	case( ALen(aColumns) < SELF:__GetRowDeclarationLength() )
		SELF:oBase:MessageFormat( "Fehler in Zeile #. Die Zeile liefert # Spalten. Laut Deklaration sollte sie jedoch # Spalten besitzen.", { x,  ALen(aColumns), SELF:__GetRowDeclarationLength() }, PROT_ART_WARNING )
      /* Nun die fehlenden Spalten an die Zeile anhängen */
		SELF:aRows[x] += SELF:oBase:StringRepeat( SELF:__GetRowDeclarationLength() - ALen(aColumns), SELF:cSeparator )
	case(  ALen(aColumns) > SELF:__GetRowDeclarationLength() )
		SELF:oBase:MessageFormat( "Fehler in Zeile #. Die Zeile liefert # Spalten. Laut Deklaration sollte sie nur # Spalten besitzen.", { x,  ALen(aColumns), SELF:__GetRowDeclarationLength() }, PROT_ART_WARNING )
	endcase
next x
return( lOK )

METHOD __AutoGenerateHeader( nSize AS INT ) AS VOID PASCAL CLASS P_BaseASCIICsv
	LOCAL x  AS INT
SELF:aHeader := {}
for x:= 1 to nSize
	AAdd( SELF:aHeader, NTrim(x) )
next x

METHOD __GetColumnDeclaration( nColumn AS INT ) AS ARRAY PASCAL CLASS P_BaseASCIICsv
// Wird nicht benötigt
return( nil )

METHOD __GetRowDeclarationLength() AS INT PASCAL CLASS P_BaseASCIICsv
return( ALen(SELF:aHeader) )

METHOD __GetColumn( cRow AS STRING, nColumn AS INT ) AS STRING PASCAL CLASS P_BaseASCIICsv
// Die Spalte aus dem Zeilen String herausholen und zurückgeben
	LOCAL cValue := ""            AS STRING
	LOCAL aColumns                AS ARRAY
aColumns := SELF:oBase:StringToArray( cRow, { SELF:cSeparator })
cValue := aColumns[nColumn]
if( SELF:lTrimImportedColumns )
	cValue := AllTrim(cValue)
endif
return( cValue )

METHOD __PutColumn( cRow AS STRING, nColumn AS INT, cValue AS STRING ) AS STRING PASCAL CLASS P_BaseASCIICsv
// Die <cValue> in dem Zeilen-String ersetzen
	LOCAL aColumns              AS ARRAY

aColumns := SELF:oBase:StringToArray( cRow, { SELF:cSeparator })
if( AllTrim(cValue) != AllTrim(aColumns[nColumn]) )
	aColumns[nColumn] := cValue
	cRow := SELF:oBase:ArrayToString( aColumns, SELF:cSeparator )
endif
return( cRow )

METHOD __GetHeaderName( nColumn AS INT ) AS STRING PASCAL CLASS P_BaseASCIICsv
if( nColumn > 0 .and. nColumn <= ALen( SELF:aHeader ) )
	return( SELF:aHeader[nColumn] )
else
	SELF:oBase:MessageFormat( "Fehler bei Zugriff auf Spalte #. Die Header enthalten nur # Spalten.", { nColumn, ALen(SELF:aHeader) }, PROT_ART_WARNING )
endif
return( NULL_STRING )




/*

   P_BaseASCIIFixedPos : Importieren / Exportieren von ASCII-Dateien
								 mit Spalten an festen Positionen.
   -------------------------------------------------------------

	Abgeleitet von P_BaseASCII()

*/
CLASS P_BaseASCIIFixedPos INHERIT P_BaseASCII
/*
   Headeraufbau: { {#Name1, Pos, Len}, {#Name2, Pos, Len}, --- }
*/

	EXPORT aHeader                 AS ARRAY
	EXPORT lTrimImportedColumns    AS LOGIC
	EXPORT lFirstRowGotHeader      AS LOGIC
	EXPORT aTranslateHeaderChars   AS ARRAY

	DECLARE METHOD FGet, FGetN
	DECLARE METHOD FPut, FPutN
	DECLARE METHOD FGetVal, FGetNVal
	DECLARE METHOD IsColumn, IsColumnName
	DECLARE METHOD ManipulateAll, ManipulateColumn
	DECLARE METHOD GetRowDocumentation


	/* Interne Methoden */
	DECLARE METHOD __GetColumnDeclaration
	DECLARE METHOD __GetRowDeclarationLength
	DECLARE METHOD __GetHeaderNo
	DECLARE METHOD __GetHeaderName
	DECLARE METHOD __GetColumn
	DECLARE METHOD __PutColumn

METHOD Init( oP_Base ) CLASS P_BaseASCIIFixedPos

	SUPER:Init( oP_Base )
	SELF:lTrimImportedColumns  := FALSE
	SELF:lFirstRowGotHeader    := FALSE
	SELF:aTranslateHeaderChars := {{" ",""}}

PROTECT METHOD __DocumentationHeader() AS STRING PASCAL CLASS P_BaseASCIIFixedPos

	LOCAL cDoc := ""          AS STRING
	LOCAl x                   AS INT
	LOCAl aDeclaration        AS ARRAY
	LOCAL cValue := ""        AS STRING

cDoc += "Zeilen-Deklaraltions-Länge: "+NTrim( SELF:__GetRowDeclarationLength() ) + CRLF
cDoc += "Anzahl Spalten: "+NTrim(ALen(SELF:aHeader)) + CRLF
cDoc += "Header-Deklaration: {"
for x:=1 upto ALen(SELF:aHeader)
	cDoc += IIF( x!=1,",","" ) + SELF:aHeader[x][1] + ", " + NTrim(SELF:aHeader[x][2]) + ", " + NTrim(SELF:aHEader[x][3])
next x
cDoc += "}"+CRLF
cDoc += CRLF
for x:=1 upto ALen( SELF:aHeader )
	aDeclaration := SELF:__GetColumnDeclaration( x )
	cValue := CHR(34)+IIF( SELF:IsColumn( 1, x ), SELF:FGet( 1,x ),"" )+CHR(34)
	cDoc += SELF:oBase:StringFormat( "[#] # (#,#) = #", { SELF:oBAse:StringZero( x,3,0 ), SELF:oBase:StringFirstUpper( SELF:__GetHeaderName(x) ), aDeclaration[fp_Position], aDeclaration[fp_Len], cValue} ) + CRLF
next x
cDoc += CRLF
cDoc += "Beschreibung: [Spalte-Nr] (Position, Länge)" + CRLF
cDoc += "* Inhalte sind nur Beispieldaten" + CRLF

return( cDoc )

METHOD ManipulateAll( cbCodeBlock AS CODEBLOCK ) AS VOID PASCAL CLASS P_BaseASCIIFixedPos
// Es werden alle Zeilen und Spalten in dem Codeblock angesprungen.
// cbCodeBlock := { |Row,Column,cColumnName, cValue| cValue := AllTrim(cValue) }
	LOCAL nRow, nColumn         AS INT
	LOCAL cOldValue, cNewValue  AS STRING
for nRow := 1 upto SELF:Count()
	SELF:oBase:ProgressIncrement()
	for nColumn := 1 upto ALen( SELF:aHeader )
		cOldValue := SELF:Fget( nRow, nColumn)
		cNewValue := Eval( cbCodeBlock, nRow, nColumn, SELF:__GetHeaderName(nColumn), cOldValue)
		if( cOldValue != cNewValue )
			SELF:FPut(nRow, nColumn, cNewValue )
		endif
	next nColumn
next nRow

METHOD ManipulateColumn( cbCodeBlock AS CODEBLOCK, uColumnName AS USUAL ) AS VOID PASCAL CLASS P_BaseASCIIFixedPos
// Es wird die Spalte <symColumnName> durchgegangen und kann manipuliert werden
// cbCodeBlock := { |Row,Column,uColumnName, cValue| cValue := AllTrim(cValue) }
	LOCAL nRow                AS INT
	LOCAL nColumn             AS INT

nColumn := SELF:__GetHeaderNo( uColumnName )
for nRow := 1 upto SELF:Count()
	SELF:FPut( nRow, nColumn, Eval( cbCodeBlock, nRow, nColumn, uColumnName, SELF:Fget( nRow, nColumn )) )
next nRow

METHOD GetRowDocumentation( nRow AS INT ) AS STRING PASCAL CLASS P_BaseASCIIFixedPos
// Es wird ein Memo mit Header:Value Inhalten zurückgegeben
	LOCAL cString := ""           AS STRING
	LOCAL x                       AS INT
	LOCAL cValue                  AS STRING

for x:=1 upto ALen( SELF:aHeader )
	cValue := IIF( SELF:IsColumn( nRow, x ), SELF:FGet( nRow,x ),"" )
	cString += SELF:oBase:StringFormat( '# = "#"', { SELF:oBase:StringFirstUpper( SELF:__GetHeaderName(x) ), cValue} ) + CRLF
next x
return( cString )

METHOD OnReadRow( nRow AS INT, cRow AS STRING ) AS STRING PASCAL CLASS P_BaseASCIIFixedPos
// OnReadRow wird beim importieren jeder Zeile gerufen und kann vererbt werden um dort einzugreifen
// Es sollte jedoch kein Super gerufen werden, da jede Instanz ihre eigene Manipulation der Zeile hat

	LOCAL x                AS INT
	LOCAL cHeaderColumn    AS STRING

cRow := SUPER:OnReadRow( nRow, cRow )
if( cRow != NULL_STRING )
	/* Nun den String zwingend auf die optimale Länge bringen */
	cRow := SELF:oBase:StringLength( cRow, SELF:__GetRowDeclarationLength() )

	if( nRow == 1 )
		if( SELF:lFirstRowGotHeader )
			// Der Header ist in der Datei definiert.
			// Da Spaltenpos und Breite bereits definiert ist, nun den Header aus der Datei
			// dazu schreiben
			for x:=1 upto ALen(SELF:aHeader)
				cHeaderColumn := Upper(AllTrim(SubStr( cRow, SELF:aHeader[x][2], SELF:aHeader[x][3] )))
				cHeaderColumn := SELF:oBase:StringTranslate( cHeaderColumn, aTranslateHeaderChars )
				SELF:aHeader[x][1] := Upper( cHeaderColumn )
			next x
			cRow := NULL_STRING
		endif
	endif

endif
return( cRow )

METHOD CheckRows() AS LOGIC PASCAL CLASS P_BaseASCIIFixedPos
// Diese Methode ist für abgeleitete Klassen interessant, weil eine Prüfung der Zeilen stattfinden kann
	LOCAL x                   AS INT
	LOCAL lOK := TRUE         AS LOGIC

for x:=1 upto SELF:Count()
	do case
	case( Len(SELF:aRows[x]) < SELF:__GetRowDeclarationLength() )
		SELF:oBase:MessageFormat( "Zeile # nicht lang genug. Die Zeile ist nur # Zeichen lang. Laut Deklaration sollte sie # Zeichen besitzen.", { x,Len(SELF:aRows[x]), SELF:__GetRowDeclarationLength() }, PROT_ART_WARNING )
	case(Len(SELF:aRows[x]) > SELF:__GetRowDeclarationLength() )
		SELF:oBase:MessageFormat( "Zeile # zu lang. Die Zeile ist # Zeichen lang. Laut Deklaration sollte sie # Zeichen besitzen.", { x, Len(SELF:aRows[x]), SELF:__GetRowDeclarationLength() }, PROT_ART_WARNING )
	endcase
next x
return( lOK )

METHOD Fget( nRow AS INT, nColumn AS INT ) AS STRING PASCAL CLASS P_BaseASCIIFixedPos

	LOCAL cValue := ""              AS STRING

if( nColumn > ALen( SELF:aHeader ) .or. nColumn <= 0  )
	SELF:oBase:MessageFormat( "Zugriff auf Spalte # ungültig. Die Datei Header-Beschreibung enthällt nur # Spalten.", { nColumn, ALen(SELF:aHeader) }, PROT_ART_WARNING )
else
	cValue := SELF:__GetColumn( SELF:GetRow(nRow), nColumn )
endif
return( cValue )

METHOD FgetVal( nRow AS INT, nColumn AS INT ) AS REAL8 PASCAL CLASS P_BaseASCIIFixedPos
return( Val( SELF:oBase:StringTranslate( SELF:Fget( nRow, nColumn ), {{",","."}} ) ) )

METHOD Fput( nRow AS INT, nColumn AS INT, cValue AS STRING ) AS VOID PASCAL CLASS P_BaseASCIIFixedPos
if( nColumn > ALen( SELF:aHeader ) .or. nColumn <= 0  )
	SELF:oBase:MessageFormat( "Zugriff auf Spalte # ungültig. Die Datei Header-Beschreibung enthällt nur # Spalten.", { nColumn, ALen(SELF:aHeader) }, PROT_ART_WARNING )
else
   SELF:aRows[nRow] := SELF:__PutColumn( SELF:GetRow(nRow), nColumn, cValue )
endif

METHOD FgetN( nRow AS INT, uColumn AS USUAL ) AS STRING PASCAL CLASS P_BaseASCIIFixedPos
return( SELF:Fget( nRow, SELF:__GetHeaderNo( uColumn ) ) )

METHOD FGetNVal( nRow AS INT, uColumn AS USUAL ) AS REAL8 PASCAL CLASS P_BaseASCIIFixedPos
return( SELF:FgetVal( nRow,  SELF:__GetHeaderNo( uColumn )  ) )

METHOD FputN( nRow AS INT, uColumn AS USUAL, cValue AS STRING ) AS STRING PASCAL CLASS P_BaseASCIIFixedPos
	LOCAL cRow := ""             AS STRING
if( SELF:IsColumn( nRow, SELF:__GetHeaderNo(uColumn) ) )
	SELF:aRows[nRow] := SELF:__PutColumn( SELF:aRows[nRow], 	SELF:__GetHeaderNo(uColumn), cValue )
endif
return( cRow )

METHOD IsColumn( nRow AS INT, nColumn AS INT ) AS LOGIC PASCAL CLASS P_BaseASCIIFixedPos
return( nRow > 0 .and. nRow <= SELF:Count() .and. nColumn > 0 .and. nColumn <= ALen( SELF:aHeader) )

METHOD IsColumnName( nRow AS INT, uColumnName AS USUAL ) AS LOGIC PASCAL CLASS P_BaseASCIIFixedPos
return( SELF:IsColumn( nRow, SELF:__GetHeaderNo( uColumnName ) ) )

METHOD __GetHeaderNo( uColumn AS USUAL ) AS INT PASCAL CLASS P_BaseASCIIFixedPos
	LOCAl x                    AS INT
	LOCAL cColumns := ""       AS STRING

for x:=1 upto ALen( SELF:aHeader )

	if( IsSymbol( uColumn ) .and. String2Symbol(SELF:__GetHeaderName( x )) == uColumn ) .or.;
		(IsString( uColumn ) .and. Upper( SELF:aHeader[x] ) ==  Upper(uColumn) )
		// ------
		return(x)
		// ------

	endif
	cColumns += ", " + SELF:aHeader[x]
next x

SELF:oBase:MessageFormat( "Fehler beim Zugriff auf Spalte #. Die Spalte wurde nicht gefunden. Verfügbare Spalten: # ", { uColumn, cColumns }, PROT_ART_ERROR, TRUE )
return( 0 )


METHOD __GetColumnDeclaration( nColumn AS INT ) AS ARRAY PASCAL CLASS P_BaseASCIIFixedPos
return( SELF:aHeader[nColumn] )

METHOD __GetRowDeclarationLength() AS INT PASCAL CLASS P_BaseASCIIFixedPos
	LOCAL aDeclaration            AS ARRAY
aDeclaration := SELF:__GetColumnDeclaration(ALen(SELF:aHeader))
return( aDeclaration[fp_Position] + aDeclaration[fp_Len] )

METHOD __GetColumn( cRow AS STRING, nColumn AS INT ) AS STRING PASCAL CLASS P_BaseASCIIFixedPos
// Die Spalte aus dem Zeilen String herausholen und zurückgeben
	LOCAL cValue := "" AS STRING
	LOCAL aDeclaration            AS ARRAY
aDeclaration := SELF:__GetColumnDeclaration(nColumn)
cValue := SubStr( cRow, aDeclaration[fp_Position], aDeclaration[fp_Len]	 )
if( SELF:lTrimImportedColumns )
	cValue := AllTrim(cValue)
endif
return( cValue )

METHOD __PutColumn( cRow AS STRING, nColumn AS INT, cValue AS STRING ) AS STRING PASCAL CLASS P_BaseASCIIFixedPos
// Die <cValue> in dem Zeilen-String ersetzen
	LOCAL aDeclaration            AS ARRAY
aDeclaration := SELF:__GetColumnDeclaration(nColumn)
cValue := SELF:oBase:StringLength( cValue, aDeclaration[fp_Len] )
cRow := SELF:oBase:StringFixPos( cRow, cValue,  aDeclaration[fp_Position] )
return( cRow )

METHOD __GetHeaderName( nColumn AS INT ) AS STRING PASCAL CLASS P_BaseASCIIFixedPos
	LOCAL aDeclaration            AS ARRAY
aDeclaration := SELF:__GetColumnDeclaration(nColumn)
return( aDeclaration[fp_Name] )

/*

   P_BaseASCII : Importieren / Exportieren von ASCII-Dateien.
   -------------------------------------------------------------

	Die Zeilen der Datei werden in P_BaseASCII:aRows gespeichert und mit FGet und FPut kann darauf sicher zugegriffen werden.


*/
CLASS P_BaseASCII INHERIT AObject

	DECLARE METHOD ReadFromFile
	DECLARE METHOD CheckRows
	DECLARE METHOD OnReadRow
	DECLARE METHOD OnWriteRow
	DECLARE METHOD WriteToFile
	DECLARE METHOD ManipulateRow
	DECLARE METHOD AddRow
	DECLARE METHOD InsertRow
	DECLARE METHOD DeleteRow
	DECLARE METHOD GetRow
	DECLARE METHOD PutRow
	DECLARE METHOD Count
	DECLARE METHOD Documentation

   EXPORT         aRows                      AS ARRAY
	EXPORT         lUniCode   := FALSE        AS LOGIC
	EXPORT         lDosFormat := TRUE         AS LOGIC
	EXPORT         aImportTranslations        AS ARRAY
	EXPORT         aExportTranslations        AS ARRAY

   DECLARE ACCESS lError
   DECLARE ACCESS oBase



	/* Interne Methoden */
	DECLARE METHOD __DocumentationHeader
	PROTECT        __oBase                AS P_Base

METHOD Init( oP_Base ) CLASS P_BaseASCII
// Es kann ein vorhandenes P_BAse-Object übergeben werden
// Dann wird mit dem gearbeitet.


	SUPER:Init()

	if( !IsNil( oP_Base ) .and. IsInstanceOfUsual(oP_Base, #P_Base))
		SELF:__oBase := oP_Base
		SELF:__oBase:AddRef()
	else
		SELF:__oBase := P_Base{}
	endif

	SELF:aRows      := {}
	SELF:aImportTranslations := {} // Format: {{"TAB",""},...
	SELF:aExportTranslations := {} // Format: {{"TAB",""},...


METHOD Destroy() AS VOID PASCAL CLASS P_BaseASCII

	SELF:__oBase:Release()

	SUPER:Destroy()

METHOD Documentation( cFileName := "" AS STRING ) AS STRING PASCAL CLASS P_BaseASCII

	LOCAL cDoc := ""        AS STRING
	LOCAL aClassTree        AS ARRAY
	LOCAL x                 AS INT
	LOCAL nLen              AS INT

aClassTree := ClassTree( SELF )
cDoc := "Dokumentation Schnittstelle (Format: "
for x:=1 upto aLen( aClassTree )-1  // Kein AObject anzeigen
	cDoc += IIF( x==1, "", " : " ) + Symbol2String(ArrayGet(aClassTree,x))
next x
cDoc += "), Stand "+DTOC(Today())+CRLF
cDoc += CRLF
cDoc += "Unicode    : "+IIF( SELF:lUniCode, "Ja","Nein" )+CRLF
cDoc += "Dos-Format : "+IIF( SELF:lDosFormat, "Ja","Nein" )+CRLF
cDoc += CRLF
cDoc += SELF:__DocumentationHeader()
cDoc += CRLF
nLen := IIF( aLen(SELF:aRows) > 5, 5, aLen(SELF:aRows) )
cDoc += SELF:oBase:StringFormat( 'Beispieldatei: "#" aus Verzeichnis "#"', { SELF:oBase:GetFileNameFromPath( cFileName ), SELF:oBase:GetPathNameFromPath( cFileName ) } ) + CRLF
cDoc += SELF:oBase:StringFormat( "Auszug aus der Datei, die ersten # Zeilen: ", {nLen} )+CRLF
for x:=1 to nLen
	cDoc += SELF:aRows[x] +CRLF
next x

/*
if( !Empty( cFileNAme ) )
	SELF:oBase:FileWriteFromString( cFileName, cDoc, FALSE, FALSE )
endif
*/
return( cDoc )

PROTECT METHOD __DocumentationHeader() AS STRING PASCAL CLASS P_BaseASCII
return( "Beispielzeile: "+IIF( aLen(aRows)>0, aRows[1], "" ) + CRLF )

	METHOD ReadFromFile( cFileName AS STRING, nStartReadFromLine := 1 AS INT ) AS LOGIC PASCAL CLASS P_BaseASCII
// Ließt aus der Datei <cFileName> die Zeilen/Spalten, getrennt durch <SELF:cSeparator> ein und stellt diese dann
// zur Verfügung. Wenn SELF:lError gesetzt, dann sind die Daten nicht vollständig und der Import hat nicht geklappt
	LOCAL aRows               AS ARRAY
	LOCAL x                   AS INT
	LOCAL cRow                AS STRING

aRows := SELF:oBase:FileReadToArray( cFileName, FALSE, SELF:lUnicode, SELF:lDOSFormat )
for x:=nStartReadFromLine upto aLen( aRows )
	cRow := SELF:OnReadRow( x, aRows[x] )
	if( cRow != NULL_STRING )
		aadd( SELF:aRows, cRow )
	endif
next x

return( SELF:CheckRows() .and. !SELF:lError )

METHOD CheckRows() AS LOGIC PASCAL CLASS P_BaseASCII
// Diese Methode ist für abgeleitete Klassen interessant, weil eine Prüfung der Zeilen stattfinden kann
return( true )

METHOD AddRow( cRow := "" AS STRING ) AS INT PASCAL CLASS P_BaseASCII
// Fügt eine leere Zeile an das Ende der Zeilen an
aadd( SELF:aRows, cRow )
return( SELF:Count() )

METHOD InsertRow( nPosition AS INT, cRow := "" AS STRING ) AS INT PASCAL CLASS P_BaseASCII
// Fügt eine leere Zeile an Position <nPosition> ein und gibt die Zeilennummer zurück,
// wenn das geklappt hat, sonst 0
if( nPosition > 0 .and. nPosition <= SELF:Count() )
	aSize( SELF:aRows, aLen(aRows) + 1)
	aIns( SELF:aRows, nPosition )
	SELF:aRows[nPosition] := cRow
else
	nPosition := 0
	SELF:oBase:MessageFormat( "Zugriff auf ungültige Zeile # in InsertRow(). Die Datei enthällt nur # Zeilen.", { nPosition, aLen(SELF:aRows) }, PROT_ART_ERROR )
endif
return( nPosition )


METHOD DeleteRow( nPosition AS INT ) AS LOGIC PASCAL CLASS P_BaseASCII
// Löscht die Zeile an der Position <nPosition> und gibt TRUE zurück, wenn das geklappt hat
	LOCAL lOK := TRUE
if( nPosition > 0 .and. nPosition <= SELF:Count() )
	aDelShrink( SELF:aRows, nPosition )
else
	SELF:oBase:MessageFormat( "Zugriff auf ungültige Zeile # in DelRow(). Die Datei enthällt nur # Zeilen.", { nPosition, aLen(SELF:aRows) }, PROT_ART_ERROR )
endif
return( lOK )

METHOD OnReadRow( nRow AS INT, cRow AS STRING ) AS STRING PASCAL CLASS P_BaseASCII
// OnReadRow wird beim importieren jeder Zeile gerufen und kann vererbt werden um dort einzugreifen
// Wird ein NULL_STRING zurückgegeben, so wird die Zeile nicht importiert
if( Empty( cRow ) )
	cRow := NULL_STRING
elseif( aLen( SELF:aImportTranslations ) != 0 )
	// Es sollen Übersetzungen durchgeführt werden
	cRow := SELF:oBase:StringTranslate( cRow, SELF:aImportTranslations )
endif
SELF:oBase:ProgressIncrement()
return( cRow )

METHOD OnWriteRow( nRow AS INT, cRow AS STRING ) AS STRING PASCAL CLASS P_BaseASCII
// OnWriteRow wird beim exportieren jeder Zeile gerufen und kann vererbt werden um dort einzugreifen.
// Wird NULL_STRING zurückgegeben, so wird die Zeile nicht exportiert

SELF:oBase:ProgressIncrement()
if( aLen(SELF:aExportTranslations) != 0 )
	cRow := SELF:oBase:StringTranslate( cRow, SELF:aExportTranslations )
endif

return( cRow )

METHOD ManipulateRow( cbCodeBlock AS CODEBLOCK ) AS VOID PASCAL CLASS P_BaseASCII
// Hiermit können bestimmte Zeilen durch einen Codeblock manipuliert werden.
// Siehe auch ManipulateAll()
//
// Beispiel : ManipulateRow( { |nRow, cLine| AllTrim(cLine) } )

	LOCAL x              AS INT

for x:=1 upto aLen( SELF:aRows )
	SELF:aRows[x] := Eval( cbCodeBlock, x, SELF:aRows[x] )
next x

METHOD WriteToFile( cFileName AS STRING, lDeleteFileIfExists := FALSE AS LOGIC) AS LOGIC PASCAL CLASS P_BaseASCII
// Schreibt die Zeilen (SELF:aRows) und ggf. den Header (SELF:lFirstRowGotHeader) in die Datei zurück.
// Jede Zeile läuft über die Methode OnWriteRow(), welche vererbt werden kann.

	LOCAL x                  AS INT
	LOCAl cString := ""      AS STRING
	LOCAL cTempString := ""  AS STRING

for x:=1 upto aLen( SELF:aRows )
	// Wird Nil zurückgegeben, so wird die Zeile nicht verarbeitet
	cTempString := SELF:OnWriteRow( x, SELF:aRows[x] )
	if( cTempString != NULL_STRING )
		cString += cTempString + IIF( x==aLen(SELF:aRows), "", CRLF )
	endif
next x

if( SELF:oBase:FileExists( cFileName ) .and. lDeleteFileIfExists)
	SELF:oBase:FileDelete( cFileName )
endif
SELF:oBase:FileWriteFromString( cFileName, cString, SELF:lUniCode, SELF:lDOSFormat )

return( !SELF:lError )

ACCESS lError	AS LOGIC PASCAL CLASS P_BaseASCII
// Wenn ein Fehler protokolliert wurde, wird hier TRUE zurückgegeben
return( SELF:oBase:lError )


ACCESS oBase	AS P_Base PASCAL CLASS P_BaseASCII
// Liefert das übergebene oder erstellte oBase-Objekt
return( SELF:__oBase )

METHOD GetRow( nRow AS INT ) AS STRING PASCAL CLASS P_BaseASCII
// Versucht auf eine ZEILE zu zugreifen und gibt des Inhalt zurück
if( nRow <= 0 .or. nRow > SELF:Count() )
	SELF:oBase:MessageFormat( "Zugriff auf Zeile # ungültig. Die Datei enthällt nur # Zeilen.", { nRow, aLen(SELF:aRows) }, PROT_ART_WARNING )
	return( "" )
endif
return( SELF:aRows[nRow] )

METHOD PutRow( nRow AS INT, uValue AS USUAL ) AS VOID PASCAL CLASS P_BaseASCII
// Versucht den Inhalt in ZEILE zu setzen
if( nRow <= 0 .or. nRow > SELF:Count())
	SELF:oBase:MessageFormat( "Zugriff auf Zeile # ungültig. Die Datei enthällt nur # Zeilen.", { nRow, aLen(SELF:aRows) }, PROT_ART_WARNING )
else
	SELF:aRows[nRow] := SELF:oBase:UsualToString( uValue )
endif

METHOD Count() AS INT PASCAL CLASS P_BaseASCII
// Gibt die Anzahl der Zeilen zurück
return( aLen( SELF:aRows ) )


function P_BaseXML_UnitTest_File( oCommConnect AS ABaseCommconnect, cImportFile AS STRING, cExportFile AS STRING, cTreeExportFile AS STRING ) AS VOID PASCAL

	LOCAL oFile          AS P_BaseXML

oFile := P_BaseXml{oCommConnect}
oFile:cTabulator := CHR(9)
oFile:ImportFromFile( cImportFile )
if( !oFile:lError )
	// Den visualisierten Baum der Knoten in einer separaten Datei speichern
	oFile:oBase:StringToDisk( cTreeExportFile, oFile:XML:XmlTree() )

	// Den Import nun wieder exportieren
	if( oFile:ExportToFile( cExportFile, TRUE ) )
	endif
endif
oFile:oBase:ShowMessage( oCommConnect , "Fehler bei Xml-Import" )
oFile:Release()

function P_BaseXml_UnitTest_Export( oCommConnect AS ABaseCommConnect ) AS VOID PASCAL
/*
<xml version="1.0" encoding="windows-1250">
<Personen>
  <Person Nummer="1">
    <Name>Finken</Name>
    <Vorname>André</Vorname>
    <Alter>43</Alter>
  </Person>
  <Person Nummer="2">
    <Name>Finken</Name>
    <Vorname>André</Vorname>
    <Alter>43</Alter>
    <Notizen>
      <Notiz>Hier Eintrag Nummer : 13</Notiz>
      <Notiz>Hier Eintrag Nummer : 14</Notiz>
      <Notiz>Hier Eintrag Nummer : 15</Notiz>
      <Notiz>Hier Eintrag Nummer : 16</Notiz>
      <Notiz>Hier Eintrag Nummer : 17</Notiz>
      <Notiz>Hier Eintrag Nummer : 18</Notiz>
      <Notiz>Hier Eintrag Nummer : 19</Notiz>
      <Notiz>Hier Eintrag Nummer : 20</Notiz>
    </Notizen>
  </Person>
</Personen>
*/

	LOCAL oFile             AS P_BaseXml
	LOCAL oXml              AS P_XmlNode
	LOCAL oPerson           AS P_XmlNode
	LOCAL x                 AS INT

oFile := P_BaseXml{ oCommConnect }
oFile:cTabulator := SPACE(2)

oXml := oFile:XML
oXml:AddNode(1, "Personen" )
oXml:GetNode(1):AddNode(2, "Person" )

oPerson := oXml:GetNode(1):GetNode(2)
oPerson:AddAttribute( 3, "Nummer","1" )
oPerson:AddNode( 4, "Name", "Finken" )
oPerson:AddNode( 5, "Vorname", "André" )
oPerson:AddNode( 6, "Alter", "43" )

oXml:GetNode(1):AddNode(7, "Person" )
oPerson := oXml:GetNode(1):GetNode(7)
oPerson:AddAttribute( 8, "Nummer","2" )
oPerson:AddNode( 9, "Name", "Finken" )
oPerson:AddNode( 10, "Vorname", "André" )
oPerson:AddNode( 11, "Alter", "43" )

oPerson:AddNode(12, "Notizen" )
for x:= 13 to 20
	oPerson:GetNode(12):AddNode(x, "Notiz", "Hier Eintrag Nummer : "+NTrim(x) )
next x

oFile:ExportToFile( "\\pc-fin\temp\ASCImport\Selfmade.xml", TRUE )

oFile:Release()

function P_BaseXML_UnitTest_Import( oCommConnect AS ABaseCommConnect ) AS VOID PASCAL

	LOCAL oXmlFile          AS P_BaseXML
	LOCAl aNodes            AS ARRAY
	LOCAL oNode             AS P_XmlNode
	LOCAL x                 AS INT

oXmlFile:=P_BaseXML{oCommConnect}
oXmlFile:ImportFromFile( "\\pc-fin\temp\ASCIMport\GaebXml.xml" )
if( !oXmlFile:lError )

	oXmlFile:oBase:ProgressIncrement( "Verarbeite /Gaeb/Award/AwardInfo" )
	aNodes := oXmlFile:XML:Reader( "/GAeb/Award/BoQ/BoQInfo" )
	if( aLen(aNodes) != 0 )
		aNodes := aNOdes[1]:aChilds
		for x:=1 upto aleN(aNodes)
			oNode := aNodes[x]
			debugPrint( "FIN: ", __ENT, __LINE__, "../BoQInfo/"+oNode:cName, oNode:cValue )
		next x
	endif

	debugPrint( "FIN: ", __ENT, __LINE__, "Len(#BoQInfo)", aLen( oXmlFile:XML:GetNodesBySymbol( #BoQInfo, TRUE )))

	oXmlFile:XML:Scan( {| oNode | P_BaseXML_UnitTest_ManipulateNOdes( oNode ) }, FALSE, TRUE )

	oXmlFile:oBase:ProgressIncrement( "Verarbeeite Baum" )
	debugPrint( "FIN: ", __ENT, __LINE__, oXmlFile:XML:XmlTree( FALSE, TRUE, FALSE ))

endif
oXmlFile:oBase:ShowMessage( oCommConnect , "Fehler bei Xml-Import" )
oXmlFile:Release()

function P_BaseXML_UnitTest_ManipulateNOdes( oNode AS P_XmlNode ) AS LOGIC PASCAL
//debugPrint( "FIN: ", __ENT, __LINE__, "Node", oNode:cName, oNode:symName)
if( oNode:symName == #ReportedBy )
	oNode:cValue := Upper( oNode:cValue)
	return( true )
endif
return( false )



function P_XmlNode_UnitTest( oCommConnect AS ABaseCommConnect ) AS VOID PASCAL

	LOCAl oBase           AS P_Base
	LOCAL oXml            AS P_XmlNode
	LOCAL oTransaction    AS P_XmlNode

oBase := P_Base{}
oXml  := P_XmlNode{xml_Auto_ID, ""}
oXml:oBase := oBase
oXml:AddNode( xml_Auto_ID,"xml" )
oXml:GetNode( xml_Auto_ID ):AddNode(xml_Auto_ID,"transactions")

oXml:GetNode(oXml:nActID, TRUE):AddNode(xml_Auto_ID,"transaction")
oTransaction := oXml:GetNode( oXml:nActID, TRUE)
oTransaction:AddAttribute( xml_Auto_ID, "name","A" )
oTransaction:AddAttribute( xml_Auto_ID, "date","1296203457" )
oTransaction:AddAttribute( xml_Auto_ID, "type","wf_export_document_attributes" )
oTransaction:AddAttribute( xml_Auto_ID, "vaultname","PDMWorksEnterprise" )

oXml:GetNodeByName("xml", true):AddNode(xml_Auto_ID,"transaction")
oTransaction := oXml:GetNode( oXml:nActID, TRUE)
oTransaction:AddAttribute( xml_Auto_ID, "name","B" )
oTransaction:AddAttribute( xml_Auto_ID, "date","1296203457" )
oTransaction:AddAttribute( xml_Auto_ID, "type","wf_export_document_attributes" )
oTransaction:AddAttribute( xml_Auto_ID, "vaultname","None" )

debugPrint( "FIN: ", __ENT, __LINE__, CRLF + oXml:XmlTree())
debugprintarray( oXml:GetNodeBySymbol(#Xml, TRUE):Reader( "transaction" ) )
debugprintarray( oXml:Reader( "xml/transactions/transaction" ) )
oXml:Release()
oBase:ShowMessage( oCommConnect, "Xml-UnitTest","",10 )
oBase:Release()

/*
  	P_BaseXML_BlankObject : Ein Grundgerüst für den Xml-Import/Export
	-----------------------------------------------------------------
   Kann als Kopiervorlage vewendet werden.




*/

CLASS P_BaseXml_BlankObject INHERIT P_BaseXml

METHOD Init( oCommConnect, oP_Base ) CLASS P_BaseXml_BlankObject
// Es kann ein ABaseCommConnect-Object übergeben werden.
// Geschieht das nicht, wird auch kein Laufbalken gezeigt
// Es kann ein vorhandenes P_BAse-Object übergeben werden
// Dann wird mit dem gearbeitet.

	SUPER:Init( oCommConnect, oP_Base)


METHOD OnReadLine( cLine AS STRING ) AS STRING PASCAL CLASS P_BaseXml_BlankObject
// Wird gerufen, wenn eine Zeile aus der Datei gelesen wird und kann in der Ableitung umgeschrieben werden
return( SUPER:OnReadLine(cLine) )

METHOD OnReadTag( oNode AS P_XmlNode, oSqlReader AS ASqlStatement ) AS VOID PASCAL CLASS P_BaseXml_BlankObject
// Wird gerufen, wenn ein XML-Tag aus dem Sql-Server an die XML_Nodes angehängt wird.
// Kann in der Ableitung umgeschrieben werden
SUPER:OnReadTag( oNode, oSqlREader )

METHOD OnWriteTag( oNode AS P_XmlNode ) AS LOGIC PASCAL CLASS P_BaseXml_BlankObject
// Wird gerufen, wenn ein XML-Tag als String erzeugt wird
// Die Rückgabe legt fest, ob der Tag (und seine Untertags) geschrieben wird
// Kann in der Ableitung umgeschrieben werden
return( SUPER:OnWriteTag( oNode ) )

METHOD OnWriteLine( cLine AS STRING ) AS STRING PASCAL CLASS P_BaseXml_BlankObject
// Wird gerufen, wenn eine Zeile in die Datei geschrieben wird und kann in der Ableitung umgeschrieben werden
return( SUPER:OnWriteLine(cLine) )




DEFINE  xml_Auto_ID          := -1
DEFINE  xmlType_Node         := 1
DEFINE  xmlType_Attribute    := 2
DEFINE  xmlType_Value        := 3
/*
  	P_BaseXML : Xml-Import und Export über MSSql - opemxml
	-------------------------------------------------------

	Als erstes sollte eine abgeleitete Klasse von P_BaseXml erstellt werden, um die Methoden
	OnReadLine(), OnReadTag(), OnWriteLine(), OnWriteTag() nutzen zu können.





*/


CLASS P_BaseXML INHERIT AOBJECT

	/* Import */
	DECLARE METHOD ImportFromFile                                            // Importiert aus einer Datei
	DECLARE METHOD ImportFromString                                          // Importiert aus einem String
	DECLARE METHOD OnReadLine                                                // Wird beim Import jeder Zeile aus einer Datei gerufen. Hier kann noch manipuliert werden.
	DECLARE METHOD OnReadTag                                                 // Wird gerufen, wenn ein Tag generiert wird. Hier kann noch manipuliert werden.

	/* Export */
	DECLARE METHOD ExportToFile                                              // Exportiert die Knoten als XML-Datei
	DECLARE METHOD ExportToString                                            // EXportiert die Knoten als XML in einen String
	DECLARE METHOD OnWriteLine                                               // Wird gerufen, wenn eine Zeile in die Datei geschrieben wird. Hier kann noch manipuliert werden
	DECLARE METHOD OnWriteTag                                                // Wird gerufen, wenn aus dem Tag eine Zeile generiert wird. Hier kann noch manipuliert werden.

	DECLARE METHOD Documentation                                             // Es wird automatisch eine Dokumentation erzeugt und in eine Datei geschrieben
	DECLARE METHOD ReleaseCargo                                              // Hiermit können alle Cargo-Objekte von SELF:XML (s. P_XmlNode) freigeben werden
	DECLARE METHOD NodeDepth                                                 // Gibt die maximale Knoten-Tiefe zurück. Siehe P_BaseXml:XML:nLevel
	DECLARE ACCESS lError                                                    // Gibt TRUE zurück, wenn eine Message mit PROT_ART_ERROR erstellt wurde
	DECLARE ACCESS oBase                                                     // Gibt das P_BAse-Objekt zurück, welches erstellt, bzw. bei Init mit übergeben wurde
	DECLARE ACCESS XML                                                       // Gibt die Ansammlung von Knoten (s. P_XmlNode ) zurück
	DECLARE ASSIGN XML

	EXPORT         oXmlHeader                     AS P_XmlHeader             // Ein Objekt zum manipulieren des XML-Headers <?XML .../> für Version und Encoding
	EXPORT         cbValueConverter               AS CODEBLOCK               // ein Codeblock zum konvertieren des Value-Wertes bei Abfrage von SELF:XML...:cValue
	EXPORT         cSqlCodeType := "varchar(max)" AS STRING                  // Der Code-Datentyp der für Texte vom SQL-Server benutzt werden soll
	EXPORT         cTabulator                     AS STRING                  // Gibt den Tabulator (TAB oder SPACE(2) etc.) für den Export an
	EXPORT         aSonderzeichen                 AS ARRAY                   // Eine Reihe von Sonderzeichen, welche beim Export übersetzt werden sollen. Der Import tut dies automatisch


	/* Interne Methoden */
	DECLARE METHOD __DocumentationHeader
	DECLARE METHOD __GetXmlStringHelper
	PROTECT        __oBase                        AS P_Base
	PROTECT        __oXMl                         AS P_XmlNode
	PROTECT        __lUnloaded                    AS LOGIC

METHOD Init( oCommConnect, oP_Base ) CLASS P_BaseXML
// Es kann ein ABaseCommConnect-Object übergeben werden.
// Geschieht das nicht, wird auch kein Laufbalken gezeigt
// Es kann ein vorhandenes P_BAse-Object übergeben werden
// Dann wird mit dem gearbeitet.

	SUPER:Init()

	if( !IsNil( oP_Base ) .and. IsInstanceOfUsual(oP_Base, #P_Base))
		SELF:__oBase := oP_Base
		SELF:__oBase:AddRef()
	else
		SELF:__oBase := P_Base{}
	endif

	if( !IsNil(oCommConnect) )
		SELF:__oBase:oCommConnect := oCommConnect
	endif

	SELF:__oBase:oProgress:ShowAlways()
	SELF:__oBase:oProgress:Title := "Xml-Handling"

	SELF:__oXml      := P_XmlNode{-9999,"",""}
	SELF:__oXml:oBase:= SELF:__oBase

	SELF:__lUnloaded      := TRUE
	SELF:cbValueConverter := NULL_CODEBLOCK
	SELF:oXmlHeader       := P_XmlHEader{ '<xml version="1.0" encoding="windows-1250"/>' }
	SELF:cTabulator       := CHR(9)

	// Hier kann von Aussen noch etwas zugefügt werden
	// Die Zeichen [1] werden in den Value-Felder
	// gegen die Zeichen [2] beim Export ausgetauscht
	// Der Import macht das bereits Selbstständig
	SELF:aSonderzeichen   := { {"&","&amp;"} ,;
										{"<","&lt;"} ,;
										{">","&gt;"} ,;
										{CHR(34),"&quot;"} ,;
										{"'","&apos;"} }


METHOD Destroy() AS VOID PASCAL CLASS P_BaseXML

	SELF:__oXml:Release()
	SELF:oXmlHeader:Release()
	SELF:__oBase:Release()

	SUPER:Destroy()

METHOD Documentation( cFileName AS STRING, aExampleNodePath AS ARRAY ) AS STRING PASCAL CLASS P_BaseXml

	LOCAL cDoc := ""         AS STRING
	LOCAL x                  AS INT
	LOCAL aClassTree         AS ARRAY

aClassTree := ClassTree( SELF )
cDoc := "Dokumentation Xml-Schnittstelle ( "
for x:=1 upto aLen( aClassTree )-1  // Kein AObject anzeigen
	cDoc += IIF( x==1, "", " : " ) + Symbol2String(ArrayGet(aClassTree,x))
next x
cDoc += " ), Stand "+DTOC(Today())+CRLF
cDoc += CRLF

cDoc += "Xml-Header: "+ SELF:oXmlHeader:cString + CRLF
cDoc += "Encoding: "+SELF:oXmlHeader:Get("Encoding") + ", Version = "+SELF:oXmlHeader:Get("Version") + CRLF
cDoc += "Übersetzung Sonderzeichen: "
for x:=1 upto aLen(SELF:aSonderzeichen)
	cDoc += IIF( x==1, "", ", ") + SELF:oBase:StringFormat( '# = #', { SELF:aSonderzeichen[x][1], SELF:aSonderzeichen[x][2] } )
next x
cDoc += CRLF
cDoc += "Sql-CodeType für Text-Konvertierung: "+SELF:cSqlCodeType +CRLF
cDoc += "Maximale Knotentiefe: "+NTrim( SELF:NodeDepth() ) + CRLF
cDoc += CRLF
for x:=1 upto aLen( aExampleNodePath )
	cDoc += SELF:__DocumentationHeader( aExampleNodePath[x] )
next x
cDoc += "XML-Baum (IDs, Attribute, Level und Values)  "+CRLF
cDoc += "--------------------------------------------------------------" + CRLF
cDoc += SELF:XML:XMlTree( TRUE, TRUE, TRUE, FALSE, TRUE ) +CRLF
cDoc += CRLF
cDoc += "Auszug aus der XML-Datei:"+CRLF
cDoc += "--------------------------------------------------------------" + CRLF
cDoc += Left( SELF:ExportToString(), 1000) + "..." + CRLF

if( !Empty(cFileName) )
	SELF:oBase:FileWriteFromString( cFileName, cDoc, FALSE, FALSE )
endif
return( cDoc )

PROTECT METHOD __DocumentationHeader( cNodePath AS STRING ) AS STRING PASCAL CLASS P_BaseXml

	LOCAL cDoc := ""        AS STRING
	LOCAL oNode             AS P_XmlNode
	LOCAL oChildNode        AS P_XmlNode
	LOCAL aNodes            AS ARRAY
	LOCAL aChildNodes       AS ARRAY
   LOCAL x,y               AS INT

aNOdes := SELF:XML:Reader( cNodePath )
for x:= 1 upto aLen( aNodes )
	oNode := aNodes[x]
	cDoc += SELF:oBase:StringFormat( '#[#] : "#", Anzahl Child-Knoten: #, Anzahl Attribute: #', { cNodePath, oNode:nID, oNode:cValue, oNode:CountNodes(), oNode:CountAttributes() } ) + CRLF
	aChildNodes := oNode:aChilds
	for y := 1 upto aLen( aChildNodes )
		oChildNode := aChildNodes[y]
		cDoc += SELF:oBase:StringFormat( '   [#] # (#) : "#" (Pfad=#) ', { oChildNode:nID, oChildNode:cName, oChildNode:nLevel, oChildNode:cValue, oChildNode:cPath } ) + CRLF
	next y
	cDoc += CRLF
next x
cDoc += CRLF
return( cDoc )


METHOD ExportToString() AS STRING PASCAL CLASS P_BaseXml
// Exportiert die Knoten zu einem Xml-String. Der <XML> Tag wird vorne an gestellt
return( SELF:__GetXmlStringHelper( SELF:XML, SELF:oXmlHeader:cString + CRLF, 0) )

PROTECT METHOD __GetXmlStringHelper( oParentNode AS P_XMLNode, cXmlString AS STRING, nLevel AS INT ) AS STRING PASCAL CLASS P_BaseXml

	LOCAL aNodes                      AS ARRAY
	LOCAL x,y                         AS INT
	LOCAL oNode                       AS P_XmlNode
	LOCAL aAttributes                 AS ARRAY
	LOCAL cAttributeString := ""      AS STRING
	LOCAl cValue := ""                AS STRING
	LOCAL cElement := ""              AS STRING
	LOCAL cTab                        AS STRING

nLevel++
aNodes := oParentNode:Scan( { |oNode| 1=1 }, TRUE, TRUE )
for x:=1 upto aLen( aNodes )
	oNode     := aNodes[x]

	if( SELF:OnWriteTag( oNode ) )
		aAttributes      := oNode:aAttributes
		cAttributeString := ""
		for y:=1 upto aLen(aAttributes)
			cAttributeString += SELF:oBase:StringFormat(' #="#"', { aAttributes[y][1], aAttributes[y][2] })
		next y

		cTab := SELF:oBase:StringRepeat( oNode:nLevel-1, SELF:cTabulator )
		do case
		case( oNode:CountNodes() > 0 )
			/* Knoten mit weiteren Knoten */
			cXmlString += cTab + "<"+ oNode:cName + cAttributeString + ">"+CRLF
			cXmlString := SELF:__GetXmlStringHelper( oNode, cXmlString, nLevel )
			cXmlString += cTab + "</"+ oNode:cName + ">"

		case( Empty(oNode:cValue) )
			/* Knoten ohne Value */
			cXmlString += cTab + "<"+ oNode:cName + cAttributeString + " />"
		otherwise
			cXmlString += cTab + "<"+ oNode:cName + cAttributeString + ">" + SELF:oBase:StringTranslate(oNode:cValue, SELF:aSonderzeichen) + "</"+oNode:cName+">"
		endcase
	endif

	cXmlString += IIF( nLevel == 1 .and. x == aLen(aNodes), "", CRLF )
Next x
return( cXmlString )

METHOD ExportToFile( cFileName AS STRING, lOverwrite := FALSE AS LOGIC, lUnicode := FALSE AS LOGIC, lDOSFormat := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseXml
// Exportiert die Knoten als XML-Dokument in die Datei <cFileName>. Wird <lOverwrite> angegeben, so kann eine
// vorhandene Datei überschrieben werden. Anderfalls muss sich selbst darum gekümmert werden.
	LOCAL lExported := FALSE      AS LOGIC
   LOCAL hFWrite                  AS PTR
	LOCAL pszLine                  AS PSZ
	LOCAL cXmlString               AS STRING

IF( !lOverWrite .and. SELF:oBase:FileExists( cFileName ) )
	SELF:oBase:MessageFormat( "Fehler beim exportieren der Datei #. Die Datei # existiert bereits am Zielort.", { cFileName, cFileName }, PROT_ART_ERROR )
ELSE
	if( SELF:oBase:FileExists( cFileName ) )
		SELF:oBase:FileDelete( cFileName )
	endif

   hfWrite := FCreate(cFileName)
   IF( hfWrite != f_ERROR )
		cXmlString := SELF:ExportToString()
		if( lUnicode )
			cXmlString := SELF:oBase:StringToUnicode( cXmlString )
		endif
		pszLine := String2Psz(cXmlString)

		FWrite4( hfWrite, pszLine, pszLen(pszLine), lDOSFormat )
   	lExported := TRUE
    	FClose( hfWrite )
    ELSE
		SELF:oBase:MessageFormat( "Fehler beim exportieren der Datei #. Fehler: #" , {cFileName, DosErrString(FError())}, PROT_ART_ERROR )
    ENDIF

ENDIF
return( lExported )


METHOD ImportFromFile( cFileName AS STRING, lUnicode := FALSE AS LOGIC, lDOSFormat := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseXML
// Importiert die Datei <cFileName> als XML-Knoten in SELF:XML
  	LOCAL hFRead                  AS PTR
  	LOCAL cLine                   AS STRING
  	LOCAL nRow := 0               AS INT
  	LOCAL nCounter := 0           AS INT
	LOCAL cXmlString              AS STRING
	LOCAL lReadEmptyLines := TRUE AS LOGIC

cXmlString := ""
hFRead := FOpen2(cFileName, FO_READWRITE)
IF( hfRead != f_ERROR )

     DO WHILE !FEof(hFRead)
          cLine := FReadLine(hFRead, 30000)
          IF( !Empty(cLine) .or. lReadEmptyLines )
					cXmlString += SELF:OnReadLine(cLine) + CRLF

					nCounter++
               if( nCounter > 20 )
						nCounter := 0
               	SELF:oBase:ProgressIncrement( SELF:oBase:StringFormat( "Lese XML Zeile # aus #",{ nRow, cFileName } ) )
               endif
          ENDIF
			 nRow++
     ENDDO

		if( lUnicode )
			cXmlString := SELF:oBase:StringFromUnicode(cXmlString)
		endif

      if( lDOSFormat )
      	cXmlString := Oem2Ansi(cXmlString)
      endif

		IF( Len(cXmlString) == 0 )
     		SELF:oBase:MessageFormat( "Die Datei # enthällt keine Daten. Die Anzahl der enthaltenen Zeilen ist Null.", {cFileName} , PROT_ART_ERROR)
     ENDIF
     FClose(hFRead)

ELSE
	SELF:oBase:MessageFormat( "Fehler beim importieren der Datei #. Fehler: #", {cFileName, DosErrString(FError())} , PROT_ART_ERROR)
ENDIF
return( SELF:ImportFromString(cXmlString ))


METHOD ImportFromString( cXml AS STRING ) AS LOGIC PASCAL CLASS P_BaseXML
// Konvertiert einen XML-String zu SELF:XML Knoten. Es wird eine Temp-Tabelle auf dem SQL-Server erstellt und der String
// aus Geschwindigkeitsgründen Stück für Stück dorthin übertragen. Mit openXml wird die komplette Knoten-Tabelle ausgelesen
// und in die SELF:XML Knoten ( P_XmlNode ) konvertiert.
	LOCAL lOK := FALSE                 AS LOGIC
	LOCAL oXmlTable                    AS ASqlStatement
	LOCAl lTempTableCreated := FALSE   AS LOGIC
	LOCAL oParentNode                  AS P_XmlNode
	LOCAL nPos, nLen                   AS INT
	LOCAL cStmt                        AS STRING
	LOCAL nCounter := 0                AS INT
	LOCAL cXmlHeader := ""             AS STRING



if( lOK := !Empty(cXml) )
	//
   // Temp-Tabelle erstellen
   //
   SELF:oBase:ProgressIncrement( "Erstellen Temporärtabelle..." )
	oXmlTable := SELF:oBase:oTransactionManager:CreateStmt("CREATE TABLE #TempXmlTable (CODE TEXT)",,TRUE)
	if( !oXmlTable:ExecDirectBatch() )
		SELF:oBase:Message( "Fehler beim erstellen der Temporärtabelle aus dem SQL-Server : "+oXmlTable:Status:GetMessage() )
	else
		lTempTableCreated := TRUE
   endif
	oXmlTable:Release()

	//
	// <?XML -- Header Eleminieren
	//
	if( !SELF:lError )
		if( Upper(Left(cXML,4)) = '<XML' .or. Upper(Left(cXML,5)) = '<?XML') .AND. (nPos := At2('>',cXML)) > 0
			SELF:oXmlHeader:cString := SELF:oBAse:StringTranslate( Left(cXML,nPos+1), {{"<?","<"},{"?>",">"}, {"/>",">"}, {">","/>"}} )
			cXml := lTrim(Right( cXml, Len( cXml) - (nPos+1) ))
		endif
	endif

	//
	// XML in die Temp-Tabelle laden
	//
	if( !SELF:lError )
		nPos := 1
		nLen := Len(cXml)
		do while( !SELF:lError .and. nPos <= nLen )
			SELF:oBase:ProgressIncrement( SELF:oBase:StringFormat("Analysiere XML Teil # von #", { (nPos/100000)+1, (nLen/100000)+1 } ) )
			if( nPos = 1 )
				cStmt := "INSERT INTO #TempXmlTable SELECT N"+QuoteText( SubStr3( cXml, nPos, 100000))
			else
				cStmt := "DECLARE @ptrVal BINARY(16), @i INT "+CRLF+;
							"SELECT TOP 1 @ptrVal = TEXTPTR(CODE) FROM #TempXmlTable "+CRLF+;
							"SET @i = (SELECT TOP 1 DATALENGTH(CODE) FROM #TempXmlTable) "+CRLF+;
							"UPDATETEXT #TempXmlTable.CODE @ptrVal @i 0"+CRLF+;
							QuoteText( SubStr3( cXml, nPos, 100000 ) )
			endif
			oXmlTable := SELF:oBase:oTransactionManager:CreateStmt( cStmt,, TRUE )
			if( !oXmlTable:ExecDirectBatch() )
				SELF:oBAse:MessageFormat( "Fehler beim hochladen Xml-String (Teil # von #) zum SQL-Server : #", {  (nPos/100000)+1, (nLen/100000)+1, oXmlTable:Status:GetMessage() }, PROT_ART_ERROR )
			endif
			oXmlTable:Release()

			nPos += 100000
		enddo
	endif

	//
	// XML von TempTabelle durch den Parser jagen und ein Select-Statement bekommen
	//
	if( !SELF:lError )
		//__Trace_SQL_Queries__(TRUE)
		SELF:oBase:ProgressIncrement( "XML Struktur auflösen und auf Server laden..." )
		oXmlTable := SELF:oBase:oTransactionManager:CreateStmt( 	"DECLARE @hDoc int   "+CRLF+;
																					"DECLARE @xmlDoc xml "+CRLF+;
																					"SET @xmlDoc = (SELECT TOP 1 CODE FROM #TempXmlTable) "+CRLF+;
																					"exec sp_xml_preparedocument @hDoc OUTPUT, @xmlDoc , "+QuoteText(cXmlHeader)+CRLF+; //'<xml version="+Chr(34)+SELF:cVersion+Chr(34)+" encoding="+Chr(34)+SELF:cEncoding+Chr(34)+" />' "+CRLF+;
																					"select ISNULL(parentid,0) as parent,  "+CRLF+;
																					"  isNull((select nodeType from openxml(@hDoc,'/',1) where id = a.parentid),0) as parentType,  "+CRLF+;
																					"  id, parentid, nodetype, localname, isNull(prev,0) as prev, "+CRLF+;
																					"  isNull((select top 1 cast([TEXT] as "+SELF:cSqlCodeType+") from openxml(@hDoc,'/',1) where parentid = a.id and nodetype=3),'') as nodeText "+CRLF+;
																					"  from openxml(@hDoc,'/',1) as a  "+CRLF+;
																					"  order by parentid, nodetype, prev, ID  "+CRLF,, TRUE )
		if( !oXmlTable:Prepare() .or. !oXmlTable:ExecuteReader() )
			SELF:oBase:MessageFormat( "Fehler beim Upload der XML-Datei zum Sql-Server : #", { oXmlTable:Status:GetMessage() }, PROT_ART_ERROR )
		else
			//__Trace_SQL_Queries__(FALSE)
			nPos := 100
			do while( oXmlTable:Fetch() )
				nCounter++
	      	if( oXmlTable:FGetN(#ID) == 0 .and. oXmlTable:FgetN(#parent) == 0 .and. oXmlTable:FgetN(#NodeType) == xmlType_Node)
					/* Einstiegsknoten */
					SELF:XML:AddNode( 0, oXmlTable:FgetN(#localname) )
				else
					do case
					case( oXmlTable:FgetN(#NodeType) == xmlType_Node )
						// NODE
						oParentNode := SELF:XML:GetNode( oXmlTable:FGetN(#parentid), TRUE )
						//debugPrint( "FIN: ", __ENT, __LINE__, "IMport",oParentNode, oXmlTable:FgetN(#ID), oXmlTable:FGetN(#localname), oXmlTable:FGetN(#nodeText))
						oParentNode:AddNode( oXmlTable:FgetN(#ID), oXmlTable:FGetN(#localname), oXmlTable:FGetN(#nodeText))
						if( SELF:cbValueConverter != NULL_CODEBLOCK )
							oParentNode:GetNode( oXmlTable:FgetN(#ID) ):cbValueConverter := SELF:cbValueConverter
						endif
						SELF:OnReadTag( oParentNode:GetNode( oXmlTable:FgetN(#ID) ), oXmlTable )

						nPos++
						if( nPos >= 25 )
							SELF:oBase:ProgressIncrement( "Importiere Pfad ..."+ AllTrim(Right(oParentNode:GetNode(oXmlTable:FgetN(#ID)):cPath,35)))
							nPOs := 1
						endif

					case( oXmlTable:FgetN(#NodeType) == xmlType_Attribute )
						// ATTRIBUTE
						oParentNode := SELF:XML:GetNode( oXmlTable:FGetN(#parentid), TRUE )
						oParentNode:AddAttribute( oXmlTable:FgetN(#ID), oXmlTable:FgetN(#localname), oXmlTable:FGetN(#nodeText) )

					case( oXmlTable:FgetN(#NodeType) == xmlType_Value )
						// VALUE
						// NIchts tun, da bereits durch #NodeText der Text in Node und Attribute gekommen ist
					endcase
	         endif
			enddo

			if( nCounter == 0 )
				SELF:oBase:Message( "Es konnten keine Daten aus dem XML-Konstrukt ermittelt werden", PROT_ART_ERROR )
			endif
			lOK := !SELF:lError
		endif
		oXmlTable:Release()
	endif

	//
	// Temp-Tabelle löschen
	//
	if( lTempTableCreated )
	   // Temp-Tabelle wieder zerstören
		oXmlTable := SELF:oBase:oTransactionManager:CreateStmt("DROP TABLE #TempXmlTable",,TRUE)
		if( !oXmlTable:ExecDirectBatch() )
			SELF:oBase:Message( "Fehler beim löschen (Drop) der SQL-Temporärtabelle #TempXmlTable : "+oXmlTable:Status:GetMessage() )
		else
			lTempTableCreated := FALSE
	   endif
		oXmlTable:Release()
	endif

	SELF:oBase:ProgressIncrement( "Verarbeitung XML-Struktur..." )
endif
return( lOK )

METHOD OnReadLine( cLine AS STRING ) AS STRING PASCAL CLASS P_BaseXML
// Wird gerufen, wenn eine Zeile aus der Datei gelesen wird und kann in der Ableitung umgeschrieben werden
return( cLine )

METHOD OnReadTag( oNode AS P_XmlNode, oSqlReader AS ASqlStatement ) AS VOID PASCAL CLASS P_BaseXml
// Wird gerufen, wenn ein XML-Tag aus dem Sql-Server an die XML_Nodes angehängt wird.
// Kann in der Ableitung umgeschrieben werden

METHOD OnWriteTag( oNode AS P_XmlNode ) AS LOGIC PASCAL CLASS P_BaseXml
// Wird gerufen, wenn ein XML-Tag als String erzeugt wird
// Die Rückgabe legt fest, ob der Tag (und seine Untertags) geschrieben wird
// Kann in der Ableitung umgeschrieben werden
return( TRUE )

METHOD OnWriteLine( cLine AS STRING ) AS STRING PASCAL CLASS P_BaseXml
// Wird gerufen, wenn eine Zeile in die Datei geschrieben wird und kann in der Ableitung umgeschrieben werden
return( cLine )

ACCESS oBase	AS P_Base PASCAL CLASS P_BaseXML
// Liefert das übergebene oder erstellte oBase-Objekt
return( SELF:__oBase )

ACCESS lError	AS LOGIC PASCAL CLASS P_BaseXML
// Gibt True zurück, wenn eine Message mit PROT_ART_ERROR abgesetzt wurde
return( SELF:__oBase:lError )

ACCESS XML AS P_XmlNode PASCAL CLASS P_BaseXML
// Die XML-Knoten im Format P_XmlNode
return( SELF:__oXml )

ASSIGN XML( oXml AS P_XmlNode ) AS P_XmlNode PASCAL CLASS P_BaseXml
// Die XML-Knoten im Format P_XmlNode
if( SELF:__oXml !=  NULL_OBJECT )
	SELF:__oXml:Release()
endif
SELF:__oXml := oXml
SELF:__oXml:AddRef()
return( oXml )

METHOD NodeDepth() AS INT PASCAL CLASS P_BaseXml
	LOCAL nMAxLevel := 0    AS INT
SELF:XML:ScanVoid( { |oNode| nMaxLevel := IIF( oNode:nLevel > nMaxLevel, oNode:nLevel, nMaxLevel ) }, FALSE, TRUE )
return( nMaxLevel )

METHOD ReleaseCargo( cbCodeBlock AS CODEBLOCK ) AS VOID PASCAL CLASS P_BaseXML
// Sollten Objekte als Cargo an die P_XmlNode(s) angehängt worden sein, so müssen
// diese vor dem P_BaseXml:Release() auch freigegeben werden. Es sei denn, es wird
// sich von Aussen darum gekümmert.
	LOCAL aNodesWithCargo           AS ARRAY

aNodesWithCargo := SELF:XML:Scan( { |oNode| oNode:oCargo != NULL_OBJECT }, FALSE, FALSE )
aeVal( aNodesWithCargo, { |oNode| oNode:Release() } )











//
// P_XmlNode : Eine Collection von Knoten, speziell für die Verarbeitung von Xml-Dateien
// ------------------------------------------------------------------------------------------------
//
//
//
//
//
//
//
//
//
//
//

CLASS P_XmlNode        INHERIT AObject

	/* Properties */
	EXPORT          cName                       AS STRING        // Name des Knoten
	EXPORT          nID                         AS INT           // Eindeutige ID des Knoten, Kann durch SELF:nNewID gesetzt werden
	EXPORT          nLevel                      AS INT           // Tiefe (1,2,3...) in der Hirarchie
	EXPORT          oOwner                      AS P_XmlNode     // Zu welcher Node gehört diese Node
	EXPORT          oCargo                      AS AObject       // Kann benutzt werden, um Informationen abzulegen. Muss jedoch selbst wieder Release() werden
	EXPORT          cbValueConverter            AS CODEBLOCK     // Kann benutzt werden, um cValue bei Abfragen zu konvertieren
	EXPORT          lReleaseCargo               AS LOGIC         // Wird ein Cargo-Objekt angehängt, so kann es bei Zerstörung des KNoten automatisch Released werden

	/* Einzelne Nodes */
	DECLARE METHOD  AddNode                                      // Fügt einen neuen Knoten an den aktuellen an. Mit AddNode( xml_Auto_ID, ...) kann eine ID vergeben werden
	DECLARE METHOD  AddNodeElements                              // Fügt einen Baum von Knoten an den aktuellen Knoten an. Die IDs müssen stimmen. Der Rest wird angepasst
	DECLARE METHOD  GetNode                                      // Gibt den Konten mit einer bestimmten ID zurück.
	DECLARE METHOD  GetNodeByName                                // Gibt den Knoten mit dem Namen zurück
	DECLARE METHOD  GetNodeBySymbol                              // Gibt den Knoten mit dem Symbolnamen zurück
	DECLARE METHOD  CountNodes                                   // Gibt die Anzahl angehängter (Childs) Knoten zurück
	DECLARE METHOD  DelNode                                      // Löscht, bzw. Released den aktuellen Knoten

	/* Liste von Nodes */
	DECLARE METHOD  GetNodesByName                               // Gibt ein Array mit allen Knoten zurück, welche den Namen haben
	DECLARE METHOD  GetNodesBySymbol                             // Gibt ein Array mit allen Knoten zurück, welche den Symbolnamen haben

	/* Attribute */
	DECLARE METHOD  AddAttribute                                 // Fügt ein Attribute an den aktuellen Knoten an. Mit AddAttribute( xml_Auto_ID,...) kann eine ID vergeben werden
	DECLARE METHOD  IsAttribute                                  // Prüft, ob es unter den Attributen zum Knoten mit dem Namen gibt
	DECLARE METHOD  isAttributeValue                             // Prüft, ob es ein Attribut mit Namen und Value gibt
	DECLARE METHOD  GetAttributeValue                            // Gibt den Inhalt (Value) zum Attribut mit dem Namen zurück
	DECLARE METHOD  GetAttribute                                 // Gibt das Attribut zur ID als Array { cName, cValue, nID } zurück
	DECLARE METHOD  ScanAttributes                               // Durchsucht die Attribute zum Knoten mit einem Codeblock
	DECLARE METHOD  CountAttributes                              // Gibt die Anzahl der Attribute zum Knoten zurück

	/* Reader und Scan */
	DECLARE METHOD  Scan                                         // Durchsucht alle Knoten den Baum nach unten mit einem Codeblock und gibt ggf. ein Array mit Knoten zurück
	DECLARE METHOD  ScanVoid                                     // Durchsucht alle Knoten den Baum nach unten und führt einen Codeblock aus. Es wird kein Ergebis erwartet
	DECLARE METHOD  Reader                                       // Durchsucht alle Knoten den Baum nach Unten nach einem Pfad "/BoQ/BoQInfo/Adress"
	DECLARE METHOD  XmlTree                                      // Gibt eine Visualisierung aller Knoten ab dem aktuellen Knoten als String zurück
	DECLARE METHOD  Verify                                       // Überprüft den aktuellen Knoten und alle darunter auf Vollständigkeit (ID-Nummerierung, oBase, Name und Value)

	/* Variablen */
	DECLARE ACCESS  cPath                                        // Der Pfad "/Root/Adresses/Adress" des aktuellen Knoten
	DECLARE ACCESS  aChilds                                      // Alle angehängten Knoten als Array { oNode, oNode,... }
	DECLARE ACCESS  aAttributes                                  // Alle angehängten Attribute als Array { {cName,cValue,nID}, {cName,cValue,nID},...}
	DECLARE ACCESS  symName                                      // Gibt den Namen (cName) als Symbol zurück. Leerzeichen wurden entfernt
	DECLARE ACCESS  oBase                                        // Gibt das P_Base-Objekt zurück, welches jedem Knoten angehängt wurde
	DECLARE ASSIGN  oBase
	DECLARE ACCESS  cValue
	DECLARE ASSIGN  cValue                                       // Gibt den Inhalt (Value) des aktuellen Knoten zurück. Siehe auch cbValueConverter
	DECLARE ACCESS  nActID                                       // Gibt die zuletzt vergebene ID zurück
	DECLARE ACCESS  nNewID                                       // Vergibt eine neue ID und speichert diese in nActID

	/* Interne Methoden */
	DECLARE METHOD  __AddPath
	DECLARE METHOD  __GetBaseNode
	DECLARE METHOD  __GetNodeHelper
	DECLARE METHOD  __GetNodesHelper
	DECLARE METHOD  __AttributeHelper
	DECLARE METHOD  __ScanHelper
	DECLARE METHOD  __PathHelper
	DECLARE METHOD  __CleanUpChildList
	PROTECT         __aChildNodes               AS ARRAY
	PROTECT         __aAttributes               AS ARRAY
	PROTECT         __oBase                     AS P_BASE
	PROTECT         __cValue                    AS STRING
	PROTECT         __nNewID                    AS INT

METHOD Init( nID, cName, cValue ) CLASS P_XmlNode
//
// Für die Main-Node kann auch nichts übergeben werden
// oVar := P_Collection{}
//

	SUPER:Init()

	SELF:__oBase         := NULL_OBJECT
	SELF:__aChildNodes   := {}
	SELF:__aAttributes   := {}

	SELF:oOwner          := NULL_OBJECT
	SELF:nID             := IfNil(nID,SELF:nNewID)
   SELF:cName           := IfNil(cName,"")
   SELF:__cValue        := IfNil(cValue, "" )
	SELF:__nNewID        := 0

   SELF:nLevel := 0
	SELF:cbValueConverter := NULL_CODEBLOCK

	// Jedem Knoten kann ein Objekt angehängt werden (z.B. mit Originalwerten etc.).
	// P_XmlNode speichert diese nur beim Knoten. Es wird sonst nichts weiter damit gemacht
	// Ein Release() muss ggf. Selbstständig erfolgen oder kann durch <lReleaseCargo> von P_XmlNode
	// gemacht werden.
	SELF:lReleaseCargo    := FALSE
	SELF:oCargo           := NULL_OBJECT

METHOD Destroy() AS VOID PASCAL CLASS P_XmlNode
// Am Ende werden auch alle enthaltenen Knoten freigegeben

	LOCAL x          as INT
	LOCAL oNode      AS P_XmlNode

	/* Meine Childs löschen */
	for x:=1 upto aLen( SELF:__aChildNodes )
		oNode := SELF:__aChildNodes[x]
		oNode:oOwner := NULL_OBJECT
		oNode:Release()
	next x
	SELF:__aChildNOdes := {}

	/* Mich bei meinem Owner austragen */
	if( SELF:oOwner != NULL_OBJECT )
		SELF:oOwner:__CleanUpChildList( SELF )
		SELF:oOwner := NULL_OBJECT
	endif

	// Wenn ein Cargo-Objekt angehängt wurde und dies bei
	// Zerstörung freigegeben werden soll, dies nun tun.
	if( SELF:lReleaseCargo .and. SELF:oCargo != NULL_OBJECT )
		SELF:oCargo:Release()
	endif

	SELF:oBase:Release()

	SUPER:Destroy()

PROTECT METHOD __CleanUpChildList( oNode AS P_XmlNode ) AS VOID PASCAL CLASS P_XmlNode
// Die übergebene Node <oNode> aus der ChildsList löschen
	LOCAL x       AS INT
for x:=1 upto aLen( SELF:__aChildNodes )
	if( SELF:__aChildNodes[x]:nID == oNode:nID  )
		aDelShrink( SELF:__aChildNodes, x )
		exit
	endif
next x

METHOD Verify() AS LOGIC PASCAL CLASS P_XmlNode
// Überprüft alle Knoten - ab dem aktuellen nach Unten - ob sie vollständig und lückenlos durchnummeriert (ID) sind
	LOCAL lOK := TRUE      AS LOGIC
	LOCAL aNodes           AS ARRAY
	LOCAL aAttr            AS ARRAY
	LOCAL aIDs             AS ARRAY
	LOCAL x                AS INT
	LOCAL oNode            AS P_XmlNode

aNodes := SELF:Scan( { |oNode| 1=1 }, FALSE, TRUE )
//aEval( aNodes, { |oNode| aadd( aIDs, oNode:nID } )

aIDs := {}
for x:=1 upto aLen( aNodes )
	oNode := aNodes[x]
	aadd( aIDs, oNode:nID )

	aAttr := oNode:aAttributes
	aEVal( aAttr, { |a| aadd( aIDs, a[3] )} )

	if( oNode:oBase != NULL_OBJECT .and. SELF:oBase == oNode:oBase)
		if( IsString(oNode:cName) )
			if( Empty( oNode:cName ) )
				SELF:oBase:MessageFormat( "Der Knoten mit der ID # hat keinen Namen (cName). Dies wird zu Fehlern beim Export führen. ", { oNode:nID }, PROT_ART_WARNING )
				lOK := FALSE
			endif
		else
			SELF:oBase:MessageFormat( "Der Knoten mit der ID # hat einen fehlerhafen Namen (cName). Es ist kein String", { oNode:nID }, PROT_ART_WARNING )
			lOK := FALSE
		endif

		if( !IsString(oNode:cValue) )
			SELF:oBase:MessageFormat( "Der Knoten mit der ID # hat einen fehlerhafen Inhalt (cValue). Es ist kein String", { oNode:nID }, PROT_ART_WARNING )
			lOK := FALSE
		endif

	else
		oNode:__oBase := SELF:oBase
		SELF:oBase:MessageFormat( "Der Knoten mit der ID # hat eine fehlerhaftes oder fehlendes Base-Objekt. Es wurde durch ein gültiges ersetzt.", { oNode:nID }, PROT_ART_ERROR )
		lOK := FALSE
	endif
next x

/*
aSort( aIDs )
for x:=2 upto aLen(aIDs)
	if( aIDs[x] != aIDs[x-1]+1 )
		SELF:oBase:MessageFormat( "Fehlerhafte Nummerierung zwischen Knoten (ID=#) und Knoten (ID=#). Es fehlen IDs.", { aIds[x-1], aIds[x] }, PROT_ART_WARNING )
		lOK := FALSE
	endif
next x
*/
return( lOK )


ACCESS symName AS SYMBOL PASCAL CLASS P_XmlNode
// Gibt den Knotennamen als Symbol (Leerzeichen sind eleminiert) zurück
return( String2Symbol( SELF:oBase:StringReplace( SELF:cName, " ", "" ) ) )

ACCESS oBase AS P_BASE PASCAL CLASS P_XmlNode
// In jedem Knoten P_XmlNode wird der gleiche Objektverweis auf P_Base gespeichert.
// So kann er in jedem Knoten benutzt werden.
return( SELF:__oBase )

ASSIGN oBase( __oBase AS P_Base ) AS P_BASE PASCAL CLASS P_XmlNode
if( SELF:__oBase == NULL_OBJECT )
	SELF:__oBase := __oBase
	SELF:__oBase:AddRef()
else
	SELF:__oBase:MessageFormat( "P_XmlNode->oBase : Es wird versucht ein bereits vorhandenes P_Base-Objekt zu ersetzen", {}, PROT_ART_ERROR )
endif
return( SELF:__oBase )

METHOD AddNode( nID := -1 AS INT, cName AS STRING, cValue := "" AS STRING ) AS P_XmlNode PASCAL CLASS P_XmlNode
// Fügt einen neuen Knoten an SELF an. Die ID muss eindeutig sein und kann mit SELF:nNewID generiert werden
// Wird <nID> mit 0 übergeben, so wird (s. xml_Auto_ID) automatisch eine neue ID aus SELF:nNewID generiert.
// Die aktuelle ID kann mit SELF:nActID abgefragt werden

	LOCAL oChild           AS P_XmlNode

nID := IIF(nID==xml_Auto_ID, SELF:nNewID, nID)
oChild := P_XmlNode{ nID, cName, cValue }
aadd( SELF:__aChildNodes, oChild )
SELF:AddNodeElements( oChild )
return( oChild )

METHOD AddNodeElements( oNode AS P_XmlNode ) AS P_XmlNode PASCAL CLASS P_XmlNode
// Fügt ein Knoten-Baum an einen vorhandenen Knoten und vergibt Owner, Base und Level Neu
// Somit kann ein vorhandener Knoten-Baum an das aktuelle Element angehängt werden
	LOCAL oSubNode      AS P_XmlNode
	LOCAL aChilds       AS ARRAY
	LOCAl x             AS INT

oNode:oOwner := SELF
oNode:oBase  := SELF:oBase
oNode:nLevel := SELF:nLevel+1

if( oNode:CountNodes() != 0 )
	aChilds := oNode:aChilds
	for x:=1 upto aLen(aChilds)
		oSubNode := aChilds[x]
		oNode:AddNodeElements( oSubNode )
	next x
endif
return( SELF )

METHOD DelNode() AS VOID PASCAL CLASS P_XmlNode
// Löscht den aktuellen Knoten, bzw. gibt ihn frei
SELF:Release()

METHOD GetNode( nID AS INT, lCompleteTreeDown := FALSE AS LOGIC ) AS P_XmlNode PASCAL CLASS P_XmlNode
// Ermittelt einen Knoten anhand der ID <nID> und gibt den ermittelten Knoten zurück. Wurde nichts gefunden, so wird NULL_OBJECT zurückgegeben.
// Wird <lCompleteTreeDown> mit TRUE angegeben, so wird der komplette Baum ab der aktuellen Node durchsucht
return( SELF:__GetNodeHelper( { |oNode| oNode:nID == nID }, lCompleteTreeDown) )

METHOD GetNodeByName( cName AS STRING, lCompleteTreeDown := FALSE AS LOGIC  ) AS P_XmlNode PASCAL CLASS P_XmlNode
// Ermittelt einen Knoten anhand des Namens <cName> und gibt den ermittelten Knoten zurück. Wurde nichts gefunden, so wird NULL_OBJECT zurückgegeben.
// Der Name muss nicht eindeutig sein und es wird der erste gefundene Knoten zurückgegeben
// Wird <lCompleteTreeDown> mit TRUE angegeben, so wird der komplette Baum ab der aktuellen Node durchsucht
cName := Upper(AllTrim(cName))
return( SELF:__GetNodeHelper( { |oNode| Upper(AllTrim(oNode:cName)) == cName },lCompleteTreeDown ) )

METHOD GetNodeBySymbol( symName AS SYMBOL, lCompleteTreeDown := FALSE AS LOGIC ) AS P_XmlNode PASCAL CLASS P_XmlNode
// Ermittelt einen Knoten anhand des SymbolNamens <symName> und gibt den ermittelten Knoten zurück. Wurde nichts gefunden, so wird NULL_OBJECT zurückgegeben.
// Der Symbol-Name muss nicht eindeutig sein und es wird der erste gefundene Knoten zurückgegeben
// Wird <lCompleteTreeDown> mit TRUE angegeben, so wird der komplette Baum ab der aktuellen Node durchsucht
return( SELF:__GetNodeHelper( { |oNode| oNode:symName == symName }, lCompleteTreeDown ) )

PROTECT METHOD __GetNodeHelper( cbCodeBlock AS CODEBLOCK, lCompleteTreeDown := FALSE AS LOGIC ) AS P_XmlNode PASCAL CLASS P_XmlNode
	LOCAl aARRAY          AS ARRAY

aArray := SELF:Scan( cbCodeBlock, !lCompleteTreeDown, TRUE )
if( aLen(aArray)>0 )
	return( aArray[1] )
endif
return( NULL_OBJECT )

PROTECT METHOD __GetNodesHelper( cbCodeBlock AS CODEBLOCK, lCompleteTreeDown := FALSE AS LOGIC ) AS ARRAY PASCAL CLASS P_XmlNode
return( SELF:Scan( cbCodeBlock, !lCompleteTreeDown, TRUE ) )

METHOD GetNodesBySymbol( symName AS SYMBOL, lCompleteTreeDown := FALSE AS LOGIC ) AS ARRAY PASCAL CLASS P_XmlNode
// Gibt ein Array mit Knoten { oNode, oNode, ... } zurück, deren Symbolnamen <symName> entspricht.
// Wird lCompleteTreeDown mit True angegeben, so werden auch alle darunterliegenden Knoten durchsucht
return( SELF:__GetNodesHelper( { |oNode| oNode:symName == symName },lCompleteTreeDown ) )

METHOD GetNodesByName( cName AS STRING, lCompleteTreeDown := FALSE AS LOGIC  ) AS ARRAY PASCAL CLASS P_XmlNode
// Gibt ein Array mit Knoten { oNode, oNode, ... } zurück, deren Namen <cName> entspricht.
// Wird lCompleteTreeDown mit True angegeben, so werden auch alle darunterliegenden Knoten durchsucht
cName := Upper(AllTrim(cName))
return( SELF:__GetNodesHelper( { |oNode| Upper(AllTrim(oNode:cName)) == cName }, lCompleteTreeDown ) )

METHOD AddAttribute( nID AS INT, cName AS STRING, cValue AS STRING ) AS VOID PASCAL CLASS P_XmlNode
// Fügt ein Attribute an den aktuellen Knoten an.
nID := IIF( nID = xml_Auto_ID, SELF:nNewID, nID )
aadd( SELF:__aAttributes, {cName, cValue, nID } )

METHOD GetAttribute( nID AS INT, lCompleteTreeDown := FALSE AS LOGIC ) AS ARRAY PASCAL CLASS P_XmlNode
// Gibt die Attribute { cName, cValue, nId } zur ID <nID> zurück
return( SELF:__AttributeHelper( { |oNode, aAttr| aAttr[3] == nID }, { |oNode, aAttr| aAttr }, lCompleteTreeDown ) )

METHOD GetAttributeValue( cName AS STRING, lCompleteTreeDown := FALSE AS LOGIC ) AS STRING PASCAL CLASS P_XmlNode
// Gibt die Attribute { cName, cValue, nId } zum Attribut mit dem Namen <cName> zurück
cName := Upper(AllTrim(cName))
return( SELF:__AttributeHelper( { |oNode, aAttr| Upper(Alltrim(aAttr[1])) == cName }, { |oNode, aAttr| aAttr[2] }, lCompleteTreeDown ) )

METHOD isAttribute( cName AS STRING, lCompleteTreeDown := FALSE AS LOGIC  ) AS LOGIC PASCAL CLASS P_XmlNode
// Prüft, ob es in der aktuellen Node ein Attribute <cNamen> mit Inhalt <cValue> gibt
cName := Upper(AllTrim(cName))
return( SELF:__AttributeHelper( { |oNode, aAttr| Upper(AllTrim(aAttr[1])) == cName }, { |oNode, aAttr| IIF( aAttr[3] != -9999, TRUE, FALSE) }, lCompleteTreeDown ) )

METHOD isAttributeValue( cName AS STRING, cValue AS STRING, lCompleteTreeDown := FALSE AS LOGIC  ) AS LOGIC PASCAL CLASS P_XmlNode
// Prüft, ob es in der aktuellen Node ein Attribute <cNamen> mit Inhalt <cValue> gibt
cName := Upper(AllTrim(cName))
cValue:= Upper(AllTrim(cValue))
return( SELF:__AttributeHelper( { |oNode, aAttr| Upper(AllTrim(aAttr[1])) == cName .and. Upper(AllTrim(aAttr[2])) == cValue }, { |oNode, aAttr| IIF( aAttr[3] != -9999, TRUE, FALSE) }, lCompleteTreeDown ) )

METHOD ScanAttributes( cbScan AS CODEBLOCK, lCompleteTreeDown := FALSE AS LOGIC ) AS ARRAY PASCAL CLASS P_XmlNode
// Gibt alle Knoten/Attribute als Array { {oNode, { cAttrName, cAttrValue, nAttrID }}, ... } zurück, auf die der Codeblock <cbScan> zutraf
	LOCAL aResults        AS ARRAY

aResults := {}
SELF:__AttributeHelper( cbScan,  { |oNode, aAttr| IIF( aAttr[3] != -9999, aadd( aResults, { oNode, aAttr }), FALSE) }, lCompleteTreeDown )
return( aResults )

PROTECT METHOD __AttributeHelper( cbScan AS CODEBLOCK, cbResult AS CODEBLOCK, lCompleteTreeDown := FALSE AS LOGIC ) AS USUAL PASCAL CLASS P_XmlNode
// <cbScan>           : { |oNode, aAttr| TRUE/FALSE } : Bei True wird der Codeblock <cbResult> ausgeführt und das Ergebnis zurückgegeben
// <cbResult>         : { |oNode, aAttr| uErgebis }   : Gibt das Ergebis zurück
// [lCompleteTreeDown]: Bei True wird der Knoten und alle Unterknoten mit den Attributen durchsucht.

   LOCAL x        AS INT
   LOCAL oChild   AS P_XmlNode

for x:=1 upto aLen( SELF:__aAttributes )
	if( eVal( cbScan, SELF, SELF:__aAttributes[x] ) )
		return( eVal( cbResult, SELF, SELF:__aAttributes[x] ) )
	endif
next x
if( lCompleteTreeDown )
	for x:=1 upto aLen( SELF:__aChildNodes )
		oChild := SELF:__aChildNodes[x]
		oChild:__AttributeHelper( cbScan, cbResult, lCompleteTreeDown )
	next x
endif
return( eVal( cbResult, SELF, { "","",-9999} ) )

METHOD CountNodes() AS INT PASCAL CLASS P_XmlNode
// Gibt die Anzahl angehängter Knoten (Childs) zurück
return( aLen( SELF:__aChildNodes ) )

METHOD CountAttributes() AS INT PASCAL CLASS P_XmlNode
// Gibt die Anzahl angehängter Attribute (Array) zurück
return( aLen( SELF:__aAttributes ) )

ACCESS aChilds AS ARRAY PASCAL CLASS P_XmlNode
// Gibt die ChildNodes im Format { oNode, oNode, ... }
return( SELF:__aChildNodes )

ACCESS aAttributes AS ARRAY PASCAL CLASS P_XmlNode
// Gibt die Attribute im Format { {cName, cValue, nID}, {...}, ... } zurück
return( SELF:__aAttributes )

METHOD Scan( cbCodeBlock AS CODEBLOCK, lOnlyFirstLevel := TRUE AS LOGIC, lTopDown := TRUE AS LOGIC ) AS ARRAY PASCAL CLASS P_XmlNode
// Ein Array mit allen Nodes, welche bei dem <cbCodeBlock> TRUE lieferten
// aArray := Scan(  { |oNode| oNode:cName = "Finken"  } ) --> liefert alle Nodes mit Namen "Finken"
// <lOnlyFirstLevel> : Bei True werden nur die Childs der aktuellen Node durchsucht, nicht tiefer. Anderfalls alle
// <lTopDown>        : Bei True Wird von Oben nach Unter gesucht. Anderfalls andersrum

	LOCAL aArray     AS ARRAY

aArray := {}
SELF:__ScanHelper( { |oNode| IIF( eVal(cbCodeBlock, oNode), aadd( aArray, oNode), FALSE ) } , lOnlyFirstLevel, lTopDown )
return( aArray )

METHOD ScanVoid( cbCodeBlock AS CODEBLOCK, lOnlyFirstLevel := TRUE AS LOGIC, lTopDown := TRUE AS LOGIC ) AS VOID PASCAL CLASS P_XmlNode
// aArray := Scan(  { |oNode| oNode:cName := oNode:cName +NTrim(oNode:nID)  } )
// <lOnlyFirstLevel> : Bei True werden nur die Childs der aktuellen Node durchsucht, nicht tiefer. Anderfalls alle
// <lTopDown>        : Bei True Wird von Oben nach Unter gesucht. Anderfalls andersrum
SELF:__ScanHelper( cbCodeBlock , lOnlyFirstLevel, lTopDown )

METHOD __ScanHelper( cbCodeBlock AS CODEBLOCK, lOnlyFirstLevel := TRUE, lTopDown := TRUE AS LOGIC ) AS VOID PASCAL CLASS P_XmlNode

	LOCAL x           AS INT
	LOCA oChild       AS P_XmlNode

for x:=1 upto aLen( SELF:__aChildNodes )
	oChild := SELF:__aChildNOdes[x]
	if( !lTopDown .and. !lOnlyFirstLevel)
		oChild:__ScanHelper( cbCodeBlock, false, lTopDown )
	endif
	eVal( cbCodeBlock, oChild )
	if( lTopDown .and. !lOnlyFirstLevel)
		oChild:__ScanHelper( cbCodeBlock, False, lTopDown )
	endif
next x

METHOD XmlTree( lWithID := TRUE, lWithAttributes := TRUE, lWithPath := FALSE AS LOGIC, lWithValue := TRUE AS LOGIC, lWithLevel := FALSE AS LOGIC ) AS STRING PASCAL CLASS P_XmlNode
/*
	Es wird ein Xml-Baum erzeugt und als String zurückgegeben
	-------------------------------------------------------------------
   + GAEB[0] : "" (xmlns[2] = "http://www.gaeb.de/GAEB_DA_XML/200407")
      + GAEBInfo[3] : ""
         + Version[4] : "3.1"
         + VersDate[5] : "2009-12"
         + Date[6] : "2012-11-27"
         + Time[7] : "16:06:13"
         + ProgSystem[8] : "/ GXML Toolbox V3.13 R20101116"
         + ProgName[9] : "ORCA AVA Standard Edition 17.00.000.18"
 */
	LOCAL cString       AS STRING
	LOCAL aNodes        AS ARRAY
	LOCAL oNode         AS P_XmlNode
	LOCAL aAttr         AS ARRAY
	LOCAL x,z           AS INT

cString := SELF:oBase:StringFormat( '/# : "#" - #',{ SELF:cName, SELF:cValue, SELF:cPath }) + CRLF
aNodes  := SELF:Scan(  { |oNode| 1=1 }, FALSE, TRUE )
for x:=1 upto aLen( aNodes )
	oNode := aNodes[x]
	cString += SELF:oBase:StringRepeat( oNode:nLevel , "   " ) + "+ " + oNode:cName + IIF(lWithID,"["+NTrim(oNode:nID)+"]","")
	cString += IIF( lWithLevel, " : Level="+NTrim(oNode:nLevel), "" )
	cString += IIF( lWithValue, " : " + CHR(34) + oNode:cValue +CHR(34), "" )

	/* Attribute */
	aAttr := oNode:aAttributes
	if( lWithAttributes .and. aLen(aAttr) != 0 )
		cString += " ("
		for z := 1 upto aLen( aAttr )
			cString += IIF(z!=1,", ","") + aAttr[z][1] + IIF(lWithID,"["+NTrim(aAttr[z][3])+"]","") + IIF( lWithValue, " = " + CHR(34) + SELF:oBase:UsualToString(aAttr[z][2]) + CHR(34),"" )
		next z
		cString += ")"
	endif

	cString += IIF( lWithPath, "    " + oNode:cPath , "" ) + CRLF
next x
return( cString )

METHOD Reader( cNodePath AS STRING, cbFilter := nil AS USUAL  ) AS ARRAY PASCAL CLASS P_XmlNode
// Reader( "/Gaeb/Award/BoQ/BoQInfo*",  { |oNode| oNode:CountAttributes() != 0 } ) --> aArray mit Nodes
//
// Am Anfang und am Ende kann mit "*" gearbeitet werden
// *BoQ/BoQInfo  : Gibt alle Knoten zurück die auf BoQ/BoQInfo enden
// BoQ/BoQInfo*  : Gibt alle Knoten zurück, die mit BoQ/BoQInfo beginnen
// *BoQ/BoQInfo* : Gibt alle Knoten zurück, die BoQ/BoQInfo enthalten
// BoQ/BoQInfo   : Gibt nur die Knoten zurück die genau BoQ/BoQInfo heissen
//
// <cNodePath>   : Der Pfad, evtl. mit Sternen am Anfang oder Ende
// [cbFilter]    : Ein optionaler Filter mit dem das Ergebnis nochmal eingeschränkt werden kann

	LOCAL cbSearch          AS CODEBLOCK

cbFilter     := ifNil( cbFilter, { |oNode| 1=1 } )
do case
case( Left(cNodePath,1) == "*" .and. Right(cNodePath,1) == "*" )
	// Alles was den Path enthält: *GAEB/GaebInfo*
	cbSearch := { |oNode| AT2(cNodePath, Upper(AllTrim( oNode:cPath ))) != 0 .and. eVal( cbFilter, oNode ) }
case( Left(cNodePath,1) == "*" )
	// Alles was mit dem Pfad endet: */GaebInfo/Prj
	cbSearch := { |oNode| Right(Upper(AllTrim(oNode:cPath)),Len(cNodePath)) == cNodePath .and. eVal( cbFilter, oNode ) }
case( Right(cNodePath,1) == "*" )
	// Alles was mit dem Pfad beginnt: /GaebInfo*
	cbSearch := { |oNode| Left(Upper(AllTrim(oNode:cPath)),Len(cNodePath)) == cNodePath .and. eVal( cbFilter, oNode ) }
otherwise
	// Genau der Pfad: /GaebInfo/Prj
	cbSearch := { |oNode| Upper(AllTrim( oNode:cPath )) == cNodePath .and. eVal( cbFilter, oNode ) }
endcase

cNodePath    := SELF:oBase:StringReplace( Upper( SELF:__AddPath( SELF:cPath, cNodePath ) ), "*","" )
return(  SELF:Scan( cbSearch, FALSE, TRUE ) )

ACCESS cPath AS STRING PASCAL CLASS P_XmlNode
// Liefert den Pfad zur aktuellen NOde zurück.
// Beispiel: /Gaeb/Info/Award/BoQ
return( "/" + SELF:__PathHelper( "" ) )

METHOD __AddPath( cBasePath AS STRING, cPath AS STRING ) AS STRING PASCAL CLASS P_XmlNode
cBasePath := Alltrim( cBasePath )
cPath := AllTrim(cPath)
if( right( AllTrim( cBasePath ),1 ) == "/" )
	cBasePath := SELF:oBase:StringDecrement( cBasePath )
endif
if( left( AllTrim( cPath ),1) != "/" )
	cPath := "/" + cPath
endif
return( cBasePath + cPath )

PROTECT METHOD	__PathHelper( cPath AS STRING ) AS STRING PASCAL CLASS P_XmlNode
if( SELF:oOwner != NULL_OBJECT )
	cPath := SELF:cName + IIF( Empty(cPath), "",  "/" + cPath )
	cPath := SELF:oOwner:__PathHelper( cPath )
endif
return( cPath )

ACCESS cValue AS STRING PASCAL CLASS P_XmlNode
// Gibt des Inhalt (Value) des aktuellen Knoten als String zurück.
// Ist ein <cbValueConverter> angegeben, so wird Value erst durch
// den Konverter geschickt
	LOCAL cReturnValue          AS STRING

cReturnValue := SELF:__cValue
if( SELF:cbValueConverter != NULL_CODEBLOCK  )
	cReturnValue := eVal( SELF:cbValueConverter, cReturnValue )
endif
return( cReturnValue )

ASSIGN cValue( cNewValue AS STRING ) AS STRING PASCAL CLASS P_XmlNode
// Setzt den Inhalt (Value) des aktuellen Knoten aus <cNewValue>
// Ist ein <cbValueConverter> angegeben, so wird Value erst durch
// den Konverter geschickt

	LOCAL cOldValue     AS STRING

cOldValue := SELF:__cValue
SELF:__cValue := cNewValue
if( SELF:cbValueConverter != NULL_CODEBLOCK )
	cOldValue := eVal( SELF:cbValueConverter, cOldValue )
endif
return( cOldValue )

PROTECT METHOD __GetBaseNode() AS P_XMLNode PASCAL CLASS P_XmlNode
// Gibt den höhsten Knotenpunkt, welcher keinen Owner mehr hat, zurück
// Es wird der Baum nach oben durchgegangen.
	LOCAL nNewID := 0         AS INT
	LOCAL oBaseNode           AS P_XmlNode

oBAseNode := SELF
do while( oBaseNode:oOwner != NULL_OBJECT )
	oBaseNode := oBaseNode:oOwner
enddo
return( oBaseNode )

ACCESS nNewID AS INT PASCAL CLASS P_XmlNode
// Zählt einen internen Zähler hoch und gibt die neue ID zurück
	LOCAL oBaseNode        AS P_XmlNode
oBaseNode := SELF:__GetBaseNode()
oBaseNode:__nNewId := oBaseNode:__nNewID + 1
return(oBaseNode:__nNewId)

ACCESS nActID AS INT PASCAL CLASS P_XmlNode
// Gibt die aktuelle ID zurück
return(SELF:__GetBaseNode():__nNewID)




//
// P_XmlHeader : Die erste Zeile in einer Xml-Datei <?XML version="1.0" encoding="Windows-1250" ?>
// ------------------------------------------------------------------------------------------------
//
// Mit diesem Objekt kann die XML-Beschreibung geändert werden.
//
//
//
//
//
//
//
//
//
CLASS P_XmlHeader INHERIT AOBJECT

	EXPORT cString       := ""                       AS STRING
   EXPORT cOriginal     := ""                       AS STRING

	DECLARE METHOD Get
	DECLARE METHOD Set
	DECLARE METHOD StringBetween
	DECLARE METHOD StringReplaceBetween

	/* Interne Methoden */
   DECLARE METHOD __GetPositionsBetween

METHOD Init( cString ) CLASS P_XmlHeader

	SUPER:Init()

	if( !IsNil( cString ) )
		SELF:cString := cString
	endif
	SELF:cOriginal := SELF:cString

METHOD Get( cTag AS STRING ) AS STRING PASCAL CLASS P_XmlHeader
return(SELF:StringBetween( SELF:cString, cTag, CHR(34), CHR(34) ))

METHOD Set( cTag AS STRING, cValue AS STRING ) AS VOID PASCAL CLASS P_XmlHeader
SELF:cString := SELF:StringReplaceBetween( SELF:cString, cTag, cValue, CHR(34), CHR(34) )

METHOD StringBetween( cString AS STRING, cFind AS STRING, cStartMarker AS STRING, cEndMarker AS STRING, lCaseSensitive := FALSE AS LOGIC ) AS STRING PASCAL CLASS P_XmlHeader
// Sucht in <cString> und liefert den Text zwischen <cStartMarker> und <cEndMarker>, nach <cFind>
// SELF:SringBetween( '<XML Version="1.0" encoding="Windows-1250"/>', "encoding", chr(34), chr(34) ) --> "Windows-1250"

	LOCAL cReturnValue := ""                   AS STRING
	LOCAL aPOsitions                           AS ARRAY

aPositions := SELF:__GetPositionsBetween( cString, cFind, cStartMarker, cEndMarker, lCaseSensitive )
if( aPositions[1] > 0  .and. aPositions[2] > 0 )
	cReturnValue := SubStr3( cString, aPositions[1], aPositions[2]-aPositions[1] )
endif
return( cReturnValue )

METHOD StringReplaceBetween( cString AS STRING, cFind AS STRING, cReplace AS STRING, cStartMarker AS STRING, cEndMarker AS STRING, lCaseSensitive := FALSE AS LOGIC ) AS STRING PASCAL CLASS P_XmlHeader
// Sucht in <cString> und liefert den Text zwischen <cStartMarker> und <cEndMarker>, nach <cFind> und ersetzt es durch <cReplace>
// SELF:SringReplaceBetween( '<XML Version="1.0" encoding="Windows-1250"/>', "encoding", "none", chr(34), chr(34) ) --> '<XML Version="1.0" encoding="none"/>'

	LOCAL aPOsitions                           AS ARRAY

aPositions := SELF:__GetPositionsBetween( cString, cFind, cStartMarker, cEndMarker, lCaseSensitive )
if( aPositions[1] > 0  .and. aPositions[2] > 0 )
	cString := Left(cString, aPositions[1]-1) + cReplace + Right(cString, Len(cString)-aPositions[2]+1)
endif
return( cString )

PROTECT METHOD __GetPositionsBetween( cString AS STRING, cFind AS STRING, cStartMarker AS STRING, cEndMarker AS STRING, lCaseSensitive := FALSE AS LOGIC ) AS Array PASCAL CLASS P_XmlHeader

	LOCAl aPositions AS ARRAY
	LOCAL nStartFind, nStartMarker, nEndMarker AS INT
	LOCAL oBase         AS P_Base

oBase  := P_Base{}
aPositions := {0,0}
if( !lCaseSensitive )
	cString      := Upper(cString)
	cFind        := Upper(cFind)
	cStartMarker := Upper(cStartMarker)
	cEndMarker   := Upper(cEndMarker)
endif

if( (nStartFind := oBase:StringFind( cString, cFind )) > 0 )
	nStartMarker := oBase:StringFind( cString, cStartMarker, nStartFind+Len(cFind))
	nEndMarker   := oBase:StringFind( cString, cEndMarker, nStartMarker+Len(cStartMarker))
	aPositions   := {nStartMarker+1, nEndMarker}
endif
oBase:Release()
return( aPositions )

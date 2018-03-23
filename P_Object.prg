CLASS P_OBJECT INHERIT AObject

    EXPORT         lMeldungAppendHistory AS LOGIC  
    DECLARE METHOD Meldung 
    DECLARE METHOD IsMeldung 
    DECLARE METHOD MeldungToProtokoll
    DECLARE METHOD MeldungToStatusStack
    DECLARE METHOD MeldungToString   
    DECLARE METHOD RecordFromTable
    DECLARE METHOD RecordToTable
    DECLARE METHOD ArrayToString
	DECLARE METHOD SQLRead
	DECLARE METHOD SQLBatch
    
	DECLARE ACCESS oTransactionManager
	DECLARE ASSIGN oTransactionManager      
	
	DECLARE METHOD __GetSeverityStatusFromProtArt
	DECLARE METHOD __GenerateSQLStatement
	
 	PROTECT        __aError              AS ARRAY 
 	PROTECT        __oTransactionManager AS ServerManager 

METHOD Init( oTransactionManagerSet ) CLASS P_Object

	SUPER:Init()
	                                           
	__aError := {}
	__oTransactionManager := NULL_OBJECT
		
	SELF:lMeldungAppendHistory := TRUE
	                                          
	if( !IsNil( oTransactionManagerSet ) ) 
		SELF:oTransactionManager := oTransactionManagerSet 
	endif		

METHOD Destroy() AS VOID PASCAL CLASS P_Object
    
    if( __oTransactionManager != NULL_OBJECT )
		SELF:__oTransactionManager:Release()
    endif
	SUPER:Destroy()

ACCESS oTransactionManager AS Servermanager PASCAL CLASS P_Object
if( __oTransactionManager == NULL_OBJECT )
	__oTransactionManager := ServerManager{}
endif
return( SELF:__oTransactionManager ) 

ASSIGN oTransactionManager( oTransactionsManagerSet AS ServerManager ) AS ServerManager PASCAL Class P_Object

if( __oTransactionManager != NULL_OBJECT )
	SELF:Meldung( PROT_ART_ERROR, "Achtung, es wird ein Transaktinosmanager zugewiesen, obwohl bereits einer Instanziiert wurde. Evtl wird auf dem falschen Transaktionsmanager gearbeitet." )
else
	__oTransactionManager := oTransactionsManagerSet
	__oTransactionManager:AddRef()
endif
return( __oTransactionManager )  


METHOD Meldung( cArt AS STRING, cText AS STRING, cHeader := "" AS STRING ) AS VOID PASCAL CLASS P_Object

debugPrint( "FIN: ", __ENT, __LINE__, cArt+":"+ iif( !Empty( cHeader ), cHeader + ":" + cText, cText ) )

if( lMeldungAppendHistory )
	cText += CRLF + CRLF + CallHistory() 
endif

if( Empty(cHeader) ) 
	cHeader := cText
endif
aadd( __aError, { cArt, cHeader, cText, SELF:__GetSeverityStatusFromProtArt(  cArt )  } )

     
METHOD isMeldung( cArt := "E" as STRING ) AS LOGIC PASCAL CLASS P_Object
return( aScan( __aError, { |a| a[1] $ cArt } ) != 0 ) 

METHOD MeldungToProtokoll ( oProtokoll AS AMSProtocol ) AS VOID PASCAL CLASS P_Object
AeVal( __aError, { |a| oProtokoll:Insert( a[3],a[2],a[1] ) } )
                                                             
METHOD MeldungToStatusStack( oStatusStack AS AStatusStack ) AS VOID PASCAL CLASS P_OBject
aEVal( __aError, { |a| AddStatusRecord( oStatusStack, SUBSYSTEM_AMS,, a[4],, a[2] ) } )  

METHOD MeldungToString( cArt := "EW" AS STRING ) AS STRING PASCAL CLASS P_Object
	LOCAL cString := ""         AS STRING
aeVal( __aError, { |a| cString += iif( a[1] $ cArt, a[2] + CRLF, "" ) } )

PROTECT METHOD __GetSeverityStatusFromProtArt( cArt AS STRING ) AS DWORD PASCAL CLASS P_OBJECT
do case
case( cArt == PROT_ART_ERROR )
	return( SEVERITY_ERROR )
case( cArt == PROT_ART_WARNING )
	return( SEVERITY_WARNING )
endcase                                                             
return( SEVERITY_INFORMATION )  

METHOD RecordFromTable( symTable AS SYMBOL, aKeyFields AS ARRAY, aFields AS ARRAY, lErrorIfNotFound := TRUE AS LOGIC ) AS AWriteRecord PASCAL CLASS P_OBject

	LOCAL oRecord           AS AWriteRecord
	LOCAL oReadRecord       AS AReadRecord
	LOCAL oServer           AS AServer
	LOCAL DIM psCond[9]     AS PTR
	LOCAL x                 AS INT
	
oRecord := NULL_OBJECT
oServer := SELF:__oTransactionManager:Open( symTable )
if( !oServer:Used )
	SELF:Meldung( PROT_ART_ERROR, "Fehler, die Tabelle " + Symbol2String( symTable ) + " konnte nicht geöffnet werden." )
else
	for x:=1 upto aLen( aKeyFields )
		if( UsualType( aKeyFields[x] ) != ARRAY )
			SELF:Meldung( PROT_ART_ERROR, "Fehler beim zuweisen der Keyfelder (Dimension: "+NTrim(x)+") zur Ermittlung des Records aus Tabelle " + Symbol2String( symTable ))
		else
			psCond[x] := BuildCondition( aKeyFields[x][1], SQL_EQUAL, aKeyFields[x][2] )
		endif
	next x
	
	oServer:ConditionSet( aLen( aKeyFields), @psCond )
	if( oServer:SQLSelect() )                                                                                                             
		if( oServer:SQLFetch() ) 
			oRecord := AWriteRecord{ symTable, TRUE }
			oReadRecord := AReadRecord{ oServer, TRUE }
			oReadRecord:ExportToWriteRecord( oRecord ) 
			oReadRecord:Release() 
		elseif( lErrorIfNotFound )
			SELF:Meldung( PROT_ART_ERROR, "Fehler bei ermitteln der Daten (" + SELF:ArrayToString( aKeyFields, { |a| a[1] + "=" + a[2] + ", " }) + ") aus Tabelle "+Symbol2String( symTable ))
		endif
	else 
		SELF:Meldung( PROT_ART_ERROR, "Fehler beim SQLSelect in Tabelle "+Symbol2String( symTable ) + ": " + oServer:Status:GetMessage() )
	endif
endif
oServer:Release()
return( oRecord )
	             
METHOD RecordToTable( symTable AS SYMBOL, aKeyFields AS ARRAY, oRecord AS AWriteRecord ) AS LOGIC PASCAL CLASS P_Object

	LOCAL oWriteRecord      AS AWriteRecord
	LOCAL oServer           AS AServer
	LOCAL DIM psCond[9]     AS PTR
	LOCAL x                 AS INT
	LOCAl lOK := TRUE       AS LOGIC
	
oServer := SELF:__oTransactionManager:Open( symTable )
if( !oServer:Used )
	SELF:Meldung( PROT_ART_ERROR, "Fehler, die Tabelle " + Symbol2String( symTable ) + " konnte nicht geöffnet werden." )
else
	for x:=1 upto aLen( aKeyFields )
		if( UsualType( aKeyFields[x] ) != ARRAY )
			SELF:Meldung( PROT_ART_ERROR, "Fehler beim zuweisen der Keyfelder (Dimension: "+NTrim(x)+") zur Ermittlung des Records aus Tabelle " + Symbol2String( symTable ))
		else
			psCond[x] := BuildCondition( aKeyFields[x][1], SQL_EQUAL, aKeyFields[x][2] )
		endif
	next x
	
	oServer:ConditionSet( aLen( aKeyFields), @psCond )
	if( oServer:SQLSelect() )                                                                                                             
		if( oServer:SQLFetch() ) 
			if( oServer:BeginUpdate() )
				for x:=1 upto oRecord:FCount
					if( !oServer:StructInfo:FieldInfo(nField):IsPrimaryKey )
						oServer:FPutPos(
					endif

				next x	

				if( !oServer:EndUPdate
			else
			endif
			oWriteRecord := AWriteRecord{ oServer }
			oReadRecord := AReadRecord{ oServer, TRUE }
			oReadRecord:ExportToWriteRecord( oRecord ) 
			oReadRecord:Release() 
		elseif( lErrorIfNotFound )
			if( oServer:BeginAppend() )
				
				
				
				if( !oServer:EndAppend() )
				endif
			else
				
			endif
			SELF:Meldung( PROT_ART_ERROR, "Fehler bei ermitteln der Daten (" + SELF:ArrayToString( aKeyFields, { |a| a[1] + "=" + a[2] + ", " }) + ") aus Tabelle "+Symbol2String( symTable ))
		endif
	else 
		SELF:Meldung( PROT_ART_ERROR, "Fehler beim SQLSelect in Tabelle "+Symbol2String( symTable ) + ": " + oServer:Status:GetMessage() )
	endif
endif
oServer:Release()
return( oRecord )



METHOD ArrayToString( aArray AS ARRAY, cbCodeBlock := nil AS USUAL ) AS STRING PASCAL CLASS P_Object

	LOCAL cString := ""        AS STRING
	LOCAl x                    AS INT                      
                      
if( isNil( cbCodeBlock ) )
	cbCodeBlock := { |a| a + "," }
endif
                        
for x:=1 upto aLen(aArray)                  
	cString += eVal( aArray, cbCodeBlock )
next x
return( cString  )
 
METHOD SQLRead( cStatement AS STRING, aParamPut := nil AS USUAL ) AS ASQLStatement PASCAL CLASS P_OBject
return( SELF:__GenerateSQLStatement( cStatement, TRUE, aParamPut ) )

METHOD SQLBatch( cStatement AS STRING, aParamPut := nil AS USUAL ) AS LOGIC PASCAL CLASS P_OBJECT
	LOCAL oStmt        AS ASqlStatement

oStmt := SELF:__GenerateSQLStatement( cStatement, FALSE , aParamPut )
if( oStmt != NULL_OBJECT )
	oStmt:Release()
	return( TRUE )
endif
return( false )
	
PROTECT METHOD __GenerateSQLStatement( cStatement AS STRING, lReader := TRUE AS LOGIC, aParamPut := nil AS USUAL ) AS ASQLStatement PASCAL CLASS P_OBJECT

	LOCAL oStmt            AS ASqlStatement
	LOCAL x                AS INT
	
oStmt := SELF:oTransactionManager:CreateStmt( cStatement )     
if( !oStmt:Prepare() )
	SELF:Meldung( PROT_ART_ERROR, "Fehler beim PrepareStatement() : " + oStmt:Status:GetMessage() )
	oStmt:Release()
	return( NULL_OBJECT )
endif           
if( !IsNil( aParamPut ) )
	for x := 1 upto aLen( aParamPut )
		oStmt:ParamPut( x, aParamPut[x] )
	next x
endif
if( ( lReader .and. !oStmt:ExecuteReader() ) .or. ( !lReader .and. !oStmt:ExecDirectBatch() ) )
	SELF:Meldung( PROT_ART_ERROR, "Fehler beim ausführen des Statements mit den Parametern ( " + SELF:ArrayToString( aParamPut ) + ") : " + oStmt:Status:GetMessage() )
	oStmt:Release() 
	return( NULL_OBJECT )
endif

return( oStmt )



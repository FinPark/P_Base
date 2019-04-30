/*
	P_BAseMatrix - André Finken 2019 
	Aufbau einer Matrix-Tabelle

	Beispiel einer Matrix:
	-----------------------------
	{ #HEAD1,   #HEAD2,  #HEAD3
   	{ #ENTRY10,  "ENTRY11", 12 ...
   	{ #ENTRY20,  "ENTRY21", 22 ...
   	{ #ENTRY30,  "ENTRY31", 32 ...

	Get( #HEAD2, "ENTRY21", #HEAD3 ) --> 22 
*/

CLASS P_BaseMatrix INHERIT AObject

    EXPORT aHeader                  AS ARRAY
    EXPORT aMatrix                  AS ARRAY 
    EXPORT oBase                    AS P_Base

	DECLARE METHOD Pos
	DECLARE METHOD Exists
	DECLARE METHOD Add
	DECLARE METHOD Get
	DECLARE METHOD ToSqlTable
	DECLARE METHOD Documentation
	DECLARE METHOD Serialize
	DECLARE METHOD DeSerialize

	/* HIDDEN */      
	DECLARE METHOD __GetPos

METHOD Init( aMatrix, aHeader) CLASS P_BaseMatrix

	SUPER:Init()
	SELF:oBase   := P_Base{}
	SELF:aMatrix := {}
	SELF:aHeader := {}

if( !IsNil(aMatrix) .and. UsualType(aMatrix) == ARRAY )
	SELF:aMatrix := aMatrix	 
endif 

if( !IsNil(aHeader) .and. UsualType(aHeader) == ARRAY )
	SELF:aHeader := aHeader
endif
	
METHOD Destroy() AS VOID PASCAL CLASS P_BaseMatrix

	SUPER:Destroy()	

METHOD __GetPos( uPos AS USUAL ) AS INT PASCAL CLASS P_BaseMatrix
return( iif( IsSymbol(uPos), AScan( SELF:aHeader, { |symEntry| symEntry==uPos } ), uPos ) )

METHOD Pos( uValue AS USUAL, uPos AS USUAL ) AS INT PASCAL CLASS P_BaseMatrix

	LOCAL nPos           AS INT

nPos := SELF:__GetPos( uPos )
return( AScan( SELF:aMatrix, { |a| aLen(a) >= nPos .and. UsualType(a[nPos]) == UsualType(uValue) .and. a[nPos] == uValue } ) )

METHOD Exists( uValue AS USUAL, uPos AS USUAL ) AS LOGIC PASCAL CLASS P_BaseMatrix
return( SELF:Pos( uValue, uPos ) != 0 )

METHOD Add( aRow AS ARRAY ) AS VOID PASCAL CLASS P_BaseMatrix
Aadd( SELF:aMatrix, aRow )

METHOD Get( uInputColumn AS USUAL, uValue AS USUAL, uOutputColumn AS USUAL ) AS USUAL PASCAL CLASS P_BaseMatrix
/* 
	<uHeaderOrPos>       : Die Position in der Überschrift, oder das Symbol aus der Überschrift. Beschreibt in welcher Spalte
	                       mit uValue nach einem Wert gesucht werden soll.
	<uValue>             : Der Wert, der in der Spalte <uHeaderOrPos> gesucht werden soll
	<uOutputColumn> : Die Spalten-Pos oder das Header-Symbol der Spalte, aus der der Rückgabewert erfolgen soll.


	Beispiel einer Matrix:
	-----------------------------
	{ #HEAD1,   #HEAD2,  #HEAD3
   	{ #ENTRY10,  "ENTRY11", 12 ...
   	{ #ENTRY20,  "ENTRY21", 22 ...
   	{ #ENTRY30,  "ENTRY31", 32 ...

	Get( #HEAD2, "ENTRY21", #HEAD3 ) --> 22 


*/ 

	LOCAl nEntryPos       AS INT
	LOCAL nPos            AS INT 
	LOCAL nResultPos      AS INT

nEntryPos := SELF:__GetPos( uInputColumn )
nPos := AScan( SELF:aMatrix, { |a| a[nEntryPos] == uValue } ) 
nResultPos := SELF:__GetPos( uOutputColumn )
return( SELF:aMatrix[nPos][nResultPos] ) 

METHOD ToSqlTable( cTableName AS STRING ) AS VOID PASCAL CLASS P_BaseMatrix

	LOCAl aStringHeader               AS ARRAY 
	LOCAL x                           AS INT
	
/* Umwandlung der #Symbol-Header in String-Header für die Überschriften der Tabelle */
aStringHeader := {}
for x:=1 upto aLen( SELF:aHeader )
	aadd( aStringHeader, Symbol2String(SELF:aHeader[x]) )
next x
oBase:ArrayToSql( cTableName, SELF:aMatrix, aStringHeader )

METHOD Documentation() AS STRING PASCAL CLASS P_BaseMatrix  

	LOCAL cString  := ""                  AS STRING
	LOCAL cHEader                         AS STRING
	LOCAl x,y                             AS INT
	LOCAL aColumnWith                     AS Array
                
/* HEader */                
aColumnWith := SELF:oBase:ArrayMaxColumnWith( SELF:aMatrix )	
for x:=1 upto aLen( SELF:aHeader )
	cHeader := SELF:oBase:StringAlign( if( aLen(SELF:aHeader) >= x, SELF:aHeader[x], "["+NTrim(x)+"]" ), aColumnWith[x], sa_left )
	cString += cHeader + "| "	
next x
                          
cString += CRLF + SELF:oBase:StringRepeat( Len(cString), "-" ) + CRLF

/* Content */
for x:=1 upto aLen( SELF:aMatrix )
	for y:=1 upto aLen( SELF:aMatrix[x] )
		cString + SELF:oBase:StringAlign( AsString( SELF:aMatrix[x][y] ), sa_left ) + " | "
	next y                                                              
	cString += CRLF
next x                          
return( cString )                          

METHOD Serialize() AS STRING PASCAL CLASS P_BaseMatrix

	LOCAL aXmlMatrix              AS ARRAY
	
aXmlMatrix := AClone( SELF:aMatrix )
aSize( aXmlMatrix, aLen(aXmlMatrix)+1 )
aIns( aXmlMatrix, 1 )
aXmlMatrix[1] := AClone( SELF:aHeader )
return( oBase:ArraySerialize( aXmlMatrix ) )

METHOD DeSerialize( cXml AS STRING, lHeaderIncluded := TRUE AS LOGIC ) AS VOID PASCAL CLASS P_BaseMatrix

	LOCAl aXmlMatrix          AS ARRAY
	LOCAl x                   AS INT

aXmlMatrix := oBase:ArrayDeserialize( cXml )

if( !lHeaderIncluded )
	SELF:aMatrix := AClone( aXmlMatrix )	
else
	SELF:aMatrix := {}
	SELF:aHeader := {}
	if( aLen( aXmlMatrix ) >= 1 )
		SELF:aHeader := aXmlMatrix[1]
	endif                
	
	if( aLen( aXmlMatrix ) >= 2 ) 
		for x:=2 upto aLen( aXmlMatrix )
			aadd( aMatrix, aXmlMatrix[2] )
		next x
	endif
endif

	

	

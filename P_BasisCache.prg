/*

  	P_BasisCache

  	Ein Object mit AStatusStack und eigenem Transaktionsmanager
  	zur Benutzung oder Vererbung


*/

CLASS P_BasisCache INHERIT P_BasisObject

	DECLARE ACCESS  aCache
	DECLARE METHOD  Put
	DECLARE METHOD  PutIfNotExists
	DECLARE METHOD  Del
	DECLARE METHOD  Get
	DECLARE METHOD  Pos
	DECLARE METHOD  Increment
	DECLARE METHOD  Exists
	DECLARE METHOD  FillFromTable
	DECLARE METHOD  FillFromArray

	PROTECT         __aCache               AS ARRAY

METHOD Init( oTransactionManager, oStatusStack ) CLASS P_BasisCache

	SUPER:Init( oTransactionManager, oStatusStack )

	//
	// Aufbau
	// { { symKat, uKey, uValue }, { symKat, uKey, uValue }, ... }
	//
	SELF:__aCache := {}


METHOD Destroy() AS VOID PASCAL CLASS P_BasisCache

	SUPER:Destroy()

ACCESS aCache AS ARRAY PASCAL CLASS P_BasisCache
return( SELF:__aCache )

METHOD Put( symKat AS SYMBOL, uKey AS USUAL, uValue AS USUAL ) AS VOID PASCAL CLASS P_BasisCache

	LOCAL nPos              AS INT

nPos := SELF:Pos( symKat, uKey )
if( nPos != 0 )
	SELF:__aCache[nPos][3] := uValue
else
	AAdd( SELF:__aCache, { symKat, uKey, uValue } )
endif

METHOD PutIfNotExists( symKat AS SYMBOL, uKey AS USUAL, uValue AS USUAL ) AS VOID CLASS P_BasisCache

if( !SELF:Exists( symKat, uKey ) )
	SELF:Put( symKat, uKey, uValue )
endif

METHOD Del( symKat := nil AS USUAL, uKey := nil AS USUAL ) AS VOID PASCAL CLASS P_BasisCache

	LOCAL nPos            AS INT

do case
case( IsNil( symKat ) )
	SELF:__aCache := {}

otherwise
	do while( nPos := SELF:Pos( symKat, uKey ) ) != 0
		ADelShrink( SELF:__aCache, nPos )
	enddo

endcase


METHOD Get( symKat AS SYMBOL, uKey AS USUAL ) AS USUAL PASCAL CLASS P_BasisCache

	LOCAL uValue := nil       AS USUAL
	LOCAl nPos                AS INT

nPos := SELF:Pos( symKat, uKey )
if( nPos != 0 )
	uValue := SELF:__aCache[nPos][3]
else
	SELF:Message( "Fehler in P_BasisCache:Get( %1, %2 ) : Eintrag nicht gefunden.", { symKat, uKey }, SEVERITY_WARNING )
endif
return( uValue )

METHOD Pos( symKat AS SYMBOL, uKey := nil AS USUAL ) AS INT PASCAL CLASS P_BasisCache
return(  AScan(  SELF:__aCache, { |a| a[1] == symKat .and. ( IsNil(uKey) .or. ( UsualType(a[2]) == UsualType(uKey) .and. a[2] == uKey ) ) } ) )

METHOD Exists( symKat := nil AS USUAL, uKey := nil AS USUAL ) AS LOGIC PASCAL CLASS P_BasisCache
return( iif( IsNil(symKat), ALen( SELF:__aCache ) != 0 , SELF:Pos( symKat, uKey ) != 0 ) )

METHOD Increment( symKat AS SYMBOL, uKey AS USUAL, nIncrement := 1 AS REAL8 ) AS VOID PASCAL CLASS P_BasisCache
if( SELF:Exists( symKat, uKey ) )
	SELF:Put( symKat, uKey, SELF:Get( symKat, uKey ) + nIncrement )
else
	SELF:Put( symKat, uKey, nIncrement )
endif

METHOD FillFromArray( symKat AS SYMBOL, aArray AS ARRAY, nKeyFieldNr := 1 AS INT, nValueFieldNr := 2 AS INT ) AS LOGIC PASCAL CLASS P_BasisCache

	LOCAL x                  AS INT
	LOCAL lOK := TRUE        AS LOGIC
	LOCAL oError             AS USUAL

BEGIN SEQUENCE
	for x:=1 upto ALen( aArray )
		SELF:Put( symKat, aArray[x][nKeyFieldNr], aArray[x][nValueFieldNr] )
	next x

RECOVER USING oError
	lOK := FALSE
END SEQUENCE
return( lOk )

METHOD FillFromTable( symKat AS SYMBOL, cSelectClause AS STRING ) AS LOGIC PASCAL CLASS P_BasisCache

	LOCAL lOK := TRUE           AS LOGIC

return( lOK )
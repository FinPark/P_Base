/*

	Caching
	Autor: André Finken

   Beispiel:

		LOCAL oCache           AS P_BaseCache

	oCache := P_BaseCache{}
	oCache:Put( #Haupt, #Finken, 10 )
	if( oCache:Exists( #Haupt, #Finken ) )
		uTemp := oCache:Get( #Haupt, #Finken )
		aTemp := oCache:ArrayExport( #Haupt )
	endif
	oCache:Release()


*/

CLASS P_BaseCache     INHERIT AObject

	EXPORT aCache               AS ARRAY
   EXPORT symTimeKategorie     AS SYMBOL
	EXPORT symRecordKategorie   AS SYMBOL

	DECLARE METHOD Add
	DECLARE METHOD Del
	DECLARE METHOD Get
	DECLARE METHOD Put
	DECLARE METHOD Increment
	DECLARE METHOD Exists
	DECLARE METHOD Pos
	DECLARE ACCESS oBase
	DECLARE METHOD Debug
	DECLARE METHOD Manipulate
	DECLARE METHOD Builder

	// Statistic
	DECLARE METHOD TimeStatisticAsString
	DECLARE METHOD TimeStart
	DECLARE METHOD TimeEnde
	DECLARE METHOD RecordCounterIncrement
	DECLARE METHOD RecordCounterStatisticAsString

	// Import/Export
	DECLARE METHOD ArrayImport
	DECLARE METHOD RecordImport
	DECLARE METHOD StatementImport
	DECLARE METHOD TableImport
	DECLARE METHOD ArrayExport
	DECLARE METHOD GetCachedParam
	DECLARE METHOD GetStringParam

	PROTECT        __oBase        AS P_Base


METHOD Init( oP_Base ) CLASS P_BaseCache

	SUPER:Init()

	if( !IsNil( oP_Base ) .and. IsInstanceOfUsual(oP_Base, #P_Base))
		SELF:__oBase := oP_Base
		SELF:__oBase:AddRef()
	else
		SELF:__oBase := P_Base{}
	endif


	SELF:aCache := {}

	SELF:symTimeKategorie   := #RunTimeStatistic
	SELF:symRecordKategorie := #RecordCounterStatistic


METHOD Destroy() AS VOID PASCAL CLASS P_BaseCache

   SELF:oBase:Release()
	SUPER:Destroy()

METHOD Add( symKat AS SYMBOL, uKey AS USUAL, uValue AS USUAL ) AS VOID PASCAL CLASS P_BaseCache
// Add( #Haupt, "Finken", 10 )
SELF:Put( symKat, uKey, uValue )

METHOD Exists( symKat := nil AS USUAL, uKey := nil AS USUAL, cbCodeblock := nil AS USUAL ) AS LOGIC PASCAL CLASS P_BaseCache
// <symKat>
// [uKey]

	LOCAL lFound := FALSE          AS LOGIC

if( IsNil( symKat ) )
	lFound := ALen( SELF:aCache ) != 0
else
	lFound := SELF:Pos( symKat, uKey ) > 0
	if( !lFound .and. !IsNil( cbCodeBlock ) )
		SELF:Builder( symKat, uKey, cbCodeBlock )
		lFound := SELF:Exists( symKat, uKey )
	endif
endif
return( lFound )

METHOD Pos( symKat AS SYMBOL, uKey := nil AS USUAL) AS INT PASCAL CLASS P_BaseCache
return(  AScan(  SELF:aCache, { |a| a[1] == symKat .and. ( IsNil(uKey) .or. ( UsualType(a[2]) == UsualType(uKey) .and. a[2] == uKey ) ) } ) )

METHOD Put( symKat AS SYMBOL, uKey AS USUAL, uValue AS USUAL ) AS VOID PASCAL CLASS P_BaseCache

	LOCAL nPos          AS INT

nPos := SELF:Pos( symKat, uKey )
if( nPos != 0 )
	SELF:aCache[nPos][3] := uValue
else
	AAdd( SELF:aCache, { symKat, uKey, uValue } )
endif

METHOD Increment( symKat AS SYMBOL, uKey AS USUAL, nIncrement AS REAL8 ) AS VOID PASCAL CLASS P_BaseCache

	LOCAL uValue            AS USUAL

uValue := SELF:Get( symKat, uKey )
if( !IsNil( uValue ) )
	if( IsNumeric( uValue ) )
		SELF:Put( symKat, uKey, uValue + nIncrement )
	else
		SELF:oBase:MessageFormat( "Fehler bei Increment( #,#,# ). Der Wert von # ist nicht Numerisch, sondern vom Typ: #", { symKat, uKey, nIncrement, uKey, SELF:oBase:UsualTypeAsString( uValue ) }, PROT_ART_ERROR, TRUE )
	endif
else
	SELF:Put( symKat, uKey, nIncrement )
endif

METHOD Get( symKat AS SYMBOL, uKey AS USUAL, uDefaultIfNotFound := nil AS USUAL, cbCodeblock := nil AS USUAL ) AS USUAL PASCAL CLASS P_BaseCache

	LOCAL uReturn          AS USUAL
	LOCAL nPos             AS INT

uReturn := uDefaultIfNotFound
nPos    := SELF:Pos( symKat, uKey )
if( nPos != 0 )
	uReturn := SELF:aCache[nPos][3]
else
	if( !IsNil( cbCodeBlock ) )
		SELF:Builder( symKat, uKey, cbCodeBlock )
		uReturn := SELF:Get( symKat, uKey )
	endif
endif
return( uReturn )

METHOD Del( uKat := nil AS USUAL, uKey := nil AS USUAL ) AS VOID PASCAL CLASS P_BaseCache

	LOCAL nPos             AS INT

if( IsNil(uKat) )
	SELF:aCache := {}
else
	do while( nPos := SELF:Pos( uKat, uKey ) )	!= 0
		ADelShrink( SELF:aCache, nPos )
	enddo
endif

METHOD ArrayImport( symKat AS SYMBOL, aArray AS ARRAY, nKeyPos := 1 AS INT, nValuePos := 2 AS INT ) AS VOID PASCAL CLASS P_BaseCache

	LOCAL x         AS INT

for x:=1 upto ALen(aArray)
	if( ALen(aArray[x])	>= nKeyPos .and. ALen(aArray[x]) >= nValuePos )
		SELF:Add( symKat, aArray[x][nKeyPos], aArray[x][nValuePos] )
	else
		SELF:oBase:MessageFormat( "Fehler in Cache bei InsertArray( # ) Für Key = # und Value = #. In Zeile # stehen nur # Spalten im Array zur Verfügung", { symKat, nKeyPos, nValuePos, x, ALen(aArray[x]) }, PROT_ART_ERROR, TRUE )
	endif
next x

METHOD RecordImport( symKat AS SYMBOL, oRecord AS AOBJECT ) AS VOID PASCAL CLASS P_BaseCache
SELF:ArrayImport(symKat, SELF:oBase:ArrayFromRecord( oRecord ) )

METHOD StatementImport( symKat AS SYMBOL, oStatement AS ASqlStatement, nKeyPos := 1 AS INT, nValuePos := 2 AS INT  ) AS VOID PASCAL CLASS P_BaseCache

do while( oStatement:Fetch() )
	SELF:Put( symKat, oStatement:FGet(nKeyPos), oStatement:FGet(nValuePos) )
enddo

METHOD TableImport( symKat AS SYMBOL, cTableName AS STRING, symField1 AS SYMBOL, symField2 AS SYMBOL, cWhere := "" AS STRING ) AS VOID PASCAL CLASS P_BaseCache

	LOCAL oData            AS ASqlStatement

oData := SELF:oBase:CreateSQLStatement( SELF:oBase:StringFormat("SELECT #,# FROM # WHERE #", { symField1, symField2, cTableName, iif( Empty(cWhere), "1=1", cWhere) } ), "Fehler beim ermitteln der Cache-Daten aus "+cTableName, TRUE )
if( oData != NULL_OBJECT )
	SELF:StatementImport( symKat, oData, 1, 2 )
	oData:Release()
endif

ACCESS oBase AS P_Base PASCAL CLASS P_BaseCache
return( SELF:__oBase )

METHOD GetStringParam( cBereich AS STRING, cParam AS STRING, cDefault AS STRING ) AS STRING PASCAL CLASS P_BaseCache
return( SELF:GetCachedParam( #string, cBereich, cParam, cDefault ) )

METHOD GetCachedParam( symParamType AS SYMBOL, cBereich AS STRING, cParam AS STRING, uDefault := nil AS USUAL ) AS USUAL PASCAL CLASS P_BaseCache

	LOCAL uReturnValue          AS USUAL

uReturnValue := SELF:Get( #ConfigParameter, cBereich+cParam, uDefault )
if( IsNil( uReturnValue ) )
	do case
	case( symParamType == #string )
		uReturnValue := oConfigManager:GetStringParam(cBereich, cParam, IfNil( uDefault, "" ))
	case( symParamType == #logic )
		uReturnValue := oConfigManager:GetLogicParam(cBereich, cParam, IfNil( uDefault, false ))
	case( symParamType == #int )
		uReturnValue := oConfigManager:GetIntParam(cBereich, cParam, IfNil( uDefault, 0 ))
	case( symParamType == #float )
		uReturnValue := oConfigManager:GetFloatParam(cBereich, cParam, IfNil( uDefault, 0.0 ))
	case( symParamType == #array )
		uReturnValue := oConfigManager:GetArrayParam(cBereich, cParam, IfNil( uDefault, {} ))
	endcase
	SELF:Put( #ConfigParameter, cBereich+cParam, uReturnValue )
endif
return( uReturnValue )

METHOD ArrayExport( uKat := nil AS USUAL  ) AS ARRAY PASCAL CLASS P_BaseCache
// Wird [uKat] übergeben, so wird ein zweidimensionales Array {{uVarName, uValue},...} zurückgegeben.
// Ohne [uKat] wird das komplette Cache-Array zurückgegeben, dreidimensional {{ uKat, uVarName, uValue },...}
	LOCAL aArray                AS ARRAY
	LOCAL x                     AS INT

aArray := {}
if( IsNil( uKat ) )
	aArray := AClone( SELF:aCache )
else
	for x:=1 upto ALen( SELF:aCache )
		if( UsualType( SELF:aCache[x][1] ) == UsualType( uKat ) .and. SELF:aCache[x][1] == uKat  )
			AAdd( aArray, { SELF:aCache[x][1], SELF:aCache[x][2] } )
		endif
	next x
endif
return( aArray )

METHOD debug( uKat := nil AS USUAL, cHeader := "" AS STRING ) AS VOID PASCAL CLASS P_BaseCache
if( IsNil( uKat ) )
	debugPrintArray( SELF:aCache , cHeader )
else
	debugPrintArray( SELF:ArrayExport( uKat ), cHeader )
endif

METHOD TimeStart( uKey AS USUAL, cDescription := "" AS STRING ) AS VOID PASCAL CLASS P_BaseCache
if( Empty( cDescription ) )
	cDescription := SELF:oBase:UsualToString( uKey )
endif
SELF:Put( symTimeKategorie, uKey, { cDescription, SELF:oBase:dwRunTime } )

METHOD TimeEnde( uKey AS USUAL ) AS REAL8 PASCAL CLASS P_BaseCache

	LOCAL aArray          AS ARRAY

aArray := SELF:Get( symTimeKategorie, uKey )
aArray[2] := (SELF:oBase:dwRunTime- aArray[2])
SELF:Put( symTimeKategorie, uKey, aArray )
return( aArray[2] )

METHOD TimeStatisticAsString( cHeader := "" AS STRING ) AS STRING PASCAL CLASS P_BaseCache

	LOCAL cString := ""     AS STRING
	LOCAL aArray, aSub      AS ARRAY
	LOCAl x                 AS INT

if( !Empty( cHeader ) )
	cString += cHeader + CRLF
endif
aArray := SELF:ArrayExport( symTimeKategorie )
for x:=1 upto ALen( aArray )
	aSub := aArray[x][3]
	cString += aSub[1] + ": " + SELF:oBase:StringDuration( aSub[2] * 60 ) +CRLF
next x
return( cString )

METHOD RecordCounterIncrement( uKey AS USUAL, cDescription := "" AS STRING, nIncrementValue := 1 AS INT ) AS VOID PASCAL CLASS P_BaseCache

	LOCAL aArray     AS ARRAY

aArray := SELF:Get(symRecordKategorie, uKey )
if( aArray != nil )
	if( Empty( cDescription ) )
		cDescription := aArray[1]
	endif
	nIncrementValue := aArray[2] + nIncrementValue
else
	if( Empty( cDescription ) )
		cDescription := SELF:oBase:UsualToString( uKey )
	endif
endif
SELF:Put( symRecordKategorie, uKey, { cDescription, nIncrementValue } )

METHOD RecordCounterStatisticAsString( cHeader := "" ) AS STRING PASCAL CLASS P_BaseCache

	LOCAL cString := ""     AS STRING
	LOCAL aArray, aSub      AS ARRAY
	LOCAl x                 AS INT

if( Empty( !cHeader ) )
	cString += cHeader + CRLF
endif
aArray := SELF:ArrayExport( symRecordKategorie )
for x:=1 upto ALen( aArray )
	aSub := aArray[x][3]
	cString += aSub[1] + ": " + NTrim(aSub[2]) + " Datensätze" +CRLF
next x
return( cString )


METHOD Manipulate( uKat := nil AS USUAL, cbCodeBlock AS CODEBLOCK ) AS VOID PASCAL CLASS P_BaseCache
// cbCodeblock := { |uKat, uKey, uValue| }

	LOCAL x             AS INT

for x:=1 upto ALen(SELF:aCache)
	if( IsNil(uKat) .or. ( UsualType(SELF:aCache[x][1]) == UsualType(uKat) .and. SELF:aCache[x][1] == uKat ) )
		Eval( cbCodeblock, @SELF:aCache[x][1],@SELF:aCache[x][2],@SELF:aCache[x][3]  )
	endif
next x

METHOD Builder( symKat AS SYMBOL, uVarName AS USUAL, cbCodeBlock AS CODEBLOCK ) AS VOID PASCAL CLASS P_BaseCache
// cbCodeBlock : { | symKat, uVar, oCache, oBase, lKatExists | uValue wird zurückgegeben }

	LOCAL uValue          AS USUAL

uValue := Eval( cbCodeBlock, symKat, uVarName, SELF, SELF:oBase, SELF:Exists( symKat ) )
if( uValue != nil .and. uVarName != nil )
	SELF:Put( symKat, uVarName, uValue )
endif
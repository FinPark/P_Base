/*
	P_BaseDictionary.prg - André Finken 2019
	
	Ein komplexes Array, Typensicher, mit folgendem Aufbau:
	
	{ { symbolKat, uKey , uValue }, ... }
	
	Hinter jedem symbolKat/uKey können noch diverse Attribute hängen. Um das ganze Schlank zu halten werden die Attribute in __oAttributes gesammelt und nur an die symbolKat/uKey
	geängt, wenn sie sich von den Initialwerten unterscheiden. 

	
*/

DEFINE pbd_Kat   := 1
DEFINE pbd_Key   := 2
DEFINE pbd_Value := 3 
DEFINE pbd_Attr  := 4
DEFINE pbd_Attr_Kat := #PrimeBaseArrayAttributes


CLASS P_BaseDictionary INHERIT AOBJECT


    EXPORT uValuelIfNotFound         AS USUAL		// NIL 
	EXPORT symValidKeyType           AS SYMBOL		// NIL = Alles 
	EXPORT aValidValueTypes          AS ARRAY		// {} = Alles
	EXPORT cbKeyConversion           AS CODEBLOCK   // { |uKey| uKey }
    EXPORT cbValueConversion         AS CODEBLOCK   // { |uValue| uValue }
    
    
	DECLARE ACCESS oBase
	DECLARE ASSIGN oBase 
	
	DECLARE METHOD Pos
	DECLARE METHOD Exists
	DECLARE METHOD Get	
	DECLARE METHOD Set	
	DECLARE METHOD Del
	DECLARE METHOD Increment	 
	DECLARE METHOD AsArray
	DECLARE METHOD ImportArray
	DECLARE METHOD ImportRecord 
	DECLARE METHOD Serialize
	DECLARE METHOD DeSerialize
	DECLARE METHOD Compare
	
	/* Attributes */
	DECLARE METHOD RegisterAttribute
	DECLARE METHOD GetAttribute
	DECLARE METHOD SetAttribute
	DECLARE METHOD IncrementAttribute
	DECLARE METHOD ExportAttributes
	DECLARE METHOD ImportAttributes
	
	/* HIDDEN */
	DECLARE METHOD __CheckField
	PROTECT __aArray            AS ARRAY
	PROTECT __oAttributes       AS P_BaseDictionary 
	PROTECT __oBase             AS P_Base      
	PROTECT __cbIsType          AS CODEBLOCK

METHOD Init( _symValidKeyType, _aValidValueTypes, oP_Base ) CLASS P_BaseDictionary

SUPER:Init()

SELF:__cbIsType := { |uField, uUsualtype| !IsNil(uField) .and. UsualType(uField) == uUsualType }
SELF:__aArray := {}
                                                
SELF:uValuelIfNotFound := nil
SELF:symValidKeyType   := nil
SELF:aValidValueTypes  := {}                                                
SELF:cbKeyConversion   := { |uKey| uKey }
SELF:cbValueConversion := { |uValue| uValue }                                                

if( EVal( SELF:__cbIsType, _symValidKeyType, SYMBOL ) )
	SELF:symValidKeyType := _symValidKeyType
endif                                    

if( EVal( SELF:__cbIsType, _aValidValueTypes, ARRAY ) )
	SELF:aValidValueTypes := _aValidValueTypes
endif 

if( !IsNil(oP_Base) .and. IsInstanceOf( oP_Base, #P_Base ) )
	SELF:oBase := oP_Base
endif

SELF:__oAttributes := P_BaseDictionary{ #SYMBOL,, SELF:oBase }
	 
	
METHOD Destroy() AS VOID PASCAL CLASS P_BaseDictionary

SELF:__oAttributes:Release()
SELF:__oBase:Release()
SUPER:Destroy()	


ASSIGN oBase( oBaseLocal AS P_Base) AS P_Base PASCAL CLASS P_BaseDictionary
if( SELF:__oBase != NULL_OBJECT )
	SELF:__oBase:Release()
endif
SELF:__oBase := oBaseLocal 
SELF:__oBase:AddRef()     
SELF:__oAttributes:oBase := SELF:__oBase
return( SELF:__oBase )

ACCESS oBase AS P_Base PASCAL CLASS P_BaseDictionary
if( SELF:__oBase == NULL_OBJECT )
	SELF:__oBase := P_Base{}
endif
return( SELF:__oBase ) 

METHOD Pos( symKat AS SYMBOL, uKey AS USUAL ) AS INT PASCAL CLASS P_BaseDictionary
return( aScan( SELF:__aArray, { |a| a[pbd_Kat] == symKat .and. UsualType(a[pbd_Key]) == UsualType(uKey) .and. a[pbd_Key] == uKey } ) )

METHOD Exists(symKat AS SYMBOL, uKey := nil AS USUAL ) AS LOGIC PASCAL CLASS P_BaseDictionary
return( iif( IsNil(uKey), aScan( SELF:__aArray, { |a| a[pbd_Kat] == symKat } ) != 0, SELF:Pos( symKat, uKey ) != 0 ) )

PROTECT METHOD __CheckField( nFieldType AS INT, uField AS USUAL ) AS USUAL PASCAL CLASS P_BaseDictionary

	LOCAL symType          AS SYMBOL
	LOCAL lOK := FALSE     AS LOGIC

symType := SELF:__oBase:oTypeTranslation:Get( #USUALTYPE, uField, #SYMBOL )

do case
case( nFieldType == pbd_Kat )
	if( symType != #SYMBOL )
		SELF:__oBase:MessageFormat( "Die übergebene Kategorie {1} ist vom Typ {2}, sollte jedoch vom Typ {3} sein. ", { uField, SELF:__oBase:UsualTypeAsString( uField ), "SYMBOL" }, "W", TRUE, TRUE ) 
		uField := AsSymbol( uField )
	endif
case( nFieldType == pbd_Key )
	if( !IsNil(SELF:symValidKeyType) .and. symType != SELF:symValidKeyType )
		SELF:__oBase:MessageFormat( "Der übergebene Key {1} ist vom Typ {2}, sollte jedoch vom Typ {3} sein. ", { uField, SELF:__oBase:UsualTypeAsString( uField ), symType }, "W", TRUE, TRUE ) 
	endif 	
	uField := eVal( SELF:cbKeyConversion, uField )
case( nFieldType == pbd_Value )
	if( aLen( SELF:aValidValueTypes ) > 0 )	
		if( aEval( SELF:aValidValueTypes, { |symValueType| symType == symValueType } ) == 0 )		
			SELF:__oBase:MessageFormat( "Der übergebene Wert (Value) {1} ist vom Typ {2}, sollte jedoch von folgenden Typen sein: {3}", { uField, SELF:__oBase:UsualTypeAsString( uField ), SELF:__oBase:ArrayToString( SELF:aValidValueTypes, "," ) }, "W", TRUE, TRUE ) 
		endif
	endif
	uField := eVal( SELF:cbValueConversion, uField )
endcase	
return( uField )
	
METHOD Set( symKat AS SYMBOL, uKey AS USUAL, uValue AS USUAL ) AS VOID PASCAL CLASS P_BaseDictionary

	LOCAL nPos            AS INT

symKat := SELF:__CheckField( pbd_Kat, symKat )
uKey   := SELF:__CheckField( pbd_Key, uKey )
uValue := SELF:__CheckField( pbd_Value, uValue )
	
nPos := SELF:Pos( symKat, uKey )
if( nPos == 0 )
	aadd( SELF:__aArray, { symKat, uKey, uValue, {} } )
else
	SELF:__aArray[nPos][pbd_Value] := uValue  
endif	

METHOD Get( symKat AS SYMBOL, uKey AS USUAL ) AS USUAL PASCAL CLASS P_BaseDictionary

	LOCAL nPos               AS INT
	LOCAL uValue             AS USUAL
	
uValue := SELF:uValuelIfNotFound	
nPos := SELF:Pos( SELF:__CheckField( pbd_Kat, symKat),SELF:__CheckField(pbd_Key, uKey ) )
if( nPos != 0 )
	uValue := SELF:__aArray[nPos][pbd_Value]
endif
return( uValue )
	
METHOD Increment( symKat AS SYMBOL, uKey AS USUAL, nIncrementValue := 1 AS REAL8 ) AS REAL8 PASCAL CLASS P_BaseDictionary

	LOCAL uValue           AS USUAL	

uValue := SELF:Get( symKat, uKey )
if( IsNumeric( uValue ) )
	uValue += nIncrementValue
	SELF:Set( symKat, uKey, uValue )
else
	SELF:__oBase:MessageFormat( "Increment auf einem nicht numerischen Wert (Typ={3}). Kat = {1}, Key = {2}", { symKat, uKey, SELF:__oBase:UsualTypeAsString( uValue ) }, "W", TRUE, TRUE ) 
	uValue := -1
endif
return( uValue )

METHOD RegisterAttribute( symAttribute AS SYMBOL, uInitialValue AS USUAL ) AS VOID PASCAL CLASS P_BaseDictionary

if( SELF:__oAttributes:Exists( pbd_Attr_Kat, symAttribute ) )
	SELF:__oBase:MessageFormat( "Das Attribute {1} wurde doppelt registriert. Die zweite Registrierung wurde ignoriert, jedoch wurde der Initialwert von {2} auf {3} gesetzt.", { symAttribute, SELF:__oAttributes:Get(pbd_Attr_Kat, symAttribute), uInitialValue  }, "W", TRUE, TRUE ) 
endif
SELF:__oAttributes:Set( pbd_Attr_Kat, symAttribute, uInitialValue )

METHOD GetAttribute( symKat AS SYMBOL, uKey AS USUAL, symAttribute AS SYMBOL ) AS USUAL PASCAL CLASS P_BaseDictionary

	LOCAL nPos, nAttrPos   AS INT
	LOCAL uAttrValue       AS USUAL
	LOCAL aAttr            AS Array
	    
uAttrValue := SELF:__oAttributes:uValuelIfNotFound
	
nPos := SELF:Pos( symKat, uKey )
if( nPos == 0 )	
	SELF:__oBase:MessageFormat( "Es wird versucht auf das Attribute {1} aus Kategorie = {2} und Key {3} zuzugreifen. Es wurde jedoch keine Kat/Key-Kombination gefunden. ", { symAttribute, symKat, uKey  }, "W", TRUE, TRUE ) 
else
	if( !SELF:__oAttributes:Exists( pbd_Attr_Kat, symAttribute ) )
		SELF:__oBase:MessageFormat( "Es wird versucht auf ein nicht registriertes Attribute = {1} zuzugreifen. ", { symAttribute }, "W", TRUE, TRUE ) 
	else
	   	aAttr := SELF:ExportAttributes( symKat, uKey )
	   	nAttrPos := aScan( aAttr, { |a| a[1] == symAttribute } )
	   	if( nAttrPos != 0 )
	   		uAttrValue := aAttr[nAttrPos][2] 
	   	endif
	endif
endif 
return( uAttrValue )

METHOD SetAttribute( symKat AS SYMBOL, uKey AS USUAL, symAttribute AS SYMBOL, uValue AS USUAL ) AS VOID PASCAL CLASS P_BaseDictionary
 
 	LOCAL nPos          AS INT
 
if( !SELF:__oAttributes:Exists( pbd_Attr_Kat, symAttribute ) )
	SELF:__oBase:MessageFormat( "Es wird versucht der nicht regustrierte Attribute {1} in der Kategorie = {2} und Key {3} zu setzen. Das Attribut wurde nun registriert. ", { symAttribute, symKat, uKey  }, "W", TRUE, TRUE ) 
	SELF:RegisterAttribute( symAttribute, uValue )
else
	if( uValue != SELF:__oAttributes:Get( pbd_Attr_Kat, symAttribute ) )
		/* Nur, wenn es sich von der InitialValue unterscheidet */
		nPos := SELF:Pos( symKat, uKey )
		if( nPos != 0 )
			aadd( SELF:__aArray[nPos][pbd_Attr], uValue )
		else
			SELF:__oBase:MessageFormat( "Es wird versucht auf das Attribute {1} aus Kategorie = {2} und Key {3} zuzugreifen. Es wurde jedoch keine Kat/Key-Kombination gefunden. ", { symAttribute, symKat, uKey  }, "W", TRUE, TRUE ) 
		endif
	endif	
endif

METHOD AsArray( symKat := nil AS SYMBOL, lIncludeAttributes := FALSE AS LOGIC ) AS ARRAY PASCAL CLASS P_BaseDictionary
/*
	Wird die [symKat] nicht angegeben, so wird ein Array im Format {{ kat, key, value }, ...} zurückgegeben.
	Ist [symKat] angegeben, so wird nur ein Array mit {{key,value},...}  
	
	Ist [lIncludeAttributes] erwünscht, so werden die Attribute als Array mit angehängt. Dieses Beispel geht von einem übergebenen symKat aus:
	{ {uKey, uValue, { {symAttr1, uValue}, {symAttr2, uValue},...}}, ... } 

*/ 

	LOCAL aTmp, aResult      AS Array
	LOCAl x                  AS INT	

aResult := {}
for x:=1 upto aLen( SELF:__aArray )
	if( IsNil( symKat ) )
		aTmp := { SELF:__aArray[x][pbd_Kat], SELF:__aArray[x][pbd_Key], SELF:__aArray[x][pbd_Value] }  	
	else
		aTmp := { SELF:__aArray[x][pbd_Key], SELF:__aArray[x][pbd_Value] } 
	endif 
	if( lIncludeAttributes )
		aadd( aTmp, SELF:ExportAttributes( SELF:__aArray[x][pbd_Kat], SELF:__aArray[x][pbd_Key] ) )		
	endif       
	aadd( aResult, aTmp )
next x
return( aResult )

PROTECT METHOD ExportAttributes( symKat AS SYMBOL, uKey AS USUAL ) AS ARRAY PASCAL CLASS P_BaseDictionary

	LOCAL aAttr         AS ARRAY 
	LOCAL nPos, x,y     AS INT        
	LOCAL symAttr       AS SYMBOL
	
aAttr := {}
nPos := SELF:Pos( symKat, uKey )
if( nPos == 0 )
	SELF:__oBase:MessageFormat( "Es wird versucht auf die Attribute aus Kategorie = {1} und Key {2} zuzugreifen. Es wurde jedoch keine Kat/Key-Kombination gefunden. ", { symKat, uKey  }, "W", TRUE, TRUE ) 
else
	aAttr := SELF:__oAttributes:AsArray( pbd_Attr_Kat )
	for x:=1 upto aLen( SELF:__aArray[nPos][pbd_Attr] )
		symAttr := SELF:__aArray[nPos][pbd_Attr][1] 
		y := aScan( aAttr, { |uKey| uKey == symAttr } )
		if( y != 0 )
			/* Es ist ein individuelles Attribut an Kat/Key vorhanden */
			aAttr[y][2] := SELF:__aArray[nPos][pbd_Attr][2]  
		else
			SELF:__oBase:MessageFormat( "In der Kat = {1}, Key = {2}, wurde das Attribut = {3} gefunden, welches nicht registriert ist.", { symKat, uKey, SELF:__aArray[nPos][pbd_Attr][1]  }, "W", TRUE, TRUE )			
			aadd( aAttr, { SELF:__aArray[nPos][pbd_Attr][1], SELF:__aArray[nPos][pbd_Attr][2] } ) 
		endif
	next x
endif
return( aAttr )

METHOD IncrementAttribute( symKat AS SYMBOL, uKey AS USUAL, symAttribute AS SYMBOL, nIncrementValue := 1 AS REAL8 ) AS REAL8 PASCAL CLASS P_BaseDictionary

	LOCAL uValue           AS USUAL
	
uValue := SELF:GetAttribute( symKat, uKey, symAttribute )
if( !IsNumeric( uValue ) )
	SELF:__oBase:MessageFormat( "Increment auf einem nicht numerischen Attribut = {3}. Kat = {1}, Key = {2}. Attributewert ist vo Typ {4}", { symKat, uKey, symAttribute, SELF:__oBase:UsualTypeAsString( uValue ) }, "W", TRUE, TRUE ) 
else
	uValue += nIncrementValue
	SELF:SetAttribute( symKat, uKey, symAttribute, uValue )
endif
return( uValue )

METHOD ImportArray( symKat AS SYMBOL, aArray AS ARRAY ) AS VOID PASCAL CLASS P_BaseDictionary

	LOCAL x           AS INT

for x:=1 upto aLen( aArray )
	if( UsualType( aArray[x] ) == ARRAY .and. aLen( aArray[x] ) >= 2 )
		SELF:Set( symKat, SELF:__CheckField( pbd_Key, aArray[x][1]), SELF:__CheckField( pbd_Value, aArray[x][2] ) )
	else
		SELF:__oBase:MessageFormat( "Fehler beim Import in Zeile {1}. Das zu importierende Array hat ein falsches Format. Der Aufbau sollte sein: {{ uKey,uValue},...}.", { x }, "W", TRUE, TRUE ) 
	endif
next x

METHOD ImportAttributes( symKat AS SYMBOL, uKey AS USUAL, aAttributes AS ARRAY ) AS VOID PASCAL CLASS P_BaseDictionary

	LOCAL x            AS INT
	
for x:=1 upto aLen( aAttributes )
	if( UsualType(aAttributes[x]) == ARRAY .and. aLen( aAttributes[x] ) >= 2 .and. usualType(aAttributes[x][1]) == SYMBOL)
		if( !SELF:__oAttributes:Exists( pbd_Attr_Kat, aAttributes[x][1] ) )
			SELF:RegisterAttribute( aAttributes[x][1], aAttributes[x][2] )
		else
			SELF:SetAttribute( symKat, uKey, aAttributes[x][1], aAttributes[x][2] )  	
		endif
	else
		SELF:__oBase:MessageFormat( "Fehler beim Import von Attributen in Zeile {1}. Das zu importierende Array hat ein falsches Format. Der Aufbau sollte sein: {{ symAttribute,uValue},...}.", { x }, "W", TRUE, TRUE ) 
	endif
next x

METHOD Del( symKat := nil AS SYMBOL, uKey := nil AS USUAL ) AS VOID PASCAL CLASS P_BaseDictionary

	LOCAL nPos             AS INT

if( isNil(symKat) .and. isNil(uKey) )
	SELF:__aArray := {}
else
	uKey := SELF:__CheckField( pbd_Key, uKey )
	while( (nPos:=aScan( SELF:__aArray, { |a| (IsNil(symKat) .or. a[pbd_Kat] == symKat) .and. (IsNil(uKey) .or. ( UsualType(a[pbd_Key]) == UsualType(uKey) .and. a[pbd_Key] == uKey ) ) } ) ) != 0 )    	
    	aDelShrink( SELF:__aArray, nPos )
	enddo
endif

METHOD Serialize() AS STRING PASCAL CLASS P_BaseDictionary
return( "" )

METHOD DeSerialize( cXML AS STRING ) AS VOID PASCAL CLASS P_BaseDictionary

METHOD ImportRecord( symKat AS SYMBOL, oRecord AS AReadRecord ) AS VOID PASCAL CLASS P_BaseDictionary
SELF:ImportArray( symKat, SELF:__oBase:ArrayFromRecord( oRecord ) )

METHOD Compare( symKat1 AS SYMBOL, symKat2 AS SYMBOL, lIncludeAttributes := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseDictionary
return( SELF:__oBase:ArrayCompare( SELF:AsArray( symKat1, lIncludeAttributes ), SELF:AsArray( symKat2, lIncludeAttributes ) ) )
 

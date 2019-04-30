/*

	P_BaseObjectRegister.prg - André Finken 2019



*/             

DEFINE pbor_Kat       := #ObjectRegisterKat

CLASS P_BaseObjectRegister INHERIT AObject

	DECLARE ACCESS oBase
	DECLARE ASSIGN oBase
	DECLARE METHOD Set
	DECLARE METHOD Get
	DECLARE METHOD Del
	DECLARE METHOD Exists
	DECLARE METHOD AsArray
  
	/* HIDDEN */
	PROTECT __oReg             AS P_BaseDictionary
	PROTECT __oBase            AS P_Base  
  
METHOD Init( uObjectArray ) CLASS P_BaseObjectRegister

	LOCAL aObjects            AS ARRAY
	LOCAl x                   AS INT
	
SUPER:Init()

SELF:__oReg := P_BaseDictionary{ #SYMBOL }
SELF:__oReg:uValuelIfNotFound := NULL_OBJECT
SELF:__oReg:RegisterAttribute( #ReleaseObject, FALSE )

if( !IsNil( uObjectArray ) )
	aObjects := iif( UsualType(uObjectArray) == ARRAY, uObjectArray, { uObjectArray } )
	for x:=1 upto aLen( aObjects ) 
		SELF:Set( aObjects[x] )
	next x		
endif

METHOD Destroy() AS VOID PASCAL CLASS P_BaseObjectRegister

	SELF:Del()
	SELF:__oReg:Release()
	SUPER:Destroy()	
 
METHOD Set( oObject AS USUAL, symClassName := nil AS SYMBOL  ) AS USUAL PASCAL CLASS P_BaseObjectRegister

if( usualType(oObject) == OBJECT ) 
	                                                  
	if( IsNil(symClassName) )
		symClassName := ClassName( oObject )
	endif 
	
	if( SELF:__oReg:Exists( pbor_Kat, symClassName ) ) 
		SELF:oBase:MessageFormat( "Es wurde ein Object der Klasse {1} gesetzt, weches jedoch schon registriert war. Das vorhandene Object wird released und das neue aufgenommen", { symClassName }, "I", TRUE, TRUE )
		SELF:__oReg:Del( pbor_Kat, symClassName )
	endif
	
	oObject:AddRef()
	SELF:__oReg:Set( pbor_Kat, symClassName, oObject )
else
	SELF:oBase:MessageFormat( "Fehler bei Set(oObject). Es wird ein Object erwartet, statt dessen wurde versucht ein Typ = {1} zu setzen. ", { usualType(oObject) }, "E", TRUE ) 
endif

return( oObject ) 

METHOD Get( symClassName AS SYMBOL ) AS USUAL PASCAL CLASS P_BaseObjectRegister
	
	LOCAL oObject               AS AObject

if( SELF:__oReg:Exists( pbor_Kat, symClassName ) )
	oObject := SELF:__oReg:Get( pbor_Kat, symClassName ) 
else
	oObject := CreateInstance( symClassName )
	SELF:__oReg:Set( pbor_Kat, symClassName, oObject )
	SELF:__oReg:SetAttribute( pbor_Kat, symClassName, #ReleaseObject, TRUE )
endif
return( oObject )

METHOD Exists( symClassName AS SYMBOL ) AS LOGIC PASCAL CLASS P_BaseObjectRegister
return( SELF:__oReg:Exists( pbor_Kat, symClassName ) )

METHOD AsArray( lIncludeAttributes := FALSE AS LOGIC ) AS ARRAY PASCAL CLASS P_BaseObjectRegister
return( SELF:__oReg:AsArray( pbor_Kat, lIncludeAttributes ) ) 

METHOD Del( symClassName := nil AS SYMBOL ) AS VOID PASCAL CLASS P_BaseObjectRegister

	LOCAL nPos          AS INT 
	LOCAL oObject       AS AOBJECT
	LOCAL aObjects      AS ARRAY

if( IsNil( symClassName ) )
	aObjects := SELF:AsArray( FALSE )
	for nPos:= 1 upto aLen( aObjects )
		SELF:Del( aObjects[nPos][1] )
	next x	
else
	if( SELF:__oReg:Exists( pbor_Kat, symClassName ) )
		oObject := SELF:__oReg:Get( pbor_Kat, symClassName )
		oObject:Release()
		SELF:__oReg:Del( pbor_Kat, symClassName )
	else
		SELF:oBase:MessageFormat( "Es wurde versucht ein Object der Klasse {1} löschen. Ein solches Object wurde jedoch nie aufgenommen.", { symClassName }, "W", TRUE, TRUE ) 
	endif	
endif

ASSIGN oBase( oBaseLocal AS P_Base) AS P_Base PASCAL CLASS P_BaseObjectRegister
SELF:Set( oBaseLocal )
return( oBaseLocal )

ACCESS oBase AS P_Base PASCAL CLASS P_BaseObjectRegister
return( SELF:Get( #P_Base ) )

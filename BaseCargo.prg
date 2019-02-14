CLASS BaseStatistics INHERIT BaseCargo
 
 
METHOD Init() CLASS BaseStatistics

	SUPER:Init()
	

	


/*

  	BaseCargo.prg
  	Autor: André Finken, 2019



*/

CLASS BaseCargo INHERIT AObject

	EXPORT          uInitialCargoValue           AS USUAL 
	EXPORT          symGeneralCategory           AS SYMBOL
	
	DECLARE METHOD  Put
	DECLARE METHOD  Get
	DECLARE METHOD  Exists
	DECLARE METHOD  Pos
	DECLARE METHOD  Del
	DECLARE METHOD  Clear 
	DECLARE METHOD  Rename
	DECLARE METHOD  ImportFromArray
	DECLARE METHOD  ExportToArray
	DECLARE METHOD  ImportFromRecord	

	DECLARE METHOD  __getCat
	PROTECT         __aCargo                     AS ARRAY

METHOD Init() CLASS BaseCargo

	SUPER:Init()

	uInitialCargoValue := nil
	symGeneralCategory := #GENERAL
	__aCargo := {}
    
METHOD Destroy() AS VOID PASCAL CLASS BaseCargo
   
	SUPER:Destroy()


PROTECT METHOD __getCat( symCat := nil AS SYMBOL ) AS SYMBOL PASCAL CLASS  BaseCargo  
return( iif( IsNil(symCat), symGeneralCategory, symCat ) )

METHOD Put( uKey AS USUAL, uValue AS USUAL, symCat := nil AS SYMBOL ) AS VOID PASCAL CLASS BaseCargo

	LOCAL nPos           AS INT
	
nPos := SELF:Pos( uKey, symCat )
if( nPos != 0 )
	__aCargo[nPos][3] := uValue 
else
	aadd( __aCargo, { uKey, SELF:__getCat(symCat), uValue } )
endif

METHOD Get( uKey AS USUAL, symCat := nil AS SYMBOL ) AS USUAL PASCAL CLASS BaseCargo

	LOCAL uReturnValue              AS USUAL
	LOCAL nPos                      AS INT

nPos := SELF:Pos( uKey, symCat )
if( nPos != 0 )
	uReturnValue := __aCargo[nPos][3]
else
	uReturnValue := uInitialCargoValue
endif

return( uReturnValue )
                                       
METHOD Exists ( uKey AS USUAL, symCat := nil AS SYMBOL ) AS LOGIC PASCAL CLASS BaseCargo 
return( SELF:Pos( uKey, symCat ) != 0 ) 

METHOD Pos( uKey AS USUAL, symCat := nil AS SYMBOL ) AS INT PASCAL CLASS BaseCargo
symCat := SELF:__getCat( symCat )
return( aScan( __aCargo, { |a| a[1] == uKey .and. a[2] == symCat } ) )

METHOD Del( uKey AS USUAL, symCat := nil AS SYMBOL ) AS VOID PASCAL CLASS BaseCargo

	LOCAL nPos             AS INT
	
nPos := SELF:Pos( uKey, symCat )
if( nPos != 0 )
	ADelShrink( __aCargo, nPos )
endif

METHOD Rename( uKey AS USUAL, uNewKey AS USUAL, symCat := nil AS SYMBOL ) AS VOID PASCAL CLASS BaseCargo

	LOCAL nPos            AS INT
	
nPos := SELF:Pos( uKey, symCat )
if( nPos != 0 )
	__aCargo[nPos][2] := uNewKey
endif   
           
METHOD Clear( symCat := nil AS SYMBOL ) AS VOID PASCAL CLASS BaseCargo
  
	LOCAL nPos          AS INT  
  
if( IsNil(symCat) )
	__aCargo := {}
else
	symCat := SELF:__getCat(symCat)
	while( ( nPos := aScan( __aCargo, { |a| a[2] == symCat } )) != 0 )
		ADelShrink( __aCargo, nPos ) 		
	enddo
endif
           
METHOD ImportFromArray( aArray AS ARRAY, symCat := nil AS SYMBOL, nKeyPos := 1 AS INT, nValuePos := 2 AS INT ) AS VOID PASCAL CLASS BaseCargo
	
	LOCAL x          AS INT

for x := 1 upto aLen( aArray )
	SELF:Put( aArray[x][nKeyPos], aArray[x][nValuePos], symCat )
next x 	      
     
METHOD ExportToArray( symCat := nil ) AS ARRAY PASCAL CLASS BaseCargo

	LOCAL aReturnArray          AS ARRAY     
	LOCAL x                     AS INT

if( IsNil(symCat) )
	aReturnArray := AClone( __aCargo )
else
	aReturnArray := {} 
	symCat := SELF:__getCat(symCat)
	for x:= 1 upto aLen(__aCargo)
		if( __aCargo[2] == symCat )
			aadd( aReturnArray, { __aCargo[1], symCat, __aCargo[3] } ) 
		endif
	next x
endif
return( aReturnArray )

METHOD ImportFromRecord( oRecord AS AReadRecord, symCat := nil AS SYMBOL ) AS VOID PASCAL CLASS BaseCargo

	LOCAL x           AS INT

symCat := SELF:__getCat(symCat)
for x := 1 upto oRecord:FCount    
	SELF:Put( symCat, oRecord:StructInfo:GetFieldName(x), oRecord:FGetPos(x) )	
next x


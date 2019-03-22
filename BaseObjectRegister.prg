/*
	BaseObjectRegister.prg - André Finken 2019


    Registrierung und Handling aller Objecte die für die Kommunikation zuständig sind.
    Bei Benutzung wird geschaut, ob das Object bereits zugewiesen wurde, oder ob ein
    Neues erstellt werden soll.


*/

CLASS BaseObjectRegister INHERIT AOBJECT

    DECLARE METHOD Get
    DECLARE METHOD Register
    DECLARE METHOD IsRegistered
    DECLARE METHOD ReleaseObject



    /* Hidden */
	PROTECT __oObjectList         AS BaseArray

METHOD Init( aCommArray  ) CLASS BaseObjectRegister
/*
	Es können hier im Konstruktor schon alle möglichen Objecte reingeworfen werden.
	Beispiel:

	o := BaseObjectRegister{ {oTxnMgr, oProgress, oProt } }
	o:Release()

*/
	LOCAl x             AS INT


SElF:__oObjectList := BaseArray{}

if( !IsNil(aCommArray) )
	if( UsualType(aCommArray) != ARRAY )
		aCommArray := {aCommArray}
	endif

	for x:=1 upto aLen( aCommArray )
		SELF:Register( aCommArray[x] )
	next x

endif

SUPER:Init()

PROTECT METHOD Destroy() AS VOID PASCAL CLASS BaseObjectRegister

	LOCAL x          AS INT


for x:=1 upto SELF:__oObjectList:Len()
	SELF:ReleaseObject( SELF:__oObjectList:GetKatFromPos(x) )
next x

SUPER:Destroy()


METHOD Get( symClassName AS SYMBOL ) AS USUAL PASCAL CLASS BaseObjectRegister
/*
  	Es wird ein registriertes Object der geforderten Klasse (z.B. #ServerManager) zurückgegeben.
  	Wenn das Object noch nicht existiert, dann wird es jetzt erstellt
*/
if( !SELF:IsRegistered( symClassName ) )
	SELF:Register( CreateInstance( symClassName ), symClassName )
endif
SELF:__oObjectList:Increment( symClassName, #AccessCounter )
return( SELF:__oObjectList:Get( symClassName, #Object ) )


METHOD IsRegistered( symClassName AS SYMBOL ) AS LOGIC PASCAL CLASS BaseObjectRegister
return( SELF:__oObjectList:Exists( symClassName, #Object ) )

METHOD ReleaseObject( symClassName AS SYMBOL ) AS LOGIC PASCAL CLASS BaseObjectRegister

	LOCAL lReleased := FALSE       AS LOGIC
	LOCAL oObject                  AS AOBJECT

if( SELF:IsRegistered( symClassName ) )
	if( __oObjectList:Get( symClassName, #Release ) )
		oObject := __oObjectList:Get( symClassName, #Object )
		oObject:Release()
	endif
endif

returN( lReleased )

METHOD Register( oObject AS AObject, symClassName := nil AS SYMBOL ) AS USUAL PASCAL CLASS BaseObjectRegister

if( IsNil( symClassName ) )
	symClassName := ClassName( oObject )
endif

if( CheckInstanceOf( oObject, symClassName ) )
	if( SELF:IsRegistered( symClassName ) )
		debugPrint( "FIN: ", __ENT, __LINE__, "Achtung: Release vom Object ", symClassName, " obwohl es bereits instanziert war" )
		SELF:ReleaseObject( symClassName )
	endif

	SELF:__oObjectList:Put( symClassName, #Object, oObject )
	SELF:__oObjectList:Put( symClassName, #AccessCounter, 0 )
endif
return( oObject )

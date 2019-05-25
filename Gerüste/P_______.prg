TEXTBLOCK == P_
//
// Autor: André Finken
//
// Benötigt P_Base.prg
//
// Grundlage für ein object, welches das P_Baseobjekt benutzt, 
// jeodch nicht von ihm angeleitet ist
//
// Beispiel für Aufruf:
/*	
	LOCAL oREImport          AS P_RE_Import

oReImport := P_RE_Import{}
oReImport:oBase:oCommConnect := _oCommConnect
oReImport:Import()
SELF:_QueueBFCRefresh()
oReImport:oBase:ShowProtIfError( _oCommConnect, "Fehler beim importieren der RE-Belege", FALSE )
oREImport:Release()
*/


CLASS P_BaseExtended INHERIT AObject

	DECLARE ACCESS  oBase
	DECLARE ACCESS  lError

	PROTECT         __oBase         AS P_Base

METHOD Init( oP_Base ) CLASS P_BaseExtended

	SUPER:Init()

	if( !IsNil( oP_Base ) .and. IsInstanceOfUsual(oP_Base, #P_Base))
		SELF:__oBase := oP_Base
		SELF:__oBase:AddRef()
	else
		SELF:__oBase := P_Base{}
	endif


METHOD Destroy() AS VOID PASCAL CLASS P_BaseExtended

	SELF:oBase:Release()
	SUPER:Destroy()


ACCESS oBase AS P_Base PASCAL CLASS P_BaseExtended
return( SELF:__oBase )

ACCESS lError AS LOGIC PASCAL CLASS P_BaseExtended
return( SELF:__oBase:lError )


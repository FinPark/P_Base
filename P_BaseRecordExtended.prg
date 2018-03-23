/*

	P_BaseRecordExtended

	Stellt Erweiterungen zum P_BaseRecord dar.
	Kann z.B. als Cache genutzt werden
	Autor: André Finken

	Beispiel:
	-------------------------------
	oCache := P_BaseRecordExtended{}
	if( !oCache:Exists( #AngPos, #KOSTENART ) )
		// Kostenart umständlich ermitteln
		oCache:Put( #AngPos, #KostenArt, cKostenart )
	endif

	cKostenart := oCache:Get( #AngPos, #Kostenart )


	Aussehen nach Aussen
	--------------------------------------
	MainKey     SubKey        Value
	--------------------------------------
	#AngPos     #Kostenart    nil
	#AngPos     #Koststelle   "1010"
	#Aufpos     #Kostenart    "1020"
	#Aufpos     #EXITST       TRUE

	Aufbau im Inneren
	--------------------------------------
	MainKey     MainValue
					SubKey        SubValue
	--------------------------------------
	#AngPos     oRecord
					#Kostenart    nil
					#Koststelle   "1010"
	#Aufpos     oRecord
					#Kostenart    "1020"
					#EXISTS       TRUE



*/

static define rec_Key            := 1
static define rec_Value          := 2

CLASS P_BaseRecordExtended INHERIT AObject

	EXPORT         oRecord      AS P_BaseRecord

	DECLARE METHOD Get
	DECLARE METHOD GetRecord
	DECLARE METHOD Put
	DECLARE METHOD PutRecord
	DECLARE METHOD Del
	DECLARE METHOD Kill
	DECLARE METHOD Manipulate
	DECLARE ACCESS oBase
	DECLARE METHOD Exists
	DECLARE METHOD ExportToArray
	DECLARE METHOD DebugPrint
	DECLARE METHOD UNitTest

   PROTECT        __oBase      AS P_Base


METHOD Init( oP_Base ) CLASS P_BaseRecordExtended
// [oBezug] kann Array, AReadRecord, AWriteRecord oder AServer sein
//          wird ein symbol übergeben (z.B. #ME) wird versucht ein
//          leeren Record aus der Strktur #ME zu erstellen

	SUPER:Init()

	if( !IsNil( oP_Base ) .and. IsInstanceOfUsual(oP_Base, #P_Base))
		SELF:__oBase := oP_Base
		SELF:__oBase:AddRef()
	else
		SELF:__oBase := P_Base{}
	endif

	SELF:oRecord := P_BaseRecord{ SELF:oBase }

METHOD Destroy() AS VOID PASCAL CLASS P_BaseRecordExtended

// Alle Values vom Objekt P_BaseRecord
// nun freigeben
SELF:Kill()

SELF:oRecord:Release()
SELF:oBase:Release()
SUPER:Destroy()

METHOD Kill() AS VOID PASCAL CLASS P_BaseRecordExtended

	LOCAL aRecord         AS ARRAY
	LOCAL oRec            AS P_BaseRecord
	LOCAL x               AS INT

aRecord := SELF:oRecord:aRecord
for x:=1 upto ALen( aRecord )
	oRec := aRecord[x][rec_Value]
	oRec:Release()
next x

SELF:oRecord:Kill()

METHOD UNitTest() AS VOID PASCAL CLASS P_BaseRecordExtended

	SELF:Put( #Hallo, #Name, "Finken" )
	SELF:Put( #Hallo, #Vorname, "André" )
	debugPrint( "FIN: ", __ENT, __LINE__, "Exitsts", SELF:Exists( #Hallo, #Name ), SELF:Get( #HALLO, #Vorname ) )
	SELF:debugPrint( "Hier die Arrays" )


ACCESS oBase AS P_Base PASCAL CLASS P_BaseRecordExtended
return( SELF:__oBase )

METHO Del( symKeyMain AS SYMBOL, symKeySub := nil AS USUAL ) AS LOGIC PASCAL CLASS P_BaseRecordExtended
// Löscht den symKeySub-Eintrag raus
	LOCAL oRec             AS P_BaseRecord
	LOCAL lFound := FALSE  AS LOGIC

if( SELF:oRecord:Exists( symKeyMain ) )
	oRec := SELF:oRecord:Get( symKeyMain )
	if( IsNil( symKeySub ) )
		// Ganzen Bereich löschen
		oRec:Release()
		SELF:oRecord:Del( symKeyMain )
		lFound := TRUE
	else
		if( oRec:Exists( symKeySub ) )
			oRec:Del( symKeySub )
			lFound := TRUE
		endif
	endif
endif
return( lFound )

METHOD Exists( symKeyMain AS SYMBOL, symKeySub := nil AS USUAL ) AS LOGIC PASCAL CLASS P_BaseRecordExtended

	LOCAL lExists := FALSE          AS LOGIC
	LOCAL oRec                      AS P_BaseRecord

oRec := SELF:GetRecord( symKeyMain )
if( oRec != NULL_OBJECT )
	lExists := IsNil(symKeySub) .or. oRec:Exists( symKeySub )
	//SELF:Manipulate( { | nRowMain, nRowSub, symKeyMain_, symKeySub_, oSubRecord_, uValue_ | lExists := IIF( symKeyMain_ == symKeyMain .and. symKeySub_ == symKeySub, TRUE, lExists ) } )
endif
return( lExists )

METHOD Get( symKeyMain AS SYMBOL, symKeySub AS SYMBOL ) AS USUAL PASCAL CLASS P_BaseRecordExtended

	LOCAL uValue := nil        AS USUAL
	LOCAL oRec                 AS P_BaseRecord

oRec := SELF:GetRecord( symKeyMain )
if( oRec != NULL_OBJECT )
	uValue := oRec:Get( symKeySub )
endif
//SELF:Manipulate( { | nRowMain, nRowSub, symKeyMain_, symKeySub_, oSubRecord_, uValue_ | uValue := IIF( symKeyMain_ == symKeyMain .and. symKeySub_ == symKeySub, uValue_, uValue ) } )
return( uValue )

METHOD Put( symKeyMain AS SYMBOL, symKeySub AS SYMBOL, uValue AS USUAL ) AS VOID PASCAL CLASS P_BaseRecordExtended

	LOCAL oRec         AS P_BaseRecord

oRec := SELF:GetRecord( symKeyMain )
if( oRec == NULL_OBJECT )
	// Haupteintrag noch nicht vorhanden
	oRec := P_BaseRecord{ SELF:oBase }
	oRec:Put( symKeySub, uValue )
	SELF:oRecord:Put( symKeyMain, oRec )
else
	oRec:Put( symKeySub, uValue )
endif

METHOD Manipulate( cbCodeBlock AS CODEBLOCK ) AS VOID PASCAL CLASS P_BaseRecordExtended
// cbCodeBlock := { | nRowMain_, nRowSub_, symKeyMain_, symKeySub_, oSubRecord_, uValue_ | }
// <uValue> kann verändert werden und wird dann wieder zurückgeschrieben

	LOCAL aRec           AS ARRAY
	LOCAL aRecSub        AS ARRAY
	LOCAL uModifiedValue AS USUAL
	LOCAL x,y            AS INT

uModifiedValue := nil
aRec := SELF:oRecord:aRecord
for x:=1 upto ALen( aRec )
	aRecSub := aRec[x][rec_Value]
	for y := 1 upto ALen( aRecSub )
		uModifiedValue := aRecSub[y][rec_Value]
		Eval( cbCodeBlock, x,y, aRec[x][rec_Key], aRecSub[y][rec_Key], aRecSub[x][rec_Value], uModifiedValue )
		SELF:Put( aRec[x][rec_Key], aRecSub[y][rec_Key], uModifiedValue )
	next y
next x

METHOD GetRecord( symKeyMain AS SYMBOL ) AS P_BaseRecord PASCAL CLASS P_BaseRecordExtended

	LOCAL oRec           AS P_BaseRecord

oRec := NULL_OBJECT
if( SELF:oRecord:Exists( symKeyMain ) )
	oRec := SELF:oRecord:Get( symKeyMain )
endif
return( oRec )

METHOD ExportToArray() AS ARRAY PASCAL CLASS P_BaseRecordExtended

	LOCAL aReturn        AS ARRAY
	LOCAL x              AS INT
	LOCAl aTemp          AS ARRAY
	LOCAL oSubRec        AS P_BaseRecord

aReturn := {}
aTemp   := SELF:oRecord:aRecord
for x:=1 upto ALen( aTemp )
	oSubRec := aTemp[x][rec_Value]
	AAdd( aReturn, { aTemp[x][rec_Key], oSubRec:ExportToArray( TRUE ) } )
next x
return( aReturn )

METHOD debugPrint( cHeader := "" AS STRING ) AS VOID PASCAL CLASS P_BaseRecordExtended

	LOCAL aArray        AS ARRAY

aArray := SELF:ExportToArray()
debugPrintArray( aArray, cHeader )

METHOD PutRecord( symKeyMain AS SYMBOL, oRecord AS P_BaseRecord, lErrorIfExists := TRUE AS LOGIC ) AS VOID PASCAL CLASS P_BaseRecordExtended
if( SELF:oRecord:Exists( symKeyMain ) )
	if( lErrorIfExists )
		SELF:oBase:MessageFormat( "Warnung in PutRecord( # ) für P_BaseRecordExtended. Der Schlüssel # ist bereits vorhaden und wird nun überschrieben.", { symKeyMain, symKeyMain }, PROT_ART_WARNING )
	endif
	SELF:Del( symKeyMain )
endif
SELF:oRecord:Put( symKeyMain, oRecord )

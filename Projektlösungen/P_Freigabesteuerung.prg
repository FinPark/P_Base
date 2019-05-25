/*

  	P_Freigabesteuerung.prg
	Autor: André Finken, 2019  	

  	Eine Klasse zum ermitteln und cachen der gültigen Freigabebereiche.


*/

STATIC DEFINE defFREIKEY     := 1
STATIC DEFINE defPOS         := 2 
STATIC DEFINE defSTARTWERT   := 3 
STATIC DEFINE defENDWERT     := 4 
STATIC DEFINE defFR_ALLE_SB  := 5 
STATIC DEFINE defVALID       := 6 

CLASS P_Freigabesteuerung INHERIT AOBJECT


	PROTECT __oBase                AS P_Base
	PROTECT __aCache               AS ARRAY

	DECLARE ACCESS oBase 
	DECLARE METHOD IsValidRangePos
	DECLARE METHOD IsValidRangeSum 
	DECLARE METHOD IsSACHBEARBValid
	DECLARE METHOD SuggestSachbearb
	DECLARE METHOD GetRangePosByValue
    DECLARE METHOD GetSachbearbFromAnwender
    DECLARE METHOD UpdateCache
	    
	/* HIDDEN: Cache */
	DECLARE METHOD __IsCached
	DECLARE METHOD __CachePos
	DECLARE METHOD __Cache
	
		
METHOD Init( oTxnMgr ) CLASS P_Freigabesteuerung

	SUPER:Init()

	SELF:__oBase := P_Base{}
	if( !IsNil( oTxnMgr ) .and. IsInstanceOf( oTxnMgr, #ServerManager ) )
		SELF:__oBase:oTransactionManager := oTxnMgr
	endif

	SELF:__aCache := {}

METHOD Destroy() AS VOID PASCAL CLASS P_Freigabesteuerung

	SELF:__oBase:Release()
	SUPER:Destroy()	


ACCESS oBase AS P_Base PASCAL CLASS P_Freigabesteuerung
return( SELF:__oBase )

METHOD GetRangePosByValue( cFreiKey AS STRING, nWert AS REAL8 ) AS INT PASCAL CLASS P_Freigabesteuerung
/* Gibt die Position (POS) des Datensatzes zurück dessen STARTWERT und ENDWERT zum nWert passt.
   Wird kein passender Satz gefunden, so wird 0 zurückgegeben */

	LOCAL x           AS INT

for x:=1 upto aLen( SELF:__aCache )
	if( SELF:__aCache[x][defFREIKEY] == cFreiKey .and. nWert >= SELF:__aCache[x][defSTARTWERT] .and. nWert <= SELF:__aCache[x][defENDWERT] )
		return( x )
	endif	
next x
return( 0 )

METHOD IsValidRangePos( cFreiKey AS STRING, nPos AS INT ) AS LOGIC PASCAL CLASS P_Freigabesteuerung
/* Gibt die Gültigkeit der Freigabepositionszeile zurück */
	LOCAL nFound            AS INT 
		
SELF:UpdateCache( cFreiKey ) 
nFound := aScan( SELF:__aCache, { |a| a[defFREIKEY] == Rtrim(cFreiKey) .and. a[defPOS] == nPos } ) 
if( nFound != 0 )  
	return( SELF:__aCache[nFound][defVALID] ) 
endif
return( false )

METHOD IsSACHBEARBValid( cFreiKey AS STRING, cSachbearb AS STRING, nWert AS REAL8 ) AS LOGIC PASCAL CLASS P_Freigabesteuerung

	LOCAL oStmt               AS ASqlStatement
	LOCAl lValid  := FALSE    AS LOGIC 
	LOCAL x                   AS INT

SELF:UpdateCache( cFreiKey )

if( SELF:IsValidRangeSum( cFreiKey ) )
	for x:=1 upto aLen( SELF:__aCache )
		
		if( SELF:__aCache[x][defFREIKEY] == cFreiKey .and. nWert >= SELF:__aCache[x][defSTARTWERT] .and. nWert <= SELF:__aCache[x][defENDWERT] )

			/* Dies ist der WErtebereich-Satz der passt */
			if( SELF:__aCache[x][defFR_ALLE_SB] )
				/* Alle Sachbearbeitung sind gültig */
				return( true )
			else
				/* Schauen, ob sich der cSachbearb in den Positionen (P_FUNZUORD) wiederfindet.       
				   Die Sachbearbeiter sind einer Funktion zugeordnet */
				oStmt := SELF:oBase:CreateSQLStatement( "select SACHBEARB.SACHBEARB, SACHBEARB.FUNKTION_1 from AMSD200 as SACHBEARB "+CRLF+;
														"	inner join PRJD952 AS FUNKTION on FUNKTION.FUNKTION_1 = SACHBEARB.FUNKTION_1 "+CRLF+;
														"	where FUNKTION.P_FREIKEY = '"+cFreiKey+"' and FUNKTION.POS = "+NTrim( SELF:__aCache[x][defPOS] )+" and SACHBEARB.SACHBEARB = '"+cSachbearb+"'	",;
												 "Fehler beim ermitteln des gültigen Sachbearbeiter zu "+ cFreiKey + " und Wert "+NTrim(nWert), TRUE) 
				if( oStmt:Fetch() )
					lValid := TRUE
				endif
				oStmt:Release()
				
			 	return( lValid )
			endif
		endif
	next x
endif
return( false )  

METHOD GetSachbearbFromAnwender( cAnwender AS STRING ) AS STRING PASCAL CLASS P_Freigabesteuerung

	LOCAl cSachbearb          AS USUAL 
	
cSachbearb := SELF:oBase:GetFieldFromTable( #SACHBEARB, {{ #ANWENDER, cAnwender }}, #SACHBEARB, FALSE )	
if( IsNil(cSachbearb) )
	cSachbearb := ""
endif
return( cSachbearb ) 

METHOD SuggestSachbearb( cFreiKey AS STRING, nWert AS REAL8 ) AS STRING PASCAL CLASS P_Freigabesteuerung
	
	LOCAL cSuggestion := ""                  AS STRING
	LOCAl cUserName                          AS STRING 
	LOCAL cSachbearb                         AS STRING
	
cUserName  := oAccessManager:LogonUser
cSachbearb := SELF:GetSachbearbFromAnwender( cUserName )	
if( !Empty( cSachbearb ) )
	if( SELF:IsSACHBEARBValid( cFreiKey, cSachbearb, nWert ) )
		cSuggestion := cSachbearb
	endif
endif
return( cSuggestion ) 

METHOD IsValidRangeSum( cFreiKey AS STRING ) AS LOGIC PASCAL CLASS P_Freigabesteuerung
/* Gibt die Gültigkeit der Freigabekopfzeile zurück. 
   Diese wird aus der Summe der Positionen gebildet */
	LOCAl x           AS INT

cFreiKey := AllTrim(cFreiKey)	
SELF:UpdateCache( cFreiKey )

/* Wenn es trotz füllen des Cache noch keinen Satz gibt, dann ist die Summe nicht gültig */
if( !SELF:__isCached( cFreiKey ) )
	return( false )
endif

for x:=1 upto aLen( SELF:__aCache )
	if( AllTrim(SELF:__aCache[x][defFREIKEY]) == cFreikey .and. SELF:__aCache[x][defVALID] == FALSE )
		/* Hier ist ein ungültiger Satz in den Positionen, daher
		   ist auch die Summe ungültig */ 
		return( false )	                
	endif
next x	

/* Es wurden keine ungültgen Positionen gefunden, daher ist die Summe gültg */
return( true )

METHOD UpdateCache( cFreiKey AS STRING, lForceRefreshCache := FALSE AS LOGIC ) AS VOID PASCAL CLASS P_Freigabesteuerung
/* Prüft, ob der Cacheinhalt vorhanden ist und ermittelt den Cache ggf. neu
   mit [lForceRefreshCache] kann ein Neuaufbau des Caches erzwungen werden */
if( lForceRefreshCache .or. !SELF:__IsCached( cFreiKey ) )
	SELF:__Cache( cFreiKey )	
endif 

PROTECT METHOD __isCached( cFreiKey AS STRING ) AS LOGIC PASCAL CLASS P_Freigabesteuerung
/* Gibt zurück, ob sich die Freigabezeile im Cache befindet */
return( SELF:__CachePos( cFreiKey ) != 0 )

PROTECT METHOD __CachePos( cFreiKey AS STRING, nPos := 0 AS INT ) AS INT PASCAL CLASS P_Freigabesteuerung
cFreiKey := ALLTrim( cFreiKey )
return( aScan( SELF:__aCache, { |a| Alltrim(a[defFREIKEY]) == cFreiKey .and. IIF( nPos != 0 , a[defPOS] == nPos, TRUE ) } ) ) 
 
PROTECT METHOD __Cache( cFreiKey AS STRING ) AS VOID PASCAL CLASS P_Freigabesteuerung
/* Leert den Cache und füllt ihn an Hand der Datensätze neu auf. Zudem wird die Gültigkeit der 
   einzelnen Positionszeilen ermittelt und in den Cache geschrieben */
	LOCAL nPos         AS INT
	LOCAL oStmt        AS ASqlStatement 
	LOCAL aTmp         AS ARRAY

aTmp := {}
cFreiKey := AllTrim(cFreiKey)

/* Cache für FreiKey leeren, falls schon was drin ist */	
while( (nPos := SELF:__CachePos( cFreiKey )) != 0 )
	aDelShrink( SELF:__aCache, nPos )
enddo

/* Cache neu füllen */
oStmt := SELF:oBase:CreateSQLStatement( "SELECT P_FREIKEY, POS, STARTWERT, ENDWERT, FR_ALLE_SB FROM {P_FREIWERT} WHERE P_FREIKEY = ? ORDER BY STARTWERT",;
								 "Fehler beim ermitteln des Wertebereiches zu "+ cFreiKey, TRUE, { cFreiKey }) 
if( oStmt != NULL_OBJECT )
	while( oStmt:Fetch() )
		aadd( aTmp, { AllTrim(oStmt:FGetN(#P_FREIKEY)) , oStmt:FGetN(#POS), oStmt:FGetN(#STARTWERT), oStmt:FGetN(#ENDWERT), oStmt:FGetN(#FR_ALLE_SB), TRUE } )  
	enddo
	
	oStmt:Release()
endif 
 
 
// Berechnung der gültigen Bereiche
// -------------------------------------------------
if( aLen(aTmp) > 0 )
	/* Beginnt der erste Satz mit dem Startwert = 0? */
	if( aTmp[1][defSTARTWERT] != 0 )
		aTmp[1][defVALID] := FALSE
	endif
	
	/* Endet der letzte Satz mit dem Maximalwert? */
	if( aTmp[aLen( aTmp )][defENDWERT] != 999999999999.99 )
		 aTmp[aLen( aTmp )][defVALID] := FALSE 
	endif

	/* Wenn es mehr als einen Satz gibt, dann prüfen, ob die Sätze natlos ineinander übergehen */
	if( aLen( aTmp ) > 1 )
		for nPos := 2 upto aLen( aTmp )
			if( aTmp[nPos][defVALID] )
				/* Werte die bereits False sind, nicht mehr auf True setzen */
				aTmp[nPos][defVALID] := aTmp[nPos][defSTARTWERT] == aTmp[nPos-1][defENDWERT] + 0.01    
			endif
		next nPos
	endif
	
	*/ Übertragen in den allgemeinen Cache */
	SELF:__aCache := SELF:oBase:ArrayCombine( SELF:__aCache, aTmp ) 
endif











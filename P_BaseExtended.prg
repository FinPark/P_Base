TEXTBLOCK == P_BaseExtended
//
// Autor: André Finken
//
// Benötigt P_Base.prg, P_BaseRecord
//
// Brauchbare Projektmethoden wie Terminierung, Anlegen Stücklistenpositionen etc.
//

define cargo_ReplaceZwingend := 1

CLASS P_BaseExtended INHERIT AObject

	DECLARE ACCESS  oBase

	DECLARE METHOD  NeuAusSTKLAK
	DECLARE METHOD  GenSTKLAK
	DECLARE METHOD  GenAPLAP
	DECLARE METHOD  GenSTKLAP
	DECLARE METHOD  GenANGEBOTPOS
	DECLARE METHOD  GenARTIKEL
	DECLARE METHOD  GenBESTAND 
	DECLARE METHOD  GetSTKLAKArrayTeilBaum  
	DECLARE METHOD  GetSTKLAKSqlTeilBaum 
	DECLARE METHOD  GetBdeNrTB

	DECLARE METHOD  Terminierung 
	DECLARE METHOD  IsTerminiert
	DECLARE METHOD  BerechnungRangEbene
	DECLARE METHOD  AngebotWertermittlung
	DECLARE METHOD  AuftragWertermittlung

	/* Interne Methoden */
	DECLARE METHOD  __GenBase
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

METHOD IsTerminiert( nBdeNr AS INT, lAngebotSTKLAK := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseExtended
// Werden Terminierungsrelievate Daten in einem Arbeitsgang oder einer Stücklistenposition geändert,
// so geht der STAT_PLAN der jeweiligen Position von "T" (Terminiert) weg.
// Es wird auch der STKLAK->STAT_PLAN umgesetzt und ebenso alle darüberliegenden Köpfe.
// Es braucht also nur der STAT_PLAN der übergebene Bde-Nr abgefragt werden.
return( SELF:oBase:GetFieldFromTable( iif( lAngebotSTKLAK, #STKLAK_ANG, #STKLAK ), {{ #BDE_NR, nBdeNr }}, #STAT_PLAN ) = "T" ) 

METHOD Terminierung( nBdeNr AS INT, lAngebotSTKLAK := FALSE AS LOGIC, cBerechnungsModus := "K" AS STRING, lMitRangEbenenBerechnung := TRUE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseExtended
// Terminiert eine Auftrags, bzw. Angebotsstückliste
// <nBdeNr>            - Die zu terminierende Auftragsstückliste
// [lAngebotSTKLAK]    - Wenn True wird in den Angebotsstücklisten gearbeitet
// [lMitRangEbe...]    - Wenn TRUE, dann wird die RangEbenenBerechnung gerufen
// [cBerechnungsModus] - Kann einen der folgenden Werte haben: "T";"S";"K". Wird Leer ("") angegeben, so wird dieser aus dem Konfigurationsparameter gezogen

// Berechnungsmodus bei der Berechnung von Stücklisten
//
// "T" (Terminierung):
// Berechnet die Struktur und Termine einer Stückliste mit allen Unterbaugruppen. D.h. die Strukturebene, der Rang, die Mengen und Termine in den Stücklistenköpfen, Stücklistenpositionen und Arbeitsgängen stimmen.
//
// "S" (Struktur):
// Berechnet die Struktur einer Stückliste mit allen Unterbaugruppen. D.h. die Strukturebene, der Rang und die Mengen in den Stücklistenköpfen stimmen.
//
// "K" (Komplett):
// Beinhaltet die o.g. Strukturberechnung zuzüglich der Berechnung aller Stücklisten- und Arbeitsplanpositionen.
//
// Der Berechnungsmodus beeinflusst die Vorschlagswerte der Felder Status TB (Techn. Bearbeitung) und Status AV (Arbeitsvorbereitung). Ist der Berechnungsmodus nicht 'Komplett' oder 'Terminierung', werden diese Felder immer als 'Gesperrt' vorbelegt.
//
	LOCAL cStatus := ""       AS STRING
	LOCAL symAlias            AS SYMBOL
	LOCAL lOK := TRUE         AS LOGIC

symAlias    := IIF( lAngebotSTKLAK, #STKLAK_ANG, #STKLAK )
if( Empty( cBerechnungsModus ) )
	cBerechnungsModus := oConfigManager:GetStringParam( iif( symAlias == #stklak, "PRM", "ANG" ), "BERECHNUNGSMODUS", "K")
endif

if( !SELF:oBase:IsLocalError( #BaseExtended_Terminierung ) )
	lOK := BerechneStueckliste(SELF:oBase:oTransactionManager, PrmAufgabeAusBerechnungsModus(cBerechnungsModus), nBdeNr, symAlias, @cStatus)
	if( !Empty(cStatus) )
		SELF:oBase:MessageFormat( "Fehler beim terminieren der Stückliste Bde-Nr #. #", { nBdeNr, cStatus }, PROT_ART_WARNING )
		lOK := FALSE
	endif
endif

if( lOK .and. lMitRangEbenenBerechnung )
	lOK := SELF:BerechnungRangEbene( nBdeNr, lAngebotSTKLAK )
endif
return( lOK )

METHOD BerechnungRangEbene( nBdeNr AS INT, lAngebotSTKLAK := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseExtended
// Rang-Ebenen-Berechnung eine Auftrags, bzw. Angebotsstückliste
// Dies wird bei der Methode Terminierung() bereits mitgemacht.
// Sollte nu genutzt werden, wenn eine komplette Terminierung nicht notwenig ist.
//
// <nBdeNr>          - Die zu terminierende Auftragsstückliste
// [lAngebotSTKLAK]  - Wenn True wird in den Angebotsstücklisten gearbeitet
//

	LOCAL cStatus := ""       AS STRING
	LOCAL symAlias            AS SYMBOL

SELF:oBase:ResetLocalError( #BaseExtended_Terminierung )

symAlias    := IIF( lAngebotSTKLAK, #STKLAK_ANG, #STKLAK )
CallSPRangEbene(nBdeNr, symAlias, @cStatus, SELF:oBase:oTransactionManager )
if( Empty(cStatus) )
     PrmTerm( SELF:oBase:oTransactionManager, symAlias, nBdeNr, @cStatus, TRUE)
endif
if( !Empty(cStatus) )
	SELF:oBase:MessageFormat( "Fehler bei Rang-Ebenen-Berechnung der Stückliste Bde-Nr #. #", { nBdeNr, cStatus }, PROT_ART_WARNING )
endif
return( Empty( cStatus ) )

METHOD NeuAusSTKLAK( nBdeNrBezug AS INT, lMitBaugruppen := TRUE AS LOGIC, lGesperrteArtikel := TRUE AS LOGIC, lMitAusschuss := TRUE AS LOGIC, lZeichnungsBezugAktualisieren := TRUE AS LOGIC ) AS INT PASCAL CLASS P_BaseExtended
// Kopieren einer kompletten Stückliste (mit einfliessenden Baugruppen)
// Die kopierte Baugruppe wird an <nBdeNrBezug> angehängt
// Geht nur für Auftragsstücklisten

	LOCAL oStklakQuelle                     AS AReadRecord
	LOCAL oTmpRecord                        AS AWriteRecord
	LOCAl nBdeNr := 0                       AS INT
	LOCAL oQuelle, oZiel, oNeuTB            AS AuftragsProduktstruktur
	LOCAL cLBBst := ""                      AS STRING
	LOCAL cLBVerl:= ""                      AS STRING
	LOCAL nRang := 1                        AS INT

oStklakQuelle := SELF:oBase:GetReadRecordFromTable( #STKLAK, {{ #BDE_NR, nBdeNrBezug}}, "Auftagsstückliste" )
if( oStklakQuelle != NULL_OBJECT )
	oTmpRecord := AWriteRecord{ oStklakQuelle, TRUE }
	oQuelle    := AuftragsProduktstruktur{oTmpRecord, , , , , SELF:oBase:oTransactionManager}
	oTmpRecord:Release()

	oZiel      := AuftragsProduktstruktur{{oStklakQuelle:Fget(#STKL_BEZUG)}, , , , , SELF:oBase:oTransactionManager }
	if( oZiel:lError  )
		SELF:oBase:Message( oZiel:cError, PROT_ART_ERROR )
	else
		oNeuTB := AuftragsProduktstruktur{NIL, oZiel, oQuelle, oStklakQuelle:Fget(#AUFTRAG), oStklakQuelle:FGet(#AUFPOS), SELF:oBase:oTransactionManager, , lGesperrteArtikel, lMitAusschuss, SELF:oBase:oProtocol, cLBBst, cLBVerl, lZeichnungsBezugAktualisieren }
		if( lMitBaugruppen .and. !oNeuTB:AktStattCopy )
			oQuelle:__EbeneAufloesen( TRUE , {PRM_AUFGABE_KOPIEREN}, @nRang, oNeuTB, lGesperrteArtikel, lMitAusschuss, SELF:oBase:oProtocol, cLBBst, cLBVerl, lZeichnungsBezugAktualisieren )
		endif

      if( oNeuTB:lError )
			SELF:oBase:Message( oNeuTB:cError, PROT_ART_ERROR )
		else
			nBdeNr := oNeuTB:FGet(#BDE_NR)
   	endif
   	oNeuTB:Release()
   endif
	oZiel:Release()
	oQuelle:Release()
	oStklakQuelle:Release()
endif
return( nBdeNr )




PROTECT METHOD __GenBase( symAlias AS SYMBOL, aKey AS ARRAY, aVorlage AS ARRAY, oReplaces AS P_BaseRecord, oDefaultReplaces AS P_BaseRecord ) AS AReadRecord PASCAL CLASS P_BaseExtended
// Grundlage für das Anlegen von Datensätzen
// 1. eine Kopie von <aVorlage> wird gemacht
// 2. Alle Defaults <aDefaultReplaces> setzen
// 3. Alle Replaces <aReplaces> machen
// 4. Key <aKey> setzen
// 5. Alle zwingenden Defaults <aDefaultReplaces> machen
// 6. Alle zwingenden Replaces <aReplaces> machen
//
// return : Es wird der AReadRecord des erstellten Datensatzes zurückgegeben
//          und muss in der aufrufenden Methode wieder released werden

	LOCAL oServer           AS AServer
	LOCAL cKey := ""        AS STRING
	LOCAL oReadRecord       AS AReadRecord

if( ALen(aVorlage) != 0 )
	oServer :=  SELF:oBase:CreateAServer( symAlias, aVorlage, TRUE )
else
	oServer := SELF:oBase:oTransactionManager:Open( symAlias )
	if( !oServer:BeginAppend() )
		SELF:oBase:MessageFormat( "Fehler beim Anlegen eines neuen Datensatzes in der Tabelle #: #", { symAlias, oServer:Status:GetMessage() }, PROT_ART_ERROR )
		oServer:Release()
		oServer := NULL_OBJECT
	endif
endif

if( oServer != NULL_OBJECT )

	// Defaults setzen
	oDefaultReplaces:ExportToRecord( oServer, FALSE )

	// Einmal alle übergebenen User Replaces machen
	oReplaces:ExportToRecord( oServer, FALSE )

	// Zwingende Defaults setzen
	oDefaultReplaces:ExportToRecord( oServer, TRUE )

	// Key setzen
   SELF:oBase:ArrayToRecord( aKey, oServer, TRUE, FALSE )

	// Alle zwingenden User-Ersetzungen durchführen
	oReplaces:ExportToRecord( oServer, TRUE )

	// ReadRecord für Rückgabe
	oReadRecord := AReadRecord{oServer,TRUE}

	if( !oServer:EndAppend("SNT") )
		// Update Schlüsselinformationen
		AEval( aKey, { |aA| aA[2] := oReadRecord:FGet(aA[1]) })

		SELF:oBase:MessageFormat( "Fehler beim erstellen # mit Schlüssel: #. # #.", { symAlias, SELF:oBase:KeyToString(aKey), iif( IsNil(oReadRecord:FGet(#BEZEICH)),"", "Bezeichnung = "+oReadRecord:FGet(#BEZEICH) ), oServer:Status:GetMessage() }, PROT_ART_ERROR, TRUE )
	endif

	oServer:Release()
endif

// !!! Achtung !!! oReadRecord muss in der aufrufenden Methode wieder released werden
return( oReadRecord)

METHOD GenSTKLAK( nBdeNrBezug AS INT, cArtikel AS STRING, aVorlage AS ARRAY , oReplaces AS P_BaseRecord, lAngebotSTKLAK := FALSE AS LOGIC ) AS AReadRecord PASCAL CLASS P_BaseExtended
// Anlegen eines Stücklistenkopfes aus einer Vorlage und durchführen der notwendigen Ersetzungen und Defaultwerte
// <nBdeNrBezug>   - Zu der Baugruppe wird Bezug genommen (STKL_BEZUG). Ist diese 0, so wird diese Stückliste als Übersichtsstückliste angelegt
// <cArtikel>      - Leer=OTeil, anderfalls eine gültige Artikelnummer
// <aVorlage>      - {} = keine Vorlage, {{ #BDE_NR, nBdeNrVorlage }} für Vorlage
// <nVorlageBdeNr> - Bde-Nr der Vorlage-Stückliste
// <aReplaces>     - Aufbau {{#FIELD, uContend [, lNecessary]}, {...
// 						#FIELD     : Feldname, z.B. #BDE_NR
//                   uContend   : FeldInhalt, z.B. 100294
//                   lNecessary : Muss gesetzt werden. Es werden Defaultwerte gesetzt, oder Ersetzungen aus dem Teilestamm gemacht.
//                                Wenn der Inhalt dieses Feldes in jedem Fall gesetzt werden soll, ist hier TRUE einzutragen
// [lAngebotSTKLAK]- Für Auftragsstückliste (FALSE) oder für Angebotsstückliste (TRUE)
//
// Return: Die erstellte #BDE_NR

	LOCAL nBdeNR                 AS INT
	LOCAL oArtikelRecord         AS AReadRecord
	LOCAL symAliasSTKLAK         AS SYMBOL
	LOCAL fBelegNr               AS REAL8
	LOCAL oStklBezugRecord       AS AReadRecord
	LOCAL nPos := 0              AS INT
   LOCAL oReadRecord            AS AReadRecord
   LOCAL oDefault               AS P_BaseRecord
   LOCAL nMenge  := 1           AS REAL8
   LOCAL oBezug                 AS AReadRecord

oReadRecord    := NULL_OBJECT
SELF:oBase:ResetLocalError( #BaseExtended_GenSTKLAK )
symAliasSTKLAK := IIF( lAngebotSTKLAK, #STKLAK_ANG, #STKLAK )


if( !GetBelegNr(NULL_OBJECT, IIF( lAngebotSTKLAK, "KS", "SB"), "SNT" , @fBelegNr) )
	SELF:oBase:Message( "Belegnummer <"+IIF(lAngebotSTKLAK, "KS", "SB")+"> konnte nicht vergeben werden :"+oServerManager:Status:GetMessage(), PROT_ART_ERROR )
else
	nBdeNr := INT(fBelegNr)

	oDefault := P_BaseRecord{SELF:oBase}

	if( nBdeNrBezug == 0 )
		// Es handelt sich um eine Übersichtsstückliste
		// Hier müssen #AUFTRAG, #AUFPOS in den aReplace sein
		oDefault:Put( #BDE_NR_TB, nBdeNr, TRUE )

		if( lAngebotSTKLAK )
			oBezug := SELF:oBase:GetReadRecordFromTable( #ANGEBOTPOS, {{#ANGEBOTSNR, oReplaces:Get(#AUFTRAG)}, { #AUFPOS, oReplaces:Get(#AUFPOS) }}, "Angebotsposition" )
		else
			oBezug := SELF:oBase:GetReadRecordFromTable( #AUFPOS, {{#AUFTRAG, oReplaces:Get(#AUFTRAG)}, { #AUFPOS, oReplaces:Get(#AUFPOS) }}, "Auftragsposition" )
		endif
		if( oBezug != NULL_OBJECT )
			oDefault:Put( #TERM_ENDE,  oBezug:FGet(#LIEFERTERM), TRUE )
			oDefault:Put( #TERM_START, oBezug:FGet(#LIEFERTERM), TRUE )
			oBezug:Release()
		endif
	else
		// Es handelt sich um eine Stückliste mit Bezug
		// Nun Ersetzungen aus der Bezugsstückliste machen
		oStklBezugRecord := SELF:oBase:GetReadRecordFromTable( symAliasSTKLAK, {{#BDE_NR, nBdeNrBezug}}, "Stückliste" )
		if( oStklBezugRecord != NULL_OBJECT )
			oDefault:Put(#STKL_BEZUG, oStklBezugRecord:Fget(#BDE_NR), TRUE)
			oDefault:Put(#AUFTRAG,    oStklBezugRecord:FGet(#AUFTRAG), TRUE)
			oDefault:Put(#AUFPOS,     oStklBezugRecord:FGet(#AUFPOS))
			oDefault:Put(#BDE_NR_TB,  oStklBezugRecord:FGet(#BDE_NR_TB))
			oDefault:Put(#STKL_ART,   oStklBezugRecord:FGet(#STKL_ART))
			oDefault:Put(#TERM_ENDE,  oStklBezugRecord:FGet(#TERM_START), TRUE) // Soll wirklich TERM_START sein
			oDefault:Put(#TERM_START, oStklBezugRecord:FGet(#TERM_START), TRUE)
			oStklBezugRecord:Release()
		endif
	endif
	oDefault:Put(#BDE_NR,     nBdeNr, TRUE)
	nPos := oConfigManager:GetIntParam("PRM", "STKLAK_POS_INKREMENT", 0)
	if( nPos != 0 )
		nPos := SELF:oBase:GetNextPos( #STKLAK, #POS, SELF:oBase:StringFormat("BDE_NR_TB = #", { oDefault:Get(#BDE_NR_TB) }), nPos )
	endif
	oDefault:Put(#POS,        nPos)

	oDefault:Put(#DISPOART,   "F")
	oDefault:Put(#POS_ART,    "O" )
	oDefault:Put(#ARTIKEL,    "")
	oDefault:Put(#KOSTENART,  oConfigManager:GetStringParam( "", "KOSTENART", ""))
	oDefault:Put(#ME,         oConfigManager:GetStringParam( "", "MENGENEINHEIT_STUECK", ""))
	oDefault:Put(#KENN_MENGE, "S")
	oDefault:Put(#APL_STATUS, "G")
	oDefault:Put(#STAT_TB,    "G")
	oDefault:Put(#STAT_AV,    "G")
	oDefault:Put(#FK_STKL, "N", TRUE)
	oDefault:Put(#FK_APL, "N", TRUE)
	oDefault:Put(#DK_FBK, "N", TRUE )
	oDefault:Put(#ET_AUS_AP, FALSE)
	oDefault:Put(#STRU_EBENE, 1)
	oDefault:Put(#RANG,1)
	oDefault:Put(#ERSTELLER, _GetUserName())

	if( !Empty(cArtikel) )
		oArtikelRecord := SELF:oBase:GetReadRecordFromTable( #ARTIKEL, {{#ARTIKEL,cArtikel}}, "Artikel" )
		if( oArtikelRecord != NULL_OBJECT )
			oDefault:Put( #ARTIKEL,  oArtikelRecord:Fget(#ARTIKEL), TRUE )
			oDefault:Put( #BEZEICH,  oArtikelRecord:Fget(#BEZEICH), TRUE )
			oDefault:Put( #STKL_NR, oArtikelRecord:Fget(#STKL_NR), TRUE )
			oDefault:Put( #WERKSTOFF, oArtikelRecord:Fget(#WERKSTOFF), TRUE )
			oDefault:Put( #NORM, oArtikelRecord:Fget(#NORM), TRUE )
			oDefault:Put( #ZEICHNUNG, oArtikelRecord:Fget(#ZEICHNUNG), TRUE )
			oDefault:Put( #ZEICH_INDX, oArtikelRecord:Fget(#ZEICH_INDX), TRUE )
			oDefault:Put( #ZEICH_FORM, oArtikelRecord:Fget(#ZEICH_FORM), TRUE )
			oDefault:Put( #ME, oArtikelRecord:Fget(#ME_S), TRUE )
			oDefault:Put( #DISPOART, oArtikelRecord:Fget(#DISPOART), TRUE )
			oDefault:Put( #LAGER, oArtikelRecord:Fget(#LAGER_HPT), TRUE )
			oDefault:Put( #MEMO_TECH, oArtikelRecord:Fget(#MEMO_TECH), TRUE )
			oDefault:Put( #KENNUNG_EV, oArtikelRecord:Fget(#KENNUNG_EV), TRUE )
			oDefault:Put( #URSPRUNG, oArtikelRecord:Fget(#URSPRUNG), TRUE )
			oDefault:Put( #INT_WARENR, oArtikelRecord:Fget(#INT_WARENR), TRUE )
			oDefault:Put( #WARENNR, oArtikelRecord:Fget(#WARENNR), TRUE )
			oArtikelRecord:Release()
		endif
	endif

	// Felder #MENGE_... aus dem Inhalt von #MENGE_EH besetzen
	do case
	case( oReplaces:Pos( #MENGE_EH ) > 0 )
		nMenge := oReplaces:Get( #MENGE_EH )
	case( oDefault:Pos( #MENGE_EH ) > 0 )
		nMenge := oDefault:Get( #MENGE_EH )
	otherwise
		nMenge := 1
	endcase

	oDefault:Put( #MENGE_GES, nMenge, TRUE )
	oDefault:Put( #MENGE_APL, nMenge, TRUE )

	oDefault:Put( #RECORDID,  NewID(), TRUE )

	if( !SELF:oBase:IsLocalError( #BaseExtended_GenSTKLAK ) )
		oReadRecord := SELF:__GenBase( symAliasStklak, {{#BDE_NR,nBdeNr}}, aVorlage, oReplaces, oDefault )
	endif
	oDefault:Release()
endif
return( oReadRecord )

METHOD GenAPLAP( nBdeNr AS INT, aVorlage AS ARRAY, cArbeitsplatz AS STRING, cKatArt AS STRING, cKatalogNr AS STRING, oReplaces AS P_BaseRecord, lAngebotAPLAP := FALSE AS LOGIC, nPosIncrement := 1 AS INT ) AS AReadRecord PASCAL CLASS P_BaseExtended
// Anlegen eines Arbeitsganges aus einer Vorlage und durchführen der notwendigen Ersetzungen und Defaultwerte
// <nBdeNrBezug>   - BdeNr der Stückliste
// <cArtikel>      - Leer=OTeil, anderfalls eine gültige Artikelnummer
// <aVorlage>      - {} - keine Vorlage, {{ #BDE_NR, nVorlageBdeNr }, { #AGNR, nVorlageAGNr }}
// <aReplaces>     - Aufbau {{#FIELD, uContend [, lNecessary]}, {...
// 						#FIELD     : Feldname, z.B. #BDE_NR
//                   uContend   : FeldInhalt, z.B. 100294
//                   lNecessary : Muss gesetzt werden. Es werden Defaultwerte gesetzt, oder Ersetzungen aus dem Teilestamm gemacht.
//                                Wenn der Inhalt dieses Feldes in jedem Fall gesetzt werden soll, ist hier TRUE einzutragen
// [lAngebotAPLAP]- Für Auftragsarbeitsgänge = FALSE, für Angebotsarbeitsgänge = TRUE
//
// Return: AReadRecord des erstellten Arbeitsganges

	LOCAL symAliasAPLAP          AS SYMBOL
	LOCAL symAliasSTKLAK         AS SYMBOL
	LOCAL oStklak                AS P_BaseRecord
	LOCAL nPos := 0              AS REAL8
   LOCAL oReadRecord            AS AReadRecord
   LOCAL oArbplatz, oKat        AS AReadRecord
   LOCAL oDefault               AS P_BaseRecord
   LOCAL oKoststelle            AS AReadRecord

symAliasAPLAP  := IIF( lAngebotAPLAP, #APLAP_ANG, #APLAP )
symAliasSTKLAK := IIF( lAngebotAPLAP, #STKLAK_ANG, #STKLAK )
oReadRecord    := NULL_OBJECT
SELF:oBase:ResetLocalError( #BaseExtended_GenAPLAP )

oStklak := P_BaseRecord{ SELF:oBase }
if( oStklak:ImportFromTable( symAliasSTKLAK, {{ #BDE_NR, nBdeNr }} ) )

	oDefault := P_BaseRecord{SELF:oBase}
	oDefault:Put( #BDE_NR,                 nBdeNr,   TRUE )
	oDefault:Put( #AGNR,                   (nPos := SELF:oBase:GetNextPos( symAliasAPLAP, #AGNR, SELF:oBase:StringFormat("BDE_NR = #", { oDefault:Get(#BDE_NR) }), nPosIncrement ) ), TRUE )

	oDefault:Put( #AUFTRAG,                oStklak:Get( #AUFTRAG ), TRUE )
	oDefault:Put( #AUFPOS,                 oStklak:Get( #AUFPOS ), TRUE )

	oDefault:Put( #TERM_ENDE,              oStklak:Get( #TERM_ENDE ), TRUE )
	oDefault:Put( #TERM_START,             oStklak:Get( #TERM_ENDE ), TRUE )
	oDefault:Put( #DAUER_ART,              "R" )
	oDefault:Put( #DAUER_FKT,              1 )
	oDefault:Put( #FK_APL,                 "N", TRUE )
	oDefault:Put( #K_STARTAG,              "N" )
	oDefault:Put( #KAT_ART,                "AG" )
	oDefault:Put( #MENGE_AUS,              1 )
	oDefault:Put( #MENGE_IST,              0, TRUE )
	oDefault:Put( #TEAM,                   1 )
	oDefault:Put( #ZEIT_MENGE,             1 )
	oDefault:Put( #ZEITART,                "S" )
	oDefault:Put( #AGNR_FV,                0 )
	oDefault:Put( #AG_ZU,                  "AG" )
	oDefault:Put( #AGMENGE_B,              99999999.999 )
	oDefault:Put( #AGMENGE_V,              1 )
	oDefault:Put( #APL_STATUS,             IIF( oConfigManager:GetStringParam( "PRM", "BERECHNUNGSMODUS", "K") $ "KT", "F", "G" ), TRUE )
	oDefault:Put( #BEENDET,                Today(), TRUE )
	oDefault:Put( #BEGONNEN,               Today(), TRUE )
	oDefault:Put( #BESTELL_NR,             "", TRUE )
	oDefault:Put( #BESTELL_PO,             0, TRUE )
	oDefault:Put( #BK1,                    FALSE, TRUE )
	oDefault:Put( #BK2,                    FALSE, TRUE )
	oDefault:Put( #DK,                     "N", TRUE )
	oDefault:Put( #FL1,                    FALSE, TRUE )
	oDefault:Put( #FL2,                    FALSE, TRUE )
	oDefault:Put( #KOSTSTELLE,             oConfigManager:GetStringParam( "", "KOSTENSTELLE", "") )
	oDefault:Put( #KOSTENART,              oConfigManager:GetStringParam( "", "KOSTENART", "") )
	oDefault:Put( #LIEGZ_INDV,             FALSE, TRUE )
	oDefault:Put( #LK1,                    FALSE, TRUE )
	oDefault:Put( #LK2,                    FALSE, TRUE )
	oDefault:Put( #STAT_PLAN,              "", TRUE )
	oDefault:Put( #STATUS_QS,              "F" )
	oDefault:Put( #TRANZ_INDV,             FALSE )
	oDefault:Put( #ZEIT_IST,               0, TRUE )
	oDefault:Put( #ME1,                    oConfigManager:GetStringParam( "", "MENGENEINHEIT", "ST") )
	oDefault:Put( #ME2,                    #ME1 )
	oDefault:Put( #ME3,                    #ME1 )
	oDefault:Put( #ME4,                    #ME1 )
	oDefault:Put( #ME5,                    #ME1 )

	// Aus der Gruppe "GVERGABE"
	oDefault:Put( #AN_KUN_POS,              1, TRUE )
	oDefault:Put( #AN_KUNDE,               "", TRUE )
	oDefault:Put( #AN_LIE_POS,              1, TRUE )
	oDefault:Put( #AN_LIEFER,              "", TRUE )
	oDefault:Put( #KENN_FREMD,             FALSE, TRUE )
	oDefault:Put( #LIE_POS_LS,             1, TRUE )
	oDefault:Put( #LIEFERANT,              "", TRUE )
	oDefault:Put( #MAT_ZU,                 "", TRUE )
	oDefault:Put( #ME,                     #ME1, TRUE )
	oDefault:Put( #MENGE_EH,               0, TRUE )
	oDefault:Put( #MENGEN_BEZ,             "", TRUE )
	oDefault:Put( #MOB_ENTSCH,             FALSE, TRUE )
	oDefault:Put( #PRUEF_KENN,             "N", TRUE )
	oDefault:Put( #WE_MENGE,               0, TRUE )
	oDefault:Put( #ZUAB,                   0, TRUE )

	if( !Empty( cKatalogNr ) )
		// Vorschläge aus dem Arbeitsgangkatalog holen
		oKat := SELF:oBase:GetReadRecordFromTable( #AGKAT, {{ #KAT_ART, cKatArt }, { #KAT_NR, cKatalogNr }}, "Arbeitsganglatalog", TRUE )
		if( oKat != NULL_OBJECT )
			oDefault:Put( #KAT_NR,              oKat:FGet( #KAT_NR ), TRUE )
			oDefault:Put( #BEZEICH,             oKat:FGet( #BEZEICH ), TRUE )
			oDefault:Put( #ARBPLATZ,            oKat:FGet( #ARBPLATZ ), TRUE )
			oDefault:Put( #LGRUPPE,             oKat:FGet( #LGRUPPE ), TRUE )
			oDefault:Put( #ZEITART,             oKat:FGet( #ZEITART ), TRUE )
			oDefault:Put( #ZEIT_EINZ,           oKat:FGet( #ZEIT_EINZ ), TRUE )
			oDefault:Put( #ZEIT_MENGE,          oKat:FGet( #ZEIT_MENGE ), TRUE )
			oDefault:Put( #ZEIT_RUEST,          oKat:FGet( #ZEIT_RUEST ), TRUE )
			oDefault:Put( #TEXTMEMO,            oKat:FGet( #TEXTMEMO ), TRUE )
			oDefault:Put( #KOSTENART,           oKat:FGet( #KOSTENART ), TRUE )
			oDefault:Put( #KENN_FREMD,          oKat:FGet( #KENN_FREMD ), TRUE )
			oDefault:Put( #LIEFERANT,           oKat:FGet( #LIEFERANT ), TRUE )
			oDefault:Put( #LIE_POS_LS,          oKat:FGet( #LIE_POS_LS ), TRUE )
			oDefault:Put( #AN_LIEFER,           oKat:FGet( #AN_LIEFER ), TRUE )
			oDefault:Put( #AN_LIE_POS,          oKat:FGet( #AN_LIE_POS ), TRUE )
			oDefault:Put( #MENGEN_BEZ,          oKat:FGet( #MENGEN_BEZ ), TRUE )
			oDefault:Put( #MENGE_EH,            oKat:FGet( #MENGE_EH ), TRUE )
			oDefault:Put( #WE_MENGE,            oKat:FGet( #WE_MENGE ), TRUE )
			oDefault:Put( #ME,                  oKat:FGet( #ME ), TRUE )
			oDefault:Put( #PREIS_P_EH,          oKat:FGet( #PREIS_P_EH ), TRUE )
			oDefault:Put( #FAKTOR1,             oKat:FGet( #FAKTOR1 ), TRUE )
			oDefault:Put( #FAKTOR2,             oKat:FGet( #FAKTOR2 ), TRUE )
			oDefault:Put( #ZUAB,                oKat:FGet( #ZUAB ), TRUE )
			oDefault:Put( #MAT_ZU,              oKat:FGet( #MAT_ZU ), TRUE )
			oDefault:Put( #PRUEF_KENN,          oKat:FGet( #PRUEF_KENN ), TRUE )
			oDefault:Put( #PRUEFPLAN,           oKat:FGet( #PRUEFPLAN ), TRUE )
			oDefault:Put( #AN_KUNDE,            oKat:FGet( #AN_KUNDE ), TRUE )
			oDefault:Put( #AN_KUN_POS,          oKat:FGet( #AN_KUN_POS ), TRUE )
			oDefault:Put( #BEZ_M1,              oKat:FGet( #BEZ_M1 ), TRUE )
			oDefault:Put( #BEZ_M2,              oKat:FGet( #BEZ_M2 ), TRUE )
			oDefault:Put( #BEZ_M3,              oKat:FGet( #BEZ_M3 ), TRUE )
			oDefault:Put( #BEZ_M4,              oKat:FGet( #BEZ_M4 ), TRUE )
			oDefault:Put( #BEZ_M5,              oKat:FGet( #BEZ_M5 ), TRUE )
			oDefault:Put( #ME1,                 oKat:FGet( #ME1 ), TRUE )
			oDefault:Put( #ME2,                 oKat:FGet( #ME2 ), TRUE )
			oDefault:Put( #ME3,                 oKat:FGet( #ME3 ), TRUE )
			oDefault:Put( #ME4,                 oKat:FGet( #ME4 ), TRUE )
			oDefault:Put( #ME5,                 oKat:FGet( #ME5 ), TRUE )
			oDefault:Put( #AG_PARAM1,           oKat:FGet( #AG_PARAM1 ), TRUE )
			oDefault:Put( #AG_PARAM2,           oKat:FGet( #AG_PARAM2 ), TRUE )
			oDefault:Put( #SUB_MODIFY,          oKat:FGet( #SUB_MODIFY ), TRUE )
			oDefault:Put( #RANG,                oKat:FGet( #RANG ), TRUE )

			// Daten aus dem Arbeitsplatz trotzdem noch ziehen
			cArbeitsplatz := oKat:FGet( #ARBPLATZ )
			oKat:Release()
		endif
	endif

	if( !Empty(cArbeitsplatz) )
		// Vorschlagswerte aus dem Arbeitsplatz holen
		oArbplatz := SELF:oBase:GetReadRecordFromTable( #ARBPLATZ, {{ #ARBPLATZ, cArbeitsplatz }}, "Arbeitsplätze", TRUE )
		if( oArbplatz != NULL_OBJECT )
			oDefault:Put( #ARBPLATZ,                 oArbplatz:FGet( #ARBPLATZ ), TRUE )
			oDefault:Put( #KOSTSTELLE,               oArbplatz:FGet( #KOSTSTELLE ), TRUE )
			if( Empty( oDefault:Get( #KOSTENART ) ) )
				// Kostenart durch den Katalog wieder auf leer gesetzt
				// Nun Kostenart aus Kostenstelle holen
				oDefault:Put( #KOSTENART, IfNil( SELF:oBase:GetFieldFromTable( #KOSTSTELLE, {{ #KOSTSTELLE, oArbplatz:FGet( #KOSTSTELLE ) }}, #KOSTENART, TRUE ), "*** nicht gefunden ***" ) )
			endif
			oDefault:Put( #TEAM,                     oArbplatz:FGet( #TEAM ), TRUE )
         oKoststelle := SELF:oBase:GetReadRecordFromTable( #KOSTSTELLE, {{ #KOSTSTELLE, oArbplatz:FGet( #KOSTSTELLE ) }}, "Kostenstellen", TRUE )
         if( oKoststelle != NULL_OBJECT )
				oDefault:Put( #KOSTENART,             oKoststelle:FGet(#KOSTENART), TRUE )
				oKoststelle:Release()
         endif
			oArbplatz:Release()
		endif
	endif

	oDefault:Put( #RECORDID,  NewID(), TRUE )

	if( oReplaces:Exists( #POS ) .and. oReplaces:IsReplace( #POS ) )
		nPos := oReplaces:Get( #POS )
	endif

   if( !SELF:oBase:IsLocalError( #BaseExtended_GenAPLAP ) )
		oReadRecord := SELF:__GenBase( symAliasAPLAP, {{#BDE_NR,nBdeNr}, { #AGNR, oDefault:Get(#AGNR) }}, aVorlage , oReplaces, oDefault )
	endif
	oDefault:Release()
endif
oStklak:Release()

return( oReadRecord )


METHOD GenSTKLAP( nBdeNr AS INT, cArtikel AS STRING, aVorlage AS ARRAY, oReplaces AS P_BaseRecord, nPosIncrement := 10 AS INT, lAngebotSTKLAK := FALSE AS LOGIC ) AS AReadRecord PASCAL CLASS P_BaseExtended
// Anlegen einer Stücklistenposition aus einer Vorlage und durchführen der notwendigen Ersetzungen und Defaultwerte
// <nBdeNr>        - Die STKLAK-BdeNr zu der die Position angelegt werden soll
// <cArtikel>      - Leer=OTeil, anderfalls eine gültige Artikelnummer
// <aVorlage>      - {} - keine Vorlage, {{ #BDE_NR, nVorlageBdeNr }, { #POS, nVorlagePos }}
// <aReplaces>     - Aufbau {{#FIELD, uContend [, lNecessary]}, {...
// 						#FIELD     : Feldname, z.B. #BDE_NR
//                   uContend   : FeldInhalt, z.B. 100294
//                   lNecessary : Muss gesetzt werden. Es werden Defaultwerte gesetzt, oder Ersetzungen aus dem Teilestamm gemacht.
//                                Wenn der Inhalt dieses Feldes in jedem Fall gesetzt werden soll, ist hier TRUE einzutragen
// [lAngebotSTKLAK]- Für Auftragsstückliste (FALSE) oder für Angebotsstückliste (TRUE)
//
// Return: Die erstellte #POS as AReadRecord

	LOCAL oStklakRecord     AS AReadRecord
	LOCAL oArtikelRecord    AS AReadRecord
	LOCAL symAliasSTKLAK    AS SYMBOL
	LOCAL symAliasSTKLAP    AS SYMBOL
	LOCAL nPos              AS REAL8
    LOCAL oReadRecord       AS AReadRecord
	LOCAL oDefault          AS P_BaseRecord  
	LOCAL oStklapStructure  AS P_BaseRecord

symAliasSTKLAK := IIF( lAngebotSTKLAK, #STKLAK_ANG, #STKLAK )
symAliasSTKLAP := IIF( lAngebotSTKLAK, #STKLAP_ANG, #STKLAP )
oREadRecord    := NULL_OBJECT
oDefault       := P_BaseRecord{ SELF:oBase }
SELF:oBase:ResetLocalError( #BaseExtended_GenSTKLAP ) 

oStklapStructure := P_BaseRecord{ SELF:oBase, #STKLAP }

oStklakRecord := SELF:oBase:GetReadRecordFromTable( symAliasSTKLAK, {{#BDE_NR, nBdeNr}}, "Stückliste" )
if( oStklakRecord != NULL_OBJECT )
	// Defaults setzen
	oDefault:Put(#BDE_NR,     nBdeNr, TRUE )
	oDefault:Put(#POS,        (nPos := SELF:oBase:GetNextPos( symAliasSTKLAP, #POS, "BDE_NR = "+NTrim(nBdeNr), nPosIncrement) ), TRUE )
	oDefault:Put(#AUFTRAG,    oStklakRecord:Fget(#AUFTRAG), TRUE )
	oDefault:Put(#AUFPOS,     oStklakRecord:Fget(#AUFPOS ), TRUE )
	oDefault:Put(#TERM_ENDE,  oStklakRecord:FGet(#TERM_ENDE), TRUE )

	oDefault:Put(#POS_ART, "O")
	oDefault:Put(#ARTIKEL, "" )
	oDefault:Put(#DISPOART, "K")
	oDefault:Put(#ARTIKEL_H,  "")
	oDefault:Put(#KENNUNG_EV, "")
	oDefault:Put(#WERKSTOFF,  "")
	oDefault:Put(#GEWICHT,    0)
	oDefault:Put(#VARIANTE,   "")
	oDefault:Put(#MEMO_TECH,   "")
	oDefault:Put(#LAGER, "" )
	oDefault:Put(#STAT_TB,    "G")
	oDefault:Put(#STAT_AV,    "G")
	oDefault:Put(#STUECK, 1)
	oDefault:Put(#BESTELL_NR, "", TRUE )
	oDefault:Put(#BESTELL_PO, 0 , TRUE )
	oDefault:Put(#MENGE_IST, 0, TRUE )
	oDefault:Put(#MENGE_QS, 0, TRUE )
	oDefault:Put(#MENGE_GEL, 0, TRUE)
	oDefault:Put(#MENGE_COL, 0, TRUE)
	oDefault:Put(#FK_STKL, "N", TRUE)
	oDefault:Put(#DK, "N", TRUE)


	//
	// A-Teile
	//
	if( !Empty(cArtikel) )
		oArtikelRecord := SELF:oBase:GetReadRecordFromTable( #ARTIKEL, {{#ARTIKEL,cArtikel}}, "Artikel" )
		if( oArtikelRecord != NULL_OBJECT )
			// Werte aus Artikel zwingend setzen. Diese können jedoch nochmal
			// durch zwingende <aReplaces> überschrieben werden
			oDefault:Put( #POS_ART,  "A", TRUE )
			oDefault:Put( #ARTIKEL,  oArtikelRecord:Fget(#ARTIKEL), TRUE )
			oDefault:Put( #BEZEICH,  oArtikelRecord:Fget(#BEZEICH), TRUE )
			oDefault:Put( #WERKSTOFF, oArtikelRecord:Fget(#WERKSTOFF), TRUE )
			oDefault:Put( #ZEICHNUNG, oArtikelRecord:Fget(#ZEICHNUNG), TRUE )
			oDefault:Put( #ZEICH_INDX, oArtikelRecord:Fget(#ZEICH_INDX), TRUE )
			oDefault:Put( #ZEICH_FORM, oArtikelRecord:Fget(#ZEICH_FORM), TRUE )
			oDefault:Put( #DISPOART, oArtikelRecord:Fget(#DISPOART), TRUE )
			oDefault:Put( #MEMO_TECH, oArtikelRecord:Fget(#MEMO_TECH), TRUE )
			oDefault:Put( #KOSTENART, oArtikelRecord:Fget(#KOSTENART), TRUE )
			oDefault:Put( #NORM, oArtikelRecord:Fget(#NORM), TRUE )
			oDefault:Put( #LIEFERANT,  oArtikelRecord:Fget(#LIEFERANT), TRUE )
			oDefault:Put( #HERSTELLER, oArtikelRecord:Fget(#HERSTELLER), TRUE )
			oDefault:Put( #ARTIKEL_H,  oArtikelRecord:Fget(#ARTIKEL_H), TRUE )
			oDefault:Put( #PRUEFPLAN, oArtikelRecord:Fget(#PRUEFPLAN), TRUE )
			oDefault:Put( #FORM_KENN, oArtikelRecord:Fget(#FORM_KENN), TRUE )
			oDefault:Put( #ME, oArtikelRecord:Fget(#ME_S), TRUE )
			oDefault:Put( #LAGER, oArtikelRecord:Fget(#LAGER_HPT), TRUE )
			oDefault:Put( #KENNUNG_EV, oArtikelRecord:Fget(#KENNUNG_EV), TRUE )

			if( oStklapStructure:Exists( #URSPRUNG ) )
				// Nur setzen, wenn ich dr Struktur vorhanden
				oDefault:Put( #URSPRUNG, oArtikelRecord:Fget(#URSPRUNG), TRUE )
			endif
			
			if( oStklapStructure:Exists( #INT_WARENR ) )
				// Nur setzen, wenn ich dr Struktur vorhanden
				oDefault:Put( #INT_WARENR, oArtikelRecord:Fget(#INT_WARENR), TRUE )
				oDefault:Put( #WARENNR, oArtikelRecord:Fget(#WARENNR), TRUE )
			endif
			oArtikelRecord:Release()
      endif
   endif

	oDefault:Put( #RECORDID,  NewID(), TRUE )

	if( oReplaces:Exists( #POS ) .and. oReplaces:IsRequired( #POS ) )
		nPos := oReplaces:Get( #POS )
	endif

   if( !SELF:oBase:IsLocalError( #BaseExtended_GenSTKLAP ) )
		oReadRecord := SELF:__GenBase( symAliasSTKLAP, {{#BDE_NR,nBdeNr}, {#POS, nPos}}, aVorlage, oReplaces, oDefault )
	endif

	oStklakRecord:Release()
	oDefault:Release()
endif  
oStklapStructure:Release()
return( oReadRecord )

METHOD GenANGEBOTPOS( cAngebotsNr AS STRING, cArtikel AS STRING, aVorlage AS ARRAY, oReplaces AS P_BaseRecord, nPosIncrement := 10 AS INT ) AS AReadRecord PASCAL CLASS P_BaseExtended
// Anlegen einer Angebotsposition aus einer Vorlage und durchführen der notwendigen Ersetzungen und Defaultwerte
// <cAngebotsNr>   - Das Angebot zu der die Position angelegt werden soll
// <cArtikel>      - Leer=OTeil, anderfalls eine gültige Artikelnummer
// <aVorlage>      - {} - keine Vorlage, {{ #ANGEBOTSNR, cAngebotsNrVorlage }, { #AUFPOS, nVorlageAUFPOS }}
// <aReplaces>     - Aufbau {{#FIELD, uContend [, lNecessary]}, {...
// 						#FIELD     : Feldname, z.B. #BDE_NR
//                   uContend   : FeldInhalt, z.B. 100294
//                   lNecessary : Muss gesetzt werden. Es werden Defaultwerte gesetzt, oder Ersetzungen aus dem Teilestamm gemacht.
//                                Wenn der Inhalt dieses Feldes in jedem Fall gesetzt werden soll, ist hier TRUE einzutragen
//
// Return: Der AReadRecord der angelegten Position

	LOCAL oDefault            AS P_BaseRecord
	LOCAL oAngebot            AS P_BaseRecord
	LOCAL oArtikel            AS P_BaseRecord
	LOCAL oReadRecord         AS AReadRecord

oDefault := P_BaseRecord{ SELF:oBase }
oAngebot := P_BaseRecord{ SELF:oBase }
SELF:oBase:ResetLocalError( #BaseExtended_GenANGEBOTPOS )

if( oAngebot:ImportFromTable( #ANGEBOT, {{ #ANGEBOTSNR, cAngebotsNr }} ) )

	// Werte aus dem Angebot übernehmen, wenn möglich
	oDefault:TableDataEmptyRecord( #ANGEBOTPOS )
	oDefault:Put( #ANGEBOTSNR,     cAngebotsNr, TRUE  )
	if( !oReplaces:Exists( #AUFPOS ) )
		oDefault:Put( #AUFPOS,         SELF:oBase:GetNextPos( #ANGEBOTPOS, #AUFPOS, "ANGEBOTSNR = '"+cAngebotsNr+"'", nPosIncrement ), TRUE )
	endif

	// Defaults setzen (Required)
	oDefault:Put( #ART,            "O", TRUE )
	oDefault:Put( #MENGE,          1, TRUE )
	oDefault:Put( #ME,             oConfigManager:GetStringParam( "", "MENGENEINHEIT", "ST"), TRUE )
	oDefault:Put( #PREIS_EK,       0, TRUE )
	oDefault:Put( #RABATT,         oAngebot:Get(#RABATT_VOR), TRUE )
	oDefault:Put( #PREIS_VAR,      0, TRUE )
	oDefault:Put( #DISPOART,       oConfigManager:GetStringParam( "ANG", "DISPOART", "K"), TRUE )
	oDefault:Put( #LIEFERTERM,     oAngebot:Get(#LIEFERTERM), TRUE )
	oDefault:Put( #POSZUSCHLA,     FALSE, TRUE )
	oDefault:Put( #PG,             oConfigManager:GetStringParam( "ANG", "ANGPOS_PG", ""), TRUE )
	oDefault:Put( #POS_TYP,        oConfigManager:GetStringParam( "ANG", "POSITIONSTYP", "10"), TRUE )
	oDefault:Put( #FABRIK_NR,      oAngebot:Get(#FABRIK_NR) )
	oDefault:Put( #NEP,            "J", TRUE )
	oDefault:Put( #NEP_ART,        "" )
	oDefault:Put( #RABATT_KEN,     "R", TRUE )
	oDefault:Put( #LIEFERTERM,     oAngebot:Get( #LIEFERTERM ), TRUE )

   if( !Empty( cArtikel ) )
		// Werte aus Artikelstamm übernehmen, wenn gewünscht
		// mit "TRUE" angegebene Werte sind zwingende Ersetzungen
		// (Required)
		oArtikel := P_BaseRecord{ SELF:oBase }
		if( oArtikel:ImportFromTable( #ARTIKEL, {{ #ARTIKEL, cArtikel }} ) )
	   	oDefault:Put( #ART,            "A", TRUE )
			oDefault:Put( #ARTIKEL,        oArtikel:Get(#ARTIKEL), TRUE )
			oDefault:Put( #LAGER,          oArtikel:Get(#LAGER_HPT), TRUE )
			oDefault:Put( #NEP,            "N", TRUE )

			oDefault:Put( #ARTIKEL_KU,     #ARTIKEL_KU )
		endif
		oArtikel:Release()
	endif

	oDefault:Put( #RECORDID,  NewID(), TRUE )

	// Nun Daten ersetzen.
   if( !SELF:oBase:IsLocalError( #BaseExtended_GenANGEBOTPOS ) )
		oReadRecord := SELF:__GenBase( #ANGEBOTPOS,  {{ #ANGEBOTSNR, cAngebotsNr }, { #AUFPOS, oDefault:Get( #AUFPOS ) }}, {}, oReplaces, oDefault )
	endif
endif
oAngebot:Release()
oDefault:Release()
return( oReadRecord )

METHOD GenARTIKEL( cArtikel AS STRING, aVorlage AS ARRAY, oReplaces AS P_BaseRecord, lBestandAnlegen := TRUE AS LOGIC ) AS AReadRecord PASCAL CLASS P_BaseExtended
// Wird Artielnummer Leer "" übergeben so wird die Artikelnummer aus den Belegnummern generiert.

	LOCAL oDefault            AS P_BaseRecord
	LOCAL oReadRecord, oTmp   AS AReadRecord
	LOCAL oRepBestand         AS P_BaseRecord  
	LOCAL cLager, cME         AS STRING

SELF:oBase:ResetLocalError( #BaseExtended_GenARTIKEL )

oDefault := P_BaseRecord{ SELF:oBase }


//
// Artikel 
//
if( Empty( cArtikel ) )
	cArtikel := NTrim(SELF:oBase:GetBelegNr( "AN", "Base" ))
endif   
oDefault:Put( #ARTIKEL,                  cArtikel, TRUE )


//
// Lager 
//
if( oReplaces:Exists( #LAGER_HPT ) .and. !Empty( oReplaces:Get(#LAGER_HPT) ) )
	cLager := oReplaces:Get( #LAGER_HPT )
else
	cLager := oConfigManager:GetStringParam(CFG_BEREICH_ALLGEMEIN, "LAGER_HAUPT", "001"	)
endif 	
oDefault:Put( #LAGER_HPT, cLager, TRUE )    
SELF:oBase:CheckFieldsFound( #LAGER, "Hauptlager", {{ #LAGER, cLager }} )  
               
cME := oConfigManager:GetStringParam(CFG_BEREICH_ALLGEMEIN, "MENGENEINHEIT", "ST")
oDefault:Put(#ME_L      , cME)
oDefault:Put(#ME_B      , cME)
oDefault:Put(#ME_S      , cME)
oDefault:Put(#ME_PREIS  , cME)
oDefault:Put(#UmFkt_ME_B, 1)
oDefault:Put(#UmFkt_ME_S, 1)
oDefault:Put(#UmFkt_ME_P, 1)

oDefault:Put(#Kalk_Menge, 1)
//oDefault:Put(#Konto     , oConfigManager:GetIntParam(CFG_BEREICH_MAT, "KONTO_BESTAND", 0))
oDefault:Put(#Konto     , SELF:oBase:GetConfigParam( INT, CFG_BEREICH_MAT, "KONTO_BESTAND", 0,  #KONTO, #KONTO, "Konten"))
oDefault:Put(#Lager_Hpt , oConfigManager:GetStringParam(CFG_BEREICH_ALLGEMEIN, "LAGER_HAUPT", "001"))
oDefault:Put(#LagerArt  , "N")
oDefault:Put(#LosGroesse, 1)
oDefault:Put(#ChargenVer, "N")
oDefault:Put(#DispoArt  , "K")
oDefault:Put(#Disponent , oConfigManager:GetStringParam("", "DISPONENT", ""))
//oDefault:Put(#KostenArt , oConfigManager:GetStringParam("", "KOSTENART", ""))  
//SELF:oBase:CheckFieldsFound( #KOSTENART, "Kostenarten", {{ #KOSTENART, oDefault:Get( #KOSTENART ) }}  
oDefault:Put(#KostenArt , SELF:oBase:GetConfigParam( STRING, "", "KOSTENART", "",  #KOSTENART, #KOSTENART, "Kostenarten"))  
oDefault:Put(#Stat_Artik, "F")
oDefault:Put(#Strat_Disp, "D")  
oDefault:Put(#Colli_VERS, FALSE )
oDefault:Put(#Sammeldru,  FALSE )    
oDefault:Put(#DOKU,       TRUE )      
oDefault:Put(#PERIODENBZ, "L" ) 
oDefault:Put(#EINH_LAGER, 1 ) 

oDefault:Put( #RECORDID,  NewID(), TRUE )

//
// Auf Platte schreiben 
//
oReadRecord := NULL_OBJECT
if( !SELF:oBase:IsLocalError( #BaseExtended_GenARTIKEL ) )
	oReadRecord := SELF:__GenBase( #ARTIKEL,  {{ #ARTIKEL, cArtikel }}, {}, oReplaces, oDefault )

	if( oReadRecord != NULL_OBJECT .and. lBestandAnlegen )
		// Bestandssatz anlegen
		oRepBestand := P_BaseRecord{ SELF:oBase }
		oRepBestand:Put( #ARTIKEL,   oReadRecord:FGet(#ARTIKEL), TRUE )
		oTmp := SELF:GenBESTAND( oReadRecord:FGet(#ARTIKEL), oReadRecord:FGet(#LAGER_HPT), {}, oRepBestand )
		if( oTmp != NULL_OBJECT )
			oTmp:Release()
		endif
		oRepBestand:Release()
	endif
endif

oDefault:Release()
return( oReadRecord )

METHOD GenBESTAND( cArtikel AS STRING, cLager AS STRING, aVorlage AS ARRAY, oReplaces AS P_BaseRecord ) AS AReadRecord PASCAL CLASS P_BaseExtended
//
// Artikel muss übergeben werden. 
// Wird ein leeres Lager übergeben, so wird das Default-Lager aus dem Konfigurationsparameter angelegt

	LOCAL oDefault            AS P_BaseRecord
	LOCAL oReadRecord         AS AReadRecord

SELF:oBase:ResetLocalError( #BaseExtended_GenBESTAND )

oDefault := P_BaseRecord{ SELF:oBase }
oDefault:Put( #ARTIKEL,                  cArtikel, TRUE )
if( Empty( cLager ) )
	cLager := oConfigManager:GetStringParam(CFG_BEREICH_ALLGEMEIN, "LAGER_HAUPT", "001"	)
endif	
oDefault:Put( #LAGER,                    cLager,   TRUE ) 
oDefault:Put( #LAGERORT,                 "" ) 
oDefault:Put( #ABWERT_FKT,               1 )
oDefault:Put( #BSTAND_J_A,               0 )
oDefault:Put( #KONTO,                    oConfigManager:GetIntParam(CFG_BEREICH_MAT, "KONTO_BESTAND", 0))       
oDefault:Put( #STAT_LAG,                 "" )                        
SELF:oBase:CheckFieldsFound( #LAGER, "Lager", {{ #LAGER, cLager }} )
/*
oTmp := SELF:oBase:GetReadRecordFromTable( #LAGER, {{#LAGER, cLager}}, "Artikel-Lager" )
if( oTmp != NULL_OBJECT )
	oTmp:Release()
endif
*/
oDefault:Put( #RECORDID,  NewID(), TRUE )

// Nun Daten ersetzen.
if( !SELF:oBase:IsLocalError( #BaseExtended_GenBESTAND ) )
	oReadRecord := SELF:__GenBase( #BESTAND,  {{ #ARTIKEL, cArtikel }, {#LAGER, cLager }}, {}, oReplaces, oDefault )
endif

oDefault:Release()
return( oReadRecord )


ACCESS oBase AS P_Base PASCAL CLASS P_BaseExtended
return( SELF:__oBase )


METHOD AngebotWertermittlung(cAngebot AS STRING, fAufPos  := 0 AS REAL8) AS LOGIC PASCAL CLASS P_BaseExtended

	LOCAL lSuccess := FALSE AS LOGIC
	LOCAL oAngebot          AS Angebot

	oAngebot := Angebot{cAngebot, SELF:oBase:oProtocol, "Base"}
	lSuccess := oAngebot:ANGWertermittlung(SELF:oBase:oTransactionManager, fAufPos)
	oAngebot:Release()

RETURN lSuccess 

METHOD AuftragWertermittlung( cAuftrag ) AS LOGIC PASCAL CLASS P_BaseExtended

	LOCAL lSuccess := TRUE  AS LOGIC
	LOCAL oAuftrag           AS Auftrag

oAuftrag := Auftrag{SELF:oBase:oTransactionManager, cAuftrag, FALSE}
if( ! oAuftrag:KAVWertFKErmittlung(FALSE) )
	lSuccess := FALSE
	SELF:oBase:Message( GetText(TXT_WERTERMITTLUNG_FUER_1, {cAuftrag}) + CRLF + oAuftrag:@@Status:GetMessage(), PROT_ART_ERROR )
else
	if( !oAuftrag:ValidAuftrag )
		SELF:oBase:Message( GetText(TXT_WERTERMITTLUNG_FUER_1, {cAuftrag}) + CRLF + oAuftrag:@@Status:GetMessage(), PROT_ART_ERROR )
	endif
endif
oAuftrag:Release()
return( lSuccess )
 
METHOD GetSTKLAKSQLTeilBaum( nBdeNr AS INT ) AS ASqlStatement PASCAL CLASS P_BaseExtended
// Gibt den Teilbaum einer Stückliste nach unten und oben zurück.
// a
// --b
//   --d
//     --e
// --c
//
// Beispiel: GetStklak( d ) liefert folgende SQL-Zeilen:
// 1.  a, 'U'  - Up
// 2.  b, 'U'  - Up 
// 3.  d, 'M'  - Me
// 4.  e, 'D'  - Down  
//
// Ist ein Fehler aufgetreten, so wird NULL_OBJECT zurückgegeben.

return( SELF:oBase:CreateSQLStatement( "select bde_nr, 'U' as dir from v_prm_teilbaum where bde_nr_teilbaum = "+nTrim(nBdeNr)+" and bde_Nr != "+nTrim(nBdeNr) + CRLF +;
										"union all "+ CRLF +;
										"select "+nTrim(nBdeNr)+" as bde_nr, 'M' as dir "+ CRLF +;
										"union all "+ CRLF +;
										"select bde_nr_teilbaum as bde_nr, 'D' as dir from v_prm_teilbaum where bde_nr = "+nTrim(nBdeNr)+" and bde_nr_teilbaum != "+nTrim(nBdeNr) + CRLF,;
										"Fehler bei der Ermittlung des Teilbaums zur Bde-Nr "+nTrim(nBdeNr), TRUE ) )
 
METHOD GetSTKLAKArrayTeilBaum( nBdeNr AS INT ) AS ARRAY PASCAL CLASS P_BaseExtended
// Gibt den Teilbaum einer Stückliste nach unten und oben zurück.
// a
// --b
//   --d
//     --e
// --c
//
// Beispiel: GetStklak( d ) liefert folgendes Array:
// { {a,#Up}, {b,#Up}, {d,#Me},{e,#Down} }
// #Up   - Die Bde-Nrn über mir
// #Me   - Mein Einstieg
// #Down - Die Bde-Nrn unter mir

	LOCAL oStmt             AS ASQLStatement
	LOCAL aResult           AS ARRAY
	
aResult := {}
oStmt := SELF:GetStklakSqlTeilBaum( nBdeNr )
if( oStmt != NULL_OBJECT )  
	do while( oStmt:Fetch() )
		aadd( aResult, { oStmt:FGetN(#BDE_NR), IIF( oStmt:FGetN(#DIR) == 'M', #Me, IIF( oStmt:FGetN(#DIR) == "U", #Up, #Down ) ) } ) 
	enddo
endif
return( aResult )


METHOD GetBdeNrTB( nBdeNr AS INT, symStklak := #STKLAK AS SYMBOL ) AS INT PASCAL CLASS P_BaseExtended
// Ermittelt die Bde-Nr-TB zur übergebenen Bde-Nr. Mit symStklak := #STKLAK_ANG kann die Angebotsstückliste angesprochen werden
return( SELF:oBase:GetFieldFromTable( symStklak, {{ #BDE_NR, nBdeNr }}, #BDE_NR_TB, TRUE ) )



















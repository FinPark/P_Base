TEXTBLOCK == P_
//
// Autor: André Finken
//
// Benötigt P_Base.prg
//
// Grundlage für ein object, welches das P_Baseobjekt benutzt,
// jeodch nicht von ihm angeleitet ist
//
// Wichtig:
// Die Zeilen mit // REPLACE müssen ggf. überarbeitet werden
//
// Beispiel für Aufruf:
/*
	LOCAL oInterface          AS P_BaseInterface

oInterface := P_BaseInterface{}
oInterface:oBase:oCommConnect := _oCommConnect
oInterface:Import()
SELF:_QueueBFCRefresh()
oInterface:oBase:ShowProtIfError( _oCommConnect, "Fehler beim importieren der RE-Belege", FALSE )
oInterface:Release()
*/

CLASS P_SNT INHERIT P_BaseInterface
     
PROTECT METHOD _ReadConfiguration() AS VOID PASCAL CLASS P_BaseInterface

     SELF:lRollBackOnError       := TRUE
     SELF:lDeleteFileAfterImport := FALSE
     SELF:lMoveFileAfterImport   := FALSE
     SELF:cFolderOnSuccess       := ""
     SELF:cFolderOnError         := ""
     SELF:cProtokollHeader       := "Import" 

PROTECT METHOD _Import( cFileName AS STRING ) AS LOGIC PASCAL CLASS P_BaseInterface

     LOCAL oFile                AS P_BaseASCIICsv
     LOCAL nRow                 AS INT

     oFile := P_BaseASCIICsv{}
     oFile:lFirstRowGotHeader := TRUE
     oFile:cSeparator := ";"
     if( oFile:ReadFromFile( cFileName ) )
          if( oFile:Count() != 0 )
               for nRow := 1 upto oFile:Count()
                    SELF:oBase:ProgressIncrement( "Importiere Datei " + SELF:oBase:GetFileNameFromPath(cFileName) + ". Verarbeite Zeile "+NTrim(nRow)  )
               
               next nRow
          else
               SELF:oBase:MessageFormat( "In der Datei # befinden sich keine verwertbaren Daten. Anzahl Zeilen=#", { cFileName, oFile:Count() }, PROT_ART_ERROR )
          endif
     endif
     oFile:Release()

return( SELF:oBase:IsLocalError( #FileImport ) )     
     
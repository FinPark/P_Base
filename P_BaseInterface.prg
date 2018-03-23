TEXTBLOCK == P_BaseInterface
//
// Autor: André Finken
//
// Benötigt P_Base.prg
// Grundobjekt für Schnittstellendateien
// Die Methoden _Import() und _ReadConfiguration() müssen bei Ableitung überschrieben werden
//

CLASS P_BaseInterface INHERIT AObject

	DECLARE METHOD  ImportFolder
	DECLARE METHOD  ImportFile
	DECLARE METHOD  ReadConfiguration
	EXPORT          lRollBackOnError                 AS LOGIC
	EXPORT          lDeleteFileAfterSuccessImport    AS LOGIC
	EXPORT          lDeleteFileAfterFailedImport     AS LOGIC
	EXPORT          lMoveFileAfterSuccessImport      AS LOGIC
	EXPORT          lMoveFileAfterFailedImport       AS LOGIC
	EXPORT          cFolderOnSuccess                 AS STRING
	EXPORT          cFolderOnError                   AS STRING
	EXPORT          cProtokollHeader := ""           AS STRING

	DECLARE ACCESS  oBase
	DECLARE ACCESS  lError


 	/* Interne Methoden */
 	DECLARE METHOD  _Import
	DECLARE METHOD  _ReadConfiguration
	PROTECT         __oBase                          AS P_Base
	PROTECT         __lConfigLoaded                  AS LOGIC

METHOD Init( oP_Base, oCommConnect, cProtokollHeader ) CLASS P_BaseInterface

	SUPER:Init()

	if( !IsNil( oP_Base ) .and. IsInstanceOfUsual(oP_Base, #P_Base))
		SELF:__oBase := oP_Base
		SELF:__oBase:AddRef()
	else
		SELF:__oBase := P_Base{}
	endif

	if( !IsNil(oCommConnect) )
		SELF:__oBase:oCommConnect := oCommConnect
	endif


	SELF:__lConfigLoaded               := FALSE
	SELF:lRollBackOnError              := TRUE
	SELF:lDeleteFileAfterSuccessImport := FALSE
	SELF:lDeleteFileAfterFailedImport  := FALSE
	SELF:lMoveFileAfterSuccessImport   := FALSE
	SELF:lMoveFileAfterFailedImport    := FALSE
	SELF:cFolderOnSuccess              := ""
	SELF:cFolderOnError                := ""
	SELF:cProtokollHeader              := IfNil( cProtokollHeader, "Import" )

	SELF:ReadConfiguration()

METHOD Destroy() AS VOID PASCAL CLASS P_BaseInterface

	SELF:oBase:Release()
	SUPER:Destroy()


ACCESS oBase AS P_Base PASCAL CLASS P_BaseInterface
return( SELF:__oBase )

ACCESS lError AS LOGIC PASCAL CLASS P_BaseInterface
return( SELF:__oBase:lError )

METHOD ReadConfiguration() AS VOID PASCAL CLASS P_BaseInterface
// Wird beim Init gerufen
SELF:oBase:CreateProtocol( "SNT", SELF:cProtokollHeader, TRUE )
SELF:__lConfigLoaded := TRUE
SELF:_ReadConfiguration()

METHOD ImportFolder( cFolder AS STRING, cFilePattern AS STRING, lSubFolder := FALSE AS LOGIC ) AS LOGIC PASCAL CLASS P_BaseInterface

	LOCAL aFiles           AS ARRAY
	LOCAL nFile            AS INT

if( !SELF:__lConfigLoaded )
	SELF:ReadConfiguration()
endif

aFiles       := SELF:oBase:GetFilesFromDir( cFolder, cFilePattern, lSubFolder )
for nFile := 1 upto ALen( aFiles )
	SELF:ImportFile( aFiles[nFile] )
next x

return( !SELF:lError )

METHOD ImportFile( cFileName AS STRING ) AS LOGIC PASCAL CLASS P_BaseInterface

	LOCAL lTransactionStatus   AS LOGIC
	LOCAL cNewFileName         AS STRING

if( !SELF:__lConfigLoaded )
	SELF:ReadConfiguration()
endif

SELF:oBase:ProgressIncrement( "Importiere Datei " + SELF:oBase:GetFileNameFromPath(cFileName) + ". Lade Inhalt"  )
SELF:oBase:ResetLocalError( #FileImport )
if( SELF:oBase:FileExists( cFileName ) )

	if( SELF:oBase:BeginTransaction() )
		//
		// Datei lesen
		//
		SELF:_Import( cFileName )

	   //
	   // Transaction schreiben
		//
		lTransactionStatus := IIF( SELF:lRollBackOnError, !SELF:oBase:IsLocalError( #FileImport ) , TRUE )
		SELF:oBase:EndTransaction( lTransactionStatus )

     	//
     	// Datei verschieben/löschen
     	//
		if( SELF:lError .and. SELF:lMoveFileAfterFailedImport) .or. ( !SELF:lError .and. SELF:lMoveFileAfterSuccessImport )
			cNewFileName := SELF:oBase:FolderReplace( cFileName, IIF( SELF:oBase:IsLocalError( #FileImport ), SELF:cFolderOnError, SELF:cFolderOnSuccess ) )
			if( SELF:oBase:FileCopy( cFileName, cNewFileName ) )
				SELF:oBase:FileDelete( cFileName )
			endif
		elseif( SELF:lError .and. SELF:lDeleteFileAfterFailedImport) .or. ( !SELF:lError .and. SELF:lDeleteFileAfterSuccessImport )
	   	// Datei importiert. Nun ggf. löschen
	   	SELF:oBase:FileDelete( cFileName )
		endif

	endif
else
	SELF:oBase:MessageFormat( "Fehler beim einlesen der Datei #. Die Datei # ist nicht vorhanden.", { SELF:oBase:GetFileNameFromPath(cFileName), cFileName }, PROT_ART_ERROR )
endif
return( SELF:oBase:IsLocalError( #FileImport ) )

PROTECT METHOD _ReadConfiguration() AS VOID PASCAL CLASS P_BaseInterface
//
// Diese Methode muss bei Ableitung überschrieben werden
//

PROTECT METHOD _Import( cFileName AS STRING ) AS LOGIC PASCAL CLASS P_BaseInterface
//
// Diese Methode muss bei Ableitung überschrieben werden
//
return( SELF:oBase:IsLocalError( #FileImport ) )

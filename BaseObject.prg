
                                                                                                                                    /*

  	BaseObject.prg
  	André Finken, 2019
  	
  	   

*/

CLASS BaseObject INHERIT AObject

	PROTECT __oTransactionManager                 AS ServerManager 
	PROTECT __lReleaseTransactionManager          AS LOGIC
	PROTECT __lReleaseStatusStack                 AS LOGIC
    PROTECT __oMsgStack                           AS AStatusStack
       
	EXPORT         uInitialCacheValue             AS USUAL
	DECLARE ACCESS oTransactionManager
	DECLARE ASSIGN oTransactionManager 
	DECLARE ACCESS oMsgStack
	DECLARE ASSIGN oMsgStack 

METHOD Init( oTransactionManager ) CLASS BaseObject

	SUPER:Init()

	uInitialCacheValue := nil

	__oTransactionManager := NULL_OBJECT    
	__lReleaseTransactionManager := FALSE  
	__oMsgStack := NULL_OBJECT
	__lReleaseStatusStack := FALSE
		
	if( !IsNil( oTransactionManager ) )
		if( IsInstanceOfUsual( oTransactionManager, #ServerManager ) )
			__oTransactionManager := oTransactionManager
			__lReleaseTransactionManager := TRUE
		else
			AddStatusRecord(__oMsgStack, SUBSYSTEM_AMS,, SEVERITY_ERROR,,"Fehler in BaseObject: Der initial zugewiesene Transactionsmanager ist Fehlerhaft. Object stammt nicht von ServerManager ab.")			
		endif
	endif
    
METHOD Destroy() AS VOID PASCAL CLASS BaseObject
   
	if( __lReleaseStatusStack )
		__oMsgStack:Release()
	endif
	
	if( __lReleaseTransactionManager )
		__oTransactionManager:Release()
	endif
	
	SUPER:Destroy()

    
ASSIGN oTransactionManager( oTxnMgr AS ServerManager ) AS ServerManager PASCAL CLASS BaseObject

	if( __oTransactionManager != NULL_OBJECT  )
		AddStatusRecord(SELF:oMsgStack, SUBSYSTEM_AMS,, SEVERITY_ERROR,,"BaseObject: The transaction manager has been assigned multiple times! The TransactionManager can be assigned at Object-Initialization.")
		__oTransactionManager:Release()
		__lReleaseTransactionManager := FALSE
	endif
	
	__oTransactionManager := oTxnMgr

return( __oTransactionManager )


ACCESS oTransactionManager AS ServerManager PASCAL CLASS BaseObject

	if( __oTransactionManager == NULL_OBJECT )
		__oTransactionManager := ServerManager{}
		__lReleaseTransactionManager := TRUE
	endif

return( __oTransactionManager )   

ASSIGN oMsgStack( oStatusStack AS AStatusStack ) AS AStatusStack PASCAL CLASS BaseObject

	if( __oMsgStack == NULL_OBJECT )
		__oMsgStack := oStatusStack
		__lReleaseStatusStack := FALSE 
	else
		if( __lReleaseStatusStack )
			// Der aktuelle StatusStack wurde von diesem Object erstellt, daher kann
			// er auch gelöscht werden. Die Infomationen werden nun in den neuen 
			// StatusStack übertragen und eine Warnung wird ausgegeben. 
			__oMsgStack:TransmitTo( oStatusStack )
			__oMsgStack:Release()
			__oMsgStack := oStatusStack   
			AddStatusRecord( __oMsgStack, SUBSYSTEM_AMS,, SEVERITY_WARNING,,"BaseObject: The status stack was already assigned and has been overwritten." )
			__lReleaseStatusStack := FALSE
		else
		endif
	endif

return( __oMsgStack )
			
ACCESS oMsgStack AS AStatusStack PASCAL CLASS BaseObject

if( __oMsgStack == NULL_OBJECT )
	__oMsgStack := AStatusStack{}
	__lReleaseStatusStack := TRUE
endif
return( __oMsgStack ) 


       

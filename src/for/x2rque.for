C
C SUBROUTINE X2RQUE
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RQUE.FOV                                   $
C  $Date::   17 Apr 1996 16:32:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2xrel.for **
C
C V02 05-DEC-94 SCD Integrate UK changes into X2X Baseline
C
C X2RQUE.FOR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C     X2RQUE(PROBUF,PROCESS)    
C       PURPOSE:
C          PLACES BUFFER ONTO APPROPRIATE QUEUE (X2XMGR OR X2XRAPP)
C
C       INPUT VARIABLES:
C          PROBUF  - BUFFER NUMBER
C          PROCESS - RELAY PROCESS NUMBER
C 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE X2RQUE(PROBUF,PROCESS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
C 
	INTEGER*4 ST, PROCESS, PROBUF
C
C=====================================================================
C       X2X MUST BE UP TO PROCESS BUFFER IF NOT RELEASE THE BUFFER
C=====================================================================
C
	IF (X2X_GAME_STATE.NE.X2X_GAMES_UP .AND.
     *	    X2X_GAME_STATE.NE.X2X_GAMES_REQUP) THEN
	   CALL RELBUF(PROBUF)
	   RETURN
	ENDIF
C 
C=====================================================================
C       THOSE MESSAGES THAT HAVE A LENGTH OR MESSAGE NUMBER GET SENT
C       TO X2XPRO. THIS INDICATES A SINGLE MESSAGE BROADCAST.
C       THOSE THAT DON'T GET ADDED TO THE BOTTOM OF THE 
C       X2XRAPP QUE. THIS INDICATES A MULTI-MESSAGE BROADCAST.
C
C=====================================================================
	IF (X2XR_APP_DATA_LEN(PROCESS).NE.0 .OR.
     *	    X2XR_APP_DATA_MSGNUM(PROCESS).GT.0) THEN
C
C          THESE ARE TO BE SENT TO X2XMGR (SINGLE MESSAGES)
C
	   CALL X2ADDPRO(PROBUF)
           IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
	      TYPE*,'ADDING TO X2XMGR BUF ',PROBUF
CV02	      CALL PRTOUT(PROBUF)
           ENDIF
	ELSE
C
C          THESE MESSAGES ARE MULTI-MESSAGE BROACASTS
C
           IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
              TYPE*,'ADDING TO X2XRAPP BUF ',PROBUF
CV02	      CALL PRTOUT(PROBUF)
   	    ENDIF
            CALL ABL(PROBUF,X2XR_APP_QUEUE(1,PROCESS),ST)
  	ENDIF
	RETURN
	END

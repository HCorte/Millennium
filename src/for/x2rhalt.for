C
C SUBROUTINE X2RHALT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RHALT.FOV                                  $
C  $Date::   17 Apr 1996 16:31:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2xrel.for **
C
C V01 19-JUL-94 WS MULTINETWORK CHANGES - Integrate UK changes 
C		   into X2X Baseline
C 
C X2RHALT.FOR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C     SUBROUTINE:
C        X2HALT(DESTINATION,PROCESS) 
C
C     PURPOSE:
C        STOPS A PROCESS BY DESTINATION
C        BUILDS STOP COMMAND 
C
C     INPUT:
C       DESTINATION  -     STATION TO BE TIMED OUT
C                          (-1 = ALL, ELSE STATION OR GROUP NUMBER)
C       PROCESS      -     PROCESS NUMBER TO BE STOPPED
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
	SUBROUTINE X2RHALT(DESTINATION,PROCESS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2RCMD.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4 BUFFER(32), ST, PROCESS, DESTINATION
	INTEGER*4 SUBNETWORK				!V01
C 
	CALL ISBYTE(X2RCMD_STOP,BUFFER,X2ROFF_CMD-1)
	CALL ISBYTE(PROCESS,BUFFER,X2ROFF_PROCESS-1)
	CALL I4TOBUF4(DESTINATION,BUFFER,X2ROFF_DEST-1)
C 
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
           TYPE *,'X2RHALT, PROCESS ',PROCESS,
     *            ' DESTINATION ',DESTINATION
        ENDIF
C
CV01	CALL X2RCMD(BUFFER,PROCESS,ST)
	CALL X2RCMD(BUFFER,PROCESS,SUBNETWORK,ST)	!V01
C
	RETURN
	END

C GET_X2X_COMM_STATE
C
C V02 06-JAN-2011 FJG MILLENNIUM MXSRV
C 
C=============================================================================
C
C  This item is the property of GTECH Corporation, West Greewich, Rhode
C  Island, and contains confidential and trade secret information. It may
C  not be transferred from the custody or control of GTECH except as
C  authorized in writing by an officer of GTECH. Neither this item not the
C  information it contains may be used, transferred, reproduced, published
C  or disclosed, in whole or in part, and directly or indirectly, except
C  as expressly authorized by an officer of GTECH, pursuant to written
C  agreement.
C
C  Any and all modifications to this item must have the prior written         
C  authorization of GTECH's Enterprise Series Platform Team.  GTECH shall     
C  not be liable in any way for any direct or indirect damages,  whatsoever,  
C  as a result of any unauthorized modifications.  The Enterprise Series      
C  Platform Team reserves the right to refuse support as a result of          
C  unauthorized modification.
C
C  Copyright 2003 GTECH Corporation. All rights reserved.
C
C=====[GET_X2X_COMM_STATE.FOR]================================================
C
C     THIS SUBROUTINE DETERMINES IS X2X COMMUNICATIONS ARE ENABLED.
C
C    INPUT
C    *****
C    NONE
C
C    OUTPUT
C    *****
C    COMMUNICATIONS STATE   0 NOT ENABLED
C                           1 ENABLED
C                          -1 SHUTDOWN
C
C=====[GET_X2X_COMM_STATE.FOR]================================================
C
	OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GET_X2X_COMM_STATE (COMM_STATE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4	COMM_STATE
C
        COMM_STATE = 0
C
        IF (X2X_GAME_STATE .EQ. X2X_GAMES_UP) THEN
C
            COMM_STATE = 1

        ELSEIF (X2X_GAME_STATE .EQ. X2X_GAMES_SHUTDOWN) THEN
C
            COMM_STATE = -1
        ENDIF
C
	RETURN
	END

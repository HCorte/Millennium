C QUEUE_TO_GAME
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
C=====[QUEUE_TO_GAME.FOR]=====================================================
C
C  FUNCTION NAME AND CALLING PARAMETERS GO HERE
C
C  Purpose:
C
C  Input Arguments:
C
C  Output Arguments:
C
C  Return Value:
C
C  Assumptions:
C
C=====[QUEUE_TO_GAME.FOR]=====================================================
C
        OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE QUEUE_TO_GAME(TERMINAL_NO,
     *                           MESSAGE_LENGTH,
     *                           MESSAGE,
     *                           RETURN_STATUS)
        IMPLICIT NONE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       INCLUDE FILES.
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       LOCAL DECLARATIONS.
C
        BYTE            MESSAGE(*)
        INTEGER*4       TERMINAL_NO
        INTEGER*4       MESSAGE_LENGTH
        INTEGER*4       RETURN_STATUS
        INTEGER*4       ST                      ! Local status
        INTEGER*4       IDX                     ! Local index
        INTEGER*4       PROBUF                  ! Procom buffer number
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        IF ((TERMINAL_NO.LE.0).OR.              ! Validate terminal number
     *      (TERMINAL_NO.GT.X2X_TERMS)) THEN
C
          RETURN_STATUS = -1
          RETURN
C
        ENDIF                                   ! End validate terminal number
C
        IF ((MESSAGE_LENGTH .GT. OUTLEN_MAX).OR. ! Validate message length
     *      (MESSAGE_LENGTH .LE. 0)) THEN
C
          RETURN_STATUS = -2
          RETURN
C
        ENDIF                                   ! End validate message length
C
        IF (X2X_GAME_STATE.NE.X2X_GAMES_UP) THEN ! Communications not enabled
C
          RETURN_STATUS = -6
          RETURN
C
        ENDIF                                   ! End validate message length
C
        CALL GETBUF(PROBUF)                     ! Get a procom buffer
C
        IF (PROBUF.GT.0) THEN                   ! Procom buffer available
C
          DO IDX = 0, (MESSAGE_LENGTH - 1)      ! Put terminal message in
                                                ! procom buffer
            BPRO((BINPTAB + IDX),PROBUF) =
     *        MESSAGE(IDX + 1)
C
          END DO                                ! End put term msg in procom
C
          IF (X2X_I4_STATION) THEN
            PRO(TERNUM,PROBUF)=TERMINAL_NO
            PRO(LINENO,PROBUF)=0
          ELSE
            HPRO(TERNUM,PROBUF)=TERMINAL_NO
            HPRO(LINENO,PROBUF)=0
          ENDIF
C
          HPRO(PRCSRC,PROBUF)=MXS_COM
          HPRO(PRCDST,PROBUF)=0
          HPRO(X2X_CONNCTL_OVR,PROBUF)=0
          HPRO(X2X_DELIVER_OVR,PROBUF)=0
          HPRO(X2X_HOST_ID,PROBUF)=0
          HPRO(TRCODE,PROBUF)=TYPREG
          HPRO(QUENUM,PROBUF)=QIN
          HPRO(MSGNUM,PROBUF)=0
          PRO(TIMOFF,PROBUF)=X2X_LOOP_TIME
          HPRO(INPLEN,PROBUF)=MESSAGE_LENGTH
          HPRO(X2X_DEST,PROBUF)=0
          HPRO(X2X_LINK,PROBUF)=0
C
          IF (X2XT_PRO(TERMINAL_NO).NE.0) THEN
C
            RETURN_STATUS = -3
C
            IF (X2XT_PRO(TERMINAL_NO).EQ.1) THEN
              RETURN_STATUS = -4
            ENDIF
C
            X2XT_PRO(TERMINAL_NO) = X2XT_PRO(TERMINAL_NO) + 1
            CALL X2RELBUF(PROBUF)
C
          ELSE
C
            X2XT_PRO(TERMINAL_NO) = 1
            HPRO(X2X_LINK,PROBUF)=-1
            CALL QUEINP(PROBUF,ST)
            RETURN_STATUS = 1
          ENDIF
C
        ELSE
C
          RETURN_STATUS = -5
        ENDIF
C
        RETURN
        END

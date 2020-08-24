C DEQUEUE_FROM_GAME
C
C V02 06-JAN-2011 FJG MILLENNIUM MXSRV
C 
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
C=====[DEQUEUE_FROM_GAME.FOR]=================================================
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
C=====[DEQUEUE_FROM_GAME.FOR]=================================================
C
        OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE DEQUEUE_FROM_GAME(TERMINAL_NO,
     *                               MESSAGE_LENGTH,
     *                               MSG_TYPE,
     *                               PROBUF_NUM,
     *                               REPLY_QUE,
     *                               CDC,
     *                               APP_NAME,
     *                               MESSAGE)
        IMPLICIT NONE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       INCLUDE FILES.
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:MXSRV_CMD.DEF'
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       LOCAL DECLARATIONS.
C
        BYTE            MESSAGE(*)
        BYTE            APP_NAME(*)
        INTEGER*4       TERMINAL_NO
        INTEGER*4       MESSAGE_LENGTH
        INTEGER*4       MSG_TYPE                ! Terminal message type (reg,
                                                ! unso, bro)
        INTEGER*4       PROBUF_NUM              ! Procom buffer number
        INTEGER*4       REPLY_QUE               ! Reply queue number
        INTEGER*4       CDC
        INTEGER*4       PROBUF                  ! Procom buffer number
C
        RECORD      /CMDSTRUC/ MXSRV_CMD
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        TERMINAL_NO = 0
        PROBUF_NUM = 0
        REPLY_QUE = 0
C
        CALL DQUTRA(MXS,PROBUF)
C
        IF (PROBUF.NE.0) THEN
C
            MSG_TYPE = HPRO(TRCODE,PROBUF)
C
            IF (MSG_TYPE.EQ.TYPCMD) THEN
C
                MSG_TYPE = 3
                TERMINAL_NO = 1
C
                IF (PRO(CTYP,PROBUF).EQ.TCMXV) THEN
                    MXSRV_CMD.COMMAND_GROUP = MXSRV_VISION_CMD
                ENDIF
C
                MXSRV_CMD.COMMAND_TYPE = PRO(CNUM,PROBUF)
                MXSRV_CMD.CMD_ARG_INT1 = PRO(IARG1,PROBUF)
                MXSRV_CMD.CMD_ARG_INT2 = PRO(IARG2,PROBUF)
C
                CALL LIB$MOVC3(MAX_CMD_ARG_STR,
     *                         PRO(SARG1,PROBUF),
     *                         MXSRV_CMD.CMD_ARG_STR1)
C
                CALL LIB$MOVC3(MAX_CMD_ARG_STR,
     *                         PRO(SARG2,PROBUF),
     *                         MXSRV_CMD.CMD_ARG_STR2)
C
                MESSAGE_LENGTH = 16 + (2 * MAX_CMD_ARG_STR)
                CALL LIB$MOVC3(MESSAGE_LENGTH,
     *                         MXSRV_CMD,MESSAGE)
C
                CALL X2RELBUF(PROBUF)
C
            ELSEIF ((MSG_TYPE.EQ.TYPREG) .OR.
     *              (MSG_TYPE.EQ.TYPUNS) .OR.
     *              (MSG_TYPE.EQ.TYPBRO)) THEN
C
                IF (X2X_I4_STATION) THEN
                   TERMINAL_NO = PRO(TERNUM,PROBUF)
                ELSE
                   TERMINAL_NO = HPRO(TERNUM,PROBUF)
                ENDIF
C
                MESSAGE_LENGTH = HPRO(OUTLEN,PROBUF)
C
                CALL LIB$MOVC3(MESSAGE_LENGTH,
     *                         PRO(OUTTAB,PROBUF),MESSAGE)
C
C               Convert EuroGOLS message types into MX delivery codes.
C
                IF (MSG_TYPE.EQ.TYPREG) THEN

                    MSG_TYPE = 1		! RESPONSE
                ELSEIF (MSG_TYPE.EQ.TYPUNS) THEN

                    IF (TERMINAL_NO .NE. -1) THEN

                         MSG_TYPE = 2		! UNSOLICITED
                    ELSE

                        MSG_TYPE = '000000FF'X  ! BROADCAST
                    ENDIF
                ELSEIF (MSG_TYPE.EQ.TYPBRO) THEN

                    MSG_TYPE = '000000FF'X	! BROADCAST
                ELSE

                    TERMINAL_NO = 0		! UNKNOWN DROP MESSAGE
                ENDIF
C
                CALL X2RELBUF(PROBUF)
C
!MXSRV!            ELSEIF (MSG_TYPE.EQ.TYPRPC) THEN
!MXSRV!C
!MXSRV!                IF (PRO(RPCOFCD,PROBUF) .EQ. 3) THEN
!MXSRV!C
!MXSRV!                    MSG_TYPE = 4
!MXSRV!                    TERMINAL_NO = 1
!MXSRV!                    PROBUF_NUM = PROBUF
!MXSRV!                    REPLY_QUE = PRO(RPCOQUE,PROBUF)
!MXSRV!                    CDC = DAYCDC
!MXSRV!C
!MXSRV!                    CALL LIB$MOVC3(32,
!MXSRV!     *                             PRO(RPCOAPP,PROBUF),APP_NAME)
!MXSRV!C
!MXSRV!                    MESSAGE_LENGTH = PRO(RPCOSIZ,PROBUF)
!MXSRV!C
!MXSRV!                    CALL LIB$MOVC3(MESSAGE_LENGTH,
!MXSRV!     *                             PRO(PRO(RPCOOFF,PROBUF),PROBUF),
!MXSRV!     *                             MESSAGE)
!MXSRV!C
!MXSRV!                ELSE
!MXSRV!C
!MXSRV!                    CALL X2RELBUF(PROBUF)
!MXSRV!C
!MXSRV!                ENDIF 
C
            ELSE
C
                CALL X2RELBUF(PROBUF)
C
            ENDIF
C
        ENDIF
C
        RETURN
        END

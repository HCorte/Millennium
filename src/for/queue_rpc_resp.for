C QUEUE_RPC_RESP
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
C  Copyright 2004 GTECH Corporation. All rights reserved.
C
C=====[QUEUE_RPC_RESP.FOR]====================================================
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
C=====[QUEUE_RPC_RESP.FOR]====================================================
C
        OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE QUEUE_RPC_RESP(PROBUF_NUM,
     *                            QUE_NUM,
     *                            RESULT_CODE,
     *                            MESSAGE_LENGTH,
     *                            MESSAGE)
        IMPLICIT NONE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       INCLUDE FILES.
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       LOCAL DECLARATIONS.
C
        BYTE            MESSAGE(*)
        INTEGER*4       PROBUF_NUM
        INTEGER*4       QUE_NUM
        INTEGER*4       RESULT_CODE
        INTEGER*4       MESSAGE_LENGTH
        INTEGER*4       IDX
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
!MXSRV!        IF (RESULT_CODE .GT. 0) THEN
!MXSRV!C
!MXSRV!            PRO(RPCIRCD,PROBUF_NUM) = 1
!MXSRV!            PRO(RPCIERR,PROBUF_NUM) = 0
!MXSRV!C
!MXSRV!        ELSE
!MXSRV!C
!MXSRV!            PRO(RPCIRCD,PROBUF_NUM) = 0
!MXSRV!            PRO(RPCIERR,PROBUF_NUM) = RESULT_CODE
!MXSRV!C
!MXSRV!        ENDIF
!MXSRV!C
!MXSRV!        PRO(RPCILEN,PROBUF_NUM) = MESSAGE_LENGTH
!MXSRV!C
!MXSRV!        DO IDX = 0, (MESSAGE_LENGTH - 1)        ! Put RPC response message in
!MXSRV!                                                ! procom buffer
!MXSRV!            BPRO((BINPTAB + IDX),PROBUF_NUM) =
!MXSRV!     *          MESSAGE(IDX + 1)
!MXSRV!C
!MXSRV!        END DO
!MXSRV!C
!MXSRV!        CALL QUETRA(QUE_NUM,PROBUF_NUM)
!MXSRV!C
        RETURN
        END

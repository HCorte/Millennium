C QUEUE_MXVIS_CMD
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
C=====[QUEUE_MXVIS_CMD.FOR]===================================================
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
C=====[QUEUE_MXVIS_CMD.FOR]===================================================
C
        OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE QUEUE_MXVIS_CMD(MESSAGE,
     *                             RETURN_STATUS)
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
        INCLUDE 'INCLIB:MXSRV_CMD.DEF'
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       LOCAL DECLARATIONS.
C
        INTEGER*4       RETURN_STATUS
        INTEGER*4       PROBUF                  ! Procom buffer number
C
        RECORD      /CMDSTRUC/ MESSAGE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        CALL GETBUF(PROBUF)                     ! Get a procom buffer
C
        IF (PROBUF.GT.0) THEN                   ! Procom buffer available
C
          HPRO(TRCODE,PROBUF) = TYPCMD
          HPRO(INPLEN,PROBUF) = 52
C
          PRO(CNUM,PROBUF) = MESSAGE.COMMAND_TYPE
          PRO(CTYP,PROBUF) = TCMXV
C
          PRO(IARG1,PROBUF) = MESSAGE.CMD_ARG_INT1
          PRO(IARG2,PROBUF) = MESSAGE.CMD_ARG_INT2
C
          CALL LIB$MOVC3(MAX_CMD_ARG_STR,
     *                   MESSAGE.CMD_ARG_STR1,
     *                   PRO(SARG1,PROBUF))
C
          CALL LIB$MOVC3(MAX_CMD_ARG_STR,
     *                   MESSAGE.CMD_ARG_STR2,
     *                   PRO(SARG2,PROBUF))
C
          CALL QUETRA(MXS,PROBUF)
          RETURN_STATUS = 0
        ELSE
C
          RETURN_STATUS = -5
        ENDIF
C
        RETURN
        END

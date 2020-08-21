C LOGRPC
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
C=====[LOGRPC.FOR]============================================================
C
C V01 29-SEP-2004 MTK  INITIAL RELEASE FOR ALPHAGOLS BASELINE
C
C=====[LOGRPC.FOR]============================================================
C
        OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE LOGRPC(ECODE,RPCTAG,TRANS,TRANSLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
C       INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
C
        INTEGER*4  ECODE
        CHARACTER  RPCTAG*(*)


        CHARACTER CRPC(64)

        BYTE      BRPC(64)
        BYTE      TRANS(*)

        EQUIVALENCE (CRPC,BRPC)

        INTEGER*4  I
        INTEGER*4  IND
        INTEGER*4  ST
        INTEGER*4  BUF
        INTEGER*4  RLEN
        INTEGER*4  TLEN
        INTEGER*4  TRANSLEN
C
C IF LIVE SYSTEM THEN GET A PROCOM BUFFER
C
        IF(P(SYSTYP).NE.LIVSYS) RETURN
        CALL GETBUF(BUF)
        IF(BUF.LT.1) RETURN
C
C GET RPC TAG LENGTH AND TRANSFER TO LOCAL BUFFER
C 
        RLEN = LEN(RPCTAG)
        RLEN = MIN(RLEN,64)

        DO I = 1,64
          CRPC(I) = ' '
        ENDDO

        DO I = 1,RLEN
          CRPC(I) = RPCTAG(I:I)
        ENDDO

        TLEN = MIN(TRANSLEN,88)

C
C TRANSFER DATA TO PROCOM BUFFER
C
        HPRO(TRCODE,BUF) = TYPMXL
        PRO(INPTAB,BUF)  = ECODE
        IND = 5
        CALL MOVBYT(BRPC,1,PRO(INPTAB,BUF),IND,64)
        IND = IND + 64
        CALL MOVBYT(TLEN,1,PRO(INPTAB,BUF),IND,1)
        IND = IND + 1
        CALL MOVBYT(TRANS,1,PRO(INPTAB,BUF),IND,TLEN)
        IND = IND + TLEN
C
C SET LENGTH AND QUEUE TO DISPAT
C
        HPRO(INPLEN,BUF) = IND - 1
        CALL ABL(BUF,QUETAB(1,DIS),ST)
        RETURN
        END


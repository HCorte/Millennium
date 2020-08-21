C
C Subroutine to process the second phase of Wager Fractioning
C
C V07 24-FEB-2000 UXN TNFRAC FIXED.
C V06 01-FEB-2000 UXN Fraction changes.
C V05 06-JUL-1999 UXN Pitka game added.
C V04 07-JUN-1999 RXK Fractioning of oddset games added.
C V03 29-MAR-1999 UXN & WS FIX FOR TIMING WRITE OF TRANSACTION BY APULOG
C V02 29-JAN-1999 UXN Partially rewritten.
C V01 03-AUG-1993 GXA Initial revision.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE FRACTWO(TRABUF,BUF,TASK)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
	INCLUDE 'INCLIB:FRAC.DEF'
C
C
	INTEGER*4 BUF				!Procom Buffer #.
C
	INTEGER*4 LOGREC(LMUREC)		!Transaction Log Record
	INTEGER*4 WAGBUF(TRALEN)		!Internal Trans Record.
	INTEGER*4 FRABUF(TRALEN)                !Fraction Wager Trans Record.
	INTEGER*4 WAGSER			!Wager Serial#.
	INTEGER*4 ST				!Subroutine Return Status.
	INTEGER*4 TASK				!Current Task
C
	BYTE	  I1TEMP(4)			!Temp Variable.
	INTEGER*2 I2TEMP(2)			!Temp Variable.
	INTEGER*4 I4TEMP			!Temp Variable.
	INTEGER*4 TKTS,I
	INTEGER*4 TER

C
	EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
C
C GET TRANSACTION FROM PROCOM BUFFER
C
        CALL LOGTRA(TRABUF,PRO(FRA_WRKTAB,BUF))
C
C FIRST CHECK FOR RETRY
C
	IF(TRABUF(TSTAT).EQ.REJT.AND.TRABUF(TERR).EQ.RETY) THEN
            CALL RLOG(TRABUF(TSDT5),LOGREC,TASK,ST)  ! First fractioned ticket
CV3
	    IF (ST.NE.0) THEN
		CALL WAIT_APUQUE
      		CALL RLOG(TRABUF(TSDT5),LOGREC,TASK,ST)  ! First fractioned ticket
	    ENDIF
CEV3
            IF(ST.NE.0) GOTO 100
            CALL LOGTRA(FRABUF,LOGREC)
	    GOTO 100
	ENDIF
C
        TKTS = PRO(FRA_TKT,BUF)
        DO I = 0,TKTS-1
           TRABUF(TSDT5+I) = PRO(FRA_FRC+I,BUF)
        END DO
C
C SET / CLEAR VARIABLES
C
	ST  = 0
	TRABUF(TERR)  = INVL
	TRABUF(TSTAT) = REJT
	TRABUF(TSUBERR) = NOER
	WAGSER = TRABUF(TSDT1)
C
C READ WAGER FROM TM FILE AND CONVERT TO INTERNAL FORMAT (WAGBUF)
C
	CALL RLOG(WAGSER,LOGREC,TASK,ST)
CV3
	IF (ST.NE.0) THEN
	    CALL WAIT_APUQUE
	    CALL RLOG(WAGSER,LOGREC,TASK,ST)
	ENDIF
CEV3
	IF(ST.NE.0) GOTO 100
	CALL LOGTRA(WAGBUF,LOGREC)
C
	IF(WAGBUF(TSTAT).NE.GOOD) GOTO 100
	IF(WAGBUF(TTYP) .NE.TWAG) GOTO 100
C
C BUILD FRACTIONAL WAGER
C
	CALL FASTMOV(WAGBUF,FRABUF,TRALEN)
	FRABUF(TFRAC)  = TRABUF(TSDT3)
	IF(FRABUF(TFRAC) .EQ.0) GOTO 100
	FRABUF(TNFRAC) = MAXFRC(FRABUF(TGAM)) / FRABUF(TFRAC)
C
C
C UPDATE FRACTIONED TICKET 
C
	FRABUF(TTIM)     = TRABUF(TTIM)
        FRABUF(TTER)     = TRABUF(TTER)
	FRABUF(TWFFLG)   = 1
	FRABUF(TFAMTFLG) = 1 ! Contains bet amount of the original ticket.
C
	WAGBUF(TSTAT)  = FRAC
	WAGBUF(TWCSER) = TRABUF(TSER)
	WAGBUF(TWCTER) = TRABUF(TTER)
C
C
C BUILD OUTPUT MESSAGE
C
C
        CALL TRALOG(WAGBUF,LOGREC)		  ! Original wager
        CALL WLOG(WAGBUF(TSER),LOGREC,TASK)
C
        DO I = 0,TKTS-1
           FRABUF(TSER) = TRABUF(TSDT5+I)
           CALL TRALOG(FRABUF,LOGREC)
           CALL WLOG(FRABUF(TSER),LOGREC,TASK)	  ! Fractional parts
           LRCCNT(WAGBUF(TGAM)) = LRCCNT(WAGBUF(TGAM)) + WAGBUF(TSIZE)
        END DO
C
	TRABUF(TSTAT) = GOOD
	TRABUF(TERR)  = NOER
C
C BUILD OUTPUT MESSAGE
C
100	CONTINUE
        CALL UPDMIS(TRABUF)
	HPRO(TRCODE,BUF)=TYPREG
	CALL TRALOG(TRABUF,LOGREC)
	CALL WLOG(TRABUF(TSER),LOGREC,TASK)
	CALL OUTFRA(TRABUF,FRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
 	END

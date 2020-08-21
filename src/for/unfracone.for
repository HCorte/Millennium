C
C Subroutine to process first phase unfraction requests.
C
C V05 06-JAN-2011 FJG MILLENNIUM MXSRV
C V04 01-DEc-2000 UXN Totogolo added.
C V03 13-OCT-1999 RXK World Tour added.
C V02 29-JAN-1999 UXN Partially rewritten from UNFRAC.FOR
C V01 03-AUG-1993 GXA Initial revision (UNFRAC.FOR)
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
        SUBROUTINE UNFRACONE(TRABUF,BUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
	INCLUDE 'INCLIB:FRAC.DEF'
C
C
	INTEGER*4 BUF
C
	INTEGER*4 LOGREC(LMUREC)		!Transaction Log Record
	INTEGER*4 WAGBUF(TRALEN)		!Original Wager Trans Record.
	INTEGER*4 FRABUF(TRALEN)                !Fraction Wager Trans Record.
	INTEGER*4 SER				!Serial #
	INTEGER*4 ST				!Subroutine Return Status.
	INTEGER*4 TASK				!Current Task
	INTEGER*4 TER				!Terminal #.
	INTEGER*4 FRASER			!Serial# of our Fraction Wager.
	INTEGER*4 GTYP
C
	BYTE	  I1TEMP(4)			!Temp Variable.
	INTEGER*2 I2TEMP(2)			!Temp Variable.
	INTEGER*4 I4TEMP			!Temp Variable.
C
	EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
C
C
C SET / CLEAR VARIABLES
C
	TASK = SPE
	ST  = 0
	TRABUF(TSTAT)   = REJT
	TRABUF(TERR)    = NFRA
	TRABUF(TSUBERR) = UFRA_NFRA
	TER = TRABUF(TTER)
	SER = TRABUF(TSDT1)
C
	HPRO(TRCODE,BUF) = TYPUNF
        HPRO(INPLEN,BUF) = OUTLEN_MAX
C
C READ WAGER FROM LOG FILE AND CONVERT TO INTERNAL FORMAT
C
	CALL RLOG(SER,LOGREC,TASK,ST)
	IF(ST.NE.0) GOTO 100
	CALL LOGTRA(WAGBUF,LOGREC)
	GTYP = WAGBUF(TGAMTYP)
C
C GET FRACTIONED SERIAL #, READ FRACTIONED WAGER FROM LOG FILE AND CONVERT
C TO INTERNAL FORMAT.
C
	FRASER = WAGBUF(TWCSER)
	CALL RLOG(FRASER,LOGREC,TASK,ST)
	IF(ST.NE.0) GOTO 100
	CALL LOGTRA(FRABUF,LOGREC)
	CALL FASTMOV(FRABUF(TSDT1),TRABUF(TSDT1),14)
C
C CHECK FOR RETRY
C
	IF(TRABUF(TTRN).EQ.AGTHTB(ATRNUM,TER).AND.
     *     AGTTAB(ALSTRA,TER).NE.0)      THEN
	   IF(WAGBUF(TWCSER).EQ.AGTTAB(ALSTRA,TER).AND.
     *        WAGBUF(TSTAT).EQ.GOOD)                     THEN
	      TRABUF(TSTAT) = REJT
	      TRABUF(TERR)  = RETY
	      GOTO 90
	   ENDIF
	ENDIF
C
C CHECK IF AFTER DRAW
C
	CALL CHKCAN(WAGBUF,ST)
	IF(WAGBUF(TSTAT).NE.FRAC.OR.ST.NE.0) THEN
	   IF(ST.NE.0)               TRABUF(TSUBERR) = UFRA_ADRW
	   IF(WAGBUF(TSTAT).NE.FRAC) TRABUF(TSUBERR) = UFRA_NFRA
	   GOTO 100
	ENDIF
90	CONTINUE
	IF(GTYP.EQ.TKIK.OR.GTYP.EQ.TLTO.OR.GTYP.EQ.TSPT.OR.
     *     GTYP.EQ.TTGL) THEN
           PRO(FRA_QUE,BUF) = WAG
C       ELSEIF(GTYP.EQ.TBNG) THEN       ! MXSRV
C          PRO(FRA_QUE,BUF) = PPP       ! MXSRV
        ELSE
           PRO(FRA_QUE,BUF) = ODD       ! Everything else goes to ODDPRO
        ENDIF
	CALL TRALOG(TRABUF,PRO(FRA_WRKTAB,BUF))
	RETURN
C
100	CONTINUE
	TRABUF(TSTAT) = REJT
        PRO(FRA_QUE,BUF) = SPE
	CALL TRALOG(TRABUF,PRO(FRA_WRKTAB,BUF))
	END

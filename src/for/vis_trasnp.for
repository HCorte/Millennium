C
C SUBROUTINE TRASNP
C $Log:   GXAFXT:[GOLS]TRASNP.FOV  $
C
C  V06 31-MAY-2000 PXO Subroutine name from TRNSNP -> TRASNP  
C  V05 17-Apr-1996 HXK Release of Finland for X.25, Telephone Betting,
C                  Instant Pass Thru Phase 1
C  V04 13-Jun-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C  V03 21 Jan 1993 DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_trasnp.for **
C
C TRASNP.FOR
C
C V02 18-AUG-92 GCAN FIXED TOTO SELECT BOARD OVERFLOW ON THIRD BOARD
C                    AND AMOUNT BET / BOARD.
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C TRANSACTION SNAPSHOT FOR VISION
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TRASNP(NUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
C
	INTEGER*4 BUF(LREC*3)
	INTEGER*4 DUMMY, K, ST, OFF, INDEX, BLOCK, UNIT, NUM
	DATA      UNIT/1/
C
	SMODE=.TRUE.
	IF(NUM.LT.0) NUM=LSTTRN
	IF(NUM.LE.0) NUM=1
	CALL GETBI(NUM,BLOCK,INDEX,OFF)
	CALL OPENW(UNIT,SFNAMES(1,PTMF),4,0,0,ST)
	IF(ST.NE.0) THEN
	   WRITE(CLIN23,800) (SFNAMES(K,PTMF),K=1,5),ST
	   CALL USRCLOS1(     1)
	   RETURN
	ENDIF
	CALL TOPEN(UNIT)
C
15	CONTINUE
	CALL RLOG(NUM,BUF,DUMMY,ST)
	IF(ST.GT.0) THEN
	   NUM=NUM-1
	   GOTO 15
	ENDIF
	CALL USRCLOS1(     1)
	IF(ST.LT.0) THEN
	   WRITE(CLIN23,801) (SFNAMES(K,PTMF),K=1,5),ST
	   RETURN
	ENDIF
C
	CALL LOGTRA(TRABUF,BUF)
	CALL TRNSNP1(TRABUF,BLOCK,INDEX)
C
800	FORMAT(5A4,' open error ',I4)
801	FORMAT(5A4,' read error ',I4)
C
	RETURN
	END

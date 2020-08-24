C
C SUBROUTINE ACCBET
C $Log:   GXAFXT:[GOLS]ACCBET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:07:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   11 Jun 1993 16:17:50   HXK
C  ADDED AGTINF.DEF, PRMAGT.DEF.
C  
C     Rev 1.0   21 Jan 1993 15:34:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_accbet.for **
C
C ACCBET.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C
C
C             CALLING SEQUENCE   Input :  NUM  - number selected
C
C                               OUTPUT :OUTSTAT - 0 - continue bets
C                                                1 - calculate winners
C                                               -1 - duplicate bets
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ACCBET(NUM,BOARD,OUTSTAT,BET)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	 INTEGER *4 NUM,OUTSTAT,BRDS(20),LSTBNUM,BOARD(*)
	 INTEGER*4 J, I, CALCULAT, BET
	 PARAMETER(CALCULAT=99)
	 DATA LSTBNUM/0/
C
C Do not accept same last number and no input
C
	 IF(NUM.LE.0.OR.LSTBNUM.EQ.NUM) RETURN
C
C
C Test if input number (NUM) is a bet or an indication to calculate
C number of winners  99 - signifies to calculate
C
	 IF(NUM.EQ.CALCULAT) GOTO 10
C
C Fill in board array with number bet on
C
	 DO 3 I=1,BET
	     BRDS(I)=BOARD(I)
 3	 CONTINUE
	 DO 5 I=BET,2,-1
	    BOARD(I-1)=BRDS(I)
 5	 CONTINUE
	 BOARD(BET)=NUM
	 LSTBNUM=NUM
	 RETURN
C
C Verify that board does not have duplicate bets
C
 10	 CONTINUE
C
	OUTSTAT=1
	DO 20 I=1,BET-1
	   DO 25 J=I+1,BET
	     IF(BOARD(I).EQ.BOARD(J)) OUTSTAT=-1
 25	   CONTINUE
 20	CONTINUE
	RETURN
	END

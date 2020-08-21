C
C SUBROUTINE DSEXP
C $Log:   GXAFXT:[GOLS]DSEXP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:00:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   23 Nov 1995 14:01:56   PXB
C  Initial revision.
C  
C SUBROUTINE TO EXPAND SUPER DOUBLE SYSTEM BETS
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	SUBROUTINE DSEXP(TRABUF,BETS,COUNT)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

C---- Local variables.

	INTEGER*4 BETS(2,100)
	INTEGER*4 I, J, COUNT


C------------------------- Start of Code -----------------------------

	IF (TRABUF(TWSYST) .EQ. NOSYS) THEN
	  COUNT = TRABUF(TWNBET)
	  DO 10 I = 1,COUNT
	    BETS(1,I) = TRABUF(TWDBROW1 + (I-1) * TWDBBLEN)
	    BETS(2,I) = TRABUF(TWDBROW2 + (I-1) * TWDBBLEN)
10	  CONTINUE
	  RETURN
	END IF

C---- Expand system bets

	COUNT = 0
	DO 100 I = 0,TRABUF(TWNBET)-1
	  DO 100 J = 0,TRABUF(TWNBET)-1
	    IF (TRABUF(TWDBROW1+I*TWDBBLEN) .EQ. 'FF'X) GOTO 100
	    IF (TRABUF(TWDBROW2+J*TWDBBLEN) .EQ. 'FF'X) GOTO 100
            IF (TRABUF(TWDBROW1+I*TWDBBLEN) .EQ. TRABUF(TWDBROW2+J*TWDBBLEN))
     *          GOTO 100
	    COUNT = COUNT+1
	    BETS(1,COUNT) = TRABUF(TWDBROW1+I*TWDBBLEN)
	    BETS(2,COUNT) = TRABUF(TWDBROW2+J*TWDBBLEN)
100	CONTINUE

	RETURN

	END

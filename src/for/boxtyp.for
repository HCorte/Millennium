C
C SUBROUTINE BOXTYP
C $Log:   GXAFXT:[GOLS]BOXTYP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:22:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:45:22   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - boxtyp.for **
C
C BOXTYP.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V02 29-AUG-90 XXX RELEASED FOR EURO-SYSTEM
C
C
C
C
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
	SUBROUTINE BOXTYP(DIG,TYPE,GAME)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INTEGER*4 DIG(4), J, I, SAME, GAME, TYPE
C
C
C
	SAME=0
C
C THREE DIGIT GAME
C
	IF(GAME.EQ.NB3TYP) THEN
	  DO 10 I=1,3
	  DO 10 J=1,I-1
	   IF(DIG(I).EQ.DIG(J)) SAME=SAME+1
10	  CONTINUE
	  IF(SAME.EQ.1.AND.TYPE.EQ.TNB3B6) TYPE=TNB3B3
	  IF(SAME.EQ.1.AND.TYPE.EQ.TNB3C6) TYPE=TNB3C3
	  IF(SAME.EQ.1.AND.TYPE.EQ.TNB3W6) TYPE=TNB3W3
	  IF(SAME.GT.1) TYPE=0
	  RETURN
	ENDIF
C
C FOUR DIGIT GAME
C
	IF(GAME.EQ.NB4TYP) THEN
	  DO 20 I=1,4
	  DO 20 J=1,I-1
	  IF(DIG(I).EQ.DIG(J)) SAME=SAME+1
20	  CONTINUE
C
	  IF(SAME.EQ.1.AND.TYPE.EQ.TNB4B24)TYPE=TNB4B12
	  IF(SAME.EQ.1.AND.TYPE.EQ.TNB4C24)TYPE=TNB4C12
	  IF(SAME.EQ.1.AND.TYPE.EQ.TNB4W24)TYPE=TNB4W12
C
	  IF(SAME.EQ.2.AND.TYPE.EQ.TNB4B24)TYPE=TNB4B6
	  IF(SAME.EQ.2.AND.TYPE.EQ.TNB4C24)TYPE=TNB4C6
	  IF(SAME.EQ.2.AND.TYPE.EQ.TNB4W24)TYPE=TNB4W6
C
	  IF(SAME.EQ.3.AND.TYPE.EQ.TNB4B24)TYPE=TNB4B4
	  IF(SAME.EQ.3.AND.TYPE.EQ.TNB4C24)TYPE=TNB4C4
	  IF(SAME.EQ.3.AND.TYPE.EQ.TNB4W24)TYPE=TNB4W4
C
	  IF(SAME.GT.3) TYPE=0
	  RETURN
	ENDIF
C
C
	TYPE=0
	RETURN
	END

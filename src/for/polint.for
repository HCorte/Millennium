C
C SUBROUTINE POLINT
C $Log:   GXAFXT:[GOLS]POLINT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:24:14   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:17:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - polint.for **
C
C POLINT.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C V02 29-AUG-90 XXX RELEASED FOR EURO-SYSTEM
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
	SUBROUTINE POLINT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POLCOM.DEF'
C
C
	INTEGER*4 DIG(4), IND2, IND1, TYPE, I, N, NUM
C
C THREE DIGIT TABLE
C
	DO 20 NUM=0,999
	 N=NUM
	 DO 10 I=1,3
	  DIG(I)=MOD(N,10)
10	 N=N/10
	 TYPE=TNB3B6
	 CALL VNUMOD(NUM,TYPE,NB3TYP,IND1,IND2)
	 CALL BOXTYP(DIG,TYPE,NB3TYP)
	 P3OFF(NUM+1,BOXADR)=IND1
	 P3OFF(NUM+1,BOXPOL)=TYPE
20	CONTINUE
C
C FOUR DIGIT TABLE
C
	DO 40 NUM=0,9999
	 N=NUM
	 DO 30 I=1,4
	  DIG(I)=MOD(N,10)
30	 N=N/10
	 TYPE=TNB4B24
	 CALL VNUMOD(NUM,TYPE,NB4TYP,IND1,IND2)
	 CALL BOXTYP(DIG,TYPE,NB4TYP)
	 P4OFF(NUM+1,BOXADR)=IND1
	 P4OFF(NUM+1,BOXPOL)=TYPE
40	CONTINUE
	RETURN
	END

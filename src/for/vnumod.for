C
C SUBROUTINE VNUMOD
C $Log:   GXAFXT:[GOLS]VNUMOD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:55:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:03:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vnumod.for **
C
C VNUMOD.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 29-AUG-90 XXX RELEASED FOR EURO-SYSTEM
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
	SUBROUTINE VNUMOD(NUM,TYPE,GAME,INDEX1,INDEX2)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POLCOM.DEF'
C
	INTEGER*4 PER(10,4),D(4)
	INTEGER*4 PR, X, SW, I, N, INDEX2, INDEX1, GAME, TYPE, NUM
C
C PER(K,I)  PERMUTATION INDEX OF I-DIGITS
C           SORTED NUMBER WITH (K-1) ON THE
C           I-TH POSITION AND PRECEDING ZEROES
C
	DATA PER/0,1,5,15,35,70,126,210,330,495,
     *	            0,1,4,10,20,35,56,84,120,165,
     *	            0,1,3,6,10,15,21,28,36,45,
     *	            0,1,2,3,4,5,6,7,8,9/
C
C THREE DIGIT GAME
C
	IF(GAME.EQ.NB3TYP) THEN
	  IF(TYPE.EQ.TNB3F2) THEN
	    INDEX1=PFP2+NUM
	    INDEX2=PFP2+NUM
	    RETURN
	  ENDIF
C
	  IF(TYPE.EQ.TNB3S2) THEN
	    INDEX1=PSM2+NUM
	    INDEX2=PSM2+NUM
	    RETURN
	  ENDIF
C
	  IF(TYPE.EQ.TNB3B2) THEN
	    INDEX1=PBP2+NUM
	    INDEX2=PBP2+NUM
	    RETURN
	  ENDIF
C
C
	  N=NUM
	  DO 10 I=1,3
	  D(I)=MOD(N,10)
10	  N=N/10
C
20	  SW=0
	  DO 30 I=2,3
	  IF(D(I-1).GE.D(I)) GOTO 30
	  SW=-1
	  X=D(I-1)
	  D(I-1)=D(I)
	  D(I)=X
30	  CONTINUE
	  IF(SW.EQ.-1) GOTO 20
C
	  IF(TYPE.GE.TNB3ST.AND.TYPE.LE.TNB3W3) THEN
	    PR=PER(D(1)+1,2)+PER(D(2)+1,3)+PER(D(3)+1,4)
	    INDEX1=PBOX+PR
	    INDEX2=PSTR+NUM
	    RETURN
	  ENDIF
	ENDIF
C
C FOUR DIGIT GAME
C
	IF(GAME.EQ.NB4TYP) THEN
	  IF(TYPE.EQ.TNB4F2) THEN
	    INDEX1=PFP2+NUM
	    INDEX2=PFP2+NUM
	    RETURN
	  ENDIF
C
	  IF(TYPE.EQ.TNB4M2) THEN
	    INDEX1=PSM2+NUM
	    INDEX2=PSM2+NUM
	    RETURN
	  ENDIF
C
	  IF(TYPE.EQ.TNB4B2) THEN
	    INDEX1=PBP2+NUM
	    INDEX2=PBP2+NUM
	    RETURN
	  ENDIF
C
C
	  N=NUM
	  DO 40 I=1,4
	  D(I)=MOD(N,10)
40	  N=N/10
C
50	  SW=0
	  DO 60 I=2,4
	  IF(D(I-1).GE.D(I)) GOTO 60
	  SW=-1
	  X=D(I-1)
	  D(I-1)=D(I)
	  D(I)=X
60	  CONTINUE
	  IF(SW.EQ.-1) GOTO 50
C
	  IF(TYPE.GE.TNB4ST.AND.TYPE.LE.TNB4W4) THEN
	    PR=PER(D(1)+1,1)+PER(D(2)+1,2)+PER(D(3)+1,3)+PER(D(4)+1,4)
	    INDEX1=PBOX+PR
	    INDEX2=PSTR+NUM
	    RETURN
	  ENDIF
	ENDIF
C
C
	INDEX1=0
	INDEX2=0
	RETURN
	END

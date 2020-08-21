C
C SUBROUTINE TSLSYS
C $Log:   GXAFXT:[GOLS]TSLSYS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:38:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:54:10   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - tslsys.for **
C
C TSLSYS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
	SUBROUTINE TSLSYS(BETTAB,TOTROW,SIMSIZ,NUMCOM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*4 BETTAB(6,40)  
	INTEGER*4 INDEX(20)
	INTEGER*4 NUMCOM, SIMSIZ, TOTROW, OFF, OFF1, OFF2
C
	NUMCOM=1
C
C     SET UP FIRST BET TO START WITH 1,2,3,...
C
	INDEX(1)=1
	DO 100, OFF=2,SIMSIZ
	   INDEX(OFF)=INDEX(OFF-1)+1
100	CONTINUE
C
110	CONTINUE
C
C     STORE ROWS IN BETTAB
C
	DO 120, OFF=1,SIMSIZ
	   BETTAB(OFF,NUMCOM)=INDEX(OFF)
120	CONTINUE
	NUMCOM=NUMCOM+1                !BUMP NUMBER OF COMBOS
C
C     GET NEXT SET OF ROWS TO STUFF IN BETTAB
C
	DO 140, OFF1=SIMSIZ,1,-1
	   INDEX(OFF1)=INDEX(OFF1)+1
	   DO 130, OFF2=OFF1,SIMSIZ-1
	      INDEX(OFF2+1)=INDEX(OFF2)+1
130	   CONTINUE
	   IF(INDEX(SIMSIZ).LE.TOTROW) GOTO 110  ! MORE COMBOS LEFT
140	CONTINUE
C
	NUMCOM=NUMCOM-1  ! DECREMENT BY ONE FOR CORRECT COMBO COUNT
	RETURN
	END

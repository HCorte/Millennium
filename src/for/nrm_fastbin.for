C
C FUNCTION FASTBIN
C $Log:   GXAFXT:[GOLS]FASTBIN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:07:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:14:50   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_fastbin.for **
C
C FASTBIN.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C     FUNCTION CALCULATING BINOMIAL
C
C     CALLING SEQUENCE:
C     BINOMIAL=FASTBIN(M,N)
C     IN -
C     M,N  - BINOMIAL ARGUMENTS (M - TOP ARGUMENT, N BOTTOM ARGUMENT)
C     OUT
C     BINOMIAL - I*4 VALUE SET
C
C
C     THIS PROCEDURE WILL CALCULATE BINOMIAL OF 2 VALUES
C
C     IT WILL TRY NOT TO EXCEED PRECISION OF I*4
C
C     IF THERE ARE ANY PROBLEMS RESULT AND WORK SHOULD BE CHANGED
C     TO REAL*8
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
	INTEGER FUNCTION FASTBIN(ARG1,ARG2)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*4 ARG1,ARG2
	INTEGER*4 OFF1, OFF, ARG
	REAL*8 RESULT,WORK
	INTEGER*4 BINTAB(0:100,0:50)      !JUST TO REMEMBER BINOMIALS
	DATA BINTAB/5151*-1/
C
C
	INTEGER*4 TAB1(100),TAB2(100)
C
	ARG=ARG2
	IF (ARG1.LT.ARG2*2) ARG=ARG1-ARG2
	IF (BINTAB(ARG1,ARG).GE.0) THEN
	   FASTBIN=BINTAB(ARG1,ARG)
	   RETURN
	ENDIF
C
	DO 20, OFF=1,ARG
	TAB2(OFF)=OFF
20	CONTINUE
C
	DO 30, OFF=ARG1-ARG+1,ARG1
	TAB1(OFF)=OFF
30	CONTINUE
C
	DO 40, OFF=ARG1-ARG+1,ARG1
C
	DO 40, OFF1=1,ARG
	IF (TAB2(OFF1).EQ.1) GOTO 40
	IF (MOD(TAB1(OFF),TAB2(OFF1)).NE.0) GOTO 40
	TAB1(OFF)=TAB1(OFF)/TAB2(OFF1)
	TAB2(OFF1)=1
40	CONTINUE
C
	RESULT=1.D0
C
	DO 50, OFF=ARG1-ARG+1,ARG1
	RESULT=DFLOAT(TAB1(OFF))*RESULT
50	CONTINUE
C
	WORK=1.D0
	DO 60, OFF=1,ARG
	WORK=WORK*DFLOAT(TAB2(OFF))
60	CONTINUE
C
	FASTBIN=IDNINT(RESULT/WORK)
	BINTAB(ARG1,ARG)=FASTBIN
	RETURN
	END

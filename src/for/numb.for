C
C SUBROUTINE NUMB
C $Log:   GXAFXT:[GOLS]NUMB.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:15:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:10:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_numb.for **
C
C NUMB.FOR
C
C V02 04-AUG-92 WLM ADDED AMOUNT ENTRY
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
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
	SUBROUTINE NUMB(SLINE,POS,NUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
C SUBROUTINE TO DECODE NUMBERS FROM COMMAND
C LINE FOR VISION SNAPSHOTS.
C
C CALLING SEQUENCE:
C     CALL NUMB(SLINE,POS,NUM)
C INPUT
C     SLINE  - COMMAND LINE (INTEGER*4)
C     POS    - STARTING LINE POSITION
C OUTPUT
C     NUM    - DECODED NUMBER (-1 IF ERROR)
C
C
	INTEGER*4 DLINE(20),SLINE(20), ERR, DIGIT, I, OK, NUM, POS
	CHARACTER CLINE(80),BLANK,ZERO,NINE,DOLLAR
	EQUIVALENCE(DLINE,CLINE)
	DATA BLANK/' '/,ZERO/'0'/,NINE/'9'/,DOLLAR/'$'/
C
	NUM=0
	OK=0
C
	DO 10 I=1,20
	DLINE(I)=SLINE(I)
10	CONTINUE
C
20	CONTINUE
	IF(POS.GT.40) GOTO 50
	IF(CLINE(POS).NE.BLANK) GOTO 25
	POS=POS+1
	GOTO 20
C
25	CONTINUE
	IF(CLINE(POS).EQ.DOLLAR) THEN
	  NUM=IMONY(SLINE,POS+1,10,BETUNIT)
	  OK=1
	  GOTO 40
	ENDIF
30	CONTINUE
	IF((CLINE(POS).LT.ZERO.OR.CLINE(POS).GT.NINE).AND.
     *	 CLINE(POS).NE.'-') GOTO 40
	IF(CLINE(POS).EQ.'-') GOTO 35
	CALL ASCBIN(DLINE,POS,1,DIGIT,ERR)
	NUM=NUM*10+DIGIT
	OK=1
35	CONTINUE
	POS=POS+1
	IF(POS.GT.40)GOTO 40
	GOTO 30
C
40	CONTINUE
	IF(OK.NE.1)NUM=-1
50	RETURN
	END

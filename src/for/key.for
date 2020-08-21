C
C SUBROUTINE KEY
C $Log:   GXAFXT:[GOLS]KEY.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:42:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:44:26   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_key.for **
C
C KEY.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
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
	SUBROUTINE KEY(SLINE,KEYS,NUM,POS,KEYNUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
C ROUTINE TO DECODE COMMAND LINE FOR VISION SNAPSHOTS.
C
C CALLING SEQUENCE:
C     CALL KEY(SLINE,KEYS,NUM,POS,KEYNUM)
C INPUT
C     SLINE  - COMMAND LINE (INTEGER*4 ARRAY)
C     KEYS   - KEY LIST (REAL*8 ARRAY)
C     NUM    - NUMBER OF KEYS IN KEY LIST
C     POS    - FIRST CHARACTER IN FIELD
C OUTPUT
C     POS    - FIRST POSITION OF NEXT FIELD
C     KEYNUM - KEY NUMBER (0 IF NO MATCH)
C
C
	INTEGER*4  J, I, LOWZ, LOWA, TEMP2, TEMP1, KEYNUM, POS, NUM
C
	REAL*8 KEYS(NUM),DKEYS(100)
	INTEGER*4 SLINE(20),DLINE(20)
C
C
	CHARACTER CKEYS(8,100),CLINE(80),CTEMP1(4),CTEMP2(4)
	CHARACTER CHAR1,CHAR2
	EQUIVALENCE(CKEYS,DKEYS),(CLINE,DLINE)
	EQUIVALENCE(CTEMP1(1),CHAR1)
	EQUIVALENCE(CTEMP2(1),CHAR2)
	EQUIVALENCE(CTEMP1,TEMP1)
	EQUIVALENCE(CTEMP2,TEMP2)
	DATA LOWA/Z61/
	DATA LOWZ/Z7A/
C
	TEMP1=0
	TEMP2=0
	DO 5 I=1,NUM
	   DKEYS(I)=KEYS(I)
5	CONTINUE
	DO 7 I=1,20
	   DLINE(I)=SLINE(I)
7	CONTINUE
C
C FIND START OF FIELD
C
	KEYNUM=0
10	CONTINUE
	IF(POS.GT.40) RETURN
	IF(CLINE(POS).NE.' ') GOTO 20
	POS=POS+1
	GOTO 10
C
C FIND MATCH
C
20	CONTINUE
	DO 40 I=1,NUM
	   DO 30 J=1,8
	      CHAR1=CKEYS(J,I)
	      CHAR2=CLINE(POS-1+J)
	      IF(CLINE(POS+J-1) .EQ. ' ') THEN
	         IF(TEMP1.GE.LOWA .AND. TEMP1.LE.LOWZ) GO TO 50
	      ENDIF
	      IF(TEMP1.GE.LOWA.AND.TEMP1.LE.LOWZ) TEMP1=TEMP1-32
	      IF(TEMP2.GE.LOWA.AND.TEMP2.LE.LOWZ) TEMP2=TEMP2-32
	      IF(CHAR1.EQ.CHAR2) GOTO 30
	      IF(CKEYS(J,I).EQ.' ') GOTO 30
	      GOTO 40
30	   CONTINUE
	   GOTO 50
40	CONTINUE
	KEYNUM=0
	RETURN
C
C FOUND MATCH, RETURN KEYNUM AND START OF NEXT FIELD
C
50	CONTINUE
	KEYNUM=I
60	IF(POS.GT.40)GOTO 80
	IF(CLINE(POS).EQ.' ')GOTO 70
	POS=POS+1
	GOTO 60
C
70	CONTINUE
	IF(POS.GT.40) GOTO 80
	IF(CLINE(POS).NE.' ') GOTO 80
	POS=POS+1
	GOTO 70
C
80	CONTINUE
	RETURN
	END

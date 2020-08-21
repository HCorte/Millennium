C
C SUBROUTINE XXXPER
C
C V02 24-MAY-1999 UXN OUTPUT LUN CHANGED  TO 6
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE XXXPER(PRMFLG, STRING,NUM,EXT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	LOGICAL	    PRMFLG
C
	INTEGER*4  INPLU,OUTLU
	PARAMETER (INPLU=5)
	PARAMETER (OUTLU=6)
C
	INTEGER*4 ST
	CHARACTER STRING*(*),CBUF(20)
	INTEGER*4 INBUF(5), EXT, FLG, NUM, POINT, I, DIG
C
	INTEGER*4   INLEN
	CHARACTER   CXINBUF*(20)
	EQUIVALENCE(INBUF,CBUF,CXINBUF)
C
	DATA ST/0/
C
C READ LU
C
10	CONTINUE
	EXT=0
	IF(PRMFLG)THEN
	  CALL PRMTEXT(STRING, CXINBUF, INLEN)
	ELSE
	  CALL WIMG(OUTLU,STRING)
	  READ (INPLU,900) INBUF
	ENDIF
C
C CHECK FOR EXIT
C
	IF (CBUF(1).EQ.'E' .OR. CBUF(1).EQ.'e') THEN
	   EXT=-1
	   RETURN
	ENDIF
C
C CONVERT TO NUMERIC DATA
C
	FLG=0
	NUM=0
	POINT=0
	DO 20 I=1,10
	   IF (CBUF(I).EQ.' ') GOTO 20
	   IF (POINT.GT.3) GOTO 20
	   IF(CBUF(I).EQ.'.') THEN
	     NUM=NUM*1000
	     POINT=1
	     GOTO 20
	   ENDIF
	   CALL ASCBIN(INBUF,I,1,DIG,ST)
	   IF (ST.LT.0) GOTO 30
	   FLG=1
	   IF(POINT.EQ.0) THEN
	     NUM=NUM*10+DIG
	   ELSE
	     IF(POINT.EQ.1) NUM=NUM+DIG*100
	     IF(POINT.EQ.2) NUM=NUM+DIG*10
	     IF(POINT.EQ.3) NUM=NUM+DIG
	     POINT=POINT+1
	   ENDIF
20	CONTINUE
	IF(POINT.EQ.0) NUM=NUM*1000
C
C CHECK IF INPUT IS VALID
C
	IF (ST.LT.0 .OR. FLG.EQ.0) GOTO 30
	IF (NUM.LE.100000) RETURN
C
C LIMIT EXCEEDED
C
	WRITE(OUTLU,901) IAM()
	GOTO 10
C
C INVALID INPUT
C
30	CONTINUE
	WRITE(OUTLU,902) IAM()
	GOTO 10
900	FORMAT(5A4)
901	FORMAT(1X,A,' Percent cannot exceed 100.00')
902	FORMAT(1X,A,' Invalid input ')
	END

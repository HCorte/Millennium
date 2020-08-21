C
C SUBROUTINE XXXTIM
C
C V02 24-MAY-1999 UXN OUTPUT LUN CHANGED TO 6
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
	SUBROUTINE XXXTIM(PRMFLG, STRING,TIME,EXT)
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
	CHARACTER STRING*(*),CBUF(20)
	INTEGER*4 INBUF(5), EXT, ST, HRS, SECS
	INTEGER*4 TIME, MINS
C
	INTEGER*4   INLEN
	CHARACTER   CXINBUF*(20)
	EQUIVALENCE(INBUF,CBUF,CXINBUF)
C
C READ LU
C
10	CONTINUE
	EXT=0
	ST=0
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
C CHECK FOR ALL
C
	IF (CBUF(1).EQ.'A' .OR. CBUF(1).EQ.'a') THEN
	   EXT=-4
	   RETURN
	ENDIF
C
C CHECK AND CONVERT TO TIME IN SECONDS
C
	IF(CBUF(3).NE.':'.OR.CBUF(6).NE.':') GOTO 30
	CALL ASCBIN(INBUF,1,2,HRS,ST)
	IF(HRS.GT.24.OR.ST.LT.0) GOTO 30
	CALL ASCBIN(INBUF,4,2,MINS,ST)
	IF(MINS.GT.60.OR.ST.LT.0) GOTO 30
	CALL ASCBIN(INBUF,7,2,SECS,ST)
	IF(SECS.GT.60.OR.ST.LT.0) GOTO 30
	TIME=HRS*3600+MINS*60+SECS
	RETURN
C
C
C
C INVALID INPUT
C
30	CONTINUE
	WRITE(OUTLU,901) IAM()
	GOTO 10
C
C
900	FORMAT(5A4)
901	FORMAT(1X,A,' Input error, time entry format is HH:MM:SS ')
	END

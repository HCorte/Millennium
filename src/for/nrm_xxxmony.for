C
C SUBROUTINE XXXMONY
C
C V02 24-MAY-1999 UXN Output LUN changed from 5 to 6
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
	SUBROUTINE XXXMONY(PRMFLG, STRING,NUM,FACTOR,EXT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	LOGICAL	    PRMFLG
C
	INTEGER*4  OUTLU,INPLU
	PARAMETER (INPLU=5)
	PARAMETER (OUTLU=6)
C
	CHARACTER STRING*(*),CBUF(20)
C
	INTEGER*4  INBUF(5), EXT, ST, FLG, NUM, DCNT, I
	INTEGER*4  DIG, ONUM, FACTOR, MON_UNIT
	LOGICAL DPNT
C
	INTEGER*4   INLEN
	CHARACTER   CXINBUF*(20)
	EQUIVALENCE(INBUF,CBUF,CXINBUF)
C
C
10	CONTINUE
	EXT=0
	ST=0
C
C ASSIGNING VALUE TO THE UNIT OF MONEY
C
        IF (FACTOR .EQ. VALUNIT) THEN
          MON_UNIT=DYN_VALUNIT
        ELSE IF (FACTOR .EQ. BETUNIT) THEN
          MON_UNIT=DYN_BETUNIT
        ELSE
          MON_UNIT=FACTOR
        END IF
 
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
	   GOTO 10000
	ENDIF
C
C CONVERT TO NUMERIC DATA
C
	FLG=0
	NUM=0
	DCNT=0
	DPNT=.FALSE.
	DO 20 I=1,20
	IF(CBUF(I).EQ.' ') GO TO 20
        IF(CBUF(I).EQ.'.') THEN
          DPNT=.TRUE.
          GOTO 20
        ENDIF
	IF(DPNT) DCNT=DCNT+1
	CALL ASCBIN(INBUF,I,1,DIG,ST)
	IF(ST.LT.0) THEN
	  WRITE(OUTLU,902) IAM()
	  GOTO 10
	ENDIF
C
	FLG=1
	NUM=NUM*10+DIG
20	CONTINUE
C
C CHECK IF INPUT IS VALID
C
	IF(ST.LT.0.OR.FLG.EQ.0.OR.DCNT.NE.CENT_LEN) THEN
	  WRITE(OUTLU,902) IAM()
	  GOTO 10
	ENDIF
C
	ONUM=NUM
	NUM=NUM/MON_UNIT
	IF(NUM*MON_UNIT.NE.ONUM) THEN
	  WRITE(OUTLU,903) IAM(),MON_UNIT
	  GOTO 10
	ENDIF
C
C
C
10000	CONTINUE
C
	RETURN
C
C
C
900	FORMAT(5A4)
902	FORMAT(1X,A,' Invalid amount')
903	FORMAT(1X,A,' Invalid amount, Amount must be divisible by ',I8)
	END

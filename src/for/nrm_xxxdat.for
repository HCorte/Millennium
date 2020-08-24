C
C SUBROUTINE XXXDAT
C
C V02 24-MAY-1999 UXN Output LUN changed from 5 to 6.
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
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE XXXDAT(PRMFLG, CDC,EXT)
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	LOGICAL	  PRMFLG
C
	INTEGER*2 SDAT(LDATE_LEN)
	CHARACTER CBUF(20)
	CHARACTER CXINBUF*(20)
	INTEGER*4 INLEN
	INTEGER*4 TEMP
	INTEGER*4 INBUF(5),DDAT(3), EXT, ST
	INTEGER*4 L, K, N, INC, CDC, I, DIG
C
	EQUIVALENCE(INBUF,CBUF,CXINBUF)
	EQUIVALENCE(DDAT,SDAT(9))
C
C
C
40	CONTINUE
	EXT=0
	DO 41 I=1,LDATE_LEN
	  SDAT(I)=0
41	CONTINUE
C
C
	IF(PRMFLG)THEN
	  CALL PRMTEXT('Enter date, Julian date, or day number ',CXINBUF,
     *                  INLEN)
	ELSE
	  CALL WIMG(6,'Enter date, Julian date, or day number ')
	  READ(5,900) INBUF
	ENDIF
C
C CHECK FOR INPUT
C
	IF(CXINBUF.EQ.' ')GOTO 40
C
C CHECK FOR EXIT
C
	IF(CBUF(1).EQ.'E'.OR.CBUF(1).EQ.'e') THEN
	  EXT=-1
	  GOTO 9000
	ENDIF
C
C
	L=0
	K=1
	DO 50 I=1,20
	 IF(CBUF(I).EQ.CHAR(0)) GOTO 50
	 IF(CBUF(I).EQ.' ') GOTO 50
	 IF(CBUF(I).EQ.'.') GOTO 45
	 IF(CBUF(I).EQ.'/') GOTO 45
	 IF(CBUF(I).LT.'0') GOTO 60
	 IF(CBUF(I).GT.'9') GOTO 60
	 CALL ASCBIN(INBUF,I,1,DIG,ST)
	 SDAT(K)=SDAT(K)*10+DIG
	 L=L+1
 	 IF(L.GE.4.AND.K.EQ.1) GOTO 60
	 GOTO 50
45	 CONTINUE
	 L=0
	 K=K+1
	 IF(K.GE.4) GOTO 60
50	CONTINUE
C
C
	IF(K.NE.3) GOTO 55
	TEMP = SDAT(2)
	SDAT(2) = SDAT(1)
	SDAT(1) = TEMP
	CALL LBDATE(SDAT)
	IF(SDAT(VDOW).EQ.0) GOTO 60
	N=SDAT(VCDC)
	GOTO 90
C
C
55	CONTINUE
	IF(K.NE.2) GOTO 60
	SDAT(VJUL)=SDAT(1)
	SDAT(VYEAR)=MOD(SDAT(2), 100)
	CALL LJDATE(SDAT)
	IF(SDAT(VDOW).EQ.0) GOTO 60
	N=SDAT(VCDC)
	GOTO 90
C
C
60	CONTINUE
	DDAT(1)=INBUF(1)
	DDAT(2)=INBUF(2)
	DDAT(3)=INBUF(3)
	CALL LAADATE(SDAT)
	IF(SDAT(VDOW).EQ.0) GOTO 70
	N=SDAT(VCDC)
	GOTO 90
C
C
70	CONTINUE
	L=0
	N=0
	K=1
	IF(CBUF(1).EQ.'A'.OR.CBUF(1).EQ.'B'.OR.
     *	     CBUF(1).EQ.'a'.OR.CBUF(1).EQ.'b') K=2
	DO 80 I=K,20
	IF(CBUF(I).EQ.' ') GOTO 80
	IF(CBUF(I).EQ.CHAR(0)) GOTO 80
	IF(CBUF(I).LT.'0') GOTO 120
	IF(CBUF(I).GT.'9') GOTO 120
	CALL ASCBIN(INBUF,I,1,DIG,ST)
	N=N*10+DIG
	L=L+1
	IF(L.GT.8) GOTO 120
80	CONTINUE
	IF(CBUF(1).NE.'A'.AND.CBUF(1).NE.'B'.AND.
     *	     CBUF(1).NE.'a'.AND.CBUF(1).NE.'b') GOTO 90
	INC=N
	IF(CBUF(1).EQ.'B'.OR.CBUF(1).EQ.'b') INC=N-1
	N=CDC+INC
C
C
90	CONTINUE
	SDAT(VCDC)=N
	CALL LCDATE(SDAT)
	WRITE(6,903) IAM(),(SDAT(I),I=7,13)
	CDC=SDAT(VCDC)
	GOTO 9000
C
C
120	CONTINUE
	WRITE(6,904)IAM()
	GOTO 40
C

9000	CONTINUE
	RETURN
C
900	FORMAT(5A4)
903	FORMAT(1X,A,'Date entered: ',7A2)
904	FORMAT(1X,A,'Invalid input ')
	END

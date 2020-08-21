C
C SUBROUTINE SCRVER
C $Log:   GXAFXT:[GOLS]SCRVER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:53:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:35:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - scrver.for **
C
C SCRVER.FOR
C
C V02 14-JUL-92 WLM CHECKED AND RELEASED FOR THE NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS LOTTERY ENTRY OF SCORE RESULTS.
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
	SUBROUTINE SCRVER(GNUM,GIND,DRAW,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
	INTEGER*4 GIND, DRAW, X, K, NUM, EXT, FLAG, ST, GNUM
	CHARACTER*45 BUF
C
C
	WRITE(5,901) IAM(),GTNAMES(TSCR),GIND,DRAW,
     *	             (DSCNM1(X),X=1,3),(DSCNM2(X),X=1,3)
100	CONTINUE
	WRITE (BUF,902) (DSCNM1(K),K=1,3)
	CALL INPNUM(BUF,NUM,0,99,EXT)
	IF(EXT.NE.0.AND.EXT.NE.-5) GOTO 100
	IF(EXT.EQ.-5) NUM=-1
	DSCHLD(1)=NUM
C
C
110	CONTINUE
	WRITE (BUF,902) (DSCNM2(K),K=1,3)
	CALL INPNUM(BUF,NUM,0,99,EXT)
	IF(EXT.NE.0.AND.EXT.NE.-5) GOTO 110
	IF(EXT.EQ.-5) NUM=-1
	DSCHLD(2)=NUM
C
	WRITE(5,903) IAM(),(DSCNM1(K),K=1,3),DSCHLD(1),
     *	             IAM(),(DSCNM2(K),K=1,3),DSCHLD(2)
	CALL WIMG(5,'Are the scores entered correct [Y/N] ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) GOTO 100
C
C CHECK AGAINST OPERATOR ENTRY
C
	IF(DSCWIN(1).NE.DSCHLD(1).OR.
     *	   DSCWIN(2).NE.DSCHLD(2)) THEN
	  TYPE*,IAM(),'Verification error, please re-enter '
	  OPDONE=0
	  DSCSTS=GAMBFD
	  ST=-1
	  RETURN
	ENDIF
C
C
	ST=0
	DSCSTS=GAMENV
	RETURN
C
C
901	FORMAT(1X,A,1X,A8,I1,' event ',I4,1X,3A4,' vs ',3A4)
902	FORMAT('Enter ',3A4,' score [C to cancel event]:')
903	FORMAT(1X,A,' Score entered for ',3A4,' is ',I2,/,
     *	       1X,A,' Score entered for ',3A4,' is ',I2)
	END

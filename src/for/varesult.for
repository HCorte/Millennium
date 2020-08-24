C SUBROUTINE VARESULT
C
C V04 30-MAR-2015 MTK Modified Super 14 game
C V03 30-OCT-2003 FRP  Modify for Batch2 Totobola Changes.
C V02 01-FEB-2000 OXK SPGNBR replaced with DSPMAX etc (Vakio changes)
C V01 15-DEC-1999 PXO Some parts taken from lliable.for
C
C SUBROUTINE TO GENERATE RESULT REPORT FOR VAKIO
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE VARESULT(GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4 I,J           !
	INTEGER*4 DRAW          !
	INTEGER*4 LINCNT        !
	INTEGER*4 PAGE          !
	INTEGER*4 ST            !
	INTEGER*4 K             !
	INTEGER*4 GNUM          !
	INTEGER*4 GIND          !
	INTEGER*4 COPY          !
	INTEGER*4 BCNT
	INTEGER*2 BSDATE(LDATE_LEN)
	INTEGER*2 ESDATE(LDATE_LEN)
	INTEGER*2 DRDATE(LDATE_LEN)
	INTEGER*4 SCORE(2)
C
	INTEGER*4 DSPFDB(7)     !
	INTEGER*4 REPLU/7/      !
C
	INTEGER*4 TOTSAL
	CHARACTER YXRISTI2(4)
	CHARACTER RCHVAL(4)
C
	INTEGER*2 DATE(LDATE_LEN) /LDATE_LEN*0/   !
C
	CHARACTER STATUS*17         !
	CHARACTER REPHDR*40         !
	CHARACTER REPNAM*14         !
C
	DATA YXRISTI2 /'1','X','C','2'/
	DATA RCHVAL   /'0','1','C','M'/
C
C BEGIN CODE -----------------------------------------
C
C
	DATE(5) = DAYCDC
	CALL LCDATE(DATE)
C
C
	GNUM=GTNTAB(TSPT,GIND)
	IF(GNUM.LT.1) GOTO 1000
	IF(DAYHDR(GNUM).LT.1) GOTO 1000

        WRITE(6,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C
C
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(DSPFDB,3,DSPSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(DSPFDB,DRAW,DSPREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
	CALL USRCLOS1(3)

	WRITE (REPHDR,8001) GSNAMES(GNUM),(DATE(K),K=7,13)
	WRITE (REPNAM,8002) GIND
	CALL ROPEN(REPNAM,REPLU,ST)
	IF(ST.NE.0) THEN
	    TYPE*,IAM(),REPNAM,' report file open error > ',ST
	    CALL GPAUSE
	ENDIF

	PAGE=1
	CALL TITLE(REPHDR,REPNAM(1:8),1,REPLU,PAGE,DAYCDC)
	WRITE(REPLU,9000)
	LINCNT = 7
C
	WRITE(REPLU, 9001)
	WRITE(REPLU, 9002)
C
	BSDATE(VCDC) = DSPBSD
	ESDATE(VCDC) = DSPESD
	DRDATE(VCDC) = DSPDAT(CURDRW)
	CALL LCDATE(BSDATE)
	CALL LCDATE(ESDATE)
	CALL LCDATE(DRDATE)
	WRITE(REPLU, 9003) GIND, DRAW, (BSDATE(K),K=9,13),
     *                           (ESDATE(K),K=9,13), DISTIM(DSPTIM),
     *                           (DRDATE(K),K=9,13)
	TOTSAL = 0
	DO I = 1, SPGENT
	    TOTSAL = TOTSAL + DSPSAL(I)
	ENDDO

	BCNT = 0
	IF(DSPFRG.NE.0) BCNT = 1

	WRITE(REPLU, 9004) CMONY(TOTSAL,13,BETUNIT)
	WRITE(REPLU, 9005) (YXRISTI2(DSPWIN(I)),I=1,DSPMAX-BCNT)
C
	IF(DSPFRG.EQ.1) THEN
	  SCORE(1)=ISHFT(DSPWIN(DSPMAX),-4)
	  SCORE(2)=IAND(DSPWIN(DSPMAX),'0F'X)
          WRITE(REPLU, 9006) (RCHVAL(SCORE(J)),J=1,2)
        ENDIF

	IF(DSPFRG.EQ.2) THEN
          WRITE(REPLU, 9007) YXRISTI2(DSPWIN(DSPMAX))
	ENDIF
C
	CALL USRCLOS1(REPLU)
	COPY=0
	CALL SPOOL(REPNAM,COPY,STATUS)

1000	CONTINUE
C
C     ===================== Format Statements =================
C
8000	FORMAT(1X,A,' Generating ',4A4,' results report')
8001	FORMAT(A4,' RESULTS  REPORT  FOR ',7A2)
8002	FORMAT('VA',I1,'RESULTS','.REP')
C
9000	FORMAT(1X,131('='))
9001    FORMAT(/,1X,'Index',2X,'Draw',25X,'Start Date',4X,'End Date',4X,
     *		  'Time',5X,'Draw Date')
9002	FORMAT(1X,131('-'))
9003	FORMAT(/,I5,2X,I4,26X, 5A2,4X,5A2,1X,A8,2X,5A2)
9004    FORMAT(/,1X,'Sales:',23X,A13)
9005	FORMAT(/,1X,'Winning Numbers:',1X,<DSPMAX-BCNT>(A1,1X))
9006	FORMAT(/,1X,'SUPER 14 Winning Results:',1X,2(A1,1X))
9007    FORMAT(/,1X,'SUPER 14 Winning Numbers:',1X,A1)
C
	RETURN
	END

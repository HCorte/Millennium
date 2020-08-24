C
C SUBROUTINE SCRESULT
C  
C SCRESULT.FOR
C
C V01 15-DEC-1999 PXO Some parts taken from lliable.for
C
C
C SUBROUTINE TO GENERATE RESULT REPORT
C SCORE, TULOS
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE SCRESULT(GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DSCREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4 I             !
	INTEGER*4 DRAW          !
	INTEGER*4 LINCNT        !
	INTEGER*4 PAGE          !
	INTEGER*4 ST            !
	INTEGER*4 K             !
	INTEGER*4 GNUM          !
	INTEGER*4 GIND          !
	INTEGER*4 COPY          !
	INTEGER*2 BSDATE(LDATE_LEN)
	INTEGER*2 ESDATE(LDATE_LEN)
	INTEGER*2 DRDATE(LDATE_LEN)
C
	INTEGER*4 DSCFDB(7)     !
	INTEGER*4 REPLU/7/      !
C
	INTEGER*4 TOTSAL
C
	INTEGER*2 DATE(LDATE_LEN) /LDATE_LEN*0/   !
C
	CHARACTER STATUS*17         !
	CHARACTER REPHDR*40         !
	CHARACTER REPNAM*14         !
C
C BEGIN CODE -----------------------------------------
C
C
	DATE(5) = DAYCDC
	CALL LCDATE(DATE)
C
C
	GNUM=GTNTAB(TSCR,GIND)
	IF(GNUM.LT.1) GOTO 1000
	IF(DAYHDR(GNUM).LT.1) GOTO 1000

        WRITE(5,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C
C
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(DSCFDB,3,DSCSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(DSCFDB,DRAW,DSCREC,ST)
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
	BSDATE(VCDC) = DSCBSD
	ESDATE(VCDC) = DSCESD
	DRDATE(VCDC) = DSCDAT
	CALL LCDATE(BSDATE)
	CALL LCDATE(ESDATE)
	CALL LCDATE(DRDATE)
	WRITE(REPLU, 9003) GIND, DRAW,
     *                     (DSCNM1(I),I=1,3),(DSCNM2(I),I=1,3),
     *                     (BSDATE(K),K=9,13),
     *                     (ESDATE(K),K=9,13), DISTIM(DSCTIM),
     *                     (DRDATE(K),K=9,13)
	TOTSAL = DSCSAL
	WRITE(REPLU, 9004) CMONY(TOTSAL,13,BETUNIT)
C
	IF(DSCSTS.GE.GAMCAN) THEN
	  WRITE(REPLU,9009)
	  GOTO 555
	ENDIF
C	
	WRITE(REPLU, 9008)
	IF(DSCSTS.LT.GAMENV) THEN
	   TYPE*,IAM(),'Results not in yet'
	ELSE
	   WRITE(REPLU, 9006) (DSCNM1(I),I=1,3), DSCWIN(1)
	   WRITE(REPLU, 9007) DSCODS/100,MOD(DSCODS,100),CMONY(DSCABW,9,BETUNIT)
	   WRITE(REPLU, 9006) (DSCNM2(I),I=1,3), DSCWIN(2)
	ENDIF
C
555	CONTINUE
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
8002	FORMAT('TU',I1,'RESULTS','.REP')
C
9000	FORMAT(1X,131('='))
9001    FORMAT(/,1X,'Index',1X,'Draw',1X,'Target',24X,'Start Date',4X,'End Date',5X,
     *          'Time',8X,'Draw Date')
9002	FORMAT(1X,131('-'))
9003	FORMAT(/,1X,I1,4X,I4,1X,3A4,' - ',3A4,3X,5A2,4X,5A2,3X,A8,4X,5A2)
9004    FORMAT(/,1X,'Sales:',23X,A13,/)
9006    FORMAT(15(' '),3A4,4(' '),I2,45(' '),/)
9007    FORMAT(33(' '),I6,'.',I2.2,7(' '),A9,19(' '),/)
9008    FORMAT(15(' '),'Teams',8(' '),'Score',4(' '),'Odds',9(' '),
     *    'Amount bet',19(' '),/)
9009    FORMAT(/,1X,'Event cancelled',/)
C
	RETURN
	END

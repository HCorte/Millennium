C SUBROUTINE SSRESULT
C
C V02 16-JAN-2000 UXN # of winners line removed.
C V01 15-DEC-1999 PXO Some parts taken from lliable.for
C
C SUBROUTINE TO GENERATE RESULT REPORT SUPERSCORE, MONIVETO
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE SSRESULT(GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DSSREC.DEF'
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
	INTEGER*4 DSSFDB(7)     !
	INTEGER*4 REPLU/7/      !
C
        CHARACTER*2  MUUT/'Mu'/
	INTEGER*4 TOTSAL
        CHARACTER*70 ACROSS
        CHARACTER    AROWS(70)
        CHARACTER*2  MUUR1,MUUR2

        EQUIVALENCE  (ACROSS,AROWS)
        EQUIVALENCE  (AROWS(5),MUUR1),(AROWS(10),MUUR2)

C
	INTEGER*2 DATE(LDATE_LEN) /LDATE_LEN*0/   !
C
	CHARACTER STATUS*17         !
	CHARACTER REPHDR*40         !
	CHARACTER REPNAM*14         !
C
C BEGIN CODE -----------------------------------------
C
	DATE(5) = DAYCDC
	CALL LCDATE(DATE)
C
C
	GNUM=GTNTAB(TSSC,GIND)
	IF(GNUM.LT.1) GOTO 1000
	IF(DAYHDR(GNUM).LT.1) GOTO 1000

        WRITE(5,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C
C
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(DSSFDB,3,DSSSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(DSSFDB,DRAW,DSSREC,ST)
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
	BSDATE(VCDC) = DSSBSD
	ESDATE(VCDC) = DSSESD
	DRDATE(VCDC) = DSSDAT
	CALL LCDATE(BSDATE)
	CALL LCDATE(ESDATE)
	CALL LCDATE(DRDATE)
	WRITE(REPLU, 9003) GIND, DRAW,
     *		           (DSSMNM(I),I=1,SSNMS_LEN/4),
     *                     (BSDATE(K),K=9,13),
     *                     (ESDATE(K),K=9,13), DISTIM(DSSTIM),
     *                     (DRDATE(K),K=9,13)
	TOTSAL = DSSSAL
	WRITE(REPLU, 9004) CMONY(TOTSAL,13,BETUNIT)
C
        IF(DSSSTS.GE.GAMENV.AND.DSSSTS.LT.GAMCAN) WRITE(REPLU,950) 
     *                          DSSODS/100,MOD(DSSODS,100),
     *                          CMONY(DSSABW,10,BETUNIT)
C
C First match
C
	WRITE(REPLU,940) (DSSSNM(I,1),I=1,SSNMS_LEN/4)
        IF(DSSSTS.GE.GAMENV.AND.DSSSTS.LT.GAMCAN) THEN
           WRITE(ACROSS,941) DSSWIN(1,1),DSSWIN(2,1)
           IF(DSSWIN(1,1).EQ.15) MUUR1=MUUT
           IF(DSSWIN(2,1).EQ.15) MUUR2=MUUT
           WRITE(REPLU,952) ACROSS
        ENDIF
C
C Second match
C
        IF(DSSEST(2).NE.GAMNUL) THEN
           WRITE(REPLU,917) '2.',(DSSSNM(I,2),I=1,SSNMS_LEN/4)
	   IF(DSSSTS.GE.GAMENV.AND.DSSSTS.LT.GAMCAN) THEN
             WRITE(ACROSS,941) DSSWIN(1,2),DSSWIN(2,2)
             IF(DSSWIN(1,2).EQ.15) MUUR1=MUUT
             IF(DSSWIN(2,2).EQ.15) MUUR2=MUUT
             WRITE(REPLU,952) ACROSS
	   ENDIF
        ENDIF
C
C Third match
C
        IF(DSSEST(3).NE.GAMNUL) THEN
          WRITE(REPLU,917) '3.', (DSSSNM(I,3),I=1,SSNMS_LEN/4)
          IF(DSSSTS.GE.GAMENV.AND.DSSSTS.LT.GAMCAN) THEN
            WRITE(ACROSS,941) DSSWIN(1,3),DSSWIN(2,3)
            IF(DSSWIN(1,3).EQ.15) MUUR1=MUUT
            IF(DSSWIN(2,3).EQ.15) MUUR2=MUUT
            WRITE(REPLU,952) ACROSS
          ENDIF
        ENDIF

C
	IF(DSSSTS.GE.GAMCAN) THEN
	   WRITE(REPLU,9005)
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
8002	FORMAT('SS',I1,'RESULTS','.REP')
C
9000	FORMAT(1X,131('='))
9001    FORMAT(/,1X,'Index',1X,'Draw',1X,'Target',31X,'Start Date',4X,
     *           'End Date',5X,'Time',8X,'Draw Date')
9002	FORMAT(1X,131('-'))
9003	FORMAT(/,1X,I1,4X,I4,1X,<SSNMS_LEN/4>A4,10X 5A2,4X,5A2,3X,A8,4X,5A2)
9004    FORMAT(/,1X,'Sales:',23X,A13,/)
C9005    FORMAT(/,1X,'Combinations Played In Euros:3X,I4.2)
950     FORMAT (4X,'Odds ',I10,'.',I2.2,2X,'Amount bet ',A10,/)
940     FORMAT (1X,'1. ',<SSNMS_LEN/4>A4)
941     FORMAT (4X,I2.2,' - ',I2.2)
951     FORMAT (2A52)
952     FORMAT (2A70)
917     FORMAT (1X,A2,1X,<SSNMS_LEN/4>A4)
9005	FORMAT (/,1X,'Game cancelled',/)
C
	RETURN
	END

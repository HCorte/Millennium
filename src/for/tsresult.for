C SUBROUTINE TSRESULT
C  
C V01 15-DEC-1999 PXO Some parts taken from lliable.for
C
C SUBROUTINE TO GENERATE RESULT REPORT TOTO SELECT, PITKAVETO
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
	SUBROUTINE TSRESULT(GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DTSREC.DEF'
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
	INTEGER*4 DTSFDB(7)     !
	INTEGER*4 REPLU/7/      !
C
	INTEGER*4 FINODDS
	INTEGER*4 TOTSAL
C

	INTEGER*2 DATE(LDATE_LEN) /LDATE_LEN*0/   !
C
	CHARACTER STATUS*17         !
	CHARACTER REPHDR*40         !
	CHARACTER REPNAM*14         !
	CHARACTER RESULT(4) /'1','2','X','C'/
C
C BEGIN CODE -----------------------------------------
C
	DATE(5) = DAYCDC
	CALL LCDATE(DATE)
C
C
	GNUM=GTNTAB(TTSL,GIND)
	IF(GNUM.LT.1) GOTO 1000
	IF(DAYHDR(GNUM).LT.1) GOTO 1000

        WRITE(5,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C
C
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(DTSFDB,3,DTSSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(DTSFDB,DRAW,DTSREC,ST)
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
	BSDATE(VCDC) = DTSBSD
	ESDATE(VCDC) = DTSESD
	CALL LCDATE(BSDATE)
	CALL LCDATE(ESDATE)
	WRITE(REPLU, 9003) DRAW, (BSDATE(K),K=9,13),
     *                           (ESDATE(K),K=9,13)
	TOTSAL = DTSSAL
	WRITE(REPLU, 9004) CMONY(TOTSAL,13,BETUNIT)
C
	WRITE(REPLU, 9007)
	
	DO I = 1, MAXSRW
	  FINODDS = 0
	  IF(DTSWIN(I).EQ.ROWCAN) THEN
	     FINODDS = 100
	  ELSE
             IF(DTSWIN(I).EQ.ROWWIN) FINODDS = DTSODS(1,I)
             IF(DTSWIN(I).EQ.ROWLOS) FINODDS = DTSODS(2,I)
             IF(DTSWIN(I).EQ.ROWTIE) FINODDS = DTSODS(3,I)
          ENDIF
	  IF(FINODDS.EQ.0) THEN
	    WRITE(REPLU, 9006) I
	  ELSE
	    DRDATE(VCDC) = DTSDAT(I)
	    CALL LCDATE(DRDATE)
	    WRITE(REPLU, 9005) I,RESULT(DTSWIN(I)),FINODDS/100,MOD(FINODDS,100),
     *      DISTIM(DTSTIM(I)), (DRDATE(K),K=9,13)                   
	  ENDIF  
	ENDDO
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
8002	FORMAT('PI',I1,'RESULTS','.REP')
C
9000	FORMAT(1X,131('='))
9001    FORMAT(/,1X,'Draw',32X,'Start Date',4X,'End Date')
9002	FORMAT(1X,131('-'))
9003	FORMAT(/,I4,32X 5A2,4X,5A2,3X,A8,4X,5A2)
9004    FORMAT(/,1X,'Sales:',23X,A13,/)
9005	FORMAT(1X,I2,5X,A1,5X,I3,'.',I2.2,10X,A8, 5X, 5A2)
9006    FORMAT(1X,I2)
9007    FORMAT(1X,'Closed:',21X,'Time',9X,'pvm')
C
	RETURN
	END

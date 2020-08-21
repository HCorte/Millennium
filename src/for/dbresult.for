C
C SUBROUTINE DBRESULT
C  
C DBRESULT.FOR
C
C V02 18-JAN-2000 UXN AMount bet field corrected
C V01 15-DEC-1999 PXO Some parts taken from lliable.for
C
C
C SUBROUTINE TO GENERATE RESULT REPORT
C SUPER DOUBLE, SUPER KAKSARI
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
	SUBROUTINE DBRESULT(GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DDBREC.DEF'
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
	INTEGER*4 DDBFDB(7)     !
	INTEGER*4 REPLU/7/      !
C
	INTEGER*4 TOTSAL
        INTEGER*4 CANROWS(18)       !cancelled rows
        INTEGER*4 LCAN              !# of cancelled rows
	INTEGER*4 R
C
	INTEGER*2 DATE(LDATE_LEN) /LDATE_LEN*0/   !
C
	CHARACTER STATUS*17         !
	CHARACTER REPHDR*40         !
	CHARACTER REPNAM*14         !
	INTEGER*4 IND, IND1, IND2
C
C BEGIN CODE -----------------------------------------
C
C
	DATE(5) = DAYCDC
	CALL LCDATE(DATE)
C
C
	GNUM=GTNTAB(TDBL,GIND)
	IF(GNUM.LT.1) GOTO 1000
	IF(DAYHDR(GNUM).LT.1) GOTO 1000

        WRITE(5,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C
C
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(DDBFDB,3,DDBSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(DDBFDB,DRAW,DDBREC,ST)
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
	BSDATE(VCDC) = DDBBSD
	ESDATE(VCDC) = DDBESD
	DRDATE(VCDC) = DDBDAT
	CALL LCDATE(BSDATE)
	CALL LCDATE(ESDATE)
	CALL LCDATE(DRDATE)
	WRITE(REPLU, 9003) GIND,DRAW,
     *		           (DDBENM(I),I=1,4),
     *                     (BSDATE(K),K=9,13),
     *                     (ESDATE(K),K=9,13), DISTIM(DDBTIM),
     *                     (DRDATE(K),K=9,13)

C Cancelled rows
        LCAN=0
        DO R=1,18
           IF(DDBSTA(R).EQ.GAMCAN) THEN
              LCAN=LCAN+1
              CANROWS(LCAN)=R
           ENDIF
        ENDDO
        IF(LCAN.GT.0) THEN
           IF(LCAN.EQ.1) THEN
              WRITE(REPLU,92221) CANROWS(1)
           ELSE
              WRITE(REPLU,9222) (CANROWS(R),R=1,LCAN)
           ENDIF
        ELSE
           WRITE(REPLU,3050)
        ENDIF
	WRITE(REPLU,3050)
C
	TOTSAL = DDBSAL(DOLAMT)
	WRITE(REPLU, 9004) CMONY(TOTSAL,13,BETUNIT)
C
	IF(DDBSTS.GE.GAMCAN) THEN
	   WRITE(REPLU, 9005)
	   GOTO 555
	ENDIF
C
	WRITE(REPLU, 1003)

	IF(DDBSTS.LT.GAMENV) THEN
	  TYPE*,IAM(),'Results not in yet'
	ELSE
  	  DO I = 1, DDBCMB 
           IND1 = DDBWIN(1,I)
           IND2 = DDBWIN(2,I)
           IND  = MAXDBLRW*(IND1-1)+IND2
	    WRITE(REPLU,3051) I
	    WRITE(REPLU,3055) DDBWIN(1,I),
     *                        (DDBNMS(K,DDBWIN(1,I)),K=1,4),
     *                        CSMONY(DDBODT(IND),12,BETUNIT),
     *                        DDBODS(I)/100,MOD(DDBODS(I),100)
            WRITE(REPLU,3056)  DDBWIN(2,I),(DDBNMS(K,DDBWIN(2,I)),K=1,4)
 	  ENDDO
	ENDIF
 
C
555	CONTINUE
C	CALL USRCLOS1(REPLU)
	COPY=0
	CALL SPOOL(REPNAM,COPY,STATUS)

1000	CONTINUE
C
C     ===================== Format Statements =================
C
8000	FORMAT(1X,A,' Generating ',4A4,' results report')
8001	FORMAT(A4,' RESULTS  REPORT  FOR ',7A2)
8002	FORMAT('SK',I1,'RESULTS','.REP')
C
9000	FORMAT(1X,131('='))
9001    FORMAT(/,1X,'Index',1X,'Draw',32X,'Start Date',4X,'End Date',5X,
     *         'Time',8X,'Draw Date')
9002	FORMAT(1X,131('-'))
9003	FORMAT(/,1X,I1,4X,I4,1X,4A4,15X,5A2,4X,5A2,3X,A8,4X,5A2)
9004    FORMAT(1X,'Sales:',23X,A13)
1003    FORMAT (/,4X,'Row Results ',10X,'Amount bet',6X,'Odds')
3050	FORMAT(/)
3051    FORMAT(1X,'(',I2.2,')')

3055    FORMAT(/,1X,'1. ',I2.2,1X,4A4,A12,I8,'.',I2.2)
3056    FORMAT(/,1X,'2. ',I2.2,1X,4A4)
9222    FORMAT (/,1X,'Cancelled: ',I2.2,<LCAN-1>(',',I2.2))
92221   FORMAT (/,1X,'Cancelled: ',I2.2)
9005	FORMAT (/,1X,'Event cancelled',/)
C
	RETURN
	END

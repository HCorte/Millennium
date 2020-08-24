C
C SUBROUTINE LORESULT
C  
C BNRESULT.FOR
C
C V01 20-DEC-1999 PXO Some parts taken from lliable.for
C
C
C SUBROUTINE TO GENERATE RESULT REPORT
C BINGO
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE BNRESULT(GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DBNREC.DEF'
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
	INTEGER*4 DBNFDB(7)     !
	INTEGER*4 REPLU/7/      !
C
	INTEGER*4 TOTSAL
	INTEGER*4 Q		!
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
	GNUM=GTNTAB(TBNG,GIND)
	IF(GNUM.LT.1) GOTO 1000
	IF(DAYHDR(GNUM).LT.1) GOTO 1000

        WRITE(5,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C
C
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(DBNFDB,3,DBNSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(DBNFDB,DRAW,DBNREC,ST)
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
	BSDATE(VCDC) = DBNBSD
	ESDATE(VCDC) = DBNESD
	DRDATE(VCDC) = DBNDAT(CURDRW)
	CALL LCDATE(BSDATE)
	CALL LCDATE(ESDATE)
	CALL LCDATE(DRDATE)
	WRITE(REPLU, 9003) DRAW, (BSDATE(K),K=9,13),
     *                           (ESDATE(K),K=9,13), DISTIM(DBNTIM),
     *                           (DRDATE(K),K=9,13)
	TOTSAL = 0
	DO I = 1, BGOENT
	    TOTSAL = TOTSAL + DBNSAL(I)
	ENDDO
	TOTSAL = TOTSAL + DBNPOL(1,BGOBAB)
	WRITE(REPLU, 9004) CMONY(TOTSAL,13,BETUNIT)
	WRITE(REPLU, 9005)
C        WRITE(REPLU,9082) (DBNWAB(Q*5+1),Q=0,4)
C        WRITE(REPLU,9082) (DBNWAB(Q*5+2),Q=0,4)
C        WRITE(REPLU,9082) (DBNWAB(Q*5+3),Q=0,4)
C        WRITE(REPLU,9082) (DBNWAB(Q*5+4),Q=0,4)
C        WRITE(REPLU,9082) (DBNWAB(Q*5+5),Q=0,4)
	WRITE(REPLU,9006) (DBNWIN(Q),Q=1,10)
	WRITE(REPLU,9006) (DBNWIN(Q),Q=11,20)
	WRITE(REPLU,9006) (DBNWIN(Q),Q=21,30)
	WRITE(REPLU,9006) (DBNWIN(Q),Q=31,40)
	WRITE(REPLU,9006) (DBNWIN(Q),Q=41,50)
	WRITE(REPLU,9006) (DBNWIN(Q),Q=51,60)
	WRITE(REPLU,9006) (DBNWIN(Q),Q=61,70)
	WRITE(REPLU,9007) (DBNWIN(Q),Q=71,75)
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
8002	FORMAT('BL',I1,'RESULTS','.REP')
C
9000	FORMAT(1X,131('='))
9001    FORMAT(/,1X,'Draw',32X,'Start Date',4X,'End Date',4x,'Time',5X,'Draw Date')
9002	FORMAT(1X,130('-'))
9003	FORMAT(/,I4,33X, 5A2,4X,5A2,1X,A8,2X,5A2)
9004    FORMAT(/,1X,'Sales:',23X,A13)
9005	FORMAT(/,1X,'Winning numbers:')
9006    FORMAT(1X,10(I2.2,1X))
9007    FORMAT(1X,5(I2.2,1X))
9082    FORMAT('                  ',5(I3.3,1X))

C
	RETURN
	END

C SUBROUTINE VORESULT
C  
C V01 15-DEC-1999 PXO Some parts taken from lliable.for
C
C SUBROUTINE TO GENERATE RESULT REPORT WINNERS TIP, VOITTAJA
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
	SUBROUTINE VORESULT(GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DWIREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4 I             !
	INTEGER*4 J		!
	INTEGER*4 DRAW          !
	INTEGER*4 LINCNT        !
	INTEGER*4 PAGE          !
	INTEGER*4 ST            !
	INTEGER*4 K             !
	INTEGER*4 W	        !
	INTEGER*4 GNUM          !
	INTEGER*4 GIND          !
	INTEGER*4 COPY          !
	INTEGER*2 BSDATE(LDATE_LEN)
	INTEGER*2 ESDATE(LDATE_LEN)
	INTEGER*2 DRDATE(LDATE_LEN)
C
	INTEGER*4 DWIFDB(7)     !
	INTEGER*4 REPLU/7/      !
C
	INTEGER*4 TOTSAL
        INTEGER*4 CANRWS(MAXWRW)    !is the row canceled ?
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
	GNUM=GTNTAB(TWIT,GIND)
	IF(GNUM.LT.1) GOTO 1000
	IF(DAYHDR(GNUM).LT.1) GOTO 1000

        WRITE(5,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C
C
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(DWIFDB,3,DWISEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(DWIFDB,DRAW,DWIREC,ST)
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
	BSDATE(VCDC) = DWIBSD
	ESDATE(VCDC) = DWIESD
	DRDATE(VCDC) = DWIDAT
	CALL LCDATE(BSDATE)
	CALL LCDATE(ESDATE)
	CALL LCDATE(DRDATE)
	WRITE(REPLU, 9003) GIND, DRAW,
     *                     (DWIENM(I),I=1,3),
     *                     (BSDATE(K),K=9,13),
     *                     (ESDATE(K),K=9,13), DISTIM(DWITIM),
     *                     (DRDATE(K),K=9,13)
	TOTSAL = DWISAL
	WRITE(REPLU, 9004) CMONY(TOTSAL,13,BETUNIT)
C
	IF(DWISTS.GE.GAMCAN) THEN
	  WRITE(REPLU,9010)
	  GOTO 555
	ENDIF
C
	IF(DWISTS.LT.GAMENV) THEN
	  TYPE*,IAM(),'Results not in yet'
	ELSE
	  WRITE(REPLU, 1003)
           DO 320 W=1,4
            IF(DWIWIN(W).NE.0) THEN
              WRITE(REPLU,1004)W,DWIWIN(W),
     *          (DWINMS(I,DWIWIN(W)),I=1,4),
     *          CMONY(DWISBR(DWIWIN(W)),10,BETUNIT),DWIODS(W)/100,
     *          MOD(DWIODS(W),100)
            ENDIF
C            IF(DWIWIN(W).EQ.0) WRITE(REPLU,9009)
320         CONTINUE
	    J = 1
            DO 330 I=1,MAXWRW
            IF(DWISTA(I).EQ.GAMCAN.OR.DWISTA(I).EQ.GAMREF) THEN
              CANRWS(J)=I
              J=J+1
            ENDIF
330         CONTINUE
            IF(J.GE.2) THEN
                  WRITE(REPLU,1006) (CANRWS(I),I=1,J-1)
            ENDIF
	ENDIF
C
555	CONTINUE
	CALL USRCLOS1(REPLU)
	COPY=0
	CALL SPOOL(REPNAM,COPY,STATUS)

1000	CONTINUE
C
C     ===================== Format Statements =================
C
8000	FORMAT(1X,A,' Generating ',4A4,' results report')
8001	FORMAT(A4,' RESULTS  REPORT  FOR ',7A2)
8002	FORMAT('VO',I1,'RESULTS','.REP')
C
9000	FORMAT(1X,131('='))
9001    FORMAT(/,1X,'Index',1X,'Draw',1X,'Targe',20X,
     *           'Start Date',4X,'End Date',5X,'Time',8X,'Draw Date')
9002	FORMAT(1X,131('-'))
9003	FORMAT(/,1X,I1,4X,I4,1X,3A4,15X 5A2,4X,5A2,3X,A8,4X,5A2)
9004    FORMAT(/,1X,'Sales:',23X,A13,/)
9005    FORMAT(/,1X,'Winner        Played Eur:',3X,I4.2)
1003	FORMAT(41X,'Amount bet',4X,'Odds')
1004    FORMAT(1X,'Results  ',I1,6X,I2.2,2X,4A4,2X,A10,1X,I8,'.',I2.2)
1006    FORMAT(/,1X,'Cancelled  rows---> ',19(I2,' '))

9009	FORMAT(/)
9010    FORMAT(/,1X,'Event cancelled',/)
C
	RETURN
	END

C
C SUBROUTINE LORESULT
C  
C LORESULT.FOR
C
C V02 03-DEC-2010 MAC LUCKY NUMBER
C V01 15-DEC-1999 PXO Some parts taken from lliable.for
C
C
C SUBROUTINE TO GENERATE RESULT REPORT
C LOTTO
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
	SUBROUTINE LORESULT(GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
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
	INTEGER*4 DLTFDB(7)     !
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
	GNUM=GTNTAB(TLTO,GIND)
	IF(GNUM.LT.1) GOTO 1000
	IF(DAYHDR(GNUM).LT.1) GOTO 1000

        WRITE(5,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C
C
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(DLTFDB,3,DLTSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(DLTFDB,DRAW,DLTREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
	CALL USRCLOS1(3)

	WRITE (REPHDR,8001) GSNAMES(GNUM),(DATE(K),K=7,13)
	WRITE (REPNAM,8002) GIND
	CALL ROPEN(REPNAM,REPLU,ST)
	IF(ST.NE.0) THEN
	    TYPE*,IAM(),REPNAM,' report file open error > ',ST
	    CALL GPAUSE
	ENDIF

	PAGE=0
	CALL TITLE(REPHDR,REPNAM(1:8),1,REPLU,PAGE,DAYCDC)
	WRITE(REPLU,9000)
	LINCNT = 7
C
	WRITE(REPLU, 9001)
	WRITE(REPLU, 9002)
C
	BSDATE(VCDC) = DLTBSD
	ESDATE(VCDC) = DLTESD
	DRDATE(VCDC) = DLTDAT(CURDRW)
	CALL LCDATE(BSDATE)
	CALL LCDATE(ESDATE)
	CALL LCDATE(DRDATE)
	WRITE(REPLU, 9003) DRAW, (BSDATE(K),K=9,13),
     *                           (ESDATE(K),K=9,13), DISTIM(DLTTIM),
     *                           (DRDATE(K),K=9,13)
	TOTSAL = 0
	DO I = 1, LTGENT
	    TOTSAL = TOTSAL + DLTSAL(I)
	ENDDO
	WRITE(REPLU, 9004) CMONY(TOTSAL,13,BETUNIT)
	IF(DLTBFL.GT.0 .AND. DLTLFL.LE.0) THEN                !V02...
	   WRITE(REPLU, 9005) (DLTWIN(I,1),I=1,DLTNUM), (DLTBNM(K,1),K=1,DLTBFL)
	ELSE IF (DLTBFL.LE.0 .AND. DLTLFL.GT.0) THEN
           WRITE(REPLU, 90052) (DLTWIN(I,1),I=1,DLTNUM), DLTLNM(1)
	ELSE IF (DLTBFL.GT.0 .AND. DLTLFL.GT.0) THEN
           WRITE(REPLU, 90053) (DLTWIN(I,1),I=1,DLTNUM), (DLTBNM(K,1),K=1,DLTBFL), DLTLNM(1)
        ELSE                                                  !...V02
	   WRITE(REPLU, 90051) (DLTWIN(I,1),I=1,DLTNUM)
	ENDIF

	CALL USRCLOS1(REPLU)
	COPY=0
	CALL SPOOL(REPNAM,COPY,STATUS)

1000	CONTINUE
C
C     ===================== Format Statements =================
C
8000	FORMAT(1X,A,' Generating ',4A4,' results report')
8001	FORMAT(A4,' RESULTS  REPORT  FOR ',7A2)
8002	FORMAT('LO',I1,'RESULTS','.REP')
C
9000	FORMAT(1X,131('='))
9001    FORMAT(/,1X,'Draw',32X,'Start Date',4X,'End Date',4x,'Time',5X,'Draw Date')
9002	FORMAT(1X,130('-'))
9003	FORMAT(/,I4,33X, 5A2,4X,5A2,1X,A8,2X,5A2)
9004    FORMAT(/,1X,'Sales:',23X,A13)
9005	FORMAT(/,1X,'Winning Numbers:',1X,<DLTNUM>(I2.2,1X),
     *           1X,'Bonnus Number  : ',<DLTBFL>(I2.2,1X))
90052	FORMAT(/,1X,'Winning Numbers:',1X,<DLTNUM>(I2.2,1X),  !V02...
     *           1X,'Lucky Number  : ',I2.2)
90053	FORMAT(/,1X,'Winning Numbers:',1X,<DLTNUM>(I2.2,1X),
     *           1X,'Bonnus Number  : ',<DLTBFL>(I2.2,1X),
     *           1X,'Lucky Number  : ',I2.2)                  !...V02
90051	FORMAT(/,1X,'Winning Numbers',1X,<DLTNUM>(I2.2,1X))
C
	RETURN
	END

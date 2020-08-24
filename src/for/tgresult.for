C SUBROUTINE TGRESULT
C 
C V01 01-DEC-2000 UXN INITIAL RELEASE.
C
C SUBROUTINE TO GENERATE RESULT REPORT FOR TOTOGOLO
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
	SUBROUTINE TGRESULT(GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DTGREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:REPPRF.DEF'
C
	INTEGER*4 I
	INTEGER*4 DRAW          
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
	INTEGER*4 DTGFDB(7)     !
	INTEGER*4 REPLU/7/      !
C
	INTEGER*4 TOTSAL
C
	INTEGER*2 DATE(LDATE_LEN) /LDATE_LEN*0/   !
C
	CHARACTER STATUS*17         !
	CHARACTER REPHDR*40         !
	CHARACTER REPNAM*14         !
        CHARACTER*1 VAL(0:3)/'0','1','2','M'/
C
C BEGIN CODE -----------------------------------------
C
C
	DATE(5) = DAYCDC
	CALL LCDATE(DATE)
C
C
	GNUM=GTNTAB(TTGL,GIND)
	IF(GNUM.LT.1) GOTO 1000
	IF(DAYHDR(GNUM).LT.1) GOTO 1000

        WRITE(6,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C
C
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(DTGFDB,3,DTGSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(DTGFDB,DRAW,DTGREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
	CALL USRCLOS1(3)

	WRITE (REPHDR,8001) GSNAMES(GNUM),(DATE(K),K=7,13)
	WRITE (REPNAM,8002) PREFIX(TTGL),GIND
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
	BSDATE(VCDC) = DTGBSD
	ESDATE(VCDC) = DTGESD
	DRDATE(VCDC) = DTGDAT(CURDRW)
	CALL LCDATE(BSDATE)
	CALL LCDATE(ESDATE)
	CALL LCDATE(DRDATE)
	WRITE(REPLU, 9003) GIND, DRAW, (BSDATE(K),K=9,13),
     *                           (ESDATE(K),K=9,13), DISTIM(DTGTIM),
     *                           (DRDATE(K),K=9,13)
	TOTSAL = 0
	DO I = 1, SPGENT
	    TOTSAL = TOTSAL + DTGSAL(I)
	ENDDO
	WRITE(REPLU, 9004) CMONY(TOTSAL,13,BETUNIT)           
	WRITE(REPLU, 9005) 

	DO I=1,DTGMAX
            WRITE(REPLU,9006)   (DTGNMS(K,1,I),K=1,TGNMS_LEN / 4),
     *                          (DTGNMS(K,2,I),K=1,TGNMS_LEN / 4),
     *                          VAL(DTGWIN(1,I)),
     *                          VAL(DTGWIN(2,I))
        ENDDO

	CALL USRCLOS1(REPLU)
	COPY=0
	CALL SPOOL(REPNAM,COPY,STATUS)

1000	CONTINUE
C
C     ===================== Format Statements =================
C
8000	FORMAT(1X,A,' Generating ',4A4,' results report')
8001	FORMAT(A4,' RESULTS  REPORT  FOR ',7A2)
8002	FORMAT(A2,I1,'RESULTS','.REP')
C
9000	FORMAT(1X,131('='))
9001    FORMAT(/,1X,'Index',2X,'Draw',25X,'Began  ',7X,'Ended   ',4X,
     *		  'Time',4X,'Draw date')
9002	FORMAT(1X,131('-'))
9003	FORMAT(/,I5,2X,I4,26X, 5A2,4X,5A2,1X,A8,2X,5A2)
9004    FORMAT(/,1X,'SALES: ',23X,A13)
9005	FORMAT(/,1X,'RESULTS:   ',/)
9006    FORMAT(1X,<TGNMS_LEN/4>A4,' - ', <TGNMS_LEN/4>A4,
     *         10X,A1,' : ',A1)
C
	RETURN
	END

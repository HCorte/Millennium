C SUBROUTINE STRESULT
C  
C V02 18-JAN-2000 UXN Fix for Amount bet.
C V01 16-DEC-1999 PXO Some parts taken from lliable.for
C
C SUBROUTINE TO GENERATE RESULT REPORT SUPER TRIPLE
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
	SUBROUTINE STRESULT(GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DSTREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:STROCOM.DEF'
	INCLUDE 'INCLIB:STRFREC.DEF'
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
	INTEGER*4 DSTFDB(7)     !
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
	INTEGER*4 IND,IND1,IND2,IND3,BETAMT, PFDB(7)
C
C BEGIN CODE -----------------------------------------
C
	DATE(5) = DAYCDC
	CALL LCDATE(DATE)
C
C
	GNUM=GTNTAB(TSTR,GIND)
	IF(GNUM.LT.1) GOTO 1000
	IF(DAYHDR(GNUM).LT.1) GOTO 1000

        WRITE(5,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C
C
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(DSTFDB,3,DSTSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(DSTFDB,DRAW,DSTREC,ST)
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
	BSDATE(VCDC) = DSTBSD
	ESDATE(VCDC) = DSTESD
	DRDATE(VCDC) = DSTDAT
	CALL LCDATE(BSDATE)
	CALL LCDATE(ESDATE)
	CALL LCDATE(DRDATE)
	WRITE(REPLU, 9003)        GIND, DRAW,
     *			         (DSTENM(I),I=1,4), 
     *				 (BSDATE(K),K=9,13),
     *                           (ESDATE(K),K=9,13), DISTIM(DSTTIM),
     *                           (DRDATE(K),K=9,13)
	TOTSAL = DSTSAL(DOLAMT)
	WRITE(REPLU, 9004) CMONY(TOTSAL,13,BETUNIT)
C	WRITE(REPLU, 9005) CMONY(DSTABW,12,BETUNIT)	     
C
	IF(DSTSTS.GE.GAMCAN) THEN
	  WRITE(REPLU,9006)
	  GOTO 555
	ENDIF	

	IF(DSTSTS.LT.GAMENV) THEN
	  TYPE*,IAM(),'Results not in yet'
	ELSE

	  WRITE(REPLU, 932)

C
C CANCELLED ROWS
C
          LCAN=0
          DO R=1,18
             IF(DSTSTA(R).EQ.GAMCAN) THEN
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
          ENDIF
C Get winning odds.
C open and read super triple pool file
C
5       CONTINUE
        CALL OPENQW(4,DSTPFN,4,0,0,ST)
        IF(ST.NE.0) THEN
           WRITE(6,904) IAM(),(DSTPFN(K),K=1,4),ST
           CALL GPAUSE
           GOTO 5
        ENDIF
        CALL IOQINIT(PFDB,4,STRFSEC*256)
        CALL READQW(PFDB,1,STRFREC,ST)
        CALL CLOSEQFIL(PFDB)
C
C
C WINNING NUMBERS
C
 	  DO I = 1, DSTCMB
            IND1 = DSTWIN(1,I)
            IND2 = DSTWIN(2,I)
            IND3 = DSTWIN(3,I)
            IND  = (IND3-1)*MAXSTRRW*MAXSTRRW+(IND2-1)*MAXSTRRW+IND1
            BETAMT = STRFODDS(STRGAMT,IND)
	    WRITE(REPLU, 3051), I	   
            WRITE(REPLU,3055) DSTWIN(1,I),
     *               (DSTNMS(K,DSTWIN(1,I)),K=1,4),
     *                CSMONY(BETAMT,12,BETUNIT),
     *                DSTODS(I)/100,MOD(DSTODS(I),100)
            WRITE(REPLU,3057)  DSTWIN(2,I),
     *               (DSTNMS(K,DSTWIN(2,I)),K=1,4)
            WRITE(REPLU,3057)  DSTWIN(3,I),
     *               (DSTNMS(K,DSTWIN(3,I)),K=1,4)
 	  ENDDO
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
8002	FORMAT('ST',I1,'RESULTS','.REP')
C
9000	FORMAT(1X,131('='))
9001    FORMAT(/,1X,'Index',1X,'Draw',1X,'Target',30X,'Start Date',4X,'End Date',4x,
     *          'Time',5X,'Draw Date')
9002	FORMAT(1X,131('-'))
9003	FORMAT(/,1X,I2,4X,I4,4A4,15X,5A2,4X,5A2,3X,A8,4X,5A2)
9004    FORMAT(/,1X,'Sales:',23X,A13)
9005	FORMAT(/,1X,'Combinations Played In Euros: ',A12)
C
932     FORMAT(/4X,'Row results ',10X,'Amount bet',6X,'Odds')
3051    FORMAT(/,1X,'(',I2.2,')')
3055    FORMAT(/,1X,'1. ',I2.2,1X,4A4,A12,I8,'.',I2.2)
3056    FORMAT(1X,'2. ',I2.2,1X,4A4,12X,I11)
3057    FORMAT(1X,'3. ',I2.2,1X,4A4,23X)

9222    FORMAT (1X,'Cancelled: ',I2.2,<LCAN-1>(',',I2.2))
92221   FORMAT (1X,'Cancelled: ',I2.2)
9006	FORMAT (/,1X,'Event cancelled',/)
904     FORMAT(1X,A,'Cannot open pool file ',4A4,' status>', I4)
C
	RETURN
	END

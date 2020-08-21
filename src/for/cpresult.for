C
C SUBROUTINE CPRESULT
C  
C CPRESULT.FOR
C
C V02 08-FEB-2000 UXN Fix for ODDS.
C V01 15-DEC-1999 PXO Some parts taken from lliable.for,parsnp.for
C
C SUBROUTINE TO GENERATE RESULT REPORT
C TODAYS COUPLE, PAIVAN PARI
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
	SUBROUTINE CPRESULT(GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DCPREC.DEF'
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
	INTEGER*4 DCPFDB(7)     !
	INTEGER*4 REPLU/7/      !
C
	INTEGER*4 TOTSAL
        INTEGER*4 NUMWIN_E1         !# winners in E1
        INTEGER*4 NUMWIN_E2         !# winners in E2
        INTEGER*4 R                 !,,      ,,
        INTEGER*4 A_CAN(18),B_CAN(18),LA,LB
        INTEGER*4   WINNERS(MAXCPLRW,2)
        CHARACTER*10 A10_SPACE
        CHARACTER*10 A10_CANC

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
        NUMWIN_E1 = 0
        NUMWIN_E2 = 0
        A10_SPACE = ' '
        A10_CANC = 'Cancelled '

	DATE(5) = DAYCDC
	CALL LCDATE(DATE)
C
C
	GNUM=GTNTAB(TCPL,GIND)
	IF(GNUM.LT.1) GOTO 1000
	IF(DAYHDR(GNUM).LT.1) GOTO 1000

        WRITE(5,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C
C
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(DCPFDB,3,DCPSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(DCPFDB,DRAW,DCPREC,ST)
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
	BSDATE(VCDC) = DCPBSD
	ESDATE(VCDC) = DCPESD
	DRDATE(VCDC) = DCPDAT
	CALL LCDATE(BSDATE)
	CALL LCDATE(ESDATE)
	CALL LCDATE(DRDATE)
	WRITE(REPLU, 9003) GIND, DRAW,
     *                     (BSDATE(K),K=9,13),
     *                     (ESDATE(K),K=9,13), DISTIM(DCPTIM),
     *                     (DRDATE(K),K=9,13)
C
C WRITE EVENT NAMES AND POSSIBLE CANCELLATIONS
C
        CALL FASTSET(0,WINNERS,2*MAXCPLRW)
        DO I = 1,MAXCPLTI
          IF (DCPWIN(1,I) .GT. 0) WINNERS(DCPWIN(1,I),1) = 1
          IF (DCPWIN(2,I) .GT. 0) WINNERS(DCPWIN(2,I),2) = 1
        ENDDO
        DO I=1, MAXCPLRW
           IF(WINNERS(I,1).GT.0) NUMWIN_E1 = NUMWIN_E1 + 1
           IF(WINNERS(I,2).GT.0) NUMWIN_E2 = NUMWIN_E2 + 1
        ENDDO

          IF (DCPEST(1) .NE. GAMCAN) THEN
            WRITE(REPLU,9022) (DCPENM(I,1),I=1,4),NUMWIN_E1,A10_SPACE
          ELSE
            WRITE(REPLU,9022) (DCPENM(I,1),I=1,4),NUMWIN_E1,A10_CANC
          END IF
          IF (DCPEST(2) .NE. GAMCAN) THEN
            WRITE(REPLU,9222) (DCPENM(I,2),I=1,4),NUMWIN_E2,A10_SPACE
          ELSE
            WRITE(REPLU,9222) (DCPENM(I,2),I=1,4),NUMWIN_E2,A10_CANC
          END IF
          LA=0
          LB=0
          DO R=1,18
             IF(DCPSTA(R).EQ.GAMCAN) THEN
                LA=LA+1
                A_CAN(LA)=R
             ENDIF
             IF(DCPSTA(R+18).EQ.GAMCAN) THEN
                LB=LB+1
                B_CAN(LB)=R
             ENDIF
          ENDDO
          IF(LA.GT.0) THEN
             IF(LA.EQ.1) THEN
                WRITE(REPLU,90241) A_CAN(1)
             ELSE
                WRITE(REPLU,9024) (A_CAN(R),R=1,LA)
             ENDIF
          ENDIF
          IF(LB.GT.0) THEN
             IF(LB.EQ.1) THEN
                WRITE(REPLU,90251) B_CAN(1)
             ELSE
                WRITE(REPLU,9025) (B_CAN(R),R=1,LB)
             ENDIF
          ENDIF

C
	TOTSAL = DCPSAL(DOLAMT)
	WRITE(REPLU, 9004) CMONY(TOTSAL,13,BETUNIT)

	IF(DCPSTS.GE.GAMCAN) THEN
	  WRITE(REPLU, 9026)
	  GOTO 555
        ENDIF

	WRITE(REPLU, 1003)
C
C RESULTS	     
C
        IF (DCPSTS .LT. GAMENV) THEN
	  TYPE*,IAM(),'Results not in yet'
	ELSE
          DO I=1,DCPCMB
            IND1 = DCPWIN(1,I)
            IND2 = DCPWIN(2,I)
            IND = (IND1-1)*(MAXCPLRW/2)+(IND2-MAXCPLRW/2)
            WRITE(REPLU,3051) I
            WRITE(REPLU,3055) DCPWIN(1,I),
     *               (DCPNMS(K,DCPWIN(1,I)),K=1,4),
     *                CSMONY(DCPODT(IND),12,BETUNIT),
     *                DCPODS(I)/100,MOD(DCPODS(I),100)
            WRITE(REPLU,3056)  DCPWIN(2,I)-MAXCPLRW/2,
     *               (DCPNMS(K,DCPWIN(2,I)),K=1,4)
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
8002	FORMAT('CP',I1,'RESULTS','.REP')
C
9000	FORMAT(1X,131('='))
9001    FORMAT(/,1X,'Index',1X,'Draw',31X,'Start Date',4X,'End Date',
     *           4X,'Time',5X,'Draw Date')
9002	FORMAT(1X,131('-'))
9003	FORMAT(/,1X,I1,4X,I4,32X 5A2,4X,5A2,3X,A8,4X,5A2)
9004    FORMAT(/,1X,'Sales:',23X,A13)

1003    FORMAT(/,3X,'Row Winner',12X,'Amount bet',7X,'Odds')
3051    FORMAT(1X,'(',I2.2,')')
3055    FORMAT(1X,'A. ',I2.2,1X,4A4,A12,I8,'.',I2.2)
3056    FORMAT(1X,'B. ',I2.2,1X,4A4)
9022    FORMAT (/,1X,'A. ',4A4,10X,I4,' winner(s)',' ',A10)
9222    FORMAT (1X,'B. ',4A4,10X,I4,' winner(s)',' ',A10)
9024    FORMAT (1X,'A. Cancelled: ',I2.2,<LA-1>(',',I2.2))
90241   FORMAT (1X,'A. Cancelled: ',I2.2)
9025    FORMAT (1X,'B. Cancelled: ',I2.2,<LB-1>(',',I2.2))
90251   FORMAT (1X,'B. Cancelled: ',I2.2)
9026    FORMAT (/,1X,'Game cancelled',/)

C
	RETURN
	END

C SUBROUTINE TRRESULT
C  
C V01 16-DEC-1999 PXO Some parts taken from lliable.for
C
C SUBROUTINE TO GENERATE RESULT REPORT TODAYS TRIO, PAIVAN TRIO
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
	SUBROUTINE TRRESULT(GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DTRREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:TROCOM.DEF'
        INCLUDE 'INCLIB:TRFREC.DEF'
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
	INTEGER*4 DTRFDB(7)     !
	INTEGER*4 REPLU/7/      !
C
	INTEGER*4 TOTSAL
        INTEGER*4  EVENT_CNT
        INTEGER*4 CANROWS(MAXTRPRW)
	INTEGER*4 J
	INTEGER*4 L
	INTEGER*4 R
        INTEGER*4 WIN_CNT(3),WINMAP(3,MAXTRPTI)
C
	INTEGER*2 DATE(LDATE_LEN) /LDATE_LEN*0/   !
C
	CHARACTER STATUS*17         !
	CHARACTER REPHDR*40         !
	CHARACTER REPNAM*14         !
	INTEGER*4 IND,IND1,IND2,IND3,PFDB(7),BETAMT
C
C BEGIN CODE -----------------------------------------
C
	DATE(5) = DAYCDC
	CALL LCDATE(DATE)
C
C
	GNUM=GTNTAB(TTRP,GIND)
	IF(GNUM.LT.1) GOTO 1000
	IF(DAYHDR(GNUM).LT.1) GOTO 1000

        WRITE(5,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C
C
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(DTRFDB,3,DTRSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(DTRFDB,DRAW,DTRREC,ST)
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
	BSDATE(VCDC) = DTRBSD
	ESDATE(VCDC) = DTRESD
	DRDATE(VCDC) = DTRDAT
	CALL LCDATE(BSDATE)
	CALL LCDATE(ESDATE)
	CALL LCDATE(DRDATE)
	WRITE(REPLU, 9003)        GIND, DRAW,
     *			         (DTRMNM(I),I=1,TRPENM_LEN/4), 
     *				 (BSDATE(K),K=9,13),
     *                           (ESDATE(K),K=9,13), DISTIM(DTRTIM),
     *                           (DRDATE(K),K=9,13)
C
        EVENT_CNT = 0
        IF(DTREST(1).NE.GAMNUL) EVENT_CNT=EVENT_CNT+1
        IF(DTREST(2).NE.GAMNUL) EVENT_CNT=EVENT_CNT+1
        IF(DTREST(3).NE.GAMNUL) EVENT_CNT=EVENT_CNT+1
        IF(EVENT_CNT.EQ.0) GOTO 999

          CALL FASTSET(0,WINMAP,3*MAXTRPTI)
          CALL FASTSET(0,WIN_CNT,3)
          DO I = 1,3
             DO J=1,MAXTRPTI
                IF(DTRWIN(I,J).GT.0) WINMAP(I,DTRWIN(I,J)) = 1
             ENDDO
             DO J=1,MAXTRPTI
                IF(WINMAP(I,J).GT.0) WIN_CNT(I) = WIN_CNT(I)+1
             ENDDO
          ENDDO
          DO I=1,3
             J=0
             DO L=1,MAXTRPRW
                IF(DTRSTA(L,I).EQ.GAMCAN) THEN
                   J=J+1
                   CANROWS(J)=L
                ENDIF
             ENDDO
             IF(J.GT.0) THEN
                WRITE(REPLU,9301) I,(DTRENM(K,I),K=1,4),WIN_CNT(I),
     *            (CANROWS(R),R=1,J)
             ELSE
                WRITE(REPLU,930) I,(DTRENM(K,I),K=1,4),WIN_CNT(I)
             ENDIF
          ENDDO


C
	TOTSAL = DTRSAL(2)
	WRITE(REPLU, 9004) CMONY(TOTSAL,13,BETUNIT)
C
	IF(DTRSTS.GE.GAMCAN) THEN
	  WRITE(REPLU,9005)
	  GOTO 999
	ENDIF
C
	WRITE(REPLU, 932)

2010    CONTINUE
        CALL OPENQW(4,DTRPFN,4,0,0,ST)
        IF(ST.NE.0) THEN
           WRITE(5,900) IAM(),(DTRPFN(K),K=1,4),ST
           CALL GPAUSE
           GOTO 2010
        ENDIF
        CALL IOQINIT(PFDB,4,TRFSEC*256)
        CALL READQW(PFDB,1,TRFREC,ST)
	CALL CLOSEQFIL(PFDB)
	
	IF(DTRSTS.LT.GAMENV) THEN
	  TYPE*,IAM(),'Results not in yet'
	ELSE
 	  DO I = 1, DTRCMB
	     IND1 = DTRWIN(1,I)
	     IND2 = DTRWIN(2,I)
	     IND3 = DTRWIN(3,I)
             IND  = IND1 + (IND2-1)*MAXTRPRW + (IND3-1)*MAXTRPRW*MAXTRPRW
	     BETAMT = TRFODDS(TRGAMT,IND)
	    WRITE(REPLU, 3051), I	   
	    IF(DTRWIN(1,I).GT.0) THEN
	      WRITE(REPLU,3052) '1',DTRWIN(1,I),(DTRNMS(K,DTRWIN(1,I),1),K=1,4),
     *                          CSMONY(BETAMT,12,BETUNIT),
     *                          DTRODS(I)/100,MOD(DTRODS(I),100)
	    ENDIF
 	    IF(DTRWIN(2,I).GT.0) THEN
	      WRITE(REPLU,3053) '2',DTRWIN(2,I),(DTRNMS(K,DTRWIN(2,I),2),K=1,4)
	    ENDIF
 	    IF(DTRWIN(3,I).GT.0) THEN
	      WRITE(REPLU,3053) '3',DTRWIN(3,I),(DTRNMS(K,DTRWIN(3,I),3),K=1,4)
	    ENDIF
 	  ENDDO
	ENDIF
C
999	CONTINUE
	CALL USRCLOS1(REPLU)
	COPY=0
	CALL SPOOL(REPNAM,COPY,STATUS)

1000	CONTINUE
C
C     ===================== Format Statements =================
C
8000	FORMAT(1X,A,' Generating ',4A4,' results report')
8001	FORMAT(A4,' RESULTS  REPORT  FOR ',7A2)
8002	FORMAT('TR',I1,'RESULTS','.REP')
C
9000	FORMAT(1X,131('='))
9001    FORMAT(/,1X,'Index',1X,'Draw',1X,'Target',30X,'Start Date',4X,'End Date',5X,
     *          'Time',8X,'Draw Date')
9002	FORMAT(1X,131('-'))
9003	FORMAT(/,I2,4X,I4,2X,<TRPENM_LEN/4>A4,8X,5A2,4X,5A2,3X,A8,4X,5A2,/)
9004    FORMAT(/,1X,'Sales:',23X,A13,/)
C
932     FORMAT(4X,'Row Winner',10X,'Amount bet',6X,'Odds')
3051    FORMAT(1X,'(',I2.2,')')
3052    FORMAT(2X,A1,'.',1X,I2.2,1X,4A4,T23,A12,I8,'.',I2.2)
3053    FORMAT(2X,A1,'.',1X,I2.2,1X,4A4)
3056    FORMAT(24X,I12)
930     FORMAT(1X,I1,'.',1X,4A4,1X,I2,' winner(s)')
9301    FORMAT(1X,I1,'.',1X,4A4,1X,I2,' winner(s),  Cancelled ',<J>I3)
9005	FORMAT(/,1X,'Game cancelled',/)
900     FORMAT(1X,A,'Cannot open pool file ',4A4,' status>', I4)

C
	RETURN
	END

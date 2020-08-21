C SUBROUTINE UPDSTA
C
C V14 30-MAR-2015 MTK Modified Super 14 game
C V13 27-OCT-2003 FRP Modify for Batch2 Totobola Changes.
C V12 29-MAR-2000 UXN Fix for RROWTOT.EQ.0
C V11 03-MAR-2000 OXK SPGNBR used with SPSTAB, NOT NUMROWS
C V10 07-FEB-2000 OXK SPSNUM(4,SYS) changed to TRABUF(TWSRW) (Vakio changes)
C V09 13-OCT-1999 RXK World Tour added.
C V08 24-FEB-1999 UXN Reduced systems statistics added here again to avoid
C                     systems going to resync because of weigthed qp revision.
C V07 04-FEB-1999 RXK Calc. of Kicker direction statistics fixed
C V06 26-JAN-1996 RXK Rfss 94166. Fix for calculation of statistics.
C                     ATTENSION! For reduced systems statistics are now 
C                     calculated in POOLSPT.FOR!
C V05 22-AUG-1993 HXK changed dimensions of arrays
C V04 16-JUL-1993 HXK ADDED VAKIO STATS MATCH LIST
C V03 21-JUN-1993 SXH Added second kicker
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 05-MAY-1992 HDB RELEASED FOR VAX
C
C
C INPUT   : TRABUF (transaction buffer)
C OUTPUT  : none, indirectly by updating the statistics common
C ERRORS  : in case of error the routine will not update the statistics
C           common
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
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE UPDSTA(TRABUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:STACOM.DEF' 
	INCLUDE 'INCLIB:RECSSF.DEF'
C
        INTEGER*4   KGAM            !kicker game number
        INTEGER*4   GAM             !game number
        INTEGER*4   GIND            !game index
        INTEGER*4   TYPIND          !coupon type (coupon/quickpick)
        INTEGER*4   BRDIND          !board index (number of pairs)
        INTEGER*4   QUIIND          !quickpick count flags/coupon index
        INTEGER*4   IND             !misc used index
        INTEGER*4   SIGN            !add or substract (1 OR -1)
        INTEGER*4   S               !transaction state
        INTEGER*4   T               !transaction type
        INTEGER*4   COUNTER(SPGNBR,3) !counter
        INTEGER*4   ROWS(SPGNBR,16) !rows
	INTEGER*4   RROWS(2,TGGNBR,12)
        INTEGER*4   J               !counter
        INTEGER*4   I               !counter
        INTEGER*4   K,L
        INTEGER*4   ROWTOT
        INTEGER*4   KIKIND          ! JOKER game index.
C
        LOGICAL     KICKER
        REAL        RROWTOT     
C
	INTEGER*4 CNV_TAB(0:7,7) !TABLE FOR NEW WINNER TRANSFORMATION
        DATA CNV_TAB/0,0,0,1, 0,1,2,0
     *              ,0,0,0,2, 0,4,4,0
     *              ,0,0,0,3, 0,5,6,0
     *              ,0,0,0,4, 0,2,1,0
     *              ,0,0,0,5, 0,3,3,0
     *              ,0,0,0,6, 0,6,5,0
     *              ,0,0,0,7, 0,7,7,0/
        INTEGER*4 BITS(0:15) /0,1,1,2,1,2,2,3, 0,1,1,1,1,1,1,1/
	INTEGER*4 INDEX_TAB(16)
        INTEGER*4 LOCAL_TAB(16)
        INTEGER*4 TAB(16)
	INTEGER*4 OFF,PTR,X
	INTEGER*4 IND_1X2(3)
	INTEGER*4 SYSNR, NUMROWS,NUMRROWS,NUM_BOARDS,CNT,BCNT

C
        INTEGER*4   IND1,IND2,SIND,LAIND(3,2)
        DATA        LAIND/1,2,3,0,4,5/
C
C SHOULD WE DO ANYTHING ?  MAYBE ?
C
        S = TRABUF(TSTAT)
        T = TRABUF(TTYP)
        IF(S.GT.INCA) RETURN
        IF(T.LT.TWAG.OR.T.GT.TINC) RETURN

	BCNT = 0
	IF(TRABUF(TWSPFRG).NE.0) BCNT = 1
C
C SET AND CHECK SOME INDEXES
C
        KICKER   =.FALSE. 
        IF(TRABUF(TWKFLG).NE.0 .OR. TRABUF(TWKFLG2).NE.0) THEN
           KGAM=TRABUF(TWKGME)
           KIKIND=GNTTAB(GAMIDX,TRABUF(TWKGME))
           KICKER = .TRUE.
        ENDIF
        GAM  = TRABUF(TGAM)
        GIND = TRABUF(TGAMIND)
        TYPIND = 1
        IF(TRABUF(TWQPF).NE.0) TYPIND = 2           !it's a QuickPick
C
        IF(GAM.LT.1.OR.GAM.GT.MAXGAM) THEN
            RETURN                                  !game out of range
        ENDIF
C
C CANCELLATION OR WAGER ?
C
        IF(T.EQ.TWAG) THEN
            SIGN = 1                                !a wager
        ELSE IF(T.EQ.TCAN .OR.
     *          T.EQ.TINC) THEN
            SIGN = -1                               !(internal) cancelation
        ELSE
            RETURN                                  !nothing to do
        ENDIF
C
C
        CALL BITCNT(TRABUF(TWQPF),4,QUIIND)         !number of QP flags
        IF(QUIIND.EQ.0) QUIIND = MAXBRD+1           !number of non QP's
        IF(QUIIND.LT.1.OR.QUIIND.GT.MAXBRD+1) THEN
            RETURN                                  !out of range
        ENDIF
C
        BRDIND = TRABUF(TWNBET)                     !number of boards
C
        IF(BRDIND.LT.1.OR.BRDIND.GT.MAXBRD) THEN
          RETURN                                    !out of range (MAXBRD)
        ENDIF
C
C UPDATE LOTTO TABLE
C
        IF(TRABUF(TGAMTYP).EQ.TLTO) THEN
           STALTO(TRACNT,TYPIND,BRDIND,QUIIND,GIND) =
     *        STALTO(TRACNT,TYPIND,BRDIND,QUIIND,GIND) + SIGN * 1
          
           STALTO(DOLAMT,TYPIND,BRDIND,QUIIND,GIND) =
     *        STALTO(DOLAMT,TYPIND,BRDIND,QUIIND,GIND) + 
     *        SIGN * TRABUF(TWAMT) * TRABUF(TWDUR)
        ENDIF
C
C UPDATE SPORTS TABLE AND VAKIO STATS
C
        IF(TRABUF(TGAMTYP).EQ.TSPT) THEN
           STASPT(TRACNT,TYPIND,BRDIND,QUIIND,GIND) =
     *        STASPT(TRACNT,TYPIND,BRDIND,QUIIND,GIND) + SIGN * 1
           STASPT(DOLAMT,TYPIND,BRDIND,QUIIND,GIND) =
     *        STASPT(DOLAMT,TYPIND,BRDIND,QUIIND,GIND) + 
     *        SIGN * TRABUF(TWAMT) * TRABUF(TWDUR)
C
C CHECK DRAW
C
          IF(TRABUF(TWBEG).GT.SPTDRW(TRABUF(TGAMIND))) THEN
             GOTO 1000
          ENDIF
          IF(TRABUF(TWEND).LT.SPTDRW(TRABUF(TGAMIND))) THEN
             GOTO 1000
          ENDIF
C
          STASPT_CUP(GIND)=STASPT_CUP(GIND)+SIGN
C
C CHECK SYSTEM TYPE
C UPDATE REDUCED SYSTEMS
C
          IF(TRABUF(TWSYST).NE.REDSYS) GOTO 100
          
          CALL FASTSET(SPGNBR,INDEX_TAB,16)
          SYSNR = TRABUF(TWSYSN)
	  IF(SYSNR.LT.1.OR.SYSNR.GT.SPGSYS) RETURN
CV10	  NUMROWS = SPSNUM(4,SYSNR)
	  NUMROWS = TRABUF(TWSRW)
	  NUMRROWS = BCNT
	  DO OFF=1,NUMROWS-NUMRROWS
	      CALL GETNIBLE(TAB(OFF),TRABUF(TWBORD),OFF+2*NUMRROWS)
	  ENDDO

	  IND_1X2(1) = SPSNUM(3,SYSNR)
          IND_1X2(2) = SPSNUM(2,SYSNR)
          IND_1X2(3) = 1

	  DO OFF=1,NUMROWS-NUMRROWS
             X = BITS(TAB(OFF))
	     INDEX_TAB(IND_1X2(X)) = OFF   
	     IND_1X2(X) = IND_1X2(X) + 1
	  ENDDO

	  PTR = SPSPTR(SYSNR)
	  IF(PTR.LE.0.OR.PTR.GT.SFTABMAX) RETURN
	  NUM_BOARDS = SPSTAB(PTR)
	  DO I=1,NUM_BOARDS    !PROCESS ALL BOARDS
             DO OFF=1,SPGNBR           !NEXT BOARD
                 PTR=PTR+1
                 X = INDEX_TAB(OFF)
                 CNT=BITS(TAB(X))
                 IF (CNT.EQ.3) THEN
                    LOCAL_TAB(X)=SPSTAB(PTR)
                 ELSEIF (CNT.EQ.2) THEN
                    LOCAL_TAB(X)=CNV_TAB(TAB(X),SPSTAB(PTR))
                 ELSEIF (CNT.EQ.1) THEN
                    LOCAL_TAB(X)=IAND(TAB(X),7)
                 ENDIF
             ENDDO
C 
C UPDATE STATISTICS TABLE
C
              DO K=1,NUMROWS-NUMRROWS
                 IF(LOCAL_TAB(K).EQ.1) THEN
                    STASPT_TAB2(K,1,GIND)=STASPT_TAB2(K,1,GIND)+SIGN
                 ELSEIF(LOCAL_TAB(K).EQ.2) THEN
                    STASPT_TAB2(K,2,GIND)=STASPT_TAB2(K,2,GIND)+SIGN
                 ELSEIF(LOCAL_TAB(K).EQ.4) THEN
                    STASPT_TAB2(K,3,GIND)=STASPT_TAB2(K,3,GIND)+SIGN
                 ENDIF
              ENDDO
          ENDDO
C
	  GOTO 500
100	  CONTINUE
C
C
          CALL GETROW(TRABUF,ROWS,RROWS)
C
          DO J=1,TRABUF(TWNBET)
             CALL FASTSET(1,COUNTER(1,1),SPGNBR*3)
             DO I=1,TRABUF(TWSRW)-BCNT
                IF(ROWS(I,J).EQ.3.OR.ROWS(I,J).EQ.5.OR.ROWS(I,J).EQ.6) THEN
                   DO K=1,TRABUF(TWSRW)-BCNT
                      IF(K.NE.I) THEN
                         DO L=1,3
                            COUNTER(K,L)=2*COUNTER(K,L)
                         ENDDO
                      ENDIF
                   ENDDO
                ELSEIF(ROWS(I,J).EQ.7) THEN
                   DO K=1,TRABUF(TWSRW)-BCNT
                      IF(K.NE.I) THEN
                         DO L=1,3
                            COUNTER(K,L)=3*COUNTER(K,L)
                         ENDDO
                      ENDIF
                   ENDDO
                ENDIF
             ENDDO
C
             DO I=1,TRABUF(TWSRW)-BCNT
               IF(ROWS(I,J).EQ.1) THEN
                  STASPT_TAB1(I,1,GIND)=STASPT_TAB1(I,1,GIND)+SIGN
                  STASPT_TAB2(I,1,GIND)=STASPT_TAB2(I,1,GIND)+
     *                                 SIGN*COUNTER(I,1)
               ELSEIF(ROWS(I,J).EQ.2) THEN
                  STASPT_TAB1(I,2,GIND)=STASPT_TAB1(I,2,GIND)+SIGN
                  STASPT_TAB2(I,2,GIND)=STASPT_TAB2(I,2,GIND)+
     *                                 SIGN*COUNTER(I,2)
               ELSEIF(ROWS(I,J).EQ.3) THEN
                  STASPT_TAB1(I,4,GIND)=STASPT_TAB1(I,4,GIND)+SIGN
                  STASPT_TAB2(I,1,GIND)=STASPT_TAB2(I,1,GIND)+
     *                                 SIGN*COUNTER(I,1)
                  STASPT_TAB2(I,2,GIND)=STASPT_TAB2(I,2,GIND)+
     *                                 SIGN*COUNTER(I,2)
               ELSEIF(ROWS(I,J).EQ.4) THEN
                  STASPT_TAB1(I,3,GIND)=STASPT_TAB1(I,3,GIND)+SIGN
                  STASPT_TAB2(I,3,GIND)=STASPT_TAB2(I,3,GIND)+
     *                                 SIGN*COUNTER(I,3)
               ELSEIF(ROWS(I,J).EQ.5) THEN
                  STASPT_TAB1(I,5,GIND)=STASPT_TAB1(I,5,GIND)+SIGN
                  STASPT_TAB2(I,1,GIND)=STASPT_TAB2(I,1,GIND)+
     *                                 SIGN*COUNTER(I,1)
                  STASPT_TAB2(I,3,GIND)=STASPT_TAB2(I,3,GIND)+
     *                                 SIGN*COUNTER(I,3)
               ELSEIF(ROWS(I,J).EQ.6) THEN
                  STASPT_TAB1(I,6,GIND)=STASPT_TAB1(I,6,GIND)+SIGN
                  STASPT_TAB2(I,2,GIND)=STASPT_TAB2(I,2,GIND)+
     *                                 SIGN*COUNTER(I,2)
                  STASPT_TAB2(I,3,GIND)=STASPT_TAB2(I,3,GIND)+
     *                                 SIGN*COUNTER(I,3)
               ELSEIF(ROWS(I,J).EQ.7) THEN
                  STASPT_TAB1(I,7,GIND)=STASPT_TAB1(I,7,GIND)+SIGN
                  STASPT_TAB2(I,1,GIND)=STASPT_TAB2(I,1,GIND)+
     *                                 SIGN*COUNTER(I,1)
                  STASPT_TAB2(I,2,GIND)=STASPT_TAB2(I,2,GIND)+
     *                                 SIGN*COUNTER(I,2)
                  STASPT_TAB2(I,3,GIND)=STASPT_TAB2(I,3,GIND)+
     *                                 SIGN*COUNTER(I,3)
               ENDIF
             ENDDO
          ENDDO
500	  CONTINUE
C
C COMPARE STATISTICS WITH PERCENT TABLE AND UPDATE IT IF NECESSARY
C
          ROWTOT=0 
          DO K=1,3
             ROWTOT=ROWTOT+STASPT_TAB2(1,K,GIND) 
          ENDDO
          RROWTOT=FLOAT(ROWTOT)
	  IF(RROWTOT.EQ.0) GOTO 600
          DO I=1,SPGNBR
             DO K=1,3
                IF(ABS(SPSROP(I,K,GIND)-
     *            JNINT(1.E4*FLOAT(STASPT_TAB2(I,K,GIND))/RROWTOT)) 
     *            .GT. 100*P(MAXSTD))  GOTO 600
             ENDDO
          ENDDO
          GOTO 1000
C
C UPDATE PERCENT TABLE
C
600       CONTINUE
          DO I=1,SPGNBR
             DO K=1,3
	        IF(RROWTOT.EQ.0) THEN
		   SPSROP(I,K,GIND) = 0
	        ELSE
                   SPSROP(I,K,GIND)=
     *               JNINT(1.E4*FLOAT(STASPT_TAB2(I,K,GIND))/RROWTOT)
	        ENDIF 
             ENDDO
          ENDDO
          SPSVER(GIND)=SPSVER(GIND)+1
          SPSUPT(GIND)=P(ACTTIM)
C
1000      CONTINUE
        ENDIF

C
C UPDATE TOTO-SELECT SYSTEM TABLE
C
        IF(TRABUF(TGAMTYP).EQ.TTSL.AND.TRABUF(TWSYST).EQ.FULSYS) THEN
          IND1=TRABUF(TWTSEL1)-3
          IND2=TRABUF(TWSYSN)-2
          IF(IND1.GE.1.AND.IND1.LE.3.AND.IND2.GE.1.AND.IND2.LE.2) THEN
            SIND=LAIND(IND1,IND2)
            STATSL(TRACNT,SIND,GIND)=STATSL(TRACNT,SIND,GIND)+1
            STATSL(DOLAMT,SIND,GIND)=STATSL(DOLAMT,SIND,GIND)+
     *                               TRABUF(TWAMT)
          ENDIF
        ENDIF
C
C ALL TABLES (EXCEPT KICKER(SNELSPEL))
C
        IF(KGAM.NE.GAM) THEN
            STAGAM(TRACNT,TYPIND,GAM) = 
     *      STAGAM(TRACNT,TYPIND,GAM) + SIGN * 1
            STAGAM(DOLAMT,TYPIND,GAM) = 
     *      STAGAM(DOLAMT,TYPIND,GAM) + 
     *      SIGN * TRABUF(TWAMT) * TRABUF(TWDUR)
        ENDIF
C
C UPDATE KICKER TABLE
C
        IF(KICKER .AND. KGAM.NE.GAM) THEN
            STAGAM(TRACNT,TYPIND,KGAM) = 
     *      STAGAM(TRACNT,TYPIND,KGAM) + SIGN * 1
            STAGAM(DOLAMT,TYPIND,KGAM) = 
     *      STAGAM(DOLAMT,TYPIND,KGAM) +
     *      SIGN * TRABUF(TWKAMT) * TRABUF(TWKDUR)
        ENDIF
C
C UPDATE COUPON KICKER STATS
C
        IND = 1
        IF(KICKER) IND = 2
        STAKIK(IND,GAM) = STAKIK(IND,GAM) + SIGN * 1
C
        RETURN

        END

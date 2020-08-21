C
C SUBROUTINE PRINTRA_SPBET
C $Log:   GXAFXT:[GOLS]PRINTRA_SPBET.FOV  $
C  
C V03 30-MAR-2015 MTK Modified Super 14 game
C V02 28-OCT-2003 FRP Modify for Batch2 Totobola Changes.
C
C     Rev 1.0   17 Apr 1996 14:29:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   12 Jul 1993 15:23:00   SXH
C  Released for Finland
C  
C     Rev 1.0   21 Jan 1993 17:21:10   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - printra.for **
C
C V01 07-JAN-93  TD  CHANGED SUBROUTINE CALLED TO PRINTRA_SPBET FROM SPBET
C
C BUILD BET IMAGE FOR SPORTS TRANSACTIONS
C ==============================================================
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINTRA_SPBET(TRABUF,BETS,LINES)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
C
        ! arguments
        INTEGER*4  BETS(20,14)            !
        INTEGER*4  LINES                  !

        ! variables
        INTEGER*4  I                      !
        INTEGER*4  J, K                      !
        INTEGER*4  ROW(0:15)              !
        INTEGER*4  ROWS(SPGNBR,12)        !
	INTEGER*4  RROW(0:15)
	INTEGER*4  RROWS(2,TGGNBR,12)
        INTEGER*4  FLAGS(32)              !
        INTEGER*4  QP(3)                  !
	INTEGER*4  BCNT

        DATA ROW/'--- ','1-- ','-X- ','1X- ','--2 ',
     *           '1-2 ','-X2 ','1X2 ','---m','1--m',
     *           '-X-m','1X-m','--2m','1-2m','-X2m',
     *           '1X2m'/

        DATA RROW/'--- ','0-- ','-1- ','01- ','--M ',
     *            '0-M ','-1M ','01M ','---m','0--m',
     *            '-1-m','01-m','--2m','0-2m','-12m',
     *            '012m'/

        DATA QP/'    ','QP  ','WQP '/
C
C
        CALL GETFLG(TRABUF(TWQPF),FLAGS,TRABUF(TWNBET))

        CALL GETROW(TRABUF,ROWS,RROWS)

	BCNT = 0
	IF(TRABUF(TWSPFRG).NE.0) BCNT = 1
        DO J = 1,TRABUF(TWNBET)
           K=FLAGS(J)+1
           IF(TRABUF(TWWEQP).NE.0) K=3
           BETS(1,J) = QP(K)
           DO I = 1,TRABUF(TWSRW)-BCNT
              IF(I.LE.SPGNBR) BETS(I+1,J) = ROW(ROWS(I,J))
           END DO
        END DO

        LINES = TRABUF(TWNBET)+1
C
C SUPER14 (RESULTS) ROW
C
	IF(TRABUF(TWSPFRG).EQ.1) THEN
	  K=TRABUF(TWNBET)+1
	  BETS(1,K) = QP(1)
	  BETS(2,K) = RROW(RROWS(1,1,1))
	  BETS(3,K) = RROW(RROWS(2,1,1))
          LINES = LINES + 1
	ENDIF

C SUPERR 14 STANDARD 1X2 ROW

        IF(TRABUF(TWSPFRG).EQ.2) THEN
          K=TRABUF(TWNBET)+1
          BETS(1,K) = QP(1)
          BETS(2,K) = ROW(RROWS(1,1,1))
          BETS(3,K) = '    '
          LINES = LINES + 1
        ENDIF

        RETURN

        END

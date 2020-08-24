C
C SUBROUTINE UPDLPL
C  
C V07 01-MAR-2000 UXN XDRAW and PRZCOM removed.
C V06 28-FEB-2000 RXK Promotion ("add 1 free week") added.
C V05 04-Dec-1995 HXK Made changes for LTPOOL_GAMENR not having MAXTYP as
C                     array size!
C V04 03-Feb-1994 HXK XDRAW ALWAYS SET TO ZERO BECAUSE:
C                     1. POOLBLD ONLY RUN ON MONDAYS.
C                     2. PRZCOM IS NOT SET FOR TWO DAYS AFTER WINSEL.
C V03 07-Oct-1993 GXA Released for Finland Dec Conversion / Oddset.
C                     (Skip pool updating if last draw was postponed).
C                     Increased number of marks to put on queue for large 
C                     system bets.
C V02 21 Jan 1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO UPDATE LOTTO POOLS
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE UPDLPL(TRABUF,DUMMY,STATUS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:POOLLTO.DEF'
C
        INTEGER*4 OFFSETS(20),BDS(120)
        INTEGER*4 IN_QUEUE, ST, I, BASE, POOLNR, POOLBET
        INTEGER*4 PGNUM, GIND, GTYP, STATUS, DUMMY
C
C CHECK IF POOLS LOTTO POOLS ARE ACTIVE
C FOR THIS GAME.
C
        STATUS=1
        IF(P(POOLACT).NE.0) RETURN
        IF(P(SUPPUD).NE.0) RETURN
C
C UPDATE POOLS AND CHECK INTERVAL CODE
C
C
        GTYP=TRABUF(TGAMTYP)
        GIND=TRABUF(TGAMIND)
        IF (GTYP.NE.TLTO) RETURN  !do this check before getting pools game num
        PGNUM=LTPOOL_GAMENR(GTYP,GIND)
        IF (PGNUM.LE.0) RETURN
C
        STATUS=2
        IF(PGNUM.GT.LTNUMGAMES) RETURN
        IF(TRABUF(TWBEG).GT.LTPOOLDRAW(PGNUM)) RETURN
        IF(TRABUF(TWEND)+TRABUF(TWADDFW).LT.LTPOOLDRAW(PGNUM)) RETURN
C
C     PROCESS NON SYSTEM BET
C
      	IF (TRABUF(TWSYSN).EQ.0) THEN
            POOLBET=LTPOOLBET(PGNUM)
            POOLNR=LTPOOLNR(PGNUM)
            CALL INTOFF(TRABUF(TWNBET),POOLBET,POOLNR,TRABUF(TWBORD),
     *                   STATUS,BDS,OFFSETS)
            IF (STATUS .EQ. 0) THEN
              BASE=LTPOOL_BASEOFF(PGNUM)
              DO 70  I=1,TRABUF(TWNBET)
                IF(TRABUF(TTYP).EQ.TCAN.OR.TRABUF(TTYP).EQ.TINC) THEN
                   OFFSETS(I)=-OFFSETS(I)-BASE
                ELSE
                   OFFSETS(I)=OFFSETS(I)+BASE
                ENDIF
60              CONTINUE
                CALL ABL(OFFSETS(I),LTOQ1(1,LTQNUM),STATUS)
                IF (STATUS .NE. 0) THEN
                  CALL XWAIT(50,1,ST)
                  GOTO 60
                ENDIF
70            CONTINUE
            ENDIF
        ELSE
C
C     PROCESS NOW SYSTEM BET
C
              POOLNR=LTPOOLNR(PGNUM)
              CALL SCHKINT(TRABUF(TWNBET),TRABUF(TWNMRK),POOLNR,
     *                    TRABUF(TWBORD),STATUS)
              IF(STATUS.NE.0) RETURN
              OFFSETS(1)=OFFSIZE*LTNUMPAG+TRABUF(TWSYSN)
              IF(TRABUF(TTYP).EQ.TCAN.OR.TRABUF(TTYP).EQ.TINC)
     *          OFFSETS(1)=-OFFSETS(1)
              IN_QUEUE=5
              OFFSETS(2)=IN_QUEUE   !5 ELEMENTS ON THE QUEUE TO FOLLOW
              OFFSETS(3)=TRABUF(TWBORD)
              OFFSETS(4)=TRABUF(TWBORD+1)
              OFFSETS(5)=TRABUF(TWBORD+2)
              OFFSETS(6)=TRABUF(TWBORD+3)
              OFFSETS(7)=TRABUF(TWBORD+4)
C
              DO 90, I=1,IN_QUEUE+2
80              CONTINUE
                CALL ABL(OFFSETS(I),LTOQ1(1,LTQNUM),STATUS)
                IF (STATUS .NE. 0) THEN
                  CALL XWAIT(50,1,ST)
                  GOTO 80
                ENDIF
90            CONTINUE
        ENDIF
        RETURN
        END

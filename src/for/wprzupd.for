C SUBROUTINE WPRZUPD
C
C V18 11-OCT-2013 SCML Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C V17 04-MAY-2010 RXK Conditions for TKIK split
C V16 26-JAN-2011 FJG Out of bounds issue
C V15 16-MAR-2009 MMO JOKER/EM
C V14 01-OCT-2008 MMO Include GETWEK for KICKER PRIZE
C V13 30-MAY-2001 EPH Change condition for game selected to be true. The way it was,
C                     if wins just on Joker, the OP was not written
C V12 22-FEB-2001 EPH Change filling of OP data to before VALLOG/DVALLOG
C                     Include GETWEK for LOTTO
C V11 07-FEB-2001 EPH Get week and year for OP
C V10 03-DEC-2000 UXN Totogolo Added
C V09 16-JAN-2001 EPH REMOVE VK2PAMT AND FILL VKOPSAMT
C V08 10-JAN-2001 EPH INSERT CODE FOR RETURNING OP DATA
C                     AND UPDATE VLF REC WITH OP INFORMATION
C                     FOR VALIDATION
C V07 11-APR-2000 UXN WINUPD.DEF ADDED.
C V06 15-FEB-2000 UXN Added BIGWRL
C V05 13-OCT-1999 RXK World Tour added.
C V04 05-AUG-1998 RXK Changed to have 2 kickers
C V03 23-NOV-1993 HXK Added Bingo
C V02 01-SEP-1993 HXK Added BIGPPP, fractions.
C V01 21-JAN-1993 DAB Initial Release Based on Netherlands Bible,
C                   DEC Baseline
C
C SUBROUTINE TO UPDATE VALIDATION RECORDS WITH PRIZE VALUES
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
        SUBROUTINE WPRZUPD(V4BUF, WINS, JOKERDIV, VALOP, VALOPJOKER, 
     *                     HIPRIZE, YEAR, WEEK)                                        !V08
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE 'INCLIB:WINUPD.DEF'
C
        INTEGER*4 KIK, BNS, BIND, SHR, DIV, DRW, I, PRZCNT, TOTKIK
        INTEGER*4 KI2, FRCS, UPD, PRG, SUB, ST
        INTEGER*4 TOTAMT, KIND, KGAM, GIND, GTYP, GAM, TER
        INTEGER*4 TOTKI2, KI2NET, KI2TAX
        INTEGER*4 KIKTAX, REGTAX, REGNET, KIKNET
        LOGICAL   PRVFLG, OK2PAY
        INTEGER*4 WINS(6)       ! Wins per division for the main game             !V08 - V16
        INTEGER*4 JOKERDIV      ! Number of Joker winning division                !V08
        INTEGER*4 VALOP         ! Value for the OP                                !V08
        LOGICAL   HIPRIZE       ! Indicate if one of the prizes on the OP is high !V08
        INTEGER*4 VALOPJOKER    ! Valor do Joker na OP                            !V08
        INTEGER*4 OP                                                                  !V08       
        LOGICAL   BELONGSTOOP(VMAX)                                                    !V08
        LOGICAL   GAME_SELECTED              !TRUE if MAIN game (not joker) was   !v08
C                                            !selected to update prizes           !V08
        INTEGER*4 YEAR, WEEK

        CALL FASTSET(0,WINS,6)         !V08 - V16
        JOKERDIV   = 0                 !V08
        VALOP      = 0                 !V08
        HIPRIZE    = .FALSE.           !V08
        VALOPJOKER = 0                       !V08
        GAME_SELECTED = .FALSE.               !V08

        CALL LOGVAL(VALREC,V4BUF)
        CALL DLOGVAL(VALREC,VDETAIL)
C
C GET GAME AND TYPE
C
        GTYP=VALREC(VGTYP)
        GIND=VALREC(VGIND)
        KGAM=VALREC(VKGME)
        GAM=VALREC(VGAM)
        TER=VALREC(VSTER)
        FRCS=VALREC(VFRAC)
        IF(KGAM .NE. 0) KIND=SCFGNT(GAMIDX,KGAM)
C
C CHECK FOR POSTPONED EVENTS. CHANGE STATUS TO PRIZE VALUES NOT SET IN ORDER
C FOR THIS ROUTINE TO HANDEL THE REST CORRECTLY.
C
        IF(VALREC(VSTAT).EQ.VPOST.OR.VALREC(VSTAT).EQ.VPRPOST) THEN
           IF((GTYP.EQ.TSPT.AND.LSPDRW(GIND).GT.0).OR.
     *        (GTYP.EQ.TLTO.AND.LLTDRW(GIND).GT.0).OR.
     *        (GTYP.EQ.TTGL.AND.LTGDRW(GIND).GT.0)) THEN
C     *        (GTYP.EQ.TBNG.AND.LBNDRW(GIND).GT.0))THEN
              IF(VALREC(VSTAT).EQ.VPOST) THEN
                 VALREC(VSTAT) = VNOPRZ
              ELSE
                 VALREC(VSTAT) = VPPNPZ
              ENDIF
           ELSEIF(GTYP.EQ.TKIK) THEN
              IF(LKKDRW(GIND).GT.0)THEN
                 IF(VALREC(VSTAT).EQ.VPOST) THEN
                    VALREC(VSTAT) = VNOPRZ
                 ELSE
                    VALREC(VSTAT) = VPPNPZ
                 ENDIF
              ENDIF
           ENDIF
        ENDIF
C
C       SET GAME_SELECTED WHEN RUN WINUPD FOR THE MAIN GAME
C       OF THE VLF RECORD BEING UPDATED
C
        IF((GTYP.EQ.TSPT.AND.LSPDRW(GIND).GT.0).OR.
     *     (GTYP.EQ.TLTO.AND.LLTDRW(GIND).GT.0).OR.
     *     (GTYP.EQ.TTGL.AND.LTGDRW(GIND).GT.0)) THEN
C     *     (GTYP.EQ.TBNG.AND.LBNDRW(GIND).GT.0))THEN
              GAME_SELECTED = .TRUE.     
        ELSEIF(GTYP.EQ.TKIK) THEN
           IF(LKKDRW(GIND).GT.0)THEN
              GAME_SELECTED = .TRUE.
           ENDIF
        ENDIF              

C
C GET AMOUNT WON
C
        TOTAMT=0
        TOTKIK=0
        TOTKI2=0
        PRZCNT=VALREC(VPZOFF)
        IF(PRZCNT.GT.VMAX) PRZCNT=VMAX
        DO 100 I=1,PRZCNT

        BELONGSTOOP(I) = .FALSE.  !V08

        DRW=VDETAIL(VDRW,I)
        DIV=VDETAIL(VDIV,I)
        SHR=VDETAIL(VSHR,I)
        BNS=VDETAIL(VBDR,I)
        KIK=VDETAIL(VKIK,I)
        KI2=VDETAIL(VKI2,I)
        UPD=VDETAIL(VUPD,I)
        PRG=VDETAIL(VPRG,I)
        SUB=VDETAIL(VSUB,I)
        OP =VDETAIL(VOP,I)                                 !V08
        IF(DRW.LE.0) GOTO 100
C        IF(UPD.NE.0) GOTO 100                             !V08
        IF(PRG.NE.0) GOTO 100
C
C        CALCULATE JOKER VALUE EXCLUSIVELLY FOR OP
C       SUPPOSE HAVE ONLY 1 JOKER (KICKER)
C       FOR PORTUGAL
C       DOESN'T MATTER IF RECORD WAS ALREADY UPDATED IN THE PAST
C       I MUST DO IT AGAIN TO CALCULATE FINAL VALUE FOR THE ORDER
C
        IF(KIK .NE. 0 .AND. OP .EQ. 0) THEN                     !V08
          IF(DRW .EQ. LKKDRW(KIND)) THEN                            !V08
             VALOPJOKER = LKKSHV(DIV,KIND)*SHR                    !V08
             JOKERDIV   = DIV                                    !V08
             IF (LKKSHV(DIV,KIND) .GE. P(VALPRZHI)) THEN      !V08
                HIPRIZE = .TRUE.                            !V08
             ENDIF                                          !V08
             BELONGSTOOP(I) = .TRUE.                            !V08  
          ENDIF                                                    !V08
        ENDIF                                                    !V08

        IF(UPD.NE.0) GOTO 100                               !V08

C
C UPDATE KICKER PRIZE
C
        IF(KIK .NE. 0 .OR. KI2 .NE. 0) THEN
          IF(DRW .NE. LKKDRW(KIND)) GOTO 100
          IF(FRCS.EQ.0.OR.FRCS.EQ.SCFFRC(KGAM)) THEN
              IF(KIK .NE. 0) TOTKIK=TOTKIK+LKKSHV(DIV,KIND)*SHR
              IF(KI2 .NE. 0) TOTKI2=TOTKI2+LKKSHV(DIV,KIND)*SHR
          ELSE
              IF(KIK .NE. 0) TOTKIK=TOTKIK+
     *          (LKKSHV(DIV,KIND)/SCFFRC(KGAM))*SHR*FRCS
              IF(KI2 .NE. 0) TOTKI2=TOTKI2+
     *          (LKKSHV(DIV,KIND)/SCFFRC(KGAM))*SHR*FRCS 
          ENDIF
          VDETAIL(VUPD,I)=1
          CALL GETWEK(DRW,GAM,WEEK,YEAR,ST)       !V14
          GOTO 100
        ENDIF
C
C UPDATE LOTTO PRIZE
C
        IF(GTYP.EQ.TLTO) THEN
          BIND=1
          IF(BNS.NE.0) BIND=2
          IF(DRW.NE.LLTDRW(GIND)) GOTO 100
          IF(FRCS.EQ.0.OR.FRCS.EQ.SCFFRC(GAM)) THEN
            TOTAMT=TOTAMT+LLTSHV(DIV,BIND,GIND)*SHR
          ELSE
            TOTAMT=TOTAMT+(LLTSHV(DIV,BIND,GIND)/SCFFRC(GAM))*
     *                    SHR*FRCS
          ENDIF
          VDETAIL(VUPD,I)=1

          IF (OP.EQ.0) THEN
C          GAME_SELECTED = .TRUE.
          BELONGSTOOP(I) = .TRUE.                               !V08
          WINS(DIV) = SHR                                       !V08
          IF (LLTSHV(DIV,BIND,GIND) .GE. P(VALPRZHI)) THEN      !V08
             HIPRIZE = .TRUE.                                        !V08
          ENDIF                                                 !V08
          CALL GETWEK(DRW,GAM,WEEK,YEAR,ST)       !V11
          ENDIF

          GOTO 100
        ENDIF
C
C UPDATE SPORTS PRIZE
C
        IF(GTYP.EQ.TSPT) THEN
          IF(DRW.NE.LSPDRW(GIND)) GOTO 100
          IF(FRCS.EQ.0.OR.FRCS.EQ.SCFFRC(GAM)) THEN
            TOTAMT=TOTAMT+LSPSHV(DIV,GIND)*SHR
          ELSE
            TOTAMT=TOTAMT+(LSPSHV(DIV,GIND)/SCFFRC(GAM))*
     *                    SHR*FRCS
          ENDIF
          VDETAIL(VUPD,I)=1

          IF (OP.EQ.0) THEN
C          GAME_SELECTED = .TRUE.
          BELONGSTOOP(I) = .TRUE.                               !V08
          WINS(DIV) = SHR                                       !V08
          IF (LSPSHV(DIV,GIND) .GE. P(VALPRZHI)) THEN           !V08
             HIPRIZE = .TRUE.                                        !V08
          ENDIF                                                 !V08
          CALL GETWEK(DRW,GAM,WEEK,YEAR,ST)       !V11
          ENDIF

          GOTO 100
        ENDIF
C
C UPDATE TOTOGOLO PRIZE
C
        IF(GTYP.EQ.TTGL) THEN
          IF(DRW.NE.LTGDRW(GIND)) GOTO 100
          IF(FRCS.EQ.0.OR.FRCS.EQ.SCFFRC(GAM)) THEN
            TOTAMT=TOTAMT+LTGSHV(DIV,GIND)*SHR
          ELSE
            TOTAMT=TOTAMT+(LTGSHV(DIV,GIND)/SCFFRC(GAM))*
     *                    SHR*FRCS
          ENDIF
          VDETAIL(VUPD,I)=1

          IF (OP.EQ.0) THEN
C          GAME_SELECTED = .TRUE.
          BELONGSTOOP(I) = .TRUE.                               !V08
          WINS(DIV) = SHR                                       !V08
          IF (LTGSHV(DIV,GIND) .GE. P(VALPRZHI)) THEN           !V08
             HIPRIZE = .TRUE.                                        !V08
          ENDIF                                                 !V08
          CALL GETWEK(DRW,GAM,WEEK,YEAR,ST)       !V11
          ENDIF

          GOTO 100
        ENDIF
C
C UPDATE BINGO PRIZE
C
        IF(GTYP.EQ.TBNG) THEN
          IF(DRW.NE.LBNDRW(GIND)) GOTO 100
          IF(FRCS.EQ.0.OR.FRCS.EQ.SCFFRC(GAM)) THEN
            TOTAMT=TOTAMT+LBNSHV(DIV,SUB,GIND)*SHR
          ELSE
            TOTAMT=TOTAMT+(LBNSHV(DIV,SUB,GIND)/SCFFRC(GAM))*
     *                    SHR*FRCS
          ENDIF
          VDETAIL(VUPD,I)=1

          IF (OP.EQ.0) THEN
C          GAME_SELECTED = .TRUE.
          BELONGSTOOP(I) = .TRUE.                               !V08
          WINS(DIV) = SHR                                       !V08
          IF (LBNSHV(DIV,SUB,GIND) .GE. P(VALPRZHI)) THEN       !V08
             HIPRIZE = .TRUE.                                        !V08
          ENDIF                                                 !V08
          CALL GETWEK(DRW,GAM,WEEK,YEAR,ST)       !V11
          ENDIF
   
          GOTO 100
        ENDIF

100        CONTINUE
        IF(VALREC(VSTAT).EQ.VCXL.OR.VALREC(VSTAT).EQ.VDEL) GOTO 400
C
C GET WINNERS TAX
C
        CALL GETTAX(TOTAMT,REGTAX,REGNET)
        CALL GETTAX(TOTKIK,KIKTAX,KIKNET)
        CALL GETTAX(TOTKI2,KI2TAX,KI2NET)
        IF(GTYP.EQ.TLTO) LLTTAX(GIND)=LLTTAX(GIND)+REGTAX
        IF(GTYP.EQ.TSPT) LSPTAX(GIND)=LSPTAX(GIND)+REGTAX
        IF(GTYP.EQ.TTGL) LTGTAX(GIND)=LTGTAX(GIND)+REGTAX
        IF(KGAM.NE.0)    LKKTAX(KIND)=LKKTAX(KIND)+KIKTAX+KI2TAX
C
C CHECK IF ALL PRIZES ARE SET
C
        OK2PAY=.FALSE.
        DO 200 I=1,PRZCNT
        IF(VDETAIL(VDRW,I).EQ.0) GOTO 200
        IF(VDETAIL(VUPD,I).EQ.0) GOTO 300
200        CONTINUE
        OK2PAY=.TRUE.
C
C CHECK IF TICKET CAN BE CASHED AT AGENT OR BANK
C
300        CONTINUE
        PRVFLG=.FALSE.
        IF(VALREC(VSTAT).EQ.VPPNPZ) PRVFLG=.TRUE.
        IF(REGNET.GT.SCFRED(GAM)) PRVFLG=.TRUE.
        IF(KGAM.NE.0) THEN
          IF((KIKNET+KI2NET).GT.SCFRED(KGAM))PRVFLG=.TRUE.
        ENDIF
        IF(VALREC(VSTAT).EQ.VPOST.OR.VALREC(VSTAT).EQ.VPRPOST) THEN
          IF(PRVFLG) VALREC(VSTAT)=VPRPOST
        ELSE
          IF(OK2PAY) THEN
            IF(PRVFLG) THEN
              VALREC(VSTAT)=VPRPAY
            ELSE
              VALREC(VSTAT)=VUNCSH
            ENDIF
          ELSE
            IF(PRVFLG) VALREC(VSTAT)=VPPNPZ
          ENDIF
        ENDIF
C
C UPDATE BIG WINNER COMMISSION
C
        IF(SCFHVL.NE.0) THEN
          IF(REGNET.GE.SCFHVL) THEN
            WINTAB(1,TER)=WINTAB(1,TER)+1
            WINTAB(2,TER)=WINTAB(2,TER)+REGNET
          ENDIF
          IF(KIKNET.GE.SCFHVL) THEN
            WINTAB(1,TER)=WINTAB(1,TER)+1
            WINTAB(2,TER)=WINTAB(2,TER)+KIKNET
          ENDIF
          IF(KI2NET.GE.SCFHVL) THEN
            WINTAB(1,TER)=WINTAB(1,TER)+1
            WINTAB(2,TER)=WINTAB(2,TER)+KI2NET
          ENDIF
         ENDIF
C
C
400        CONTINUE

        VALREC(VPAMT)=VALREC(VPAMT)+REGNET
        VALREC(VKPAMT)=VALREC(VKPAMT)+KIKNET+KI2NET
C        VALREC(VK2PAMT)=VALREC(VK2PAMT)+KI2NET        !V09
        VALREC(VTAMT)=VALREC(VTAMT)+REGTAX
        VALREC(VKTAMT)=VALREC(VKTAMT)+KIKTAX+KI2TAX

C
C        GET TOTAL OP VALUE AND DECIDE IF IT IS REALLY
C       GONNA BE AN OP (IT HAS TO HAVE A CERTAIN VALUE)
C
        VALOP = TOTAMT + VALOPJOKER                                    !V08

C----+------------------------------------------------------------------
C V18| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C----+------------------------------------------------------------------
C        IF (VALOP.LT.P(VALORDER) .OR. .NOT. GAME_SELECTED) THEN      !V08
        IF (  VALOP.LT.P(VALORDER) 
     *   .OR. .NOT. GAME_SELECTED
     *   .OR. P(OPGENFLG) .EQ. 0 ) THEN      !V18
            IF( P(OPGENFLG) .EQ. 0 ) THEN
                VALREC(VOPSCNT) = 0                    
                VALREC(VOPSAMT) = VALREC(VOPSAMT) + TOTAMT
                VALREC(VKOPSAMT) = VALREC(VKOPSAMT) + VALOPJOKER
            ENDIF
C----+------------------------------------------------------------------
C V18| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C----+------------------------------------------------------------------
           VALOP = 0     !DO NOT GENERATE AN OP                            !V08
        ELSE                                                            !V08
C                                                                    !V08
C          Mark vdetail records that are part of this OP            !V08
C                                                                    !V08
           DO 201 I=1,PRZCNT                                            !V08
              IF (BELONGSTOOP(I)) THEN                                    !V08
                 VDETAIL(VOP,I) = 1   !IS PART OF AN OP                    !V08
              ENDIF                                                    !V08
201           CONTINUE                                                    !V08
C
C          Update Orders count and value in Valrec
C
           VALREC(VOPSCNT) = VALREC(VOPSCNT) + 1                    !V08
           VALREC(VOPSAMT) = VALREC(VOPSAMT) + TOTAMT               !V08  !V09
           VALREC(VKOPSAMT) = VALREC(VKOPSAMT) + VALOPJOKER         !V09
        ENDIF                                                            !V08                      

        CALL DVALLOG(VALREC,VDETAIL)
        CALL VALLOG(VALREC,V4BUF)

        RETURN
        END

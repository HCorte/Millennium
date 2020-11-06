C SUBROUTINE UPDSUB
C $Log:   GXAFIP:[GOLS]UPDSUB.FOV  $
C
C V13 14-JAN-2013 SCML Uniforming agent prize pay amount to add only prize amounts
C V12 17-DEC-2013 SCML In the accounting of the total amount of prizes paid for the Online Games:
C                       - use TRABUF(TVPAY) for prizes before OPS supression
C                       - use TRABUF(TVOPPAY) for prizes after OPS supression
C V11 13-DEC-2013 SCML New bank validation mode added for Online Games Validation.
C V10 18-MAR-2013 SCML Fix: don't accumulate agent and daily prize amounts 
C                      which have OP.
C                      Since prizes began to be taxed (01.01.2013), the
C                      difference between prize amount and OP prize amount
C                      is no longer zero for prizes taxed, which mean
C                      the difference began to be accumulated, erroneously,
C                      in the accounting of the total amount of prizes.
C V09 16-MAY-2011 RXK  Remember serial for Passive validation inquiries
C V08 20-APR-2010 FRP  Add addtional controls for OutOfBounds
C V07 14-OCT-2010 FJG  RETAFTDRW incorrect sum
C V06 31-MAR-2010 RXK  ePassive changes
C                 FJG  More changes
C V05 25-APR-2006 FRP  Modify for IPS Distribution.
C
C     Rev 1.5   15 DEC 2000 18:16:28   CS
C  
C  
C  INCLUDED PASSIVE GAME TO PORTUGAL
C     Rev 1.4   05 Mar 1997 18:16:28   HXK
C  
C  
C  Fix for summing valid cashes only
C  
C     Rev 1.3   19 Feb 1997 14:53:18   HXK
C  Sum DAYIVAL
C  
C     Rev 1.2   11 Feb 1997 18:33:24   WPW
C  DAYCRS updates added.
C  
C     Rev 1.1   28 Jan 1997 17:28:38   RXK
C  Last instant validation transaction update added 
C  
C     Rev 1.0   17 Apr 1996 15:43:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.7   23 Nov 1995 13:14:08   HXK
C  Merge of post 65 stuff; changes for Double/Couple
C  
C     Rev 1.6   15 Oct 1994 16:40:44   HXK
C  Adding /developing Bingo (15.Oct.94)
C  
C     Rev 1.5   02 Sep 1994 18:15:40   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.5   21 May 1994 11:21:14   HXK
C  SET ATRNUM TO -1 FOR ALL GAMES (NOT JUST KICKER)
C  
C     Rev 1.4   11 Mar 1994 11:43:28   HXK
C  SET ATRNUM TO -1 FOR INC.
C  
C     Rev 1.3   24 Sep 1993 23:22:50   GXA
C  Added updating of last second kicker number by agent.
C  
C     Rev 1.2   30 Aug 1993 23:35:34   GXA
C  Added Check for 'Paid to Bank', don't include in accounting here.
C  
C     Rev 1.1   21 Jun 1993 10:17:34   SXH
C  Added second kicker
C  
C     Rev 1.0   21 Jan 1993 17:58:46   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - updsub.for **
C
C UPDSUB.FOR
C
C V04 16-JAN-01 EPH  UPDATE ONLY CASH PAY (EXCLUDE VALUE IN OPS)
C V03 15-FEB-92 GCAN IN CASE OF KICKER TINC DO NOT INCREASE CANCELS BUT
C                    DO REMOVE FROM SALES.
C V02 01-NOV-91 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C FINANCIAL UPDATE SUBROUTINE
C UPDATE SYSTEM AND AGENT FINANCIAL TOTALS.
C
C CALLING SEQUENCE
C     CALL UPDSUB(TRABUF)
C INPUT
C     TRABUF - INTERNAL FORMAT OF TRANSACTION
C OUTPUT
C     NONE
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE UPDSUB(TRABUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'


        INTEGER*4  KGAME           !
        INTEGER*4  AMT             !
        INTEGER*4  TER             !
        INTEGER*4  GAME            !
        INTEGER*4  TYPE            !
        INTEGER*4  GTYP            !
        INTEGER*4  GIND            !
        INTEGER*4  DISAMT          !
        INTEGER*4  OFFSET          !
        INTEGER*4  I               !
        INTEGER*4  AMOUNT
        INTEGER*4  CLERK
        integer*4  emis
        integer*4  offs
        integer*4  xtik
        integer*4  xamt

        INTEGER*4  TCKS
C----+------------------------------------------------------------------
C V13| Uniforming agent prize pay amount to add only prize amounts
C----+------------------------------------------------------------------
        LOGICAL    IS_BANK_TRANSFER
C----+------------------------------------------------------------------
C V13| Uniforming agent prize pay amount to add only prize amounts
C----+------------------------------------------------------------------

        LOGICAL     KICKER         !
C       FUNCTION
        INTEGER*4  GETPAGEMI    
C
        TYPE   =  TRABUF(TTYP)
        GAME   =  TRABUF(TGAM)
        GIND   =  TRABUF(TGAMIND)
        TER    =  TRABUF(TTER)
        KICKER = .FALSE.
C
        IF (TYPE.NE.TCRS) GTYP=GNTTAB(GAMTYP,GAME)
C
        AMT    = TRABUF(TWAMT)*TRABUF(TWDUR)
        DISAMT = TRABUF(TWDAMT)
        IF(TRABUF(TWKFLG).NE.0 .OR. TRABUF(TWKFLG2).NE.0) KICKER=.TRUE.
        IF(TRABUF(TWKGME).EQ.0) KICKER = .FALSE.   !!!!!*******!!!!! test this
C
C
        IF(TYPE.EQ.TWAG) GOTO 100     !WAGER UPDATE
        IF(TYPE.EQ.TCAN) GOTO 200     !CANCEL UPDATE
        IF(TYPE.EQ.TINC) GOTO 300     !DELETE UPDATE
        IF(TYPE.EQ.TVAL) GOTO 400     !VALID  UPDATE
        IF(TYPE.EQ.TREF) GOTO 400     !REFUND UPDATE
        IF(TYPE.EQ.TCRS) GOTO 600     !INSTANT VALIDATION OR SUPPLY ORDER
        IF(TYPE.EQ.TRET) GOTO 700     !PASSIVE UPDATES
        RETURN
C
C WAGER UPDATE
C
100     CONTINUE
C
C UPDATE DAILY AND GAME TOTALS
C
        LRCCNT(GAME) = LRCCNT(GAME) + TRABUF(TSIZE)
        IF(TRABUF(TGAMTYP).EQ.TKIK) THEN
            CALL UPDKIK(TRABUF)
            CALL UPDGAM(TRABUF,KICKER)
            AGTGAM(GTKCHG,GAME,TER)=AGTGAM(GTKCHG,GAME,TER)+TRABUF(TWTKC)
            IF(DISAMT.NE.0) THEN
                DAYDIS(TRACNT,GAME) = DAYDIS(TRACNT,GAME)+1
                DAYDIS(DOLAMT,GAME) = DAYDIS(DOLAMT,GAME)+DISAMT
                AGTGAM(GDCNT,GAME,TER) = AGTGAM(GDCNT,GAME,TER)+1
                AGTGAM(GDAMT,GAME,TER) = AGTGAM(GDAMT,GAME,TER)+DISAMT
            ENDIF
            AGTTAB(ALSWAG,TER) = TRABUF(TSER)
            AGTTAB(ALSTRA,TER) = TRABUF(TSER)
            AGTTAB(AGTLKN,TER) = TRABUF(TWKICK)
            AGTTAB(AGTLKN2,TER)= TRABUF(TWKICK2)
             
            RETURN
        ENDIF
C
C
        DAYTYP(TRACNT,TWAG,GAME) = DAYTYP(TRACNT,TWAG,GAME)+1
        DAYTYP(DOLAMT,TWAG,GAME) = DAYTYP(DOLAMT,TWAG,GAME)+AMT
        IF(KICKER) CALL UPDKIK(TRABUF)
        CALL UPDGAM(TRABUF,KICKER)
C
C UPDATE AGENT INFORMATION
C
        AGTGAM(GSCNT,GAME,TER) = AGTGAM(GSCNT,GAME,TER)+1
        AGTGAM(GSAMT,GAME,TER) = AGTGAM(GSAMT,GAME,TER)+AMT
        AGTGAM(GTKCHG,GAME,TER) = AGTGAM(GTKCHG,GAME,TER)+TRABUF(TWTKC)
        IF(DISAMT.NE.0) THEN
            DAYDIS(TRACNT,GAME) = DAYDIS(TRACNT,GAME)+1
            DAYDIS(DOLAMT,GAME) = DAYDIS(DOLAMT,GAME)+DISAMT
            AGTGAM(GDCNT,GAME,TER) = AGTGAM(GDCNT,GAME,TER)+1
            AGTGAM(GDAMT,GAME,TER) = AGTGAM(GDAMT,GAME,TER)+DISAMT
        ENDIF
        AGTTAB(ALSWAG,TER) = TRABUF(TSER)
        AGTTAB(ALSTRA,TER) = TRABUF(TSER)
        AGTTAB(AGTLKN,TER) = TRABUF(TWKICK)
        AGTTAB(AGTLKN2,TER)= TRABUF(TWKICK2)
        IF(TRABUF(TGAMTYP).EQ.TBNG) THEN
           AGTTAB(AGTBSED,TER) = TRABUF(TWBSED)
        ENDIF
        IF(TRABUF(TGAMTYP).EQ.TPAS) THEN
           AGTTAB(AGTEPS,TER) = TRABUF(TWEPSD)   
        ENDIF 

        RETURN
C
C CANCEL UPDATE
C
200     CONTINUE
C
C UPDATE DAILY AND GAME TOTALS
C
        IF(TRABUF(TGAMTYP).EQ.TKIK) THEN
            CALL UPDKIK(TRABUF)
            AGTGAM(GTKCHG,GAME,TER)=AGTGAM(GTKCHG,GAME,TER)-TRABUF(TWTKC)
            IF(DISAMT.NE.0) THEN
                DAYDIS(TRACNT,GAME) = DAYDIS(TRACNT,GAME)-1
                DAYDIS(DOLAMT,GAME) = DAYDIS(DOLAMT,GAME)-DISAMT
                AGTGAM(GDCNT,GAME,TER) = AGTGAM(GDCNT,GAME,TER)-1
                AGTGAM(GDAMT,GAME,TER) = AGTGAM(GDAMT,GAME,TER)-DISAMT
            ENDIF
            CALL UPDGAM(TRABUF,KICKER)
            AGTTAB(ALSCAN,TER) = TRABUF(TSER)
            AGTTAB(ALSTRA,TER) = TRABUF(TSER)
            AGTTAB(AGTLKN,TER) = -1
            AGTTAB(AGTLKN2,TER)= -1

            RETURN
        ENDIF
C       TRACNT (transaction count)| TCAN (cancelamento de aposta mutua)
C       DOLAMT (transaction amount)| TWAG (aposta mutua)
        DAYTYP(TRACNT,TCAN,GAME) = DAYTYP(TRACNT,TCAN,GAME)+1
        DAYTYP(DOLAMT,TCAN,GAME) = DAYTYP(DOLAMT,TCAN,GAME)+AMT
        DAYTYP(TRACNT,TWAG,GAME) = DAYTYP(TRACNT,TWAG,GAME)-1
        DAYTYP(DOLAMT,TWAG,GAME) = DAYTYP(DOLAMT,TWAG,GAME)-AMT
        IF(KICKER) CALL UPDKIK(TRABUF)
        CALL UPDGAM(TRABUF,KICKER)
C
C UPDATE AGENT INFORMATION
C GCCNT=Cancel Count | GCAMT=Cancel amount | GTKCHG=ticket charge
C DISAMT -> TWDAMT (discount amout)
C GDCNT = Discount count | GDAMT=discount amount
        AGTGAM(GCCNT,GAME,TER) = AGTGAM(GCCNT,GAME,TER)+1
        AGTGAM(GCAMT,GAME,TER) = AGTGAM(GCAMT,GAME,TER)+AMT
        AGTGAM(GTKCHG,GAME,TER) = AGTGAM(GTKCHG,GAME,TER)-TRABUF(TWTKC)
        IF(DISAMT.NE.0) THEN
            DAYDIS(TRACNT,GAME) = DAYDIS(TRACNT,GAME)-1
            DAYDIS(DOLAMT,GAME) = DAYDIS(DOLAMT,GAME)-DISAMT
            AGTGAM(GDCNT,GAME,TER) = AGTGAM(GDCNT,GAME,TER)-1
            AGTGAM(GDAMT,GAME,TER) = AGTGAM(GDAMT,GAME,TER)-DISAMT
        ENDIF
        AGTTAB(ALSCAN,TER) = TRABUF(TSER) !ALSCAN=last cancel serial
        AGTTAB(ALSTRA,TER) = TRABUF(TSER) !ALSTRA=last transaction serial
        AGTTAB(AGTLKN,TER) = -1 !AGTLKN=last kicker number for retries (no longer used ignore)
        AGTTAB(AGTLKN2,TER)= -1 !AGTLKN2=last second kicker number for retries (no longer used ignore)
        IF(TRABUF(TGAMTYP).EQ.TBNG) THEN !game type bingo (not used by SCML so ignore)
           AGTTAB(AGTBSED,TER) = -1
        ENDIF
        IF(TRABUF(TGAMTYP).EQ.TPAS) THEN !game type passive lotarial nacional (no longer used in Millennium)
           AGTTAB(AGTEPS,TER) = -1
           AGTTAB(AGTEPC,TER) = AGTTAB(AGTEPC,TER) + 1      
        ENDIF 
        RETURN
C
C INTERNAL CANCELLATION UPDATE
C
300     CONTINUE
        IF(TRABUF(TGAMTYP).EQ.TKIK) THEN !game type joker (no longer used)
            CALL UPDKIK(TRABUF)
            CALL UPDGAM(TRABUF,KICKER)
            AGTGAM(GTKCHG,GAME,TER) = AGTGAM(GTKCHG,GAME,TER)-TRABUF(TWTKC)
            IF(DISAMT.NE.0) THEN
                DAYDIS(TRACNT,GAME) = DAYDIS(TRACNT,GAME)-1
                DAYDIS(DOLAMT,GAME) = DAYDIS(DOLAMT,GAME)-DISAMT
                AGTGAM(GDCNT,GAME,TER) = AGTGAM(GDCNT,GAME,TER)-1
                AGTGAM(GDAMT,GAME,TER) = AGTGAM(GDAMT,GAME,TER)-DISAMT
            ENDIF
            AGTTAB(ALSWAG,TER) = 0
            AGTTAB(ALSTRA,TER) = 0
            AGTHTB(ATRNUM,TER) = -1
            AGTTAB(AGTLKN,TER) = -1
            AGTTAB(AGTLKN2,TER)= -1
            RETURN
        ENDIF
C
C UPDATE DAILY AND GAME TOTALS
C
        DAYTYP(TRACNT,TINC,GAME) = DAYTYP(TRACNT,TINC,GAME)+1 !TRACNT=TRANSACTTION COUNT
        DAYTYP(DOLAMT,TINC,GAME) = DAYTYP(DOLAMT,TINC,GAME)+AMT !TINC=INTERNAL CANCELLATION
        DAYTYP(TRACNT,TWAG,GAME) = DAYTYP(TRACNT,TWAG,GAME)-1 !TWAG=WAGER 
        DAYTYP(DOLAMT,TWAG,GAME) = DAYTYP(DOLAMT,TWAG,GAME)-AMT !DOLAMT=TRANSACTION AMOUNT
        IF(KICKER) CALL UPDKIK(TRABUF) !joker (no longer in use - ignore)
        CALL UPDGAM(TRABUF,KICKER) !joker (no longer in use - ignore)
C
C UPDATE AGENT INFORMATION
C
        AGTGAM(GSCNT,GAME,TER) = AGTGAM(GSCNT,GAME,TER)-1 !GSCNT=SALES COUNT
        AGTGAM(GSAMT,GAME,TER) = AGTGAM(GSAMT,GAME,TER)-AMT !GSAMT=Sales amount
        AGTGAM(GTKCHG,GAME,TER) = AGTGAM(GTKCHG,GAME,TER)-TRABUF(TWTKC) !GTKCHG=TICKET CHARGE | TWTKC=TICKET CHARGE
        IF(DISAMT.NE.0) THEN
            DAYDIS(TRACNT,GAME) = DAYDIS(TRACNT,GAME)-1
            DAYDIS(DOLAMT,GAME) = DAYDIS(DOLAMT,GAME)-DISAMT
            AGTGAM(GDCNT,GAME,TER) = AGTGAM(GDCNT,GAME,TER)-1
            AGTGAM(GDAMT,GAME,TER) = AGTGAM(GDAMT,GAME,TER)-DISAMT
        ENDIF
        AGTTAB(ALSWAG,TER) = 0
        AGTTAB(ALSTRA,TER) = 0
        AGTHTB(ATRNUM,TER) = -1    ! rev 1.5
        AGTTAB(AGTLKN,TER) = -1
        AGTTAB(AGTLKN2,TER)= -1
        IF(TRABUF(TGAMTYP).EQ.TBNG) THEN !bingo (not used)
           AGTTAB(AGTBSED,TER) = -1
        ENDIF
        IF(TRABUF(TGAMTYP).EQ.TPAS) THEN !passive (LN) (no longer in use in Millennium)
           AGTTAB(AGTEPS,TER) = -1
        ENDIF 
        RETURN
C
C VALIDATION/REFUND UPDATE
C
400     CONTINUE
C
C IF PASSIVE "BUNCH" VALIDATIONS  
C
        IF(TRABUF(TGAMTYP).EQ.TPAS) GOTO 700 !passive (LN) (no longer in use in Millennium)
C
C IF ALLREADY PAYED TO BANK, IGNORE.
C
        IF(TRABUF(TVTYPE).EQ.VPTB) GOTO 410 !TVTYPE=validation type |VPTB= pay to bank
C VNREG=NEW VALIDATION INQUIRY REGULAR !V70
C VNIBO=NEW VALIDATION INQUIRY MID-TIER BANK ONLY !V70 
C VNINQ=NEW VALIDATION INQUIRY MID-TIER !V70       
        IF(TRABUF(TVTYPE).EQ.VNREG .OR. TRABUF(TVTYPE).EQ.VNIBO .OR. 
     *     TRABUF(TVTYPE).EQ.VNINQ) GOTO 410 !V11
C
C UPDATE DAILY AND AGENT TOTALS 
C
!------------------>>V11 -----------------------------------------------
        IF(TRABUF(TVTYPE) .EQ. VNDON) THEN !CASH ACCEPTED VNDON=NEW VALIDATION INQUIRY CASH ACCEPTED !V70
          IF(TRABUF(TVPAY).NE.0) THEN ! TVPAY=AMOUNT PAID
            DAYTYP(TRACNT,TVAL,GAME) = DAYTYP(TRACNT,TVAL,GAME) + 1
            AGTGAM(GVCNT,GAME,TER)   = AGTGAM(GVCNT,GAME,TER)   + 1
            IF(TRABUF(TVOPPAY).GT.0) THEN !V12
                !WITH OPS SUPRESSION, TRABUF(TVOPPAY) CORRESPONDS TO NET PRIZE AND TRABUF(TVPAY) CORRESPONDS TO PRIZE VALUE WITH NO TAX.
C----+------------------------------------------------------------------
C V13| Uniforming agent prize pay amount to add only prize amounts
C----+------------------------------------------------------------------
              !Prize amount comes in TRABUF(TVPAY)
              !Net prize amount comes in TRABUF(TVOPPAY)
              !DAYTYP(DOLAMT,TVAL,GAME) = DAYTYP(DOLAMT,TVAL,GAME) + TRABUF(TVOPPAY)
              !AGTGAM(GVAMT,GAME,TER)   = AGTGAM(GVAMT,GAME,TER)   + TRABUF(TVOPPAY)
              DAYTYP(DOLAMT,TVAL,GAME) = DAYTYP(DOLAMT,TVAL,GAME) + TRABUF(TVPAY)
              AGTGAM(GVAMT,GAME,TER)   = AGTGAM(GVAMT,GAME,TER)   + TRABUF(TVPAY)
C----+------------------------------------------------------------------
C V13| Uniforming agent prize pay amount to add only prize amounts
C----+------------------------------------------------------------------
            ELSEIF(TRABUF(TVOPPAY).EQ.0) THEN !V12
              !UNTIL THE SUPRESSION OF OPS, TRABUF(TVOPPAY) = 0 FOR CASHABLE PRIZES AND TRABUF(TVPAY) CORRESPONDS TO NET PRIZE.
              DAYTYP(DOLAMT,TVAL,GAME) = DAYTYP(DOLAMT,TVAL,GAME) + TRABUF(TVPAY)
              AGTGAM(GVAMT,GAME,TER)   = AGTGAM(GVAMT,GAME,TER)   + TRABUF(TVPAY)
            ENDIF
          ENDIF
          IF(TRABUF(TVKPAY).NE.0) THEN
            KGAME=TRABUF(TVKGME)
            DAYTYP(TRACNT,TVAL,KGAME) = DAYTYP(TRACNT,TVAL,KGAME) + 1
            AGTGAM(GVCNT,KGAME,TER)   = AGTGAM(GVCNT,KGAME,TER)   + 1
            IF(TRABUF(TVKOPPAY).GT.0) THEN !V12
              !WITH OPS SUPRESSION, TRABUF(TVKOPPAY) CORRESPONDS TO NET PRIZE AND TRABUF(TVKPAY) CORRESPONDS TO PRIZE VALUE WITH NO TAX.
C----+------------------------------------------------------------------
C V13| Uniforming agent prize pay amount to add only prize amounts
C----+------------------------------------------------------------------
              !Prize amount comes in TRABUF(TVKPAY)
              !Net prize amount comes in TRABUF(TVOKPPAY)
              !DAYTYP(DOLAMT,TVAL,KGAME) = DAYTYP(DOLAMT,TVAL,KGAME) + TRABUF(TVKOPPAY)
              !AGTGAM(GVAMT,KGAME,TER)   = AGTGAM(GVAMT,KGAME,TER)   + TRABUF(TVKOPPAY)
              DAYTYP(DOLAMT,TVAL,KGAME) = DAYTYP(DOLAMT,TVAL,KGAME) + TRABUF(TVKPAY)
              AGTGAM(GVAMT,KGAME,TER)   = AGTGAM(GVAMT,KGAME,TER)   + TRABUF(TVKPAY)
C----+------------------------------------------------------------------
C V13| Uniforming agent prize pay amount to add only prize amounts
C----+------------------------------------------------------------------
            ELSEIF(TRABUF(TVKOPPAY).EQ.0) THEN !V12
              !UNTIL THE SUPRESSION OF OPS, TRABUF(TVKOPPAY) = 0 FOR CASHABLE PRIZES AND TRABUF(TVKPAY) CORRESPONDS TO NET PRIZE.
              DAYTYP(DOLAMT,TVAL,KGAME) = DAYTYP(DOLAMT,TVAL,KGAME) + TRABUF(TVKPAY)
              AGTGAM(GVAMT,KGAME,TER)   = AGTGAM(GVAMT,KGAME,TER)   + TRABUF(TVKPAY)
            ENDIF
          ENDIF
          GOTO 405 !BYPASS OPS UPDATE
        ELSEIF(TRABUF(TVTYPE) .EQ. VNBNK) THEN !BANK TRANSFER ACCEPTED
          IF(TRABUF(TVPAY).NE.0) THEN 
            DAYTYP(TRACNT,TVAL,GAME) = DAYTYP(TRACNT,TVAL,GAME) + 1
            AGTGAM(GVCNT,GAME,TER)   = AGTGAM(GVCNT,GAME,TER)   + 1
          ENDIF
          IF(TRABUF(TVKPAY).NE.0) THEN
            KGAME=TRABUF(TVKGME)
            DAYTYP(TRACNT,TVAL,KGAME) = DAYTYP(TRACNT,TVAL,KGAME) + 1
            AGTGAM(GVCNT,KGAME,TER)   = AGTGAM(GVCNT,KGAME,TER)   + 1
          ENDIF
          GOTO 405 !BYPASS OPS UPDATE
        ENDIF
!------------------ V11<<-----------------------------------------------
C
        IF(TRABUF(TVPAY).NE.0) THEN
!------------------ V10 BEGIN --------------------------------------------------
!            DAYTYP(TRACNT,TVAL,GAME) = DAYTYP(TRACNT,TVAL,GAME)+1
!            DAYTYP(DOLAMT,TVAL,GAME) = DAYTYP(DOLAMT,TVAL,GAME)+
!     *                                 TRABUF(TVPAY) - TRABUF(TVOPPAY)  !V04
!            AGTGAM(GVCNT,GAME,TER) = AGTGAM(GVCNT,GAME,TER)+1
!            AGTGAM(GVAMT,GAME,TER) = AGTGAM(GVAMT,GAME,TER)+
!     *                               TRABUF(TVPAY) - TRABUF(TVOPPAY) !V04

            DAYTYP(TRACNT,TVAL,GAME) = DAYTYP(TRACNT,TVAL,GAME) + 1
            AGTGAM(GVCNT,GAME,TER)   = AGTGAM(GVCNT,GAME,TER)   + 1
            IF(TRABUF(TVOPPAY) .EQ. 0) THEN
              DAYTYP(DOLAMT,TVAL,GAME) = DAYTYP(DOLAMT,TVAL,GAME) + TRABUF(TVPAY)
              AGTGAM(GVAMT,GAME,TER)   = AGTGAM(GVAMT,GAME,TER)   + TRABUF(TVPAY)
            ENDIF
!------------------ V10 END --------------------------------------------------
        ENDIF
C
C
        IF(TRABUF(TVKPAY).NE.0) THEN
!------------------ V10 BEGIN --------------------------------------------------
!            KGAME=TRABUF(TVKGME)
!            DAYTYP(TRACNT,TVAL,KGAME) = DAYTYP(TRACNT,TVAL,KGAME)+1
!            DAYTYP(DOLAMT,TVAL,KGAME) = DAYTYP(DOLAMT,TVAL,KGAME)+
!     *                                  TRABUF(TVKPAY) - TRABUF(TVKOPPAY)                      !V04
!            AGTGAM(GVCNT,KGAME,TER)=AGTGAM(GVCNT,KGAME,TER)+1
!            AGTGAM(GVAMT,KGAME,TER)=AGTGAM(GVAMT,KGAME,TER)+TRABUF(TVKPAY) - TRABUF(TVKOPPAY)  !V04

            KGAME=TRABUF(TVKGME)
            DAYTYP(TRACNT,TVAL,KGAME) = DAYTYP(TRACNT,TVAL,KGAME) + 1
            AGTGAM(GVCNT,KGAME,TER)   = AGTGAM(GVCNT,KGAME,TER)   + 1
            IF(TRABUF(TVKOPPAY) .EQ. 0) THEN
              DAYTYP(DOLAMT,TVAL,KGAME) = DAYTYP(DOLAMT,TVAL,KGAME) + TRABUF(TVKPAY)
              AGTGAM(GVAMT,KGAME,TER)   = AGTGAM(GVAMT,KGAME,TER)   + TRABUF(TVKPAY)
            ENDIF
!------------------ V10 END --------------------------------------------------
        ENDIF
C
405     CONTINUE !V11

        IF(TRABUF(TVREF).NE.0) THEN
            DAYTYP(TRACNT,TREF,GAME) = DAYTYP(TRACNT,TREF,GAME)+1
            DAYTYP(DOLAMT,TREF,GAME) = DAYTYP(DOLAMT,TREF,GAME)+
     *                                 TRABUF(TVREF)
            AGTGAM(GRCNT,GAME,TER) = AGTGAM(GRCNT,GAME,TER)+1
            AGTGAM(GRAMT,GAME,TER) = AGTGAM(GRAMT,GAME,TER)+
     *                               TRABUF(TVREF)
        ENDIF
C
410     CONTINUE
        AGTTAB(ALSTRA,TER) = TRABUF(TSER)
        AGTTAB(ALSVAL,TER) = TRABUF(TSER)

        RETURN
C
C INSTANT VALIDATION UPDATE
C
600     CONTINUE

        OFFSET = TRABUF(TITYP) + 1
        IF(TRABUF(TITYP) .NE. IVAL) DAYCRS(OFFSET) = DAYCRS(OFFSET) + 1

        IF(TRABUF(TITYP).EQ.IVAL) THEN
           CLERK = AGTHTB(AGTPASOFF,TER) !AGTPASOFF=AGENT PASSNUMBER OFFSET
           AGTTAB(ALSIVA,TER) = TRABUF(TSER) !ALSIVA=LAST INSTANT VALIDATION SERIAL #
           AGTTAB(ALSVAL,TER) = TRABUF(TSER) !ALSVAL=LAST VALIDATION SERIAL #
           AGTTAB(ALSTRA,TER) = TRABUF(TSER) !ALSTRA=LAST TRANSACTION SERIAL #
C----+------------------------------------------------------------------
C V13| Uniforming agent prize pay amount to add only prize amounts
C----+------------------------------------------------------------------
           IS_BANK_TRANSFER = .FALSE.
           IF(   TRABUF(TIVMT)  .EQ. IBVMT !IBVMT=INSTANT BANK VALIDATION MODE (NEW)
     *     .AND. TRABUF(TIVALM) .EQ. IVBM_BNK ) THEN !IVBM_BNK  = NEW BANK VALIDATION HIGH TIER BANK TRANSFER REQUEST / BANK TRANSFER ACCEPTED
              IS_BANK_TRANSFER = .TRUE.
           ENDIF
C----+------------------------------------------------------------------
C V13| Uniforming agent prize pay amount to add only prize amounts
C----+------------------------------------------------------------------
           DO I = 0,TRABUF(TIBCH) - 1
              IF(I.LE.TIVMX) THEN !(TIVMX=7) !7 VALIDATION MAX IN BATCH (see Millennium->Millennium_Buffers->LOGBUF.ods->Lotaria_Instantanêa_(IPS)_-_VAL)
                 IF(TRABUF(TISTS1+I).EQ.INOER) THEN !TISTS1=INSTANT VALIDATION STATUS |INOER=NO ERROR
                    DAYCRS(OFFSET) = DAYCRS(OFFSET) + 1 !numero total de instant validations que correu bem
C                   DAYIVAL = INSTANT VALIDATION AMT (total do dia)                   
                    DAYIVAL = DAYIVAL + TRABUF(TIPRZ1+I) !TIPRZ1=INSTANT PRIZE FROM GAME PLAN 
                    AMOUNT=TRABUF(TIPRZ1+I) !TIPRZ1=INSTANT PRIZE FROM GAME |  AGTMIS -> MISCELANIOUS SALES
                    AGTMIS(CLERK,1,TER)=AGTMIS(CLERK,1,TER)+1 !incrementa o offset do AGENT PASSNUMBER
C----+------------------------------------------------------------------
C V13| Uniforming agent prize pay amount to add only prize amounts
C----+------------------------------------------------------------------
                    IF( IS_BANK_TRANSFER .EQ. .FALSE. ) THEN !primeiro nivel de premios (dinheiro vivo) 2,3 niveis transferencia
                        AGTMIS(CLERK,2,TER)=AGTMIS(CLERK,2,TER)+AMOUNT
                    ENDIF
C----+------------------------------------------------------------------
C V13| Uniforming agent prize pay amount to add only prize amounts
C----+------------------------------------------------------------------
                 ENDIF
              ENDIF
           ENDDO
        ELSE IF(TRABUF(TITYP).EQ.IMNU) THEN !IMNU=INSTANT SUPPLY ORDER (se for do tipo encomenda)
           AGTTAB(ALSORD,TER) = TRABUF(TSER)
           AGTTAB(ALSTRA,TER) = TRABUF(TSER)
        ENDIF

        RETURN
C
C PASSIVE LOTTERY VALIDATIONS AND RETURNS - NO LONGER IN USE
C
700     CONTINUE
        IF(TRABUF(TERR).NE.NOER) GOTO 710                        !V09
!+++++++Get total validation amount
        xamt = 0
        xtik = 0
        if(trabuf(TPOFFTER).gt.0) then
          if(trabuf(TPOFFTER).le.NUMAGT) then
            if(tsbit(agttab(AGTTYP,trabuf(TPOFFTER)),AGTBNK)) ter = trabuf(TPOFFTER)
          else
            TYPE*,'============ UPDSUB ============'
            TYPE*,'STATUS:   ',TRABUF(TSTAT)
            TYPE*,'ERROR:    ',TRABUF(TERR)
            TYPE*,'CDC:      ',TRABUF(TCDC)
            TYPE*,'SERIAL:   ',TRABUF(TSER)
            TYPE*,'TERMINAL: ',TRABUF(TTER)
            TYPE*,'AGENT:    ',TRABUF(TAGT)
            TYPE*,'TRNTYPE:  ',TRABUF(TTYP)
            TYPE*,'GAME:     ',TRABUF(TGAM)
            TYPE*,'GTYPE:    ',TRABUF(TGAMTYP)
            TYPE*,'GINDEX:   ',TRABUF(TGAMIND)
            TYPE*,'TPOFFTER: ',TRABUF(TPOFFTER)
          endif
        endif
!
        if(trabuf(TTYP).eq.tret) then ! returns
          do tcks = 1,trabuf(TPTCK)
            offs = offtra*(tcks-1)
            emis = getpagemi(trabuf(TPEMIS1+offs),gind)            
            if(trabuf(TPSTS1+offs).eq.RETURND.or.trabuf(TPSTS1+offs).eq.RETAFDR) then
              xamt = xamt + trabuf(TPPAY1+offs)
              if(trabuf(tpretyp).eq.ALLTCK) then
                if (trabuf(TGAMIND).eq.PSBPOP) then
                  xtik = pasnoffra(emis,gind) * 2
                else
                  xtik = pasnoffra(emis,gind)
                endif
              elseif(trabuf(TPRETYP).eq.BYFRAC) then
                xtik = 1
              elseif(trabuf(TPRETYP).eq.HALFTCK) then
                if(trabuf(TGAMIND).eq.PSBPOP) then
                  xtik = pasnoffra(emis,gind)
                else
                  xtik = pasnoffra(emis,gind)/2
                endif
              elseif (trabuf(TPRETYP).eq.QUARTCK) then
                if(trabuf(TGAMIND).eq.PSBPOP) then
                  xtik = pasnoffra(emis,gind)
                else
                  xtik = pasnoffra(emis,gind)/4
                endif
              endif

              pasfin(TRACNT,TRET,emis,gind) = pasfin(TRACNT,TRET,emis,gind) + xtik
              pasfin(DOLAMT,TRET,emis,gind) = pasfin(DOLAMT,TRET,emis,gind) + trabuf(TPPAY1+offs)
              pasretcnt(emis,gind) = pasretcnt(emis,gind) + xtik
!=============if(trabuf(TPSTS1+offs).eq.RETAFDR) pasretaftamt(emis,gind) = pasretaftamt(emis,gind) + trabuf(TPPAY1+offs)
            endif
          enddo    
          daytyp(TRACNT,TRET,game) = daytyp(TRACNT,TRET,game) + trabuf(TPFRCNT)
          daytyp(DOLAMT,TRET,game) = daytyp(DOLAMT,TRET,game) + xamt 
          
          agtgam(GCLCNT,game,ter) = agtgam(GCLCNT,game,ter) + trabuf(TPFRCNT)
          agtgam(GCLAMT,game,ter) = agtgam(GCLAMT,game,ter) + xamt          

          agttab(ALSUPA,trabuf(TTER)) = trabuf(TSER)          !V09
        else
          do tcks = 1,trabuf(TPTCK)
            offs = offtra*(tcks-1)
            emis = getpagemi(trabuf(TPEMIS1+offs),gind)
            if  (trabuf(TPSTS1+offs).eq.VWINNER) then
              xamt = xamt + trabuf(TPPAY1+offs)
              xtik = xtik + 1
              pasfin(TRACNT,TVAL,emis,gind) = pasfin(TRACNT,TVAL,emis,gind) + 1
              pasfin(DOLAMT,TVAL,emis,gind) = pasfin(DOLAMT,TVAL,emis,gind) + trabuf(TPPAY1+offs)              
              pastodpay(emis,gind) = pastodpay(emis,gind) + 1 
            endif
          enddo
C----+------------------------------------------------------------------
C V13| Uniforming agent prize pay amount to add only prize amounts
C----+------------------------------------------------------------------
!          daytyp(TRACNT,TVAL,game) = daytyp(TRACNT,TVAL,game) + xtik
!          daytyp(DOLAMT,TVAL,game) = daytyp(DOLAMT,TVAL,game) + xamt     
!              
!          if(trabuf(TVTYPE).ne.VPNBNK) then  ! Issued to BANK does not count agents
!            agtgam(GVCNT,game,ter) = agtgam(GVCNT,game,ter) + xtik
!            agtgam(GVAMT,game,ter) = agtgam(GVAMT,game,ter) + xamt
!          endif
          daytyp(TRACNT,TVAL,game) = daytyp(TRACNT,TVAL,game) + xtik
          agtgam(GVCNT,game,ter) = agtgam(GVCNT,game,ter) + xtik

          if(trabuf(TVTYPE).ne.VPNBNK) then  ! Issued to BANK does not count agents
            daytyp(DOLAMT,TVAL,game) = daytyp(DOLAMT,TVAL,game) + xamt
            agtgam(GVAMT,game,ter) = agtgam(GVAMT,game,ter) + xamt
          endif
C----+------------------------------------------------------------------
C V13| Uniforming agent prize pay amount to add only prize amounts
C----+------------------------------------------------------------------

          agttab(ALSPAS,trabuf(TTER)) = trabuf(TSER)
          agttab(ALSVAL,trabuf(TTER)) = trabuf(TSER)
        endif
!+++++++        
710     CONTINUE                                               !V09  
        agttab(ALSTRA,trabuf(TTER)) = trabuf(TSER)             !V09
        return
        end

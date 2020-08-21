C VALUPD.FOR
C
C V44 24-aPR-2017 MTK Modified for cancelled sports draws
C V43 28-NOV-2013 SCML New Validation Messages
C V42 19-AUG-2009 FJG Even when KEXP is reached.
C V41 29-JUL-2009 FRP/FJG Avoid exchange ticket generation for KIKER postponed
C V40 15-FEB-2001 EPH Check for selling agent when ticket has an OP associated
C V39 05-FEB-2001 EPH Holding days for Hi Prizes, using P() parameter.
C V38 01-DEC-2000 UXN TOTOGOLO ADDED.
C V37 21-MAR-2000 UXN Holding limits checked also for ODDSET games fraction
C                     winners. 
C V36 10-MAR-2000 OXK DAYDRW checkedfor TSPT
C V35 01-FEB-2000 OXK SPT_PRZDRW used for TSPT (Vakio changes)
C V34 13-OCT-1999 RXK World Tour added.
C V33 08-JUN-1999 UXN Don't cash 0 mks winners !!
C V32 24-MAY-1999 UXN Temporary change for SPEDE removed.
C V31 17-MAY-1999 UXN Super Triple added.
C V30 05-MAY-1999 UXN Fix for holding limits for LOTTO,VIKING, etc. games
C                     with JOKER win only.
C V29 07-SEP-1998 RXK Draw number for Kicker game always set
C V28 12-JAN-1998 RXK Super Score and Today's Triple added.
C V27 27-NOV-1997 UXN Changes for LOTTO extra draw.
C V26 18-MAR-1997 RXK Use of DSKFFLG commented out because after first fail 
C                     VLF was never read
C V25 20-FEB-1997 RXK For oddset games wins of more than 100 draws can be
C                     cashed
C V24 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V23 19-FEB-1996 HXK Fix for READ error when same coupon is high tier win 
C                     (i.e. same coupon again) 
C V22 18-FEB-1996 HXK Changed IREAD error handling on VLF
C V21 23-NOV-1995 HXK Merge of post 65 stuff; changes for Double/Couple
C V20 04-AUG-1995 HXK Batch of fixes for Ravi V5 installation
C V19 20-DEC-1994 HXK Changes for holding limits
C V18 11-DEC-1994 HXK Added Win Hold limits modifications
C V17 05-DEC-1994 HXK Merging from 25-Nov -> 5 Dec
C V16 01-DEC-1994 HXK Fix for VPZOFF
C V15 30-NOV-1994 HXK Changed PRZCOM for Bingo
C V14 29-NOV-1994 HXK Added Fraction value check to holding limits check
C V13 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V12 26-APR-1994 JXP Check new win holding amount and day limits
C V11 09-OCT-1993 GXA Removed Type statements.
C V10 18-SEP-1993 HXK PUT IN TYPE STATEMENTS TO TRY AND SORT OUT VERSION 
C                     PROBLEMS
C V09 18-SEP-1993 HXK Removed PVCS problem
C V08 17-SEP-1993 HXK Fix for VIKING / LOTTO exch draws
C V07 15-SEP-1993 HXK Fix for Viking Winsel
C V06 10-SEP-1993 GXA Changed TWBNK to TVBNK for PAY TO BANK transactions.
C V05 22-AUG-1993 GXA Released for Finland Dec Conversion / Oddset.
C V04 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V03 21-APR-1992GCAN FIX TO VALSUP TO CHECK SUPPRESSION ON COMPANION GAME
C                     (KICKER) AS WELL AS THE MAIN GAME.
C V02 07-OCT-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C SUBROUTINE TO RETRIEVE AND UPDATE TRANSACTIONS FROM THE VALIDATION FILE.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE VALUPD(TRABUF,WRKBUF,EXCFLG,VALREC)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:NBRCOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:PRZCOM.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'

        LOGICAL EXCFLG,EFLAG,CLAIMED,VFLAG,CFLAG,PRVFLG
CRXK        LOGICAL DSKFFLG/.FALSE./  !disk fail flag
        LOGICAL DSKFFLG
C
        INTEGER*4 DRAW, KGIND, KDRAW, KGAM, KIK, KI2
        INTEGER*4 GIND, GTYP, GAM, VST, ST, TER, FILE, I
 
        INTEGER*4 LBUF(LREC*3)
        INTEGER*4 WRKBUF(TRALEN)
        INTEGER*4 REGAMT            !REGULAR GAMES WINNINGS
        INTEGER*4 KIKAMT            !KICKER GAMES WINNNINGS
        INTEGER*4 REFAMT            !REFUND AMOUNT
        INTEGER*4 TOTAMT            !TOTAL AMOUNT
        INTEGER*4 INDEX

        INTEGER*4 DIV, DOFF, SUB, AMT, CDC, SHR
        INTEGER*4   BIND
	INTEGER*4   MAX_PRZDRW
	INTEGER*4   FRAC_CNT,MAXFRAC
	INTEGER*4   LSTKDRAW             !V42
C
C VARIABLES TO CONTROL EXCHANGED TICKET FOR POSPONED DRAW
C
        INTEGER * 4 LST_DRW_DATE(MAXGAM)  / MAXGAM * 0 /  ! LAST DRAW DATE
        INTEGER * 4 DRWEXP                                ! EXPIRED DRAW NUMBER
        INTEGER * 4 GNUM                                  ! GAME NUMBER
C
        LOGICAL FRSTIME / .TRUE. /       ! FIRST TIME THAT RUN THIS RUNTINE
        LOGICAL PNDDRW                   ! POSTPONED DRAW ( YES / NO )
        LOGICAL CHECK_LOTTO_STATUS       ! FUNCTION
C
        PRVFLG=.FALSE.
        EFLAG=.FALSE.
        CFLAG=.FALSE.
        VFLAG=.FALSE.
        CLAIMED=.FALSE.
        REGAMT=0
        KIKAMT=0
        REFAMT=0
        TRABUF(TSTAT)=REJT
        TRABUF(TERR)=INVL
        TRABUF(TVSTS)=VNOWIN
        TRABUF(TVCODE)=NOWIN
        TRABUF(TFIL)=EARLY
        TER=TRABUF(TTER)
        CALL FASTSET(0,VALREC,VALLEN)
        VALREC(VSTAT)=VNOWIN
C
        DSKFFLG = .FALSE.
C
C
C *** IF TICKET WAS SOLD TODAY THEN TRY TO CASH FROM TMF
C
        IF(TRABUF(TVCDC).EQ.DAYCDC) GOTO 1000
C
C
C
C
C *** TRY TO READ THE TRANSACTION FROM THE VALIDATION FILE.
C     IF THE RECORD IS NOT IN THE VALIDATION FILE THEN TRY
C     TO READ IT FROM THE DAILY CARRYOVER FILE.
C       
CRXK    IF(DSKFFLG) THEN
CRXK       CALL VALERR(ST,VLF,TRABUF(TVSER))
CRXK           TRABUF(TSTAT)  = REJT
CRXK           TRABUF(TERR)   = SUPR
CRXK           TRABUF(TVCODE) = NOWIN
CRXK       RETURN
CRXK    ENDIF
        CALL IREAD(TRABUF(TVCDC),V4BUF,VLF,ST)

        IF(ST.EQ.ERRRNF) THEN
           RETURN
        ELSE
           IF(ST.NE.0) THEN
              DSKFFLG=.TRUE.
CRXK          CALL VALERR(ST,VLF,TRABUF(TVSER))
              CALL VALERR(ST,VLF,TRABUF(TSER))   !to get ability to trace
              TRABUF(TSTAT)  = REJT
              TRABUF(TERR)   = SUPR
              TRABUF(TVCODE) = NOWIN
              RETURN
C***          !Finland doesn't have Numbers game so don't go to DCF (GOTO 2000)
C***          CALL FASTSET(0,VALREC,VALLEN)
C***          GOTO 2000
           ENDIF
        ENDIF
C
C
        VFLAG=.TRUE.
        CALL LOGVAL(VALREC,V4BUF)
        VST=VALREC(VSTAT)
        GAM=VALREC(VGAM)
        GTYP=VALREC(VGTYP)
        GIND=VALREC(VGIND)
        KGAM=VALREC(VKGME)
        TRABUF(TVSTS)=VST
        TRABUF(TGAM)=GAM
        TRABUF(TGAMTYP)=GTYP
        TRABUF(TGAMIND)=GIND
        TRABUF(TVKGME)=KGAM
        TRABUF(TVSTER)=VALREC(VSTER)
        TRABUF(TVWCDC)=VALREC(VWCDC)
        CALL VALSUP(TRABUF,ST)
        IF(ST.NE.0) RETURN
C
C CHECK GAME TYPE AND GAME INDEX
C
        IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP.OR.
     *     GIND.LT.1.OR.GIND.GT.MAXIND) RETURN
C
C CHECK FOR VALIDATION DETAILES INQUIRY
C
        IF(TRABUF(TVTYPE).EQ.VDET) THEN
           TRABUF(TERR) = VINQ
           TRABUF(TVCODE) = VDETREP
           RETURN
        ENDIF
C
C CHECK FOR ALREADY PAID
C

C----+------------------------------------------------------------------
C V43| Adding New Validation Messages
C----+------------------------------------------------------------------
C        IF(VST.EQ.VCASH.OR.VST.EQ.VCASHX) THEN
        IF(    VST .EQ. VCASH
     *    .OR. VST .EQ. VCASHX
     *    .OR. VST .EQ. VBANK
     *  ) THEN

C----+------------------------------------------------------------------
C V43| Adding New Validation Messages
C----+------------------------------------------------------------------
          TRABUF(TVCODE)=APAID
          RETURN
        ENDIF
C
C CHECK FOR RESULTS NOT IN
C
        IF(VST.EQ.VNOPAY.OR.VST.EQ.VNOPRZ.OR.VST.EQ.VPOST.OR.
     *     VST.EQ.VPPNPZ.OR.VST.EQ.VPRPOST) THEN
          IF(VST.EQ.VNOPAY) TRABUF(TVCODE)=NOTIN
C         IF(VST.EQ.VNOPRZ) TRABUF(TVCODE)=NOTIN
          IF(VST.EQ.VNOPRZ) TRABUF(TVCODE)=NPRZ
          IF(VST.EQ.VPOST ) TRABUF(TVCODE)=NOTIN
C         IF(VST.EQ.VPPNPZ) TRABUF(TVCODE)=NOTIN
          IF(VST.EQ.VPPNPZ) TRABUF(TVCODE)=NPRZ
          RETURN
        ENDIF
C
C CHECK IF PRIZE WAS PAID TO THE BANK
C
        IF(VST.EQ.VBANK) THEN
           TRABUF(TVCODE) = VTOBNK
           RETURN
        ENDIF
C
C CHECK IF PRIZE IS GOING TO BE PAID TO THE BANK
C
        IF(VALREC(VBNKID).NE.0.AND.VALREC(VBNKNUM).NE.0.AND.
     *     VST.NE.VCXL.AND.VST.NE.VDEL) THEN
           TRABUF(TVCODE) = VTOBNK
           IF(TRABUF(TVTYPE).EQ.VPTB) TRABUF(TVCODE) = BASET
           RETURN
        ENDIF
C
C CHECK IF THIS IS THE TERMINAL THAT SOLD THE TICKET
C IN CASE THIS TICKET HAS AN OP ASSOCIATED TO IT
C
C REMOVED BECASUSE IF THE TERMINAL THAT SOLD THE WAGER IS REMOVED WE CAN'T VALIDATE IN ANY TERMINAL
C
C	IF (VALREC(VOPSAMT)+VALREC(VKOPSAMT).NE.0) THEN    !HAS AN OP
C           IF (VALREC(VSTER).NE.TER) THEN   !IT IS NOT THE SELLING TERMINAL
C              TRABUF(TSTAT)  = REJT
C              TRABUF(TERR)   = SUPR
C              TRABUF(TVCODE) = TOAGTN    !GO TO AGENT
C              RETURN
C           ENDIF
C	ENDIF

C
C CHECK IF ALREADY CLAIMED
C
        IF(VST.EQ.VCLAM.OR.VST.EQ.VCLAMX) THEN
          IF(TSBIT(AGTTAB(AGTTYP,TER),AGTPRV)) THEN
            CLAIMED=.TRUE.
          ELSE
            TRABUF(TVCODE)=ACLAM
            RETURN
          ENDIF
        ENDIF
C
C CHECK IF TICKET CAN BE CASHED
C
        IF((VST.NE.VUNCSH.AND.VST.NE.VPRPAY).AND..NOT.CLAIMED) RETURN
C
C CHECK IF TICKET HAS EXPIRED
C
        CALL CHKEXP(VALREC,ST)
        IF(ST.NE.0) RETURN
C
C CHECK IF TICKET IS UNDER HOLD
C
        REGAMT=VALREC(VPAMT)
        KIKAMT=VALREC(VKPAMT)
        REFAMT=VALREC(VRAMT)
        TOTAMT = REGAMT + KIKAMT + REFAMT

        IF(VALREC(VPZOFF).LT.1.OR.TOTAMT.LE.0) THEN
           TRABUF(TSTAT)=REJT
           TRABUF(TERR)=INVL
           TRABUF(TVCODE)=VNOWIN
           RETURN
        ENDIF

	FRAC_CNT = VALREC(VFRAC)
	IF(FRAC_CNT.EQ.0) FRAC_CNT = 10
	MAXFRAC = MAXFRC(VALREC(VGAM))
	IF(MAXFRAC.EQ.0) MAXFRAC = 10
	FRAC_CNT = MAXFRAC/FRAC_CNT
	IF(FRAC_CNT.LE.0) FRAC_CNT = 1

        CALL DLOGVAL(VALREC,VDETAIL)
        DO 100 I=1,VALREC(VPZOFF)
           IF(VDETAIL(VPRG,I).EQ.1) GOTO  100           !Skip Purged winners.
C
           DRAW = VDETAIL(VDRW,I)
           KIK =  VDETAIL(VKIK,I)
           KI2 =  VDETAIL(VKI2,I)
           GAM =  VALREC(VGAM)   !ensure gam is reset correctly
           IF(KIK.NE.0.OR.KI2.NE.0) THEN
	      GAM   = KGAM
              KGIND = GNTTAB(GAMIDX,GAM)
	   ENDIF
           DOFF = DAYHDR(GAM) - DRAW + 1
           DIV  = VDETAIL(VDIV,I)
           SUB  = VDETAIL(VSUB,I)
           SHR  = VDETAIL(VSHR,I)


           IF(DOFF.LT.1) THEN
              TRABUF(TSTAT)=REJT
              TRABUF(TERR)=INVL
              TRABUF(TVCODE)=VNOWIN
              RETURN
           ENDIF
	   MAX_PRZDRW = PRZDRW 
           IF(DOFF.GT.MAX_PRZDRW .AND.
     *        (GTYP.EQ.TLTO.OR.GTYP.EQ.TSPT.OR.GTYP.EQ.TTGL.OR.
     *         GTYP.EQ.TKIK.OR.GTYP.EQ.TBNG)) THEN
              TRABUF(TSTAT)=REJT
              TRABUF(TERR)=INVL
              TRABUF(TVCODE)=VNOWIN
              RETURN
           ENDIF
 
           IF(GTYP.EQ.TKIK.OR.KIK.NE.0.OR.KI2.NE.0) THEN
              IF(DIV.LE.0) THEN
                 TRABUF(TSTAT)=REJT
                 TRABUF(TERR)=INVL
                 TRABUF(TVCODE)=VNOWIN
                 RETURN
              ENDIF
              AMT = KPZSHV(DIV,DOFF,KGIND)
              CDC = KPZDAT(DOFF,KGIND)
           ELSEIF(GTYP.EQ.TLTO) THEN
              IF(DIV.LE.0) THEN
                 TRABUF(TSTAT)=REJT
                 TRABUF(TERR)=INVL
                 TRABUF(TVCODE)=VNOWIN
                 RETURN
              ENDIF
              BIND = 1
              IF(VDETAIL(VBDR,I).NE.0) BIND = 2
              AMT = LPZSHV(DIV,BIND,DOFF,GIND)
              CDC = LPZDAT(DOFF,GIND)
           ELSEIF(GTYP.EQ.TSPT) THEN
              IF(DIV.LE.0) THEN
                 TRABUF(TSTAT)=REJT
                 TRABUF(TERR)=INVL
                 TRABUF(TVCODE)=VNOWIN
                 RETURN
              ENDIF
              AMT = SPZSHV(DIV,DOFF,GIND)
              CDC = SPZDAT(DOFF,GIND)
           ELSEIF(GTYP.EQ.TTGL) THEN
              IF(DIV.LE.0) THEN
                 TRABUF(TSTAT)=REJT
                 TRABUF(TERR)=INVL
                 TRABUF(TVCODE)=VNOWIN
                 RETURN
              ENDIF
              AMT = TGZSHV(DIV,DOFF,GIND)
              CDC = TGZDAT(DOFF,GIND)
           ELSEIF(GTYP.EQ.TTSL) THEN
              AMT = SHR*FRAC_CNT
              CDC = VALREC(VWCDC)
           ELSEIF(GTYP.EQ.TWIT) THEN
              AMT = SHR*FRAC_CNT
              CDC = VALREC(VWCDC)
           ELSEIF(GTYP.EQ.TSCR) THEN
              AMT = SHR*FRAC_CNT
              CDC = VALREC(VWCDC)
           ELSEIF(GTYP.EQ.TBNG) THEN
              IF(SUB.EQ.0.OR.DIV.LE.0) THEN
                 TRABUF(TSTAT)=REJT
                 TRABUF(TERR)=INVL
                 TRABUF(TVCODE)=VNOWIN
                 RETURN
              ENDIF
              AMT = BPZSHV(DIV,SUB,DOFF,GIND)
              CDC = BPZDAT(DOFF,GIND)
           ELSEIF(GTYP.EQ.TDBL) THEN
              AMT = SHR*FRAC_CNT
              CDC = VALREC(VWCDC)
           ELSEIF(GTYP.EQ.TCPL) THEN
              AMT = SHR*FRAC_CNT
              CDC = VALREC(VWCDC)
           ELSEIF(GTYP.EQ.TSSC) THEN
              AMT = SHR*FRAC_CNT
              CDC = VALREC(VWCDC)
           ELSEIF(GTYP.EQ.TTRP) THEN
              AMT = SHR*FRAC_CNT
              CDC = VALREC(VWCDC)
           ELSEIF(GTYP.EQ.TSTR) THEN
              AMT = SHR*FRAC_CNT
              CDC = VALREC(VWCDC)
           ELSE
              TRABUF(TSTAT)=REJT
              TRABUF(TERR)=INVL
              TRABUF(TVCODE)=VNOWIN
              RETURN
           ENDIF

           INDEX=0
           IF (SCC_HLDLIM(GAM,1).LE.AMT) THEN
                INDEX=1
           ELSEIF (SCC_HLDLIM(GAM,2).LE.AMT) THEN
                INDEX=2
           ELSEIF (SCC_HLDLIM(GAM,3).LE.AMT) THEN
                INDEX=3
           ENDIF
           IF(INDEX.NE.0) THEN
              IF(DAYCDC.LE.CDC+SCC_HLDDAY(GAM,INDEX)) THEN
                 TRABUF(TSTAT)=REJT
                 TRABUF(TERR)=INVL
                 TRABUF(TVCODE)=WINHOLD
                 RETURN 
              ENDIF
           ENDIF
C                                                                                !v39
C          HOLDING FOR PORTUGAL BIG PRIZES                                       !v39
C          CDC IS DRAW CDC (CLOSE CDC)                                           !v39
C                                                                                !v39
           IF (AMT .GE. P(VALPRZHI)  .AND.  DAYCDC .LE. CDC+P(DAYHDPHI)) THEN    !v39
              TRABUF(TSTAT)=REJT                                                 !v39
              TRABUF(TERR)=INVL                                                  !v39
              TRABUF(TVCODE)=WINHOLD                                             !v39
              RETURN                                                             !v39
           ENDIF                                                                 !v39

100     CONTINUE
C================================================================================        
C V42
C================================================================================
C
C GET INFORMATION FROM PREVIOUS DRAW. THIS IS FOR POSTPONED DRAWS. WE NEED THIS
C INFORMATION ONLY ONE TIME, ONLY TO INITIATE LAST DRAW TABLE DATE
C
        IF(FRSTIME .EQ. .TRUE.) THEN
          CALL GET_PREVIOS_DRAW_DATE(LST_DRW_DATE, LTODRW, TLTO)
          CALL GET_PREVIOS_DRAW_DATE(LST_DRW_DATE, KIKDRW, TKIK)                        
          FRSTIME = .FALSE.
        ENDIF
C================================================================================        
C V42
C================================================================================
C
C CHECK KICKER GAME STATUS
C
        DRAW  = 0
        KDRAW = 0
        IF(KGAM.NE.0) THEN
          KGIND=GNTTAB(GAMIDX,KGAM)
          IF(VALREC(VKEXP).GE.KIKDRW(KGIND).AND.
     *       KIKDRW(KGIND).GT.0.AND..NOT.CLAIMED) THEN
            EFLAG=.TRUE.
            KDRAW=KIKDRW(KGIND)
            IF(GTYP.EQ.TKIK) DRAW = KDRAW
            IF(KIKSTS(KGIND).GE.GAMBFD) THEN
              TRABUF(TFIL)=LATE
              TRABUF(TVCODE)=NOTIN
              RETURN
            ENDIF
C================================================================================        
C V42
C================================================================================
C           IF(FRSTIME .EQ. .TRUE.) THEN
C             CALL GET_PREVIOS_DRAW_DATE(LST_DRW_DATE, LTODRW, TLTO)
C             CALL GET_PREVIOS_DRAW_DATE(LST_DRW_DATE, KIKDRW, TKIK)              
C             FRSTIME = .FALSE.
C           ENDIF
C================================================================================        
C V42
C================================================================================                 
            IF(LST_DRW_DATE(KGAM) .GE. DAYCDC) THEN
              TRABUF(TSTAT)=REJT
              TRABUF(TERR)=INVL
              TRABUF(TVCODE)=NPRZ
              RETURN
            ENDIF
C================================================================================        
C V42
C================================================================================
          ELSE
            LSTKDRAW=KIKDRW(KGIND)-1
            IF(VALREC(VKEXP).EQ.LSTKDRAW.AND.
     *         LST_DRW_DATE(KGAM).GE.DAYCDC) THEN
              TRABUF(TSTAT)=REJT
              TRABUF(TERR)=INVL
              TRABUF(TVCODE)=NPRZ
              RETURN        
            ENDIF
C================================================================================        
C V42
C================================================================================                
          ENDIF
        ENDIF
C
C CHECK LOTTO GAME STATUS ( IF NOT LOTTO GAME, GO TO CHEK NEXT GAME STATUS )
C
        IF(GTYP .NE. TLTO) GOTO 10000
C================================================================================        
C V42
C================================================================================
C
C GET INFORMATION FROM PREVIOUS DRAW. THIS IS FOR POSTPONED DRAWS. WE NEED THIS
C INFORMATION ONLY ONE TIME, ONLY TO INITIATE LAST DRAW TABLE DATE
C
C       IF(FRSTIME .EQ. .TRUE.) THEN
C         CALL GET_PREVIOS_DRAW_DATE(LST_DRW_DATE, LTODRW, TLTO)
C         CALL GET_PREVIOS_DRAW_DATE(LST_DRW_DATE, KIKDRW, TKIK)                        
C         FRSTIME = .FALSE.
C       ENDIF
C================================================================================        
C V42
C================================================================================
C
C CHECK IF DRAW THAT ARE VALIDATING IT'S A POSTPONED ONE
C
        PNDDRW = .FALSE.
        GNUM = TRABUF(TGAM)
        IF(LST_DRW_DATE(GNUM) .GE. DAYCDC) PNDDRW = .TRUE.
C
C CALCULATE DRAW NUMBER TO CHECK IF VALIDATION IS EXPIRED OR NOT
C
       DRWEXP = LTODRW(GIND)
       IF(PNDDRW .EQ. .TRUE.) DRWEXP = LTODRW(GIND) - 1
C
C CHECK IF VAIDATION HAVE TO PRODUCE EXCHANGE TICKET OR NOT
C
       IF(DRWEXP .LE. 0) GOTO 10000
       IF(VALREC(VEXP) .GE. DRWEXP .AND. .NOT. CLAIMED) THEN
         EFLAG = .TRUE.
         DRAW = LTODRW(GIND) 
         IF(PNDDRW .EQ. .TRUE.) DRAW = DRAW - 1
         IF(CHECK_LOTTO_STATUS(LTOSTS(GIND), PNDDRW, DRAW, GNUM)) THEN
           TRABUF(TFIL) = LATE
           TRABUF(TVCODE) = NOTIN
           RETURN
         ENDIF
       ENDIF
C
C END TO TEST LOTTO GAME STATUS
C
10000   CONTINUE
C
C CHECK SPORTS GAME STATUS
C
        IF(GTYP.EQ.TSPT) THEN
          IF(VALREC(VEXP).GE.SPTDRW(GIND).AND.
     *       SPTDRW(GIND).GT.0.AND..NOT.CLAIMED.AND.
     *	     DAYDRW(VALREC(VGAM)).NE.0) THEN
            EFLAG=.TRUE.
            DRAW=SPTDRW(GIND)
            IF(SPTDCD(GIND).NE.0) THEN                           !V44
              IF(VALREC(VEXP).EQ.SPTDRW(GIND)) EFLAG = .FALSE.   !V44
            ELSEIF(SPTSTS(GIND).GE.GAMBFD) THEN                  !V44
              TRABUF(TFIL)=LATE
              TRABUF(TVCODE)=NOTIN
              RETURN
            ENDIF
          ENDIF
        ENDIF
C
C CHECK TOTOGOLO GAME STATUS
C
        IF(GTYP.EQ.TTGL) THEN
          IF(VALREC(VEXP).GE.TGLDRW(GIND).AND.
     *       TGLDRW(GIND).GT.0.AND..NOT.CLAIMED.AND.
     *	     DAYDRW(VALREC(VGAM)).NE.0) THEN
            EFLAG=.TRUE.
            DRAW=TGLDRW(GIND)
            IF(TGLSTS(GIND).GE.GAMBFD) THEN
              TRABUF(TFIL)=LATE
              TRABUF(TVCODE)=NOTIN
              RETURN
            ENDIF
          ENDIF
        ENDIF
C
C CHECK NUMBERS GAME STATUS
C
        IF(GTYP.EQ.TNBR) THEN
          IF(VALREC(VEXP).GE.NBRDRW(GIND).AND.
     *       NBRDRW(GIND).GT.0) THEN
            IF(.NOT.CLAIMED) EFLAG=.TRUE.
            DRAW=NBRDRW(GIND)
            IF(NBRSTS(GIND).GE.GAMBFD) THEN
              CFLAG=.TRUE.
              TRABUF(TFIL)=LATE
              IF(NBRSTS(GIND).LT.GAMDON) THEN
                TRABUF(TVCODE)=NOTIN
                RETURN
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C
        IF(CFLAG) GOTO 2000
        IF(EFLAG) GOTO 3000
C
C
C *** VALIDATE TICKET FROM VALIDATION FILE ONLY
C
        REGAMT=VALREC(VPAMT)
        KIKAMT=VALREC(VKPAMT)
        REFAMT=VALREC(VRAMT)
        CALL CHKRED(TRABUF,VALREC,REGAMT,KIKAMT,REFAMT,EFLAG,CLAIMED)
        IF(TRABUF(TSTAT).NE.GOOD) RETURN
        CALL VALLOG(VALREC,V4BUF)
        CALL IWRITE(V4BUF,VLF,ST)
        IF(ST.NE.0) THEN
          DSKFFLG=.TRUE.
          CALL VALERR(ST,VLF,TRABUF(TSER))
          TRABUF(TSTAT)=REJT
          TRABUF(TERR)=SUPR
          TRABUF(TVCODE)=NOWIN
          RETURN
        ENDIF
        RETURN
C
C VALIDATE TICKET FROM TM FILE
C
1000    CONTINUE
        CALL FASTSET(0,VALREC,VALLEN)
        EFLAG=.FALSE.
        TRABUF(TFIL)=LATE
        CALL RLOG(TRABUF(TVSER),LBUF,VAL,ST)
        IF(ST.NE.0) RETURN
        CALL LOGTRA(WRKBUF,LBUF)
        IF(WRKBUF(TTYP).NE.TWAG) RETURN
        IF(WRKBUF(TSTAT).EQ.CASH.OR.WRKBUF(TSTAT).EQ.XCSH) THEN
          TRABUF(TVCODE)=APAID
          RETURN
        ENDIF
        IF(WRKBUF(TSTAT).EQ.CLAM.OR.WRKBUF(TSTAT).EQ.XCLM) THEN
          IF(TSBIT(AGTTAB(AGTTYP,TER),AGTPRV)) THEN
            TRABUF(TVCODE)=NOTIN
            RETURN
          ELSE
            TRABUF(TVCODE)=ACLAM
            RETURN
          ENDIF
        ENDIF
        IF(WRKBUF(TSTAT).NE.GOOD.AND.WRKBUF(TSTAT).NE.EXCH.AND.
     *     WRKBUF(TSTAT).NE.CLAM.AND.WRKBUF(TSTAT).NE.XCLM) RETURN
        TRABUF(TGAMTYP)=WRKBUF(TGAMTYP)
        TRABUF(TGAMIND)=WRKBUF(TGAMIND)
        TRABUF(TGAM   )=WRKBUF(TGAM   )
        TRABUF(TVSTER )=WRKBUF(TTER   )
        CALL VALSUP(TRABUF,ST)
        IF(ST.NE.0) RETURN
C
C
        GTYP=WRKBUF(TGAMTYP)
        IF(GTYP.EQ.TNBR) THEN
          CALL CHKNBR(WRKBUF,REGAMT,EFLAG,DRAW,PRVFLG)
          IF(PRVFLG) VALREC(VSTAT)=VPRPAY
        ELSE
          RETURN
        ENDIF
        IF(REGAMT.EQ.0)RETURN
C
C
        CALL CHKRED(TRABUF,VALREC,REGAMT,KIKAMT,REFAMT,EFLAG,CLAIMED)
        IF(TRABUF(TSTAT).NE.GOOD) RETURN
        IF(EFLAG.AND..NOT.EXCFLG) THEN
          EXCFLG=.TRUE.
          RETURN
        ENDIF
C
C
        IF(TRABUF(TTYP).EQ.TVAL.OR.TRABUF(TTYP).EQ.TREF) THEN
          IF(WRKBUF(TSTAT).EQ.GOOD.OR.
     *       WRKBUF(TSTAT).EQ.CLAM) WRKBUF(TSTAT)=CASH
          IF(WRKBUF(TSTAT).EQ.EXCH.OR.
     *       WRKBUF(TSTAT).EQ.XCLM) WRKBUF(TSTAT)=XCSH
        ELSE
          IF(WRKBUF(TSTAT).EQ.GOOD) WRKBUF(TSTAT)=CLAM
          IF(WRKBUF(TSTAT).EQ.EXCH) WRKBUF(TSTAT)=XCLM
        ENDIF
        WRKBUF(TWCTER)=TRABUF(TTER)
        WRKBUF(TWCSER)=TRABUF(TSER)
        WRKBUF(TWVSTS)=TRABUF(TVSTS)
        TRABUF(TFIL)=LATE
        CALL TRALOG(WRKBUF,LBUF)
        CALL WLOG(WRKBUF(TSER),LBUF,VAL)
        IF(EXCFLG) CALL BUILDX(WRKBUF,TRABUF(TVEXC),DRAW,KDRAW,LATE)
        RETURN
C
C VALIDATE TICKET FROM NUMBERS CARRYOVER AND/OR VALIDATION FIL
C
2000    CONTINUE
        EFLAG=.FALSE.
        CALL IREAD(TRABUF(TVCDC),LBUF,DCF,ST)
        IF(ST.NE.0) THEN
          TRABUF(TSTAT)=REJT
          TRABUF(TERR)=INVL
          TRABUF(TVCODE)=NOWIN
          RETURN
        ENDIF
C
C
        CALL LOGTRA(WRKBUF,LBUF)
        IF(WRKBUF(TTYP).NE.TWAG)RETURN
        IF(WRKBUF(TSTAT).EQ.CASH.OR.WRKBUF(TSTAT).EQ.XCSH) THEN
          TRABUF(TVCODE)=APAID
          RETURN
        ENDIF
        IF(WRKBUF(TSTAT).EQ.CLAM.OR.WRKBUF(TSTAT).EQ.XCLM) THEN
          IF(TSBIT(AGTTAB(AGTTYP,TER),AGTPRV)) THEN
            CLAIMED=.TRUE.
            IF(.NOT.VFLAG) THEN
              TRABUF(TVCODE)=NOTIN
              RETURN
            ENDIF
          ELSE
            TRABUF(TVCODE)=ACLAM
            RETURN
          ENDIF
        ENDIF
        IF(WRKBUF(TSTAT).NE.GOOD.AND.WRKBUF(TSTAT).NE.EXCH.AND.
     *     WRKBUF(TSTAT).NE.CLAM.AND.WRKBUF(TSTAT).NE.XCLM.AND.
     *     WRKBUF(TSTAT).NE.XCHD) RETURN
        TRABUF(TGAMTYP)=WRKBUF(TGAMTYP)
        TRABUF(TGAMIND)=WRKBUF(TGAMIND)
        TRABUF(TGAM   )=WRKBUF(TGAM   )
        TRABUF(TVSTER )=WRKBUF(TTER   )
        CALL VALSUP(TRABUF,ST)
        IF(ST.NE.0) RETURN
C
C
        GTYP=WRKBUF(TGAMTYP)
        IF(GTYP.EQ.TNBR) THEN
          GIND=WRKBUF(TGAMIND)                             
          IF(NBRSTS(GIND).GE.GAMBFD) TRABUF(TFIL) = LATE   
          KIKAMT=0
          CALL CHKNBR(WRKBUF,REGAMT,EFLAG,DRAW,PRVFLG)
          IF(PRVFLG) VALREC(VSTAT)=VPRPAY
        ELSE
          RETURN
        ENDIF
        IF(REGAMT.EQ.0.AND.REFAMT.EQ.0.AND..NOT.VFLAG) RETURN
        IF(VFLAG) THEN
          REGAMT=REGAMT+VALREC(VPAMT)
          KIKAMT=KIKAMT+VALREC(VKPAMT)
          REFAMT=REFAMT+VALREC(VRAMT)
        ENDIF
        CALL CHKRED(TRABUF,VALREC,REGAMT,KIKAMT,REFAMT,EFLAG,CLAIMED)
        IF(TRABUF(TSTAT).NE.GOOD) RETURN
        IF(EFLAG.AND..NOT.EXCFLG) THEN
          EXCFLG=.TRUE.
          RETURN
        ENDIF
C
        IF(VFLAG) THEN
           CALL VALLOG(VALREC,V4BUF)
           CALL IWRITE(V4BUF,VLF,ST)
           IF(ST.NE.0) THEN
              DSKFFLG=.TRUE.
              CALL VALERR(ST,VLF,TRABUF(TSER))
              TRABUF(TSTAT)=REJT
              TRABUF(TERR)=SUPR
              TRABUF(TVCODE)=NOWIN
           ENDIF
        ENDIF
        IF(REGAMT.EQ.0.AND.REFAMT.EQ.0.AND..NOT.EFLAG) RETURN
C
        IF(TRABUF(TTYP).EQ.TVAL.OR.TRABUF(TTYP).EQ.TREF) THEN
          IF(WRKBUF(TSTAT).EQ.GOOD.OR.
     *       WRKBUF(TSTAT).EQ.CLAM) WRKBUF(TSTAT)=CASH
          IF(WRKBUF(TSTAT).EQ.EXCH.OR.
     *       WRKBUF(TSTAT).EQ.XCLM) WRKBUF(TSTAT)=XCSH
        ELSE
          IF(WRKBUF(TSTAT).EQ.GOOD) WRKBUF(TSTAT)=CLAM
          IF(WRKBUF(TSTAT).EQ.EXCH) WRKBUF(TSTAT)=XCLM
        ENDIF
        WRKBUF(TWCTER)=TRABUF(TTER)
        WRKBUF(TWCSER)=TRABUF(TSER)
        WRKBUF(TWVSTS)=TRABUF(TVSTS)
        CALL TRALOG(WRKBUF,LBUF)
        CALL IWRITE(LBUF,DCF,ST)
        IF(ST.NE.0) CALL VALERR(ST,DCF,TRABUF(TSER))
        IF(EXCFLG) CALL BUILDX(WRKBUF,TRABUF(TVEXC),DRAW,KDRAW,LATE)
        RETURN
C
C
C VALIDATION FROM VLF ONLY WITH EXCHANGE TICKET
C
3000    CONTINUE
        REGAMT=VALREC(VPAMT)
        KIKAMT=VALREC(VKPAMT)
        REFAMT=VALREC(VRAMT)
        CALL CHKRED(TRABUF,VALREC,REGAMT,KIKAMT,REFAMT,EFLAG,CLAIMED)
        IF(TRABUF(TSTAT).NE.GOOD) RETURN
C
        IF(.NOT.EXCFLG.AND.TRABUF(TVTYPE).NE.VPTB) THEN
          EXCFLG=.TRUE.
          RETURN
        ENDIF
C
C
        FILE=TCF
        IF(GTYP.EQ.TNBR) FILE=DCF
        CALL IREAD(VALREC(VSCDC),LBUF,FILE,ST)
        IF(ST.NE.0) THEN
          CALL VALERR(ST,FILE,TRABUF(TSER))
          TRABUF(TSTAT)=REJT
          TRABUF(TERR)=SUPR
          TRABUF(TVCODE)=NOWIN
          RETURN
        ENDIF
C
C
        CALL VALLOG(VALREC,V4BUF)
        CALL IWRITE(V4BUF,VLF,ST)
        IF(ST.NE.0) THEN
          DSKFFLG=.TRUE.
          CALL VALERR(ST,VLF,TRABUF(TSER))
          TRABUF(TSTAT)=REJT
          TRABUF(TERR)=SUPR
          TRABUF(TVCODE)=NOWIN
          RETURN
        ENDIF
C
C
        CALL LOGTRA(WRKBUF,LBUF)
        IF(TRABUF(TVTYPE).EQ.VPTB) THEN
           WRKBUF(TWBNKID) = TRABUF(TVBNKID)
           WRKBUF(TWBNKNM) = TRABUF(TVBNKNUM)
           CALL TRALOG(WRKBUF,LBUF)
           CALL IWRITE(LBUF,FILE,ST)
           IF(ST.NE.0) CALL VALERR(ST,FILE,TRABUF(TSER))
        ELSE
           WRKBUF(TSTAT)=XCHD
           WRKBUF(TWCTER)=TRABUF(TTER)
           WRKBUF(TWCSER)=TRABUF(TSER)
           WRKBUF(TWVSTS)=TRABUF(TVSTS)
           CALL TRALOG(WRKBUF,LBUF)
           CALL IWRITE(LBUF,FILE,ST)
           IF(ST.NE.0) CALL VALERR(ST,FILE,TRABUF(TSER))
           CALL BUILDX(WRKBUF,TRABUF(TVEXC),DRAW,KDRAW,EARLY)
C 
C ADD EXCHANGE TICKET INTO THE CARRYOVER FILE.
C THIS IS DONE FOR THE NEW WINSEL SCHEM.
C
           CALL TRALOG(WRKBUF,LBUF)             
           CALL IWRITE(LBUF,FILE,ST)
           IF(ST.NE.0) CALL VALERR(ST,FILE,TRABUF(TSER))
        ENDIF
C
C          
        RETURN
        END


C ******************************************************************************
C
C     SUBROUTINE: GET_PREVIOS_DRAW_DATE
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 14 / 09 / 2001
C
C ******************************************************************************
C
C FUNCTION TO GET LAST DRAW DATE FOR SPECIFIC GAME TYPE
C
C=======OPTIONS /CHECK = NOOVERFLOW /EXT
      SUBROUTINE  GET_PREVIOS_DRAW_DATE(LST_DRW_DATE, ACTDRW, TYPGAM)
      IMPLICIT NONE
C
C INCLUDES DEFINITION GET LAST DRAW DATE FOR SPECIFIC GAME TYPE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:DLTREC.DEF'
      INCLUDE 'INCLIB:DKKREC.DEF'
C
C PARAMETERS DEFINITION TO GET LAST DRAW DATE FOR PECIFIC GAME TYPE
C
      INTEGER * 4 LST_DRW_DATE(MAXGAM)  ! LAST DRAW DATE ( OUTPUT VARIABLE )
      INTEGER * 4 ACTDRW(MAXIND)        ! ACTUAL GAME DRAW BY INDEX
      INTEGER * 4 TYPGAM                ! TYPE OF GAME
C
C VARIABLES DEFINITION TO GET LAST DRAW DATE FOR PECIFIC GAME TYPE
C
      INTEGER * 4 DRAW                  ! DRAW NUMBER TO READ INFORMATION
      INTEGER * 4 GNUM                  ! GAME NUMBER
      INTEGER * 4 GTYP                  ! GAME TYPE
      INTEGER * 4 GIND                  ! GAME INDEX
      INTEGER * 4 IDFIL / 1 /           ! IDENFIFICATION FILE NUMBER 
      INTEGER * 4 FDB(7)                ! FILE DESCRIPTOR BLOCK
      INTEGER * 4 FSTS                  ! FUNCTION STATUS
      INTEGER * 4 CNTA                  ! COUNTER A
      
C
C LOOP TO GET ACTIVE GAMES IN THE SYSTEM
C
      DO 1000 GNUM = 1, MAXGAM
C
C GET GAME TYPE AND GAME INDEX FOR ACTUAL GAME NUMBER
C
        GTYP = GNTTAB(GAMTYP, GNUM)
        GIND = GNTTAB(GAMIDX, GNUM)
C
C CHECK ACTUAL GAME SHOULD BE PROCESSED OR NOT
C
        IF(GTYP .NE. TYPGAM) GOTO 1000
        IF(GTYP .LT. 1 .OR. GTYP .GT. MAXTYP) GOTO 1000
        IF(GIND .LT. 1 .OR. GIND .GT. MAXIND) GOTO 1000
C
C SET DRAW NUMBER TO READ PREVIOUS DRAW INFORMATION 
C
        DRAW = ACTDRW(GIND) - 1
        IF(DRAW .LE. 0) GOTO 1000
C
C OPEN GAME FILE TO READ PREVIOS DRAW NUMBER
C
        CALL OPENW(IDFIL, GFNAMES(1, GNUM), 4, 0, 0, FSTS)
        IF(GTYP .EQ. TLTO) CALL IOINIT(FDB, IDFIL, DLTSEC * 256)
        IF(GTYP .EQ. TKIK) CALL IOINIT(FDB, IDFIL, DKKSEC * 256)
        IF(FSTS .NE. 0) THEN
	  TYPE *, IAM()
	  TYPE 100, IAM(), (GFNAMES(CNTA, GNUM), CNTA = 1, 5)
	  TYPE *, IAM()
          CALL GPAUSE
        ENDIF
C
C READ PREVIOS DRAW INFORMATION
C
        IF(GTYP .EQ. TLTO) CALL READW(FDB, DRAW, DLTREC, FSTS)
        IF(GTYP .EQ. TKIK) CALL READW(FDB, DRAW, DKKREC, FSTS)
        IF(FSTS .NE. 0) THEN
          TYPE *, IAM()
          TYPE 200, IAM(), DRAW, GNUM
          TYPE *, IAM()
          CALL GPAUSE
        ENDIF
C
C SET LAST DRAW DATE FOR ACTUAL GAME NUMBER
C
        IF(GTYP .EQ. TLTO) LST_DRW_DATE(GNUM) = DLTDAT(CURDRW)
        IF(GTYP .EQ. TKIK) LST_DRW_DATE(GNUM) = DKKDAT(CURDRW)
C
C CLOSE GAME FILE
C
        CALL USRCLOS1(IDFIL)
C
C END OF LOOP TO GET ACTIVE GAMES IN THE SYSTEM
C
1000  CONTINUE
C
C FORMATS DEFINITION TO GET LAST DRAW DATE FOR SPECIFIC GAME TYPE
C
100   FORMAT(A, 'Error Opening System File: ', 5A4)
200   FORMAT(A, 'Error Reading Draw ', I5.5, ' For Game Number: ', I5.5)
C
C THIS IS THE END TO GET LAST DRAW DATE FOR SPECIFIC GAME TYPE
C
      END


C ******************************************************************************
C
C     SUBROUTINE: CHECK_LOTTO_STATUS
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 14 / 09 / 2001
C
C ******************************************************************************
C
C FUNCTION TO CHECK IF LOTTO VALIDATION COULD BE VALIDATE ( LOTTO GAME STATUS )
C
C=======OPTIONS /CHECK = NOOVERFLOW /EXT
      LOGICAL FUNCTION CHECK_LOTTO_STATUS(ACTSTS, PNDDRW, DRAW, GNUM)
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO CHECK IF LOTTO VALIDATION COULD BE VALIDATE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:DLTREC.DEF'
C
C PARAMETERS DEFINITION TO CHECK IF LOTTO VALIDATION COULD BE VALIDATE
C
      INTEGER * 4 ACTSTS            ! ACTUAL MEMORY STATUS
      INTEGER * 4 DRAW              ! DRAW NUMBER TO CHECK STATUS FOR POSTPONED
      INTEGER * 4 GNUM              ! GAME NUMBER
C
      LOGICAL PNDDRW                ! POSTPONED DRAW ( YES / NO )
C
C VARIABLES DEFINITION TO CHECK IF LOTTO VALIDATION COULD BE VALIDATE
C
      INTEGER * 4 IDFIL / 1 /           ! IDENFIFICATION FILE NUMBER 
      INTEGER * 4 FDB(7)                ! FILE DESCRIPTOR BLOCK
      INTEGER * 4 FSTS                  ! FUNCTION STATUS
      INTEGER * 4 CNTA                  ! COUNTER A
C
C INITIATE VARIABLES TO CHECK IF LOTTO VALIDATION COULD BE VALIDATE
C
      CHECK_LOTTO_STATUS = .FALSE.
C
C CHECK STATUS IF WE DO NOT HAVE POSTPONED DRAW
C
      IF(PNDDRW .EQ. .FALSE.) THEN
        IF(ACTSTS .GE. GAMBFD) CHECK_LOTTO_STATUS = .TRUE.
        RETURN
      ENDIF
C
C CHECK STATUS FOR POSTPONED DRAW ( OPEN FILENAME TO READ DRAW STATUS )
C
        CALL OPENW(IDFIL, GFNAMES(1, GNUM), 4, 0, 0, FSTS)
        CALL IOINIT(FDB, IDFIL, DLTSEC * 256)
        IF(FSTS .NE. 0) THEN
	  TYPE *, IAM()
	  TYPE 100, IAM(), (GFNAMES(CNTA, GNUM), CNTA = 1, 5)
	  TYPE *, IAM()
          CALL GPAUSE
        ENDIF
C
C READ ACTUAL DRAW INFORMATION ( TO KNOW IF RESULT ARE ENTERED OR NOT )
C
        CALL READW(FDB, DRAW, DLTREC, FSTS)
        IF(FSTS .NE. 0) THEN
          TYPE *, IAM()
          TYPE 200, IAM(), DRAW, GNUM
          TYPE *, IAM()
          CALL GPAUSE
        ENDIF
C
C CHECK LOTTO STATUS
C
        IF(DLTSTS .GE. GAMENV) CHECK_LOTTO_STATUS = .TRUE.
C
C RETURN FUNCTION
C
        RETURN
C
C FORMATS DEFINITION TO CHECK IF LOTTO VALIDATION COULD BE VALIDATE
C
100   FORMAT(A, 'Error Opening System File: ', 5A4)
200   FORMAT(A, 'Error Reading Draw ', I5.5, ' For Game Number: ', I5.5)
C
C THIS IS THE END TO CHECK IF LOTTO VALIDATION COULD BE VALIDATE
C
      END




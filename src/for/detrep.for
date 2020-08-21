C SUBROUTINE DETREP
C
C V24 03-JAN-2011 HXK LOTTO2 CHANGES (TOTOLOTOQ/S) ... FIGCCC
C V23 29-NOV-2000 UXN Totogola added.
C V22 10-MAR-2000 OXK KGNUM from VALREC, not CONCOM
C V21 18-JAN-2000 OXK SPT_PRZDRW instead of PRZDRW for TSPT (Vakio changes)
C V20 13-OCT-1999 RXK World Tour added.
C V19 13-MAY-1999 UXN Super Triple added.
C V18 18-MAR-1999 RXK Gtyp+gind 5+3 bits change. Removed temp. change for 
C                     Viking.
C V17 16-SEP-1998 RXK Changes for new Jokeri game
C V16 27-NOV-1997 UXN Changes for LOTTO extra draw.
C V15 23-NOV-1995 HXK Merge of post 65 stuff; changes for Double/Couple
C V14 20-AUG-1995 HXK Fix for Viking bonus draw flag to be reset
C V13 04-AUG-1995 HXK Check for Bonus draw winner
C V12 27-JUL-1995 PXB V5 Game name bug fix.
C V11 23-NOV-1994 HXK Added Bingo subgames
C V10 15-OCT-1994 HXK Adding /developing Bingo (15.Oct.94)
C V09 17-JAN-1994 HXK INITIALIZE KIK1, KIK2 AFTER USE.
C V08 21-OCT-1993 GXA Skip purged winning detailes.
C V07 09-OCT-1993 GXA Changed order of detailes to appear. First to last.
C V06 06-SEP-1993 GXA Corrected Match tables for Lotto.
C                     Added Oddset detailes. (Increased share field to 4 bytes).
C V05 01-SEP-1993 HXK Send back correct kicker amount (vkpamt only)
C V04 27-AUG-1993 HXK put game index to end of ltowtb array
C V03 26-AUG-1993 GXA Some more modifications to Lotto Match tables.....
C                     Changed declaration of I4TEMP from I*2 to I*4.
C V02 26-AUG-1993 GXA Corrected Match Table Setup for Lotto.
C                     Check for division allready used up.
C V01 23-AUG-1993 HXK Initial revision.
C
C SUBROUTINE TO BUILD DETAILD VALIDATION OUTPUT MESSAGES.
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
        SUBROUTINE DETREP(TRABUF,VALREC,OUTTAB,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:PRZCOM.DEF'
C
        BYTE      OUTTAB(*)                     !Terminal Output Message.
C
        INTEGER*2 OUTLEN                        !Output Message Length.
C
        INTEGER*4 YEAR
C
        INTEGER*4   NUMDIGIT                    !Max number of digits in a I4
        PARAMETER   (NUMDIGIT=9)
C
        INTEGER*4 LTO_MATCH(LTGDIV,2,NUMLTO)    !Lotto Division Match Table
        INTEGER*4 KIK_MATCH(KIGDIV,2,NUMKIK)    !Kicker Division Match Table
        INTEGER*4 GNUM                          !Game Number.
        INTEGER*4 GTYP                          !Game Type
        INTEGER*4 GIND                          !Game Index.
        INTEGER*4 KGNUM                         !Kicker Game #.
        INTEGER*4 KGIND                         !Kicker Game Index.
        INTEGER*4 DRAW                          !Draw #
        INTEGER*4 DOFF                          !Draw Offset.
        INTEGER*4 WEEK                          !Week #
        INTEGER*4 CDC                           !Day CDC
        INTEGER*4 IND                           !Index into Output table.
        INTEGER*4 TMPIND                        !Save Index for Later.
        INTEGER*4 TMPIND2                       !Save Index for Later.
        INTEGER*4 KIK1                          !
        INTEGER*4 KIK2                          !
        INTEGER*4 DIV                           !Division Won.
        INTEGER*4 BDIV                          !Bonus Division Won.
        INTEGER*4 MDIV                          !# Matched for Division DIV.
        INTEGER*4 SHRS                          !Shares Won for DIV.
        INTEGER*4 BDRW                          !bonus draw (Viking only)
        INTEGER*4 I,K                           !Loop Variables.
        INTEGER*4 TIME                          !Transaction Time.
        INTEGER*4 TOTWIN                        !Total # winnings.
        INTEGER*4 KMNUM                         !Kicker Winning # Match map.
        INTEGER*4 SUB                           !
        INTEGER*4 DETTYP/Z13/                   !Detail Type/Subtype
        INTEGER*4 PRZEND/ZFF/                   !End of Detailed Prize data.
C
        LOGICAL   FIRST/.TRUE./                 !Init some stuff on startup.
        LOGICAL   FIRSTKIK                      !False after 1st kicker win found
C                                                (if other game+new kicker played)
        BYTE      I1TEMP(4)                     !Temp Variable.
        INTEGER*4 I4TEMP                        !Temp Variable.
        EQUIVALENCE(I4TEMP,I1TEMP)
        INTEGER*4   BIND
        INTEGER*4   OFF
        INTEGER*4   MAX_PRZDRW
C
C
C DEFINE DIVISION MATCH TABLE IF FIRST TIME AROUND FOR LOTTO AND KICKER.
C OTHER GAMES WILL COME OUT OF PRZCOM SINCE THEY HAVE VARIABLE MATCH TABLES.
C
        IF(FIRST) THEN
           FIRST = .FALSE.
           CALL FASTSET(0,LTO_MATCH,LTGDIV*2*NUMLTO)
           DO GIND = 1,NUMLTO                           
              DO K = 1,LTGBET
                 IF(LTOBET(K,GIND).NE.0) THEN
                    DO I = 1,LTOBET(K,GIND)
                       DIV = LTOWTB(I,1,K,GIND)
                       IF(DIV.GT.0.AND.DIV.LE.LTGDIV) 
     *                    LTO_MATCH(DIV,1,GIND) = I
C
C Check for Regular allready Set. If set do not set again.
C
                       BDIV = LTOWTB(I,2,K,GIND)
                       IF(BDIV.GT.0.AND.BDIV.LE.LTGDIV) THEN
                          IF(LTO_MATCH(BDIV,1,GIND).EQ.0) THEN      
                             LTO_MATCH(BDIV,2,GIND) = 1
                             LTO_MATCH(BDIV,1,GIND) = I
                          ENDIF
                       ENDIF
                    END DO
                 ENDIF
              END DO
           END DO
C
           CALL FASTSET(0,KIK_MATCH,KIGDIV*2*NUMKIK)
           DO GIND = 1,NUMKIK
              DO K = 1,KIGDIV
                 KMNUM = KIKMAT(1,K,GIND)
                 DO I = 1,NUMDIGIT
                    IF(MOD(KMNUM,10).EQ.9) THEN
                       KIK_MATCH(K,1,GIND) = KIK_MATCH(K,1,GIND) + 1
                    ENDIF
                    KMNUM = KMNUM / 10
                 END DO
              END DO
           END DO
C
        ENDIF
C
C SET / CLEAR VARIABLES
C
        DRAW = 0
        DOFF = 0
        WEEK = 0
C
C CONVERT DETAILED RECORD TO INTERNAL FORMAT
C
        CALL DLOGVAL(VALREC,VDETAIL)       
        IF(VALREC(VPZOFF).LT.1) GOTO 1000       !No detailes.
C
C BUILD OUTPUT MESSAGE
C
        OUTTAB(2) = DETTYP
        IND = 5
C
        TIME = P(ACTTIM)
        CALL PUTIME(TIME,OUTTAB,IND)
C
        I4TEMP = DAYCDC
        OUTTAB(IND+0) = I1TEMP(2)
        OUTTAB(IND+1) = I1TEMP(1) 
        IND = IND + 2
C                                       !*** CHECK IF VTYPE CAN BE USED
        I4TEMP = -1
        IF(VALREC(VSTAT).EQ.VCASH.OR.VALREC(VSTAT).EQ.VCASHX.OR.
     *     VALREC(VSTAT).EQ.VCLAM.OR.VALREC(VSTAT).EQ.VCLAMX)
     *     I4TEMP = VREG
C
        IF(VALREC(VSTAT).EQ.VBANK.OR.VALREC(VBNKID).NE.0.OR.
     *     VALREC(VBNKNUM).NE.0) I4TEMP = VPTB
C
        IF(I4TEMP.EQ.-1) GOTO 1000
C
        OUTTAB(IND+0) = I1TEMP(1)       !Validation Status.
        TMPIND2 = IND                  
        IND = IND + 1
C
        IF(I4TEMP.EQ.VPTB) THEN
           I4TEMP = VALREC(VBNKID)
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
C
           I4TEMP = VALREC(VBNKNUM)
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDIF
C
        I4TEMP = VALREC(VPAMT) + VALREC(VKPAMT)
        OUTTAB(IND+0) = I1TEMP(4)
        OUTTAB(IND+1) = I1TEMP(3)
        OUTTAB(IND+2) = I1TEMP(2)
        OUTTAB(IND+3) = I1TEMP(1)
        IND = IND + 4
C
        TMPIND = IND                    !Save Space for # winnings.
        IND = IND + 1
C
        GNUM = VALREC(VGAM)
        GTYP = GNTTAB(GAMTYP,GNUM)
        GIND = GNTTAB(GAMIDX,GNUM)

        OUTTAB(IND+0) = GTYP
        OUTTAB(IND+1) = GIND
        IND = IND + 2
C
        KGNUM = VALREC(VKGME)
        IF(KGNUM.GT.0) KGIND = GNTTAB(GAMIDX,KGNUM)
C
C LOOP FOR ALL WINNINGS
C
        TOTWIN = 0
	KIK1 = 0
	KIK2 = 0
        DO 100 I = 1, VALREC(VPZOFF)
           IF(VDETAIL(VPRG,I).EQ.1) GOTO  100           !Skip Purged winners.
           IF(DRAW.NE.VDETAIL(VDRW,I)) THEN
              TOTWIN = TOTWIN + 1
              IF(TOTWIN.NE.1) THEN
                 OUTTAB(IND+0) = PRZEND
                 I4TEMP = ISHFT(KIK2,4) + KIK1
                 KIK1 = 0
                 KIK2 = 0
                 OUTTAB(IND+1) = I1TEMP(1)                 
                 IND = IND + 2
              ENDIF
           ELSE   
              GOTO 200
           ENDIF
C
           DRAW = VDETAIL(VDRW,I)
           DOFF = DAYHDR(GNUM) - DRAW + 1
           FIRSTKIK=.TRUE.

           MAX_PRZDRW = PRZDRW
           IF((DOFF.LT.1.OR.DOFF.GT.MAX_PRZDRW).AND.
     *        (GTYP.EQ.TLTO.OR.GTYP.EQ.TSPT.OR.GTYP.EQ.TTGL.OR.
     *         GTYP.EQ.TKIK.OR.GTYP.EQ.TBNG)) GOTO 1000
C
C GET DRAW DATES
C
           IF(GTYP.EQ.TLTO) THEN
              CDC = LPZDAT(DOFF,GIND)
              OFF = 0           
           ELSEIF(GTYP.EQ.TSPT) THEN
              CDC = SPZDAT(DOFF,GIND)
              OFF = 0           
           ELSEIF(GTYP.EQ.TTGL) THEN
              CDC = TGZDAT(DOFF,GIND)
              OFF = 0           
           ELSEIF(GTYP.EQ.TKIK) THEN
              CDC = KPZDAT(DOFF,GIND)
              OFF = 0           
           ELSEIF(GTYP.EQ.TTSL) THEN
              CDC = VALREC(VWCDC)
              OFF = WEEK_OFFSET         
           ELSEIF(GTYP.EQ.TWIT) THEN
              CDC = VALREC(VWCDC)
              OFF = WEEK_OFFSET         
           ELSEIF(GTYP.EQ.TSCR) THEN
              CDC = VALREC(VWCDC)
              OFF = WEEK_OFFSET         
           ELSEIF(GTYP.EQ.TBNG) THEN
              CDC = VALREC(VWCDC)
              OFF = 0           
           ELSEIF(GTYP.EQ.TDBL) THEN
              CDC = VALREC(VWCDC)
              OFF = WEEK_OFFSET         
           ELSEIF(GTYP.EQ.TCPL) THEN
              CDC = VALREC(VWCDC)
              OFF = WEEK_OFFSET         
           ELSEIF(GTYP.EQ.TSSC) THEN
              CDC = VALREC(VWCDC)
              OFF = WEEK_OFFSET         
           ELSEIF(GTYP.EQ.TTRP) THEN
              CDC = VALREC(VWCDC)
              OFF = WEEK_OFFSET         
           ELSEIF(GTYP.EQ.TSTR) THEN
              CDC = VALREC(VWCDC)
              OFF = WEEK_OFFSET         
           ELSE
              GOTO 1000
           ENDIF           
C
	   IF(GTYP.EQ.TLTO.AND.(GIND.EQ.3.OR.GIND.EQ.4)) THEN
	     CALL FIGCCC(CDC-OFF,WEEK,YEAR)
	   ELSE
             CALL FIGWEK(CDC-OFF,WEEK,YEAR)
	   ENDIF
           OUTTAB(IND+0) = WEEK
           OUTTAB(IND+1) = MOD(YEAR,100)
           IND = IND + 2
C
C GET DETAILS BY GAME
C
200        CONTINUE
           DIV  = VDETAIL(VDIV,I)
           SHRS = VDETAIL(VSHR,I)
           SUB  = VDETAIL(VSUB,I)
           BDRW = 0
           IF(VDETAIL(VBDR,I).NE.0) BDRW = 1
C
           IF(VDETAIL(VKIK,I).NE.0) THEN
              KIK1 = KIK_MATCH(DIV,1,KGIND)
	      GOTO 100
           ENDIF
C
           IF(VDETAIL(VKI2,I).NE.0) THEN
              KIK2 = KIK_MATCH(DIV,1,KGIND)
              GOTO 100
           ENDIF
C
           IF(GTYP.EQ.TLTO) THEN
              MDIV = LTO_MATCH(DIV,1,GIND)
              IF(LTO_MATCH(DIV,2,GIND).NE.0) MDIV=IOR(MDIV,'00000010'X)
              BIND = 1
              IF(BDRW.NE.0) BIND = 2
              IF(LPZSHV(DIV,BIND,DOFF,GIND).EQ.0) GOTO 100
           ELSEIF(GTYP.EQ.TSPT) THEN
              MDIV = SPZMAT(DIV,DOFF,GIND)
              IF(SPZSHV(DIV,DOFF,GIND).EQ.0) GOTO 100
           ELSEIF(GTYP.EQ.TTGL) THEN
              MDIV = TGZMAT(DIV,DOFF,GIND)
              IF(TGZSHV(DIV,DOFF,GIND).EQ.0) GOTO 100
           ELSEIF(GTYP.EQ.TKIK) THEN
              MDIV = KIK_MATCH(DIV,1,KGIND)
              IF(KPZSHV(DIV,DOFF,NUMKIK).EQ.0) GOTO 100
           ELSEIF(GTYP.EQ.TTSL) THEN
              I4TEMP = VDETAIL(VREF,I)
              MDIV   = ISHFT(I4TEMP,4) + DIV
           ELSEIF(GTYP.EQ.TWIT) THEN
              I4TEMP = VDETAIL(VREF,I)
              MDIV   = ISHFT(I4TEMP,4) + DIV
           ELSEIF(GTYP.EQ.TSCR) THEN
              I4TEMP = VDETAIL(VREF,I)
              MDIV   = ISHFT(I4TEMP,4) + DIV
           ELSEIF(GTYP.EQ.TBNG) THEN
              CALL BINGO_DIV_TRANSFORM(DRAW,DOFF,GIND,DIV,SUB,MDIV)
              IF(BPZSHV(DIV,SUB,DOFF,GIND).EQ.0) GOTO 100
           ELSEIF(GTYP.EQ.TDBL) THEN
              I4TEMP = VDETAIL(VREF,I)
              MDIV   = ISHFT(I4TEMP,4) + DIV
           ELSEIF(GTYP.EQ.TCPL) THEN
              I4TEMP = VDETAIL(VREF,I)
              MDIV   = ISHFT(I4TEMP,4) + DIV
           ELSEIF(GTYP.EQ.TSSC) THEN
              I4TEMP = VDETAIL(VREF,I)
              MDIV   = ISHFT(I4TEMP,4) + DIV
           ELSEIF(GTYP.EQ.TTRP) THEN
              I4TEMP = VDETAIL(VREF,I)
              MDIV   = ISHFT(I4TEMP,4) + DIV
           ELSEIF(GTYP.EQ.TSTR) THEN
              I4TEMP = VDETAIL(VREF,I)
              MDIV   = ISHFT(I4TEMP,4) + DIV
           ENDIF
C
           IF(GTYP.EQ.TBNG) THEN
              I4TEMP = MDIV
                             !for old bingo set 1 in upper nibble of val.status
              IF(DRAW.LE.BNGLOB(GIND).AND.I.EQ.1) THEN
                 OUTTAB(TMPIND2)=OUTTAB(TMPIND2)+'10'X
              ENDIF
           ELSE
              IF(IAND(SUB,1).NE.0) BDRW = IOR(BDRW,'02'X) ! SET EXTRA DRAW FLAG...
              I4TEMP = ISHFT(BDRW,4) + MDIV
           ENDIF
           OUTTAB(IND+0) = I1TEMP(1)
           IND = IND + 1

           I4TEMP = SHRS
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
100     CONTINUE
C
C STORE END OF PRIZE
C
        OUTTAB(IND+0) = PRZEND
        IND = IND + 1
C
        I4TEMP = ISHFT(KIK2,4) + KIK1
        KIK1 = 0
        KIK2 = 0
        OUTTAB(IND+0) = I1TEMP(1)
        IND = IND + 1      
C
C STORE TOTAL # OF WINNINGS FOUND
C
        I4TEMP = TOTWIN
        OUTTAB(TMPIND) = I1TEMP(1)
C
        OUTLEN = IND - 1
        TRABUF(TVCODE) = VDETREP
        TRABUF(TSTAT)  = GOOD
        RETURN
C
C ERROR RETURN
C
1000    CONTINUE
        TRABUF(TVCODE) = VNODET
        TRABUF(TSTAT)  = REJT
        TRABUF(TERR)   = INVL
        RETURN
        END

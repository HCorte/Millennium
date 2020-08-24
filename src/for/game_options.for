C
C V26 04-APR-2009 TRG MODYFY STANDALONE EM JOKER BIT
C V25 25-MAR-2009 LRS STANDALONE EM JOKER
C V24 29-NOV-2000 UXN TTGL added.
C V23 03-MAR-2000 UXN Fixes for base price
C V22 19-OCT-1999 UXN Fixes for Alpha.
C V21 13-OCT-1999 RXK World Tour added.
C V20 07-JUL-1999 UXN MAXSCR added.
C V19 14-MAY-1999 UXN Super Triple added.
C V18 18-MAR-1999 RXK Game type,game index in separate bytes. 
C                     Removed hack of V5. 
C V17 01-FEB-1996 HXK Fix for Double Couple match list reports 
C                     (close dates, subevent dates)
C V16 29-JAN-1996 HXK Voittaja/Double/Couple max bet limit
C V15 23-NOV-1995 HXK Merge of post 65 stuff; changes for Double/Couple
C V14 24-APR-1995 HXK Merge of V5 development with March 10th 1995 bible
C V13 22-FEB-1995 HXK Hacked for V5 for terminal dudes
C V12 18-FEB-1995 HXK Changes for V5 game
C V11 05-DEC-1994 HXK Merging from 25-Nov -> 5 Dec
C V10 29-NOV-1994 HXK Disable Lucky Number for Bingp 
C V09 15-OCT-1994 HXK Adding /developing Bingo (15.Oct.94)
C V08 11-NOV-1993 GXA Made sure the multi-week field in mestab is cleared 
C                     beyond the 52 week limit since the terminal does not 
C                     check for this limit and will allow bets on
C                     53,54,55,56 week wagers.
C V07 31-AUG-1993 HXK Set Vakio text message
C V06 31-AUG-1993 SXH Look at text revision nibble for Ravi, to determine 
C                     if text should be enabled
C V05 27-AUG-1993 SXH MODIFIED RAVI TEXT REVISION LOGIC
C V04 22-AUG-1993 HXK refer to HXK
C V03 06-AUG-1993 HXK REMOVED SPGRMX,SPPRMX. USE MAXSPT,MAXPPP.
C V02 28-JUL-1993 GXA Only use TNUM if greater then zero.
C V01 16-JUL-1993 CXK Added CLRBIT to MDS filed in message to ensure clean 
C                     message.
C
C  CALLING SEQUENCE:
C      CALL GAME_OPTIONS(MESTAB,MESIND,TNUM,GNUM,PARTIAL_MSG)
C INPUT
C     MESTAB            - TERMINAL INPUT MESSAGE
C     MESIND            - INDEX INTO MESTAB OF WHERE TO START FILLING UP BUFFER
C     TNUM              - TERMINAL NUMBER (If -1 does not apply!)
C     GNUM              - GAME NUMBER TO GET OPTIONS FOR.
C     PARTIAL_MSG       - FLAG TO INDICATE WHETER COMPLETE MSG. IS TO BE DONE
C                         OR PARTIAL (DOES NOT FIT IN SON, SEE SON.FCC)
C
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     MESIND - INDEX INTO MESTAB WHERE NEXT ROUTINE CAN CONTINUE TO FILL BUFFER
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE GAME_OPTIONS(MESTAB,MESIND,TNUM,GNUM,PARTIAL_MSG)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SPECOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:NBRCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:TSLCOM.DEF'
        INCLUDE 'INCLIB:WITCOM.DEF'
        INCLUDE 'INCLIB:SCRCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:DBLCOM.DEF'
        INCLUDE 'INCLIB:CPLCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:SSCCOM.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:STRCOM.DEF'
C
C
C INPUT / OUTPUT VARIABLE DECLARATIONS
C
        BYTE      MESTAB(*)             !Terminal Message Table.
C
        INTEGER*4 MESIND                !Index into Message Table.
        INTEGER*4 TNUM                  !Terminal Number we are processing.
        INTEGER*4 GNUM                  !Game Number we are processing. 
C
        LOGICAL   PARTIAL_MSG           !Build partial msg flag.
C
C GENERAL VARIABLE DECLARATIONS
C
        BYTE      MDSTAB(MAXMLTD_AVL)   !Multi-Draw Selected table.
C
        INTEGER*4 OPTIONS               !Option Bytes hold variable.
        INTEGER*4 GTYP                  !Game Type to process.
        INTEGER*4 GIND                  !Game Index to process.
        INTEGER*4 KGNUM                 !Kicker Game Number.
        INTEGER*4 KGIND                 !Kicker Game Index.
        INTEGER*4 GAMSTS                !Game Status.
        INTEGER*4 GAMREV                !Game Control and Text revision numbers.
        INTEGER*4 MLTDRW                !Multi draw (max draw number).
        INTEGER*4 BPRICE                !Game base price.
        INTEGER*4 BETLIM                !Bet limit. (TTSL,TV65=Amount,
C                                       !            TSPT,TPPP=Row Maximum.)
        INTEGER*4 J, I                  !General Loop Variable.
        INTEGER*4 DRWCDC                !CDC date when Draw will take place.
        INTEGER*4 SALEND_DAT            !Sales close date
        INTEGER*4 SALEND_TIM            !Sales close time
        INTEGER*4 CPLEVD_DAT(2)         !Couple subevent occurrance dates
        INTEGER*4 TRPSSC_DAT(3)         !Triple/SScore subevent occurrance dates
        INTEGER*4 TRPSSC_TIM(3)         !Triple/SScore subevent occurrance times
        INTEGER*4 MAXROW                !System size limit (TSSC)
        INTEGER*4 SYSBET                !System bet amount limit (TSSC,TTRP)
C
C INDICATOR FLAGS.
C
        LOGICAL   KICK_FLAG             !Kicker Active with game flag.
C
C GAME OPTION FLAGS (2 BYTES)
C
        INTEGER*2 KICK_OPT    /Z8000/   !Kicker Acvtive for this Game option.
        INTEGER*2 KICKFRE_OPT /Z4000/   !Double Kicker Free option.
        INTEGER*2 GAMNAC_OPT  /Z2000/   !Game Not currently Active option.
        INTEGER*2 SALEND_OPT  /Z0400/   !Sales end date and time
        INTEGER*2 CPLEVD_OPT  /Z0200/   !Couple/Triple/Superscore subevent 
                                        !occurrance dates
        INTEGER*2 MAXROW_OPT  /Z0100/   !Maximum number of rows option
        INTEGER*2 SYSBET_OPT  /Z0080/   !Maximum system bet limit option
        INTEGER*2 BETLIM_OPT  /Z0040/   !Bet Limit option.
        INTEGER*2 FRAC_OPT    /Z0020/   !Mamimum number of Fractions option.
        INTEGER*2 MDS_OPT     /Z0010/   !Multi-Draw Selected bitmap option.
        INTEGER*2 TKTC_OPT    /Z0008/   !Ticket Charge option.
        INTEGER*2 CTRL_OPT    /Z0004/   !Control Revision option.
        INTEGER*2 TEXT_OPT    /Z0002/   !Text Reevision option.
        INTEGER*2 TTXT_OPT    /Z0001/   !Ticket Text Revision option.
C
C TIME VARIABLES
C
        INTEGER*4 HH,MM,SS              !hours, minutes, seconds
C
C TEMP. WORK VARIABLES
C
        BYTE      I1TEMP(4)
        INTEGER*2 I2TEMP(2)
        INTEGER*4 I4TEMP
        EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)

C
C GET GAME TYPE AND INDEX AND CHECK BOUNDARYS
C
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) RETURN
        GTYP = GNTTAB(GAMTYP,GNUM)
        GIND = GNTTAB(GAMIDX,GNUM)
        IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) RETURN
        IF(GIND.LT.1.OR.GIND.GT.MAXIND) RETURN
C
C
C GET ALL INFO. NEEDED ON GAME TYPE BASIS
C
        BPRICE = 0                       !Base price
        MLTDRW = 0                       !Multi draw (max draw number)
        GAMSTS = 0                       !Game status
        GAMREV = 0                       !Game control and revision #.
C
        IF(GTYP.EQ.TLTO)    THEN
           BPRICE = LTOPRC(GIND)
           MLTDRW = LTOMLT(GIND)
           GAMSTS = LTOSTS(GIND)
           GAMREV = LTOREV(GIND)
           CALL MOVBYT(LTOMDS(1,GIND),1,MDSTAB,1,MAXMLTD_AVL)
           DRWCDC = LTODAT(CURDRW,GIND)
        ELSEIF(GTYP.EQ.TSPT) THEN
           BPRICE = SPTPRC(GIND)
           MLTDRW = SPTMLT(GIND)
           GAMSTS = SPTSTS(GIND)
           GAMREV = SPTREV(GIND)
           BETLIM = P(MAXSPT)
           CALL MOVBYT(SPTMDS(1,GIND),1,MDSTAB,1,MAXMLTD_AVL)
           DRWCDC = SPTDAT(CURDRW,GIND)
        ELSEIF(GTYP.EQ.TTGL) THEN
           BPRICE = TGLPRC(GIND)
           MLTDRW = TGLMLT(GIND)
           GAMSTS = TGLSTS(GIND)
           GAMREV = TGLREV(GIND)
           BETLIM = P(MAXTGL)
           CALL MOVBYT(TGLMDS(1,GIND),1,MDSTAB,1,MAXMLTD_AVL)
           DRWCDC = TGLDAT(CURDRW,GIND)
        ELSEIF(GTYP.EQ.TNBR) THEN
           BPRICE = NBRPRC(GIND)
           MLTDRW = NBRMLT(GIND)
           GAMSTS = NBRSTS(GIND)
           GAMREV = NBRREV(GIND)
           DRWCDC = NBRDAT(CURDRW,GIND)
        ELSEIF(GTYP.EQ.TKIK) THEN
           BPRICE = KIKPRC(GIND)
           MLTDRW = KIKMLT(GIND)
           GAMSTS = KIKSTS(GIND)
           GAMREV = KIKREV(GIND)
           CALL MOVBYT(KIKMDS(1,GIND),1,MDSTAB,1,MAXMLTD_AVL)
           DRWCDC = KIKDAT(CURDRW,GIND)
           SYSBET = P(MAXKSYS) 
        ELSEIF(GTYP.EQ.TTSL) THEN
           BPRICE = TSLPRC(GIND)
           GAMSTS = TSLSTS(GIND)
           GAMREV = TSLREV(GIND)
           BETLIM = P(TSWMAX)
           CALL MOVBYT(TSLMDS(1,GIND),1,MDSTAB,1,MAXMLTD_AVL)
           DRWCDC = TSLDTE(GIND)
        ELSEIF(GTYP.EQ.TWIT) THEN
           BPRICE = WITPRC(GIND)
           GAMSTS = WITSTS(GIND)
           GAMREV = WITREV(GIND)
           CALL MOVBYT(WITMDS(1,GIND),1,MDSTAB,1,MAXMLTD_AVL)
           DRWCDC = WITDAT(GIND)
           IF(BPRICE.NE.0) THEN
              BETLIM = P(MAXWIT)
           ELSE
              BETLIM = 1
           ENDIF
        ELSEIF(GTYP.EQ.TSCR) THEN
           BPRICE = SCRPRC(GIND)
           GAMSTS = SCRSTS(GIND)
           GAMREV = SCRREV(GIND)
           CALL MOVBYT(SCRMDS(1,GIND),1,MDSTAB,1,MAXMLTD_AVL)
           DRWCDC = SCRDAT(GIND)
           SYSBET = P(MAXSCRS) 
           IF(BPRICE.NE.0) THEN
              BETLIM = P(MAXSCR)
           ELSE
              BETLIM = 1
           ENDIF
        ELSEIF(GTYP.EQ.TBNG) THEN
           BPRICE = BNGPRC(GIND)
           GAMSTS = BNGSTS(GIND)
           GAMREV = BNGREV(GIND)
           CALL MOVBYT(BNGMDS(1,GIND),1,MDSTAB,1,MAXMLTD_AVL)
           DRWCDC = BNGDAT(CURDRW,GIND)
        ELSEIF(GTYP.EQ.TDBL) THEN
           BPRICE = DBLPRC(GIND)
           GAMSTS = DBLSTS(GIND)
           GAMREV = DBLREV(GIND)
           CALL MOVBYT(DBLMDS(1,GIND),1,MDSTAB,1,MAXMLTD_AVL)
           DRWCDC = DBLDAT(GIND)
           SALEND_DAT = DBLESD(GIND)
           SALEND_TIM = DBLTIM(GIND)
           IF(BPRICE.NE.0) THEN
              BETLIM = P(MAXDBL)
           ELSE
              BETLIM = 1
           ENDIF
           SYSBET = P(MAXDBLS) 
        ELSEIF(GTYP.EQ.TCPL) THEN
           BPRICE = CPLPRC(GIND)
           GAMSTS = CPLSTS(GIND)
           GAMREV = CPLREV(GIND)
           CALL MOVBYT(CPLMDS(1,GIND),1,MDSTAB,1,MAXMLTD_AVL)
           DRWCDC = CPLDAT(GIND)
           SALEND_DAT = CPLESD(GIND)
           SALEND_TIM = CPLTIM(GIND)
           CPLEVD_DAT(1) = CPLEVD(1,GIND)
           CPLEVD_DAT(2) = CPLEVD(2,GIND)
           IF(BPRICE.NE.0) THEN
              BETLIM = P(MAXCPL)
           ELSE
              BETLIM = 1
           ENDIF
           SYSBET = P(MAXCPLS) 
        ELSEIF(GTYP.EQ.TSSC) THEN
           BPRICE = SSCPRC(GIND)
           GAMSTS = SSCSTS(GIND)
           GAMREV = SSCREV(GIND)
           DRWCDC = SSCDAT(GIND)
           SALEND_DAT = SSCESD(GIND)
           SALEND_TIM = SSCTIM(GIND)
           DO I=1,3 
              TRPSSC_DAT(I) = SSCECD(I,GIND)
              TRPSSC_TIM(I) = SSCECT(I,GIND)
           ENDDO
           IF(BPRICE.NE.0) THEN
              BETLIM = P(MAXSSC)
           ELSE
              BETLIM = 1
           ENDIF
           MAXROW = P(MAXSSN) 
           SYSBET = P(MAXSSCS) 
        ELSEIF(GTYP.EQ.TTRP) THEN
           BPRICE = TRPPRC(GIND)
           GAMSTS = TRPSTS(GIND)
           GAMREV = TRPREV(GIND)
           DRWCDC = TRPDAT(GIND)
           SALEND_DAT = TRPESD(GIND)
           SALEND_TIM = TRPTIM(GIND)
           DO I=1,3 
              TRPSSC_DAT(I) = TRPEVD(I,GIND)
              TRPSSC_TIM(I) = TRPEVT(I,GIND)
           ENDDO
           IF(BPRICE.NE.0) THEN
              BETLIM = P(MAXTRP)
           ELSE
              BETLIM = 1
           ENDIF
           SYSBET = P(MAXTRPS) 
        ELSEIF(GTYP.EQ.TSTR) THEN
           BPRICE = STRPRC(GIND)
           GAMSTS = STRSTS(GIND)
           GAMREV = STRREV(GIND)
           CALL MOVBYT(STRMDS(1,GIND),1,MDSTAB,1,MAXMLTD_AVL)
           DRWCDC = STRDAT(GIND)
           SALEND_DAT = STRESD(GIND)
           SALEND_TIM = STRTIM(GIND)
           IF(BPRICE.NE.0) THEN
              BETLIM = P(MAXSTR)
           ELSE
              BETLIM = 1
           ENDIF
           SYSBET = P(MAXSTRS) 
        ENDIF
C
C IF AGENT RAM SET REQUESTED CLEAR GAME REVISION.
C (IF AGENT STUFF DOES NOT APLY, TNUM = -1).
C
        IF(TNUM.GT.0) THEN
	  IF(TSBIT(AGTTAB(AGTTYP,TNUM),AGTRAM)) GAMREV = 0
        ENDIF
C
C SET GAME TYPE AND GAME INDEX FIELDS
C
        IF(PARTIAL_MSG) THEN
           MESTAB(MESIND) = ISHFT(GTYP,3) + GIND
           MESIND = MESIND + 1
        ELSE
           MESTAB(MESIND) = GTYP
           MESTAB(MESIND+1) = GIND
           MESIND = MESIND + 2
        ENDIF
C
C SET GAME OPTION FLAGS
C
        OPTIONS = 0
        KICK_FLAG = .FALSE.
C
C SET JOKER GAME ACTIVE FOR THIS GAME FLAG
C P(EUMJOKER)
        KGNUM=KGNTAB(GNUM)
        IF(KGNUM.NE.0) KGIND = GNTTAB(GAMIDX,KGNUM)
        
        IF(GTYP .EQ. TKIK) THEN
          IF(P(EUMJOKER) .EQ. 1) THEN   ! V25 P(EUMJOKER)=1 ALLOWS EM JOKER TO BE PALYED AS STANDALONE ONE
            OPTIONS = OPTIONS + KICK_OPT
          ENDIF
        ELSE
          IF(KGNUM.GE.1.AND.KGNUM.LE.MAXGAM) THEN
            OPTIONS = OPTIONS + KICK_OPT
          ENDIF
        ENDIF
C
C PORTUGAL ONLY HAVE ONE JOKER NUMBER ( KICK_FLAG MUST BE FALSE )
C
C          KICK_FLAG = .TRUE.
C        ENDIF
C
C SET DOUBLE JOKERI FREE FLAG
C
        IF(KICK_FLAG) THEN
           IF(KIKDFF(KGIND).NE.0) OPTIONS = OPTIONS + KICKFRE_OPT
	ENDIF
C
C SET DRAW ALREADY HELD FLAG
C
        IF(GAMSTS.GE.GAMBFD)   OPTIONS = OPTIONS + GAMNAC_OPT
C
C SET SALES CLOSE DATE AND TIME  FLAG
C
        IF(GTYP.EQ.TDBL.OR.GTYP.EQ.TCPL.OR.
     *     GTYP.EQ.TSSC.OR.GTYP.EQ.TTRP.OR.
     *     GTYP.EQ.TSTR) THEN
          OPTIONS = OPTIONS + SALEND_OPT
        ENDIF
C
C SET COUPLE SUBEVENT OCCURRANCE DATE FLAG
C
        IF(GTYP.EQ.TCPL.OR.GTYP.EQ.TTRP.OR.GTYP.EQ.TSSC) THEN
          OPTIONS = OPTIONS + CPLEVD_OPT
        ENDIF
C
C SET MAXIMUM SYSTEM SIZE LIMIT
C
        IF(GTYP.EQ.TSSC) THEN
          OPTIONS = OPTIONS + MAXROW_OPT
        ENDIF
C
C
C SET MAXIMUM SYSTEM BET AMOUNT LIMIT
C
C ( PORTUGAL DON'T HAVE SYSTEM LIMIT AMOUNT FOR JOKER, REMOVED TKIK )
C
        IF(GTYP.EQ.TSSC.OR.GTYP.EQ.TTRP.OR.
     *     GTYP.EQ.TSCR.OR.GTYP.EQ.TCPL.OR.GTYP.EQ.TDBL.OR.
     *     GTYP.EQ.TSTR) THEN
          OPTIONS = OPTIONS + SYSBET_OPT
        ENDIF
C
C SET BET LIMIT FLAG
C
        IF(GTYP.EQ.TSPT.OR.GTYP.EQ.TTGL.OR.
     *     GTYP.EQ.TTSL.OR.GTYP.EQ.TDBL.OR.GTYP.EQ.TCPL.OR.
     *     GTYP.EQ.TWIT.OR.GTYP.EQ.TTRP.OR.GTYP.EQ.TSSC.OR.
     *     GTYP.EQ.TSTR) THEN
          OPTIONS = OPTIONS + BETLIM_OPT
        ENDIF
C
C MAX. FRACTIONS PRESENT.
C
        IF(MAXFRC(GNUM).NE.0) OPTIONS = OPTIONS + FRAC_OPT
C
C SET MULTI-DRAW SELECT TABLE FLAG (MAX DRAWS PRESENT)
C
        IF(MLTDRW.GT.1) OPTIONS = OPTIONS + MDS_OPT
C
C TICKET CHARGE PRESENT
C
        IF(TKTCHG(GNUM).NE.0) OPTIONS = OPTIONS + TKTC_OPT
C
C SET CONTROL REVISION PRESENT FLAG (NO ONE SHOULD BE WITHOUT ONE.....)
C
        OPTIONS = OPTIONS + CTRL_OPT
C
C SET TEXT REVISION PRESENT FLAG
C
        IF(GTYP.EQ.TTSL.OR.GTYP.EQ.TWIT.OR.
     *     GTYP.EQ.TSCR.OR.GTYP.EQ.TSPT.OR.
     *     GTYP.EQ.TDBL.OR.GTYP.EQ.TCPL.OR.
     *     GTYP.EQ.TSSC.OR.GTYP.EQ.TTRP.OR.
     *     GTYP.EQ.TSTR.Or.GTYP.EQ.TTGL) THEN
          OPTIONS = OPTIONS + TEXT_OPT
        ENDIF
C
C SET TICKET MESSAGE FLAG
C
        IF(TNUM.GT.0) THEN
           IF(TSBIT(AGTTAB(AGTTYP,TNUM),AGTTKM).AND.TKTMLN(GNUM).NE.0) THEN
             OPTIONS = OPTIONS + TTXT_OPT
           ENDIF
        ENDIF
C
C SET BINGO OPTIONS (LUCKY NUMBER ACTIVE)
C
C***    IF(GTYP.EQ.TBNG) THEN
C***      OPTIONS = OPTIONS + BLNACT_OPT  ! Lucky number NOT active 
C***    ENDIF
C
C NOTE!!
C IF PARTIAL MESSAGE WAS REQUESTED CLEAR OPTION BIT FLAGS THAT DOES NOT APPLY
C TO THE PARTIAL MESSAGE. 
C
        IF(PARTIAL_MSG) THEN
          OPTIONS = IAND(OPTIONS, (CTRL_OPT + TEXT_OPT + TTXT_OPT))
        ELSE
          OPTIONS = IAND(OPTIONS,('FFFF'X - TEXT_OPT))
          OPTIONS = IAND(OPTIONS,('FFFF'X - TTXT_OPT))
C
C    DRAW NUMBER / DRAW CDC DATE
C
           I4TEMP = DRWCDC
           MESTAB(MESIND+0) = I1TEMP(2)
           MESTAB(MESIND+1) = I1TEMP(1)
           MESIND = MESIND + 2
C
C    BASE PRICE
C
           I4TEMP = BPRICE / P(PRFACTOR)
           MESTAB(MESIND+0) = I1TEMP(2)
           MESTAB(MESIND+1) = I1TEMP(1)
           MESIND = MESIND + 2
        ENDIF
C
C OPTION BYTES
C
        I4TEMP = OPTIONS
        MESTAB(MESIND+0) = I1TEMP(2)
        MESTAB(MESIND+1) = I1TEMP(1)
        MESIND = MESIND + 2
C
C SET OPTIONAL FIELDS
C
C
C SET SALES END CLOSE DATE AND TIME
C
        IF(IAND(OPTIONS,SALEND_OPT).NE.0) THEN
           I4TEMP = SALEND_DAT
           MESTAB(MESIND+0) = I1TEMP(2)
           MESTAB(MESIND+1) = I1TEMP(1)
           MESIND = MESIND + 2
           IF(SALEND_TIM.GT.'40000000'X) SALEND_TIM=SALEND_TIM-'40000000'X
           HH = SALEND_TIM/3600
           MESTAB(MESIND+0) = HH  !hours
           SALEND_TIM = SALEND_TIM - (HH*3600)
           MM = SALEND_TIM/60
           MESTAB(MESIND+1) = MM  !mins
           SS = SALEND_TIM - (MM*60)
           MESTAB(MESIND+2) = SS  !seconds
           MESIND = MESIND + 3
        ENDIF
C
C SET COUPLE ,TRIPLE AND SUPERSCORE SUBEVENT OCCURRANCE DATES/TIMES
C
        IF(IAND(OPTIONS,CPLEVD_OPT).NE.0) THEN
           IF(GTYP.EQ.TCPL) THEN
              I4TEMP = CPLEVD_DAT(1)
              MESTAB(MESIND+0) = I1TEMP(2)
              MESTAB(MESIND+1) = I1TEMP(1)
              MESIND = MESIND + 2
              I4TEMP = CPLEVD_DAT(2)
              MESTAB(MESIND+0) = I1TEMP(2)
              MESTAB(MESIND+1) = I1TEMP(1)
              MESIND = MESIND + 2
           ELSEIF(GTYP.EQ.TSSC.OR.GTYP.EQ.TTRP) THEN
              DO I=1,3
                 I4TEMP = TRPSSC_DAT(I)
                 MESTAB(MESIND+0) = I1TEMP(2)
                 MESTAB(MESIND+1) = I1TEMP(1)
                 MESIND = MESIND + 2
                 I4TEMP = TRPSSC_TIM(I)
                 HH = I4TEMP/3600
                 I4TEMP = I4TEMP - HH*3600
                 MM = I4TEMP/60
                 SS = I4TEMP - MM*60
                 MESTAB(MESIND+0) = HH
                 MESTAB(MESIND+1) = MM
                 MESTAB(MESIND+2) = SS
                 MESIND = MESIND + 3
              ENDDO     
           ENDIF  
        ENDIF
C
C SET MAXIMUM SYSTEM SIZE LIMIT
C
        IF(IAND(OPTIONS,MAXROW_OPT).NE.0) THEN
           I4TEMP  = MAXROW
           MESTAB(MESIND+0) = I1TEMP(4)
           MESTAB(MESIND+1) = I1TEMP(3)
           MESTAB(MESIND+2) = I1TEMP(2)
           MESTAB(MESIND+3) = I1TEMP(1)
           MESIND = MESIND + 4
        ENDIF
C
C SET MAXIMUM SYSTEM BETE LIMIT
C
        IF(IAND(OPTIONS,SYSBET_OPT).NE.0) THEN
           I4TEMP  = SYSBET
           MESTAB(MESIND+0) = I1TEMP(4)
           MESTAB(MESIND+1) = I1TEMP(3)
           MESTAB(MESIND+2) = I1TEMP(2)
           MESTAB(MESIND+3) = I1TEMP(1)
           MESIND = MESIND + 4
        ENDIF
C
C SET MAXIMUM BET LIMIT
C
        IF(IAND(OPTIONS,BETLIM_OPT).NE.0) THEN
           I4TEMP = BETLIM
           MESTAB(MESIND+0) = I1TEMP(4)
           MESTAB(MESIND+1) = I1TEMP(3)
           MESTAB(MESIND+2) = I1TEMP(2)
           MESTAB(MESIND+3) = I1TEMP(1)
           MESIND = MESIND + 4
        ENDIF
C
C SET MAX NUMBER OF FRACTIONS
C
        IF(IAND(OPTIONS,FRAC_OPT).NE.0) THEN
           MESTAB(MESIND) = MAXFRC(GNUM)
           MESIND = MESIND + 1
        ENDIF
C
C SET MAX NUMBER OF DRAWS
C
        IF(IAND(OPTIONS,MDS_OPT).NE.0) THEN
           DO J = 0,6                              !Clear Mestab range
              MESTAB(MESIND+J) = 0
           END DO
           DO J = 1,MAXMLTD_AVL
              IF(MDSTAB(J).NE.0) THEN
                 CALL SETBIT_BSTRNG(MESTAB(MESIND),J-1)
              ELSE
                 CALL CLRBIT_BSTRNG(MESTAB(MESIND),J-1)
              ENDIF
           END DO
           MESIND = MESIND + 7                     !52 BITS = 7 BYTES
        ENDIF
C
C SET TICKET CHARGE
C
        IF(IAND(OPTIONS,TKTC_OPT).NE.0) THEN
           I4TEMP = TKTCHG(GNUM)
           MESTAB(MESIND+0) = I1TEMP(2)
           MESTAB(MESIND+1) = I1TEMP(1)
           MESIND = MESIND + 2
        ENDIF
C
C SET CONTROL REVISION IF BIT WAS SET
C
        IF(IAND(OPTIONS,CTRL_OPT).NE.0) THEN
           I4TEMP = GAMREV
           MESTAB(MESIND+0)=I1TEMP(1)
           MESTAB(MESIND+1)=I1TEMP(2)
           MESIND = MESIND + 2
        ENDIF
C
C SET TEXT REVISION IF BIT WAS SET
C
        IF(IAND(OPTIONS,TEXT_OPT).NE.0) THEN
           I4TEMP = GAMREV
           MESTAB(MESIND+0)=I1TEMP(3)
           MESTAB(MESIND+1)=I1TEMP(4)
           MESIND = MESIND + 2
        ENDIF
C
C SET TICKET TEXT REVISION
C
        IF(IAND(OPTIONS,TTXT_OPT).NE.0) THEN
           I4TEMP = TKTMRV(GNUM)
           MESTAB(MESIND+0) = I1TEMP(1)
           MESTAB(MESIND+1) = I1TEMP(2)
           MESIND = MESIND + 2
        ENDIF
C
C
        RETURN
        END

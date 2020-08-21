C GAMEREP.FOR
C 
C SUBROUTINE GAMEREP
C
C V66 08-AUG-2017 SCML Hard-code Joker shares div 1, 2 and 3 for the last Joker draw 843.
C V65 30-MAR-2017 MTK  Modified Super 14 game
C V64 19-JAN-2016 FRP  CR43&CR44 Add winning sequence numbers (algarismos) for Lotaria Nacional
C V63 30-NOV-2010 HXK  LOTO2 Changes: Use draw id (DDD/YY) instead of 
C                      week (WW/YY) to get draw
C                 FJG  Out of Bounds issue
C V62 30-OCT-2003 FRP  Modify for Batch2 Totobola Changes.
C V61 26-JAN-2001 CS   INCLUDED PASSIVE FOR PORTUGAL
C V60 29-NOV-2000 UXN  TTGL ADDED.
C V59 08-JUN-2000 UXN  1521 label added, code cleaned up.
C V58 13-APR-2000 UXN  Fix for Bingo
C V57 10-MAR-2000 OXK  Kicker draw # hardcoded for Vakio 1 (for Sunday sales)
C V56 08-MAR-2000 RXK  Bingo numbers for first hits not sent.
C V55 03-MAR-2000 OXK  Vakio changes
C V54 28-FEB-2000 RXK  Bingo part changed (5 subphases, old AB bingo not sent).
C V53 13-OCT-1999 RXK  World Tour game added. 
C V52 11-OCT-1999 UXN  Fix for Lotto bonus draw bitmap.
C V51 14-JUL-1999 UXN  Fix for Voittaja, Paivan Pari team name length.
C V50 14-MAY-1999 UXN  Super Triple added. Todays Couple, Super Double 
C                      and Todays Trio changed.
C V49 17-MAR-1999 RXK  Game type in separate byte, hack for V5 removed.
C V48 15-FEB-1999 UXN  DPPREC_M added.
C V47 12-JAN-1999 UXN  PeliSuomi changes.
C V46 18-SEP-1998 RXK  Lotto Bonus draw results hardcoded (divisions in DLTWTB
C                      table are not set for bonus draw)
C V45 29-JAN-1998 UXN  Super Score and Todays Triple added.
C V44 23-AUG-1996 RXK  Bug fixed (the case of cancelled Voittaja and Paivan 
C                      Pari events)
C V43 15-MAR-1996 HXK  Putting Rita's fixes (RXK) into PVCS archive
C V42 08-MAR-1996 RXK  Fix for Pitka 2 lists
C V41 15-FEB-1996 HXK  Fix for Ravi draw and default draw for oddset games
C V40 29-JAN-1996 HXK  Debug of Pitka / Double / Couple game reports
C V39 12-JAN-1996 HXK  Fix for PITKA
C V38 09-JAN-1996 HXK  Added PITKA change: send down two draws of info, 
C                      if active
C V37 23-NOV-1995 PXB  Couple and Double games added
C V36 14-SEP-1995 HXK  Handle Ravi draw differently
C V35 17-AUG-1995 HXK  Further fix for Kicker
C V34 16-AUG-1995 HXK  Fix for showing Kicker shares
C V33 24-APR-1995 HXK  Merge of V5 development with March 10th 1995 bible
C V32 22-FEB-1995 HXK  HACK FOR V5 
C V31 06-JAN-1995 HXK  Fix for Christmas week changes, 1994
C V30 23-NOV-1994 HXK  Fixed GAMEREP for Bingo
C V29 17-OCT-1994 HXK  removed bingos's dependency on LBNMAT table
C V28 15-OCT-1994 HXK  Adding /developing Bingo (15.Oct.94)
C V27 16-JAN-1994 HXK  CHANGED TEAM NAME LENGTHS TO TERMINAL MAX.
C V26 13-JAN-1994 HXK  CHANGED TEAM NAME LENGTHS TO 14.
C V25 12-JAN-1994 HXK  SET #WINNING NUMBERS FOR SCORE TO 1.
C V24 22-DEC-1993 HXK  ACCEPT ACTUAL DRAW DATE FOR TSCR, TWIT GAMES.
C V23 08-NOV-1993 HXK  FIXED BUG IN KLUDGE.
C V22 08-NOV-1993 HXK  PUT IN KLUDGE FOR SHARES.
C V21 05-NOV-1993 HXK  FIX FOR RAVI, TO SHOW MATCH 4'S.
C V20 01-NOV-1993 HXK  REARRANGED GAME STATUS FLAGS.
C V19 29-OCT-1993 HXK  CHANGE GAME STATE FOR TERMINAL DUDES!
C V18 21-OCT-1993 GXA  Added row close and row refunded message to Toto Select.
C V17 11-OCT-1993 GXA  Removed Type Statements.
C V16 04-OCT-1993 GXA  Fixed Ravi winning rows and scratches.
C V15 01-OCT-1993 GXA  Decode week# and Year from input message and use them 
C                      for old draws.
C V14 29-SEP-1993 GXA  Cleared I4TEMP befor setting Option Flags (do not carry 
C                      over other games) for Sports, Spede and Ravi.
C V13 23-SEP-1993 HXK  Removed Year (as terminal already calculates it from 
C                      cdc date)
C V12 14-SEP-1993 SXH  Added Division info. for Joker
C V11 07-AUG-1993 HXK  FIXED SHARE BITMAP BUG IN TOTOSELECT
C V10 17-JUL-1993 CXK  Changed check of GNUM from LT.0 to LT.1 . 
C V09 17-JUL-1993 CXK  Check boundary of GNUM before use.
C V08 28-JUN-1993 HXK  changed err message length from 5 to 6
C V07 10-JUN-1993 HXK  Initial release for Finland VAX conversion
C V06 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                      DEC Baseline
C V05 25-AUG-1992 GCAN NO KICKER RESULTS FOR TOTO SELECT AND RESULT
C                      REQUEST FOR CURRENT  COULD BE VALID.
C V04 05-AUG-1992 GCAN ADDED FILED LENGTHS TO SCORE, WINTIP MSG. FORMATS.
C V03 23-APR-1992 GCAN USE DAYHDR TO DETERMINE IF RESULTS EXISTS OR NOT.
C                      CHECK LAST DRAW REQUESTED BY GAME TO MINIMIZE FILE 
C                      ACCESS.
C V02 13-NOV-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C CALLING SEQUENCE:
C     CALL GAMEREP(TRABUF,MESTAB,OUTLEN)
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     OUTLEN - OUTPUT MESSAGE LENGTH
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
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE GAMEREP(TRABUF,MESTAB,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:LLTREC.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:LSPREC.DEF'
        INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:LTGREC.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:LKKREC.DEF'
        INCLUDE 'INCLIB:NBRCOM.DEF'
        INCLUDE 'INCLIB:LNBREC.DEF'
        INCLUDE 'INCLIB:TSLCOM.DEF'
        INCLUDE 'INCLIB:LTSREC.DEF'
        INCLUDE 'INCLIB:SCRCOM.DEF'
        INCLUDE 'INCLIB:LSCREC.DEF'
        INCLUDE 'INCLIB:WITCOM.DEF'
        INCLUDE 'INCLIB:LWIREC.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:LBNREC.DEF'
        INCLUDE 'INCLIB:DBLCOM.DEF'
        INCLUDE 'INCLIB:LDBREC.DEF'
        INCLUDE 'INCLIB:CPLCOM.DEF'
        INCLUDE 'INCLIB:LCPREC.DEF'
        INCLUDE 'INCLIB:SSCCOM.DEF'
        INCLUDE 'INCLIB:LSSREC.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:LTRREC.DEF'
        INCLUDE 'INCLIB:STRCOM.DEF'
        INCLUDE 'INCLIB:LSTREC.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SPECOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:PRZCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:LPAREC.DEF'
        INCLUDE 'INCLIB:TTSREC.DEF'   !used for second Pitka draw  
        INCLUDE 'INCLIB:PASNAM.DEF'

C

        ! arguments
        INTEGER*2   OUTLEN                  !
        BYTE        MESTAB(*)               !
C
        INTEGER*4   ROWCLS,ROWREF
        PARAMETER   (ROWREF = 5)            !Toto Select Row Refunded.
        PARAMETER   (ROWCLS = 6)            !Toto Select Row close
C                                           !!!This should be moved to PRMTSL
        INTEGER*4   MAXSENT                 !Max number divisions sent
        PARAMETER   (MAXSENT=16)      
C                                     
        ! variables
        INTEGER*4   I4TEMP                  !
        INTEGER*4   MESS(EDLEN)             !
        INTEGER*4   MYCHKSUM                !
        INTEGER*4   CHKLEN                  !
        INTEGER*4   WINNER                  !
        INTEGER*4   WIN1                    !
        INTEGER*4   WIN2                    !
        INTEGER*4   TOT_WIN                 !
        INTEGER*4   LEN                     !
        INTEGER*4   I                       !
        INTEGER*4   J,K,L
        INTEGER*4   IND                     !
        INTEGER*4   KDRAW                   !
        INTEGER*4   DRAW                    !
        INTEGER*4   GNUM                    !
        INTEGER*4   YEAR                    !
        INTEGER*4   GIDX                    !
        INTEGER*4   GTYP                    !
	INTEGER*4   OGIDX                   !
        INTEGER*4   ERRTYP                  !
        INTEGER*4   STK                     !
        INTEGER*4   ST                      !
        INTEGER*4   KGNUM                   !
        INTEGER*4   MONTH                   !
        INTEGER*4   DAY                     !
        INTEGER*4   WEEK                    !
        INTEGER*4   CDC                     !
        INTEGER*4   YEAR2                   !
        INTEGER*4   BONDIV(LTGDIV)          !
        INTEGER*4   REGDIV(LTGDIV)          !
        INTEGER*4   ALLDIV(MAXSENT)         !
        INTEGER*4   FHDIV(12)               !#of Bingo FH divs of Lottery list
                                            !to match map 
        INTEGER*4   RW1,RW2,RW3
        INTEGER*4   KGIDX                   !
        INTEGER*4   LAST_DRAW(MAXGAM)          ! Last Draw Requested for game.

        INTEGER*4   NUM_ACTIVE_TTSL         ! number of used TTSL draws
        INTEGER*4   BEGDOW
        INTEGER*4   ENDDOW
	INTEGER*4   BCNT

        LOGICAL     SKIPIT                  !
        LOGICAL     LOGGAM_TTSREC           !

        BYTE        I1TEMP(4)               !

        INTEGER*2   I2TEMP(2)               !
        INTEGER*2   I2DATE(VDATE)           !
        INTEGER*4   DIVMAP                  !bingo divs map, points Lottery list
        INTEGER*4   FHLOTD(BGOLOT)          !active bingo divs from Lottery list
        INTEGER*4   LENGTH,LENX(3)
        INTEGER*4   FRST_WIN,LAST_WIN,NR_WIN_IND
        INTEGER*4   WINSET,OPTION,WINSET_CNT,NUMDIV
        INTEGER*4   NAME_LEN
        INTEGER*4   BSUBPH                  !number of bingo subphases in phase1
        INTEGER*4   SPH                     
        INTEGER*4   DIVBITMAP               ! BIT MAP OF DIVISIONS SEND
        INTEGER*4   SHRPOS                  ! SHARE POSITION
        INTEGER*4   ESTPRZPOS               ! ESTIMATED PRIZE POSITION
        INTEGER*4   DRAWCDC                 ! DRAW CDC
        INTEGER*4   SENDJOKDIV              ! EST. PRIZES ( JOKER ONLY DIV 1 )
C
        LOGICAL     JOKDIVSEND              ! JOKER SEND DIV ( JOKER DIV 1 )
C
        INTEGER*4   GETDRW                  ! FUNCTION ( DRAW - WEEK / YEAR )
        INTEGER*4   GETTOTOLOTOGAM          ! FUNCTION ( GAM  -  CCC / YEAR )

C
        INTEGER*4   INDPAS,
     *              INDEMI,
     *              NUMWIN,
     *              NUMOFWINS,          ! NUMBER OF PASSIVE WINNING NUMBERS
     *              DIVTYP,             ! DIVISION TYPE
     *              NUMDIG,             ! NUMBER OF DIGITS
     *              NUMWINPOS,          ! POSITION TO SEND NUMBER OF WINERS
     *              FIDX,               ! INDEX OF FATOR ARRAY
     *              PRZTYP,             ! PRIZE TYPE (PR_APRX,PR_CENT,PR_SEQ,PR_DIG)
     *              NUMOFBYTES          ! NUMBER OF BYTES TO SEND TO TERMINAL
C

        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)

        DATA        BONDIV/'8000'X,'2000'X,'0800'X,'0200'X,
     *                     '0080'X,'0020'X,'0008'X,'0002'X/
        DATA        REGDIV/'4000'X,'1000'X,'0400'X,'0100'X,
     *                     '0040'X,'0010'X,'0004'X,'0001'X/
        DATA        ALLDIV/'8000'X,'4000'X,'2000'X,'1000'X,
     *                     '0800'X,'0400'X,'0200'X,'0100'X,
     *                     '0080'X,'0040'X,'0020'X,'0010'X,
     *                     '0008'X,'0004'X,'0002'X,'0001'X/
        DATA        FHDIV /'8000'X,'4000'X,'2000'X,'1000'X,
     *                     '0800'X,'0400'X,'0200'X,'0100'X,
     *                     '0080'X,'0040'X,'0020'X,'0010'X/

C
        DATA        ERRTYP /Z90/
        DATA        LAST_DRAW/MAXGAM*0/
        INTEGER*4   SEGNO
C
C SET / CLEAR VARIABLES
C
        KGNUM = 0
        ST    = 0
        STK   = 0
        DAY   = 0
        WEEK  = 0
        MONTH = 0
        YEAR  = 0
	DRAW  = 0
C
C GET GAME REPORT OPTIONS
C
        GTYP  = ZEXT( MESTAB(5) )
        GIDX  = ZEXT( MESTAB(6) )
        IF(GTYP.EQ.TSCR.OR.GTYP.EQ.TWIT.OR.GTYP.EQ.TSSC) THEN
           DAY = ZEXT( MESTAB(9) )
           MONTH   = ZEXT( MESTAB(10) )
           YEAR  = ZEXT( MESTAB(11) )
        ELSEIF(GTYP.EQ.TTRP.OR.GTYP.EQ.TSTR.OR.
     *         GTYP.EQ.TCPL.OR.GTYP.EQ.TDBL) THEN
           DAY   = ZEXT( MESTAB(9) )
           MONTH = ZEXT( MESTAB(10))
           YEAR  = ZEXT( MESTAB(11))
           SEGNO = ZEXT( MESTAB(12))
        ELSE
           WEEK  = ZEXT( MESTAB(9) )   !also used for Lotto3, Lotto4 draw id
           YEAR  = ZEXT( MESTAB(10))
        ENDIF
C
        TRABUF(TSDT1)=GTYP
        TRABUF(TSDT2)=GIDX
C
C CHECK RANGE OF GAME TYPE AND GAME INDEX
C
        IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) THEN
           TRABUF(TERR)=SYNT
           GOTO 10000
        ENDIF
C
C CHECK PASSIVE GAME INDEX
C
        IF(GTYP.EQ.TPAS) THEN
          IF(GIDX.LT.1 .OR. GIDX.GT.NUMPAS) THEN
            TRABUF(TERR)=SYNT
            GOTO 10000
          ENDIF
        ELSEIF(GIDX.LT.1.OR.GIDX.GT.MAXIND) THEN
           TRABUF(TERR)=SYNT
           GOTO 10000
        ENDIF
	OGIDX = GIDX  !keep original game index
C
        GNUM=GTNTAB(GTYP,GIDX)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
           TRABUF(TERR)=SYNT
           GOTO 10000
        ENDIF
C
C DATE ENTERED  (DECODE TO A INTERNAL DRAW DATE)
C
	IF(WEEK .NE. 0 .OR. YEAR .NE. 0) THEN
          IF(WEEK .EQ. 0 .OR. YEAR .EQ. 0) THEN
            TRABUF(TERR) = INVL
            GOTO 10000
          ENDIF
	  IF(GTYP.EQ.TLTO.AND.GIDX.GT.2) THEN    !totoloto always request with
            GNUM = GETTOTOLOTOGAM(YEAR, WEEK)    !gind=3 but can be either  
            IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN !sabado(=3) or quarta(=4)
               TRABUF(TERR)=SYNT
               GOTO 10000
            ENDIF            
            GIDX = GNTTAB(GAMIDX,GNUM)           !if gnum changes then also
	  ENDIF                                  !change gidx
          DRAW = GETDRW (YEAR, WEEK, GNUM)
        ENDIF 
C
C CHECK KICKER GAME NUMBER
C
        IF(KGNTAB(GNUM).GT.0.AND.GTYP.NE.TTSL) THEN
           KGNUM = KGNTAB(GNUM)
           KGIDX = GNTTAB(GAMIDX,KGNUM)
        ENDIF
C
C CHECK IF ANY SALES REPORTS ARE SUPRESSED
C
        IF(TSBIT(P(SUPRPT),GAMREP)) THEN
           TRABUF(TERR)=SUPR
           GOTO 10000
        ENDIF
        IF(TSBIT(P(SUPRPT),GNUM)) THEN
           TRABUF(TERR)=SUPR
           GOTO 10000
        ENDIF
C
C GET LAST DRAW WE THINK WE HAVE RESULTS FOR.
C (FOR TOTO SELECT IT COULD BE THE CURRENT DRAW)
C
	IF(GTYP.NE.TLTO) THEN     !"Lotto2 change" batch change
	  IF(DRAW.EQ.0) THEN
            IF(DAYDRW(GNUM) .GT. 1 .AND. GTYP .NE. TTSL) THEN
              DRAW = DAYDRW(GNUM) - 1
            ELSE
              DRAW = DAYHDR(GNUM)
            ENDIF
	  ENDIF
	ELSE
	  IF(DRAW.EQ.0) THEN
	    IF(GTYP.EQ.TLTO.AND.GIDX.GT.2) THEN    !get current TOTOLOTO game
              CDC = DAYCDC                         !so get cdc first then
              CALL FIGCCC(CDC,WEEK,YEAR)           !get week/year, then get
              GNUM = GETTOTOLOTOGAM(YEAR,WEEK)     !current game ...
              GIDX = GNTTAB(GAMIDX,GNUM)           !... and game index,
              IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN !then check that game is
                TRABUF(TERR)=SYNT                  !in valid range, then ... 
                GOTO 10000
              ENDIF
	      IF(GIDX.EQ.3) THEN                   !switch to other TOTOLOTO
	        GNUM=7                             !as it will have result
	        GIDX=4
	      ELSE
	        GNUM=6
	        GIDX=3
	      ENDIF
              IF(DAYDRW(GNUM) .GT. 1) THEN
                DRAW = DAYDRW(GNUM) - 1
              ELSE
                DRAW = DAYHDR(GNUM)
              ENDIF
	    ELSE
              IF(DAYDRW(GNUM) .GT. 1) THEN
                DRAW = DAYDRW(GNUM) - 1
              ELSE
                DRAW = DAYHDR(GNUM)
              ENDIF
  	    ENDIF            
	  ELSEIF(DRAW.GE.DAYHDR(GNUM)) THEN
            IF(DAYDRW(GNUM) .GT. 1) THEN
              DRAW = DAYDRW(GNUM) - 1
             ELSE
              DRAW = DAYHDR(GNUM)
	    ENDIF
	    IF(GTYP.EQ.TLTO.AND.GIDX.GT.2) THEN  !get correct week,year
	      CALL GETWEK(DRAW,GNUM,WEEK,YEAR,ST)
	      IF(ST.NE.0) THEN
	        TRABUF(TERR)=SYNT
	        GOTO 10000
	      ENDIF
	    ENDIF
	  ENDIF
	ENDIF
C
        TRABUF(TSDT3)=GNUM
C
        KDRAW=0
        IF(KGNUM.GT.0.AND.GTYP.NE.TKIK) THEN
           IF(DAYDRW(KGNUM).GT.0) THEN
              KDRAW = DAYDRW(KGNUM) - 1
           ELSE
              KDRAW = DAYHDR(KGNUM)
           ENDIF
        ENDIF
C
        IF(GTYP .EQ. TKIK) KDRAW = DRAW
C
C CHECK IF RESULTS ARE IN OR IF REQUEST IS INVALID
C
        IF(DRAW.GT.DAYHDR(GNUM).AND.DAYHDR(GNUM).NE.0) THEN
           TRABUF(TERR) = RNIN
           GOTO 10000
        ENDIF
C
        IF(DAYHDR(GNUM).NE.0.AND.DRAW.EQ.0) THEN
           TRABUF(TERR) = RNIN
           GOTO 10000
        ENDIF
C
        IF(DRAW.LT.1) THEN
          TRABUF(TERR)=INVL
          GOTO 10000
        ENDIF
C
C CHECK FOR PASSIVE GAME ON COMMON AREA
C
        IF  (GTYP.EQ.TPAS) THEN
            IF(DRAW.LE.PAS_DRW_OFFSET) THEN
              TRABUF(TERR) = RNIN
              GOTO 10000
            ENDIF
C
            INDEMI = CURDRW
            DO  WHILE ( INDEMI.LE.PAGEMI            .AND.
     *                  DRAW.NE.PASEMIS(INDEMI,GIDX)     )
                INDEMI = INDEMI + 1
            ENDDO
C
C IF DRAW IS IN MEMORY, LOAD FROM MEMORY ELSE FORCE FIRST DRAW (LPAREC) 
C
            IF  (INDEMI.LE.PAGEMI) THEN
                CALL FASTMOV(PASSTS,LPASTS,LPARECLEN)
                GOTO 60
            ELSE
                INDEMI = 1
            ENDIF
        ELSE
C
C IF DRAW IS FOR TODAY MOVE COMMON INFO INTO GAME RECORD
C
           SKIPIT=.FALSE.
           IF(DRAW.EQ.DAYDRW(GNUM)) THEN
              IF(GTYP.NE.TKIK) SKIPIT=.TRUE.
              CALL FASTMOV(TSLSTS,LTSSTS,LTSRECLEN)
           ENDIF
C
           IF(SKIPIT) THEN
              CALL GAMLOG(TKIK,GIDX,DKKREC,KIKSTS)
              GOTO 60
           ENDIF
        ENDIF
C
C READ GAME FILE FOR REQUESTED DRAW
C
        IF(P(SUPFIL).EQ.1) THEN
           TRABUF(TERR)=SUPR
           GOTO 10000
        ENDIF
C
C CHECK AGAINST LAST DRAW TABLE, ONLY READ GAME FILE IF DIFFERENT.
C
        IF (GTYP.EQ.TSPT .OR. GTYP .EQ. TTGL) THEN
           KGNUM = KGNTAB(GNUM)
!          KGIDX = GNTTAB(GAMIDX, KGNUM) !FJG OOB
C          KDRAW = DRAW
C
	   IF(WEEK.EQ.0 .AND. YEAR.EQ.0) CALL GETWEK(DRAW,GNUM,WEEK,YEAR,STK)
           IF(KGNUM.GT.0) THEN
	     KDRAW = GETDRW (YEAR, WEEK, KGNUM)
	     IF(KDRAW .EQ. 0) THEN
               TRABUF(TERR)=INVL
               GOTO 10000
	     ENDIF
	   ENDIF
C
        ENDIF
        IF(KGNUM.GT.0) THEN
           KGIDX = GNTTAB(GAMIDX, KGNUM)
           IF(KDRAW.NE.LAST_DRAW(KGNUM)) THEN
              CALL READW(GAMFDB(1,KGNUM),KDRAW,DKKREC,STK)
              CALL LOGGAM(TKIK,KGIDX,DKKREC,LKKSTS)
C          ELSE                         !(HDB)
              LAST_DRAW(KGNUM) = KDRAW
           ENDIF
        ENDIF
C
C CHECK READ STATUS AND REPORT ANY ERROR TO THE CONSOLE (SECOND KICKER GAME)
C
        IF(STK.NE.0) THEN
           MESS(1)=SPE
           MESS(2)=TEGEN
           MESS(3)=4
           CALL FASTMOV(SFNAMES(1,KGNUM),MESS(4),5)
           MESS(9)=STK
           MESS(10)=KDRAW
           CALL QUEMES(MESS)
           TRABUF(TERR)=INVL
           GOTO 10000
        ENDIF
C
        IF(DRAW.EQ.LAST_DRAW(GNUM)) THEN
	  IF(KGNUM.GT.0) THEN
	    IF(KDRAW.EQ.LAST_DRAW(KGNUM)) GOTO 60
	  ELSE
	    GOTO 60
	  ENDIF
	ENDIF
C
C CHECK IF FILE IS OPEN (TO PREVENT CRASH)
C
        IF(DAYHDR(GNUM).LT.1) THEN
           TRABUF(TERR)=INVL
           GOTO 10000
        ENDIF
C
C READ GAME FILE BY GAME TYPE
C
        IF(GTYP.EQ.TLTO) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DLTREC,ST)
           CALL LOGGAM(TLTO,GIDX,DLTREC,LLTSTS)
        ELSEIF(GTYP.EQ.TSPT) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DSPREC,ST)
           CALL LOGGAM(TSPT,GIDX,DSPREC,LSPSTS)
        ELSEIF(GTYP.EQ.TTGL) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DTGREC,ST)
           CALL LOGGAM(TTGL,GIDX,DTGREC,LTGSTS)
        ELSEIF(GTYP.EQ.TKIK) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DKKREC,ST)
           CALL LOGGAM(TKIK,GIDX,DKKREC,LKKSTS)
        ELSEIF(GTYP.EQ.TNBR) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DNBREC,ST)
        ELSEIF(GTYP.EQ.TTSL) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DTSREC,ST)
           CALL LOGGAM(TTSL,GIDX,DTSREC,LTSSTS)
        ELSEIF(GTYP.EQ.TWIT) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DWIREC,ST)
           CALL LOGGAM(TWIT,GIDX,DWIREC,LWISTS)
           IF(LWISTS(GIDX).LT.GAMENV.AND.DRAW.GT.1) THEN  
              DRAW = DRAW - 1
              CALL READW(GAMFDB(1,GNUM),DRAW,DWIREC,ST)
              CALL LOGGAM(TWIT,GIDX,DWIREC,LWISTS)
           ENDIF
        ELSEIF(GTYP.EQ.TSCR) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DSCREC,ST)
           CALL LOGGAM(TSCR,GIDX,DSCREC,LSCSTS)
           IF(LSCSTS(GIDX).LT.GAMENV.AND.DRAW.GT.1) THEN  
              DRAW = DRAW - 1
              CALL READW(GAMFDB(1,GNUM),DRAW,DSCREC,ST)
              CALL LOGGAM(TSCR,GIDX,DSCREC,LSCSTS)
           ENDIF
        ELSEIF(GTYP.EQ.TDBL) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DDBREC,ST)
           CALL LOGGAM(TDBL,GIDX,DDBREC,LDBSTS)
           IF(LDBSTS(GIDX).LT.GAMENV.AND.DRAW.GT.1) THEN  
              DRAW = DRAW - 1
              CALL READW(GAMFDB(1,GNUM),DRAW,DDBREC,ST)
              CALL LOGGAM(TDBL,GIDX,DDBREC,LDBSTS)
           ENDIF
        ELSEIF(GTYP.EQ.TCPL) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DCPREC,ST)
           CALL LOGGAM(TCPL,GIDX,DCPREC,LCPSTS)
           IF(LCPSTS(GIDX).LT.GAMENV.AND.DRAW.GT.1) THEN  
              DRAW = DRAW - 1
              CALL READW(GAMFDB(1,GNUM),DRAW,DCPREC,ST)
              CALL LOGGAM(TCPL,GIDX,DCPREC,LCPSTS)
           ENDIF
        ELSEIF(GTYP.EQ.TSSC) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DSSREC,ST)
           CALL LOGGAM(TSSC,GIDX,DSSREC,LSSSTS)
           IF(LSSSTS(GIDX).LT.GAMENV.AND.DRAW.GT.1) THEN  
              DRAW = DRAW - 1
              CALL READW(GAMFDB(1,GNUM),DRAW,DSSREC,ST)
              CALL LOGGAM(TSSC,GIDX,DSSREC,LSSSTS)
           ENDIF
        ELSEIF(GTYP.EQ.TTRP) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DTRREC,ST)
           CALL LOGGAM(TTRP,GIDX,DTRREC,LTRSTS)
           IF(LTRSTS(GIDX).LT.GAMENV.AND.DRAW.GT.1) THEN  
              DRAW = DRAW - 1
              CALL READW(GAMFDB(1,GNUM),DRAW,DTRREC,ST)
              CALL LOGGAM(TTRP,GIDX,DTRREC,LTRSTS)
           ENDIF
        ELSEIF(GTYP.EQ.TSTR) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DSTREC,ST)
           CALL LOGGAM(TSTR,GIDX,DSTREC,LSTSTS)
           IF(LSTSTS(GIDX).LT.GAMENV.AND.DRAW.GT.1) THEN  
              DRAW = DRAW - 1
              CALL READW(GAMFDB(1,GNUM),DRAW,DSTREC,ST)
              CALL LOGGAM(TSTR,GIDX,DSTREC,LSTSTS)
           ENDIF
        ELSEIF(GTYP.EQ.TBNG) THEN
           CALL READW(GAMFDB(1,GNUM),DRAW,DBNREC,ST)
           CALL LOGGAM(TBNG,GIDX,DBNREC,LBNSTS)
        ELSEIF (GTYP.EQ.TPAS) THEN              ! LOAD PASSIVE FILE RECORD
           CALL READW(GAMFDB(1,GNUM),(DRAW-PAS_DRW_OFFSET),DPAREC,ST)
           INDEMI = 1                           ! FORCE FIRST DRAW (LPAREC)
           CALL LOGGAMPAS(INDEMI,GIDX,DPAREC,LPASTS)
        ENDIF
C
        IF(KGNUM.GT.0) THEN
          CALL READW(GAMFDB(1,KGNUM),KDRAW,DKKREC,ST) !KICKER IS REPORTED
          CALL LOGGAM(TKIK,KGIDX,DKKREC,DKKSTS)      !WITH MANY OTHER GAMES
	ENDIF
C
C CHECK READ STATUS AND REPORT ANY ERROR TO THE CONSOLE
C
        IF(ST.NE.0) THEN
           MESS(1)=SPE
           MESS(2)=TEGEN
           MESS(3)=4
           CALL FASTMOV(SFNAMES(1,GNUM),MESS(4),5)
           MESS(9)=ST
           MESS(10)=DRAW
           CALL QUEMES(MESS)
           TRABUF(TERR)=INVL
           GOTO 10000
        ENDIF
C
C ON GAME TYPE BUILD APPROPRIATE RESULTS REPORT
C
C BUILD HEADER FOR APPROPRIATE RESPONSE
C
60      CONTINUE
        LAST_DRAW(GNUM) = DRAW
        IND=5
C
C MOVE TIME IN HH:MM:SS FORMAT
C
        CALL PUTIME(TRABUF(TTIM), MESTAB, IND)
C
C MOVE GAME TYPE AND GAME INDEX
C
        MESTAB(IND) = GTYP
        IND=IND+1
        MESTAB(IND) = OGIDX
        IND=IND+1
C
	IF(GTYP.EQ.TLTO) GOTO 100
	IF(GTYP.EQ.TSPT) GOTO 200
	IF(GTYP.EQ.TNBR) GOTO 300
	IF(GTYP.EQ.TKIK) GOTO 400
	IF(GTYP.EQ.TDBL) GOTO 500
	IF(GTYP.EQ.TSCR) GOTO 600
	IF(GTYP.EQ.TWIT) GOTO 700
	IF(GTYP.EQ.TTSL) GOTO 800
	IF(GTYP.EQ.TTGL) GOTO 900
	IF(GTYP.EQ.TSSC) GOTO 1100
	IF(GTYP.EQ.TBNG) GOTO 1200
	IF(GTYP.EQ.TCPL) GOTO 1300
	IF(GTYP.EQ.TSTR) GOTO 1400
	IF(GTYP.EQ.TTRP) GOTO 1500
        IF(GTYP.EQ.TPAS) GOTO 1600
C
111     CONTINUE
        TRABUF(TERR)=INVL
        GOTO 10000
C
C BUILD LOTTO GAME TYPE RESULTS REPORT
C
100     CONTINUE
        IF(LLTSTS(GIDX).LT.GAMENV) THEN
           TRABUF(TERR)=RNIN
           GOTO 10000
        ENDIF
C
C SETS OF WINNING NUMBERS SENT
C
        WINSET_CNT = LLTBDR(GIDX) + 1   
        WINSET_CNT = MIN(WINSET_CNT,2) ! MAXIMUM 2 WINNING SETS !!!
        MESTAB(IND) = WINSET_CNT
        IND = IND + 1
        DO 140 WINSET=1,WINSET_CNT
C
C SHARE BITMAP 
C
        I4TEMP = 0
        IF(LLTSTS(GIDX).GE.GAMENV) I4TEMP = '01'X
        IF(LLTSTS(GIDX).GE.GFINAL) I4TEMP = '40'X
        IF(LLTSTS(GIDX).EQ.GAMCAN) I4TEMP = '04'X
        IF(LLTSTS(GIDX).LE.GAMBFD) I4TEMP = '08'X
        MESTAB(IND) = I1TEMP(1)
        IND=IND+1
C
C SET MESSAGE OPTION BITMAP
C
        I4TEMP = 0
        IF(KGNUM.GT.0.AND.WINSET.EQ.1) I4TEMP = '80'X   !KICKER NUMBER PRESENT
        I4TEMP = I4TEMP + '08'X                         !DIVISIONS PRESENT
	! IF(LLTLFL(GIDX).NE.0) I4TEMP = I4TEMP + '02'X   !LUCKY NUMBER PRESENT
        MESTAB(IND) = I1TEMP(1)
        OPTION = I1TEMP(1)
        IND = IND + 1
C
C NUMBER OF DIVISIONS AND BIT MAP OF DIVISIONS SENT
C
        I4TEMP = 0
        NUMDIV = 0
        IF(WINSET.EQ.1) THEN
	   IF(LLTLFL(GIDX).EQ.0) THEN
             DO 105 I=LLTNUM(GIDX),1,-1
               IF(LLTWTB(I,2,WINSET,GIDX).NE.0) THEN
                 I4TEMP = I4TEMP + BONDIV(LLTNUM(GIDX)-I+1)
                 NUMDIV = NUMDIV + 1
               ENDIF
               IF(LLTWTB(I,1,WINSET,GIDX).NE.0) THEN
                 I4TEMP = I4TEMP +  REGDIV(LLTNUM(GIDX)-I+1)
                 NUMDIV = NUMDIV + 1
               ENDIF
105          CONTINUE
	   ELSE
             DO 107 I=LLTNUM(GIDX),1,-1
               IF(LLTWTB(I,1,WINSET,GIDX).NE.0) THEN
	         IF(LLTLNC(1,LLTWTB(I,1,WINSET,GIDX),GIDX).NE.0) THEN
	           IF(LLTLNC(1,LLTWTB(I,1,WINSET,GIDX),GIDX).NE.
     *                LLTLNC(2,LLTWTB(I,1,WINSET,GIDX),GIDX))THEN
                     I4TEMP = I4TEMP + BONDIV(LLTNUM(GIDX)-I+1)
                     NUMDIV = NUMDIV + 1
	             IF(LLTLNC(2,LLTWTB(I,1,WINSET,GIDX),GIDX).NE.0) THEN
                       I4TEMP = I4TEMP + REGDIV(LLTNUM(GIDX)-I+1)
                       NUMDIV = NUMDIV + 1
	             ENDIF
	           ELSE
                     I4TEMP = I4TEMP + REGDIV(LLTNUM(GIDX)-I+1)
                     NUMDIV = NUMDIV + 1
	           ENDIF
	         ENDIF
               ENDIF
107          CONTINUE
	     I4TEMP = I4TEMP + BONDIV(LLTNUM(GIDX)+1)
             NUMDIV = NUMDIV + 1
	   ENDIF
        ELSE
           NUMDIV = 1
           I4TEMP = REGDIV(1)
        ENDIF

        DIVBITMAP = I4TEMP
        MESTAB(IND+0) = NUMDIV        
        MESTAB(IND+1) = I1TEMP(2)
        MESTAB(IND+2) = I1TEMP(1)
        IND = IND + 3
C
C FREE FOR ESTIMATED PRIZE BIT MAP
C
        ESTPRZPOS = IND
        IND = IND + 2
C
C CDC DATE
C
        I4TEMP = LLTDAT(CURDRW, GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
C SET YEAR / WEEK NUMBER
C
        CDC = LLTESD(GIDX)
	IF(GIDX.GT.2) THEN
          CALL FIGCCC(CDC,WEEK,YEAR2)
	ELSE
	  CALL FIGWEK(CDC,WEEK,YEAR2)
	ENDIF
        MESTAB(IND+0) = MOD(YEAR2,100)
        MESTAB(IND+1) = WEEK
        IND = IND + 2
C
C NEXT BYTE HAS # WINNING NUMBERS, # BONUS NUMBERS
C
        IF(WINSET.EQ.1) THEN
          I4TEMP = ISHFT(LLTNUM(GIDX),4) + LLTBFL(GIDX)
	  IF(LLTLFL(GIDX).NE.0) I4TEMP=ISHFT(LLTNUM(GIDX),4) + LLTBFL(GIDX)+1
        ELSE
          I4TEMP = ISHFT(LLTNUM(GIDX),4) 
        ENDIF
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C SET WINNING NUMBERS
C
        DO 110 I=1,LLTNUM(GIDX)
           MESTAB(IND) = LLTWIN(I,WINSET,GIDX)
           IND=IND+1
110     CONTINUE    
C
C SET WINNING LUCKY NUMBER
C
	IF(LLTLFL(GIDX).NE.0) THEN
	  MESTAB(IND) = LLTLNM(WINSET,GIDX)
	  IND=IND+1
	ENDIF
C
C SET BONUS NUMBERS
C
        IF(LLTBFL(GIDX).GT.0.AND.WINSET.EQ.1) THEN
            DO 120 I=1,LLTBFL(GIDX)
               MESTAB(IND) = LLTBNM(I,1,GIDX)
               IND=IND+1
120         CONTINUE
	    IF(LLTLFL(GIDX).NE.0) THEN
	      MESTAB(IND) = LLTLNM(WINSET,GIDX)
	      IND=IND+1
	    ENDIF
        ENDIF
C
C KICKER NUMBER (IF PRESENT)
C
        IF(IAND(OPTION,'80'X).NE.0) THEN
           I4TEMP = LKKWIN(KGIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND=IND+4
        ENDIF
C
C SHARE COUNTS AND AMOUNTS
C
        SHRPOS = IND
        IF(WINSET.EQ.1) THEN
         DO 130 I=LLTNUM(GIDX),1,-1
	   IF(LLTLFL(GIDX).EQ.0) THEN
             IF(LLTWTB(I,2,WINSET,GIDX).NE.0) THEN
               I4TEMP = LLTTSR(LLTWTB(I,2,WINSET,GIDX),WINSET,GIDX)
               MESTAB(IND+0) = I1TEMP(4)
               MESTAB(IND+1) = I1TEMP(3)
               MESTAB(IND+2) = I1TEMP(2)
               MESTAB(IND+3) = I1TEMP(1)
               IND=IND+4
C
               I4TEMP = LLTSHV(LLTWTB(I,2,WINSET,GIDX),WINSET,GIDX)
C
               MESTAB(IND+0) = I1TEMP(4)
               MESTAB(IND+1) = I1TEMP(3)
               MESTAB(IND+2) = I1TEMP(2)
               MESTAB(IND+3) = I1TEMP(1)
               IND=IND+4
             ENDIF
C
             IF(LLTWTB(I,1,WINSET,GIDX).NE.0) THEN
               I4TEMP = LLTTSR(LLTWTB(I,1,WINSET,GIDX),WINSET,GIDX)
               MESTAB(IND+0) = I1TEMP(4)
               MESTAB(IND+1) = I1TEMP(3)
               MESTAB(IND+2) = I1TEMP(2)
               MESTAB(IND+3) = I1TEMP(1)
               IND=IND+4
C
               I4TEMP = LLTSHV(LLTWTB(I,1,WINSET,GIDX),WINSET,GIDX)
               MESTAB(IND+0) = I1TEMP(4)
               MESTAB(IND+1) = I1TEMP(3)
               MESTAB(IND+2) = I1TEMP(2)
               MESTAB(IND+3) = I1TEMP(1)
               IND=IND+4
             ENDIF
	   ELSE
             IF(LLTWTB(I,1,WINSET,GIDX).NE.0) THEN
	       IF(LLTLNC(1,LLTWTB(I,1,WINSET,GIDX),GIDX).NE.0) THEN
                 I4TEMP = LLTTSR(LLTLNC(1,LLTWTB(I,1,WINSET,GIDX),GIDX),WINSET,GIDX)
                 MESTAB(IND+0) = I1TEMP(4)
                 MESTAB(IND+1) = I1TEMP(3)
                 MESTAB(IND+2) = I1TEMP(2)
                 MESTAB(IND+3) = I1TEMP(1)
                 IND=IND+4
C
                 I4TEMP = LLTSHV(LLTLNC(1,LLTWTB(I,1,WINSET,GIDX),GIDX),WINSET,GIDX)
                 MESTAB(IND+0) = I1TEMP(4)
                 MESTAB(IND+1) = I1TEMP(3)
                 MESTAB(IND+2) = I1TEMP(2)
                 MESTAB(IND+3) = I1TEMP(1)
                 IND=IND+4
	       ENDIF
	       IF(LLTLNC(2,LLTWTB(I,1,WINSET,GIDX),GIDX).NE.
     *            LLTLNC(1,LLTWTB(I,1,WINSET,GIDX),GIDX) ) THEN
                 I4TEMP = LLTTSR(LLTLNC(2,LLTWTB(I,1,WINSET,GIDX),GIDX),WINSET,GIDX)
                 MESTAB(IND+0) = I1TEMP(4)
                 MESTAB(IND+1) = I1TEMP(3)
                 MESTAB(IND+2) = I1TEMP(2)
                 MESTAB(IND+3) = I1TEMP(1)
                 IND=IND+4
C
                 I4TEMP = LLTSHV(LLTLNC(2,LLTWTB(I,1,WINSET,GIDX),GIDX),WINSET,GIDX)
                 MESTAB(IND+0) = I1TEMP(4)
                 MESTAB(IND+1) = I1TEMP(3)
                 MESTAB(IND+2) = I1TEMP(2)
                 MESTAB(IND+3) = I1TEMP(1)
                 IND=IND+4
	       ENDIF
             ENDIF
	   ENDIF
130      CONTINUE
	 IF(LLTLFL(GIDX).NE.0) THEN
           I4TEMP = LLTTSR(LLTLDV(GIDX),WINSET,GIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND=IND+4
C
           I4TEMP = LLTSHV(LLTLDV(GIDX),WINSET,GIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND=IND+4
	 ENDIF
        ELSEIF(WINSET.EQ.2) THEN
           I4TEMP = LLTTSR(1,2,GIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND=IND+4
           I4TEMP = LLTSHV(1,2,GIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND=IND+4
        ENDIF
C
140     CONTINUE
C
C SET ESTIMATED / FINAL PRIZE BITMAP
C
        DRAWCDC = LLTDAT(CURDRW, GIDX)
        CALL ESTPRIZES(MESTAB, SHRPOS, DRAWCDC, DIVBITMAP, ESTPRZPOS)
C
C END FOR LOTTO
C
        OUTLEN = IND - 1
        GOTO 9000
C
C BUILD SPORT 1X2 GAME TYPE RESULTS REPORT
C
200     CONTINUE
        IF(LSPSTS(GIDX).LT.GAMENV) THEN
           TRABUF(TERR)=RNIN
           GOTO 10000
        ENDIF
C
C SETS OF WINNING NUMBERS SENT
C
        MESTAB(IND) = 1
        IND = IND + 1
C
C SHARE BITMAP 
C
        I4TEMP = 0
        IF(LSPSTS(GIDX).GE.GAMENV) I4TEMP = '01'X
        IF(LSPSTS(GIDX).GE.GFINAL) I4TEMP = '40'X
        IF(LSPSTS(GIDX).EQ.GAMCAN) I4TEMP = '04'X
        IF(LSPSTS(GIDX).LE.GAMBFD) I4TEMP = '08'X
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C MESSAGE OPTION BITMAP
C
        I4TEMP = 0
        IF(KGNUM.GT.0) I4TEMP = '80'X           !KICKER NUMBER PRESENT
        I4TEMP = I4TEMP + '08'X                 !DIVISIONS PRESENT
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C NUMBER OF DIVISIONS
C
        I4TEMP = LSPDIV(GIDX)
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C BIT MAP OF DIVISIONS SENT
C
        I4TEMP = 0
        DO 205 I=1,LSPDIV(GIDX)
           IF(LSPMAT(I,GIDX).NE.0) I4TEMP = I4TEMP + ALLDIV(I)
205     CONTINUE
        DIVBITMAP = I4TEMP
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND = IND +2
C
C FREE FOR ESTIMATED / FINAL BIT MAP
C
        ESTPRZPOS = IND
        IND = IND + 2
C
C CDC DATE
C
        I4TEMP = LSPDAT(CURDRW, GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
C WEEK NUMBER
C
        CDC = LSPESD(GIDX)
        CALL FIGWEK(CDC,WEEK,YEAR2)
        MESTAB(IND+0) = MOD(YEAR2, 100)
        MESTAB(IND+1) = WEEK
        IND = IND + 2
C
C NEXT BYTE HAS # WINNING NUMBERS, # BONUS NUMBERS
C
        I4TEMP = ISHFT(LSPMAX(GIDX),4) + 0
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C SPORTS WINNING NUMBERS
C
	BCNT = 0
	IF(LSPFRG(GIDX).NE.0) BCNT = 1
        LEN=LSPMAX(GIDX)-BCNT
        IF(INT(LSPMAX(GIDX)/2).NE.(LSPMAX(GIDX)/2)) LEN=LSPMAX(GIDX)+1
        DO 210 I=1,LEN,2
           WIN1=LSPWIN(I,GIDX)
           IF(I+1.GT.LSPMAX(GIDX)-BCNT) THEN
              WIN2=0
           ELSE
              WIN2=LSPWIN(I+1,GIDX)
           ENDIF
           WINNER=ISHFT(WIN1,4)+WIN2
           MESTAB(IND) = ZEXT( WINNER )
           IND=IND+1
210     CONTINUE
C
	IF(LSPFRG(GIDX).EQ.1) THEN
           MESTAB(IND) = ZEXT( LSPWIN(DSPMAX,GIDX) )
           IND=IND+1
	ENDIF

        IF(LSPFRG(GIDX).EQ.2) THEN
           MESTAB(IND) = IAND(LSPWIN(DSPMAX,GIDX),'0F'X)
           IND=IND+1
        ENDIF

C
C SET KICKER NUMBER
C
        IF(KGNUM.GT.0) THEN
           I4TEMP = LKKWIN(KGIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND=IND+4
        ENDIF
C
C SET SHARE COUNT AND AMOUNT
C
        SHRPOS = IND
        DO 220 I=1,LSPDIV(GIDX)
           IF(LSPMAT(I,GIDX).NE.0) THEN
             I4TEMP = LSPTSR(I,GIDX)
             MESTAB(IND+0) = I1TEMP(4)
             MESTAB(IND+1) = I1TEMP(3)
             MESTAB(IND+2) = I1TEMP(2)
             MESTAB(IND+3) = I1TEMP(1)
             IND=IND+4
C
             I4TEMP = LSPSHV(I,GIDX)
             MESTAB(IND+0) = I1TEMP(4)
             MESTAB(IND+1) = I1TEMP(3)
             MESTAB(IND+2) = I1TEMP(2)
             MESTAB(IND+3) = I1TEMP(1)
             IND=IND+4
           ENDIF
220     CONTINUE
C
C SET ESTIMATED / FINAL BIT MAP
C
        DRAWCDC = LSPDAT(CURDRW, GIDX)
        CALL ESTPRIZES(MESTAB, SHRPOS, DRAWCDC, DIVBITMAP, ESTPRZPOS) 
C
C END FOR SPORT GAMES
C
        OUTLEN=IND - 1
        GOTO 9000
C
C
C BUILD NUMBERS GAME TYPE RESULTS REPORT
C
300     CONTINUE
        IF(LNBSTS(GIDX).LT.GAMENV) THEN
           TRABUF(TERR)=RNIN
           GOTO 10000
        ENDIF
C
C SETS OF WINNING NUMBERS SENT
C
        MESTAB(IND) = LNBBDR(GIDX) + 1
        IND = IND + 1
C
C SHARE BITMAP 
C
        I4TEMP = 0
        IF(LNBSTS(GIDX).EQ.GAMENV) I4TEMP = I4TEMP + '80'X
        IF(LNBSTS(GIDX).EQ.GFINAL) I4TEMP = I4TEMP + '40'X
        IF(LNBSTS(GIDX).EQ.GAMCAN) I4TEMP = I4TEMP + '08'X
        IF(LNBBDR(GIDX).NE.0)      I4TEMP = I4TEMP + '20'X
        MESTAB(IND) = I1TEMP(1)
        IND=IND+1
C
C MESSAGE OPTION BITMAPS
C
        I4TEMP = 0
        IF(KGNUM.GT.0) I4TEMP = I4TEMP + '80'X
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C NUMBER OF DIVISIONS
C
C***    MESTAB(IND) = 3
C***    IND = IND + 1
C
C BIT MAP OF DIVISIONS SENT
C
C***    I4TEMP = 0
C***    MESTAB(IND+0) = I1TEMP(2)
C***    MESTAB(IND+1) = I1TEMP(1)
C***    IND = IND +2
C
C CDC DATE
C
        I4TEMP = LNBDAT(1,GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
C WEEK NUMBER
C
        CDC = LNBDAT(1,GIDX)
        CALL FIGWEK(CDC,WEEK,YEAR2)
        MESTAB(IND+0) = MOD(YEAR2,100)
        MESTAB(IND+1) = WEEK
        IND = IND + 2
C
C NEXT BYTE HAS # WINNING NUMBERS, # BONUS NUMBERS
C
        I4TEMP = ISHFT(1,4) + 0
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C WINNING NUMBER
C
        I4TEMP = LNBWIN(TNB3ST,1,GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
C JOKER NUMBER (IF PRESENT)
C
        IF(KGNUM.GT.0) THEN
           I4TEMP = LKKWIN(KGIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDIF
C
C GET SHARE AMOUNTS
C
C       IF(DNBTYP.EQ.NB3TYP) THEN
C         NTYP=TNB3B6
C         N=DNBWIN(TNB3ST,1)
C         DO 390 I=1,3
C         DIG(I)=MOD(N,10)
C         N=N/10
C390      CONTINUE
C         CALL BOXTYP(DIG,NTYP,NB3TYP)
C         SPRIZE=DNBPRZ(TNB3ST,1)
C         IF(NTYP.NE.0) THEN
C           BPRIZE=DNBPRZ(NTYP,1)
C           CPRIZE=DNBPRZ(NTYP+2,1)
C         ELSE
C           BPRIZE=0
C           CPRIZE=0
C         ENDIF
C         PPRIZE=DNBPRZ(TNB3F2,1)
C       ELSE
C         NTYP=TNB4B24
C         N=DNBWIN(TNB4ST,1)
C         DO 395 I=1,4
C         DIG(I)=MOD(N,10)
C         N=N/10
C395      CONTINUE
C         CALL BOXTYP(DIG,NTYP,NB4TYP)
C          SPRIZE=DNBPRZ(TNB4ST,1)
C         IF(NTYP.NE.0) THEN
C           BPRIZE=DNBPRZ(NTYP,1)
C            CPRIZE=DNBPRZ(NTYP+4,1)
C         ELSE
C            BPRIZE=0
C            CPRIZE=0
C         ENDIF
C          PPRIZE=DNBPRZ(TNB4F2,1)
C       ENDIF
C
C STRAIGHT PRIZE CNT AND AMOUNT
C
C       I4TEMP = 0
C       MESTAB(IND+0) = I1TEMP(4)
C       MESTAB(IND+1) = I1TEMP(3)
C       MESTAB(IND+2) = I1TEMP(2)
C       MESTAB(IND+3) = I1TEMP(1)
C       IND = IND +4
C
C       I4TEMP = SPRIZE
C       MESTAB(IND+0) = I1TEMP(4)
C       MESTAB(IND+1) = I1TEMP(3)
C       MESTAB(IND+2) = I1TEMP(2)
C       MESTAB(IND+3) = I1TEMP(1)
C       IND=IND+4
C
C BOX PRIZE CNT AND AMOUNT
C
C***    I4TEMP = 0
C***    MESTAB(IND+0) = I1TEMP(4)
C***    MESTAB(IND+1) = I1TEMP(3)
C***    MESTAB(IND+2) = I1TEMP(2)
C***    MESTAB(IND+3) = I1TEMP(1)
C***    IND = IND + 4
C***
C***    I4TEMP = BPRIZE
C***    MESTAB(IND+0) = I1TEMP(4)
C***    MESTAB(IND+1) = I1TEMP(3)
C***    MESTAB(IND+2) = I1TEMP(2)
C***    MESTAB(IND+3) = I1TEMP(1)
C***    IND=IND+4
C
C STR/BOX PRIZE CNT AND AMOUNT
C
C***    I4TEMP = 0
C***    MESTAB(IND+0) = I1TEMP(4)
C***    MESTAB(IND+1) = I1TEMP(3)
C***    MESTAB(IND+2) = I1TEMP(2)
C***    MESTAB(IND+3) = I1TEMP(1)
C***    IND = IND +4
C***
C***    I4TEMP = CPRIZE
C***    MESTAB(IND+0) = I1TEMP(4)
C***    MESTAB(IND+1) = I1TEMP(3)
C***    MESTAB(IND+2) = I1TEMP(2)
C***    MESTAB(IND+3) = I1TEMP(1)
C***    IND=IND+4
C
C PAIR PRIZE CNT AND AMOUNT
C
C***    I4TEMP = 0
C***    MESTAB(IND+0) = I1TEMP(4)
C***    MESTAB(IND+1) = I1TEMP(3)
C***    MESTAB(IND+2) = I1TEMP(2)
C***    MESTAB(IND+3) = I1TEMP(1)
C***    IND = IND +4
C***
C***    I4TEMP = PPRIZE
C***    MESTAB(IND+0) = I1TEMP(4)
C***    MESTAB(IND+1) = I1TEMP(3)
C***    MESTAB(IND+2) = I1TEMP(2)
C***    MESTAB(IND+3) = I1TEMP(1)
C***    IND=IND+4
C
        OUTLEN=IND - 1
        GOTO 9000
C
C
C BUILD KICKER GAME TYPE RESULTS REPORT
C
400     CONTINUE
        IF(LKKSTS(GIDX).LT.GAMENV) THEN
           TRABUF(TERR)=RNIN
           GOTO 10000
        ENDIF
C
C SETS OF WINNING NUMBERS SENT
C
        MESTAB(IND) = 1
        IND = IND + 1
C
C SET SHARE BITMAP
C
        I4TEMP = 0
        IF(LKKSTS(GIDX).GE.GAMENV) I4TEMP = '01'X
        IF(LKKSTS(GIDX).GE.GFINAL) I4TEMP = '40'X 
        IF(LKKSTS(GIDX).EQ.GAMCAN) I4TEMP = '04'X
        IF(LKKSTS(GIDX).LE.GAMBFD) I4TEMP = '08'X
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C SET MESSAGE OPTION BITMAP
C
        I4TEMP = 0
        IF(KGNUM.GT.0) I4TEMP = I4TEMP + '80'X
        I4TEMP = I4TEMP + '08'X                 !DIVISIONS PRESENT
        MESTAB(IND+0) = I1TEMP(1)
        IND = IND + 1
C
C NUMBER OF DIVISIONS
C
        I4TEMP = LKKDIV(GIDX)
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C BIT MAP OF DIVISIONS SENT
C
        I4TEMP = 0
        JOKDIVSEND = .TRUE.
        DO 405 I=1, KIGDIV
          IF(LKKMAT(1,I,GIDX).NE.0) THEN
            I4TEMP = I4TEMP + ALLDIV(I)
            IF(JOKDIVSEND) THEN
              JOKDIVSEND = .FALSE.
              SENDJOKDIV = I4TEMP
            ENDIF
          ENDIF
405     CONTINUE
        DIVBITMAP = I4TEMP
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND = IND +2
C
C FREE FOR ESTIMATED / FINAL PRIZES
C
        ESTPRZPOS = IND
        IND = IND + 2
C
C CDC DATE
C
        I4TEMP = LKKDAT(CURDRW, GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
C WEEK NUMBER
C
        CDC = LKKESD(GIDX)
        CALL FIGWEK(CDC,WEEK,YEAR2)
        MESTAB(IND+0) = MOD(YEAR2,100)
        MESTAB(IND+1) = WEEK
        IND = IND + 2
C
C NEXT BYTE HAS # WINNING NUMBERS, # BONUS NUMBERS
C
        I4TEMP = ISHFT(1,4) + 0
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C 
        I4TEMP = LKKWIN(GIDX)
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND = IND + 4
C
        SHRPOS = IND
        DO 410 I=1,LKKDIV(GIDX)
            I4TEMP = LKKSHR(I,GIDX)
            MESTAB(IND+0) = I1TEMP(4)
            MESTAB(IND+1) = I1TEMP(3)
            MESTAB(IND+2) = I1TEMP(2)
            MESTAB(IND+3) = I1TEMP(1)
            IND = IND + 4
C    
            I4TEMP = LKKSHV(I,GIDX)
            IF(LKKDRW(GIDX).EQ.843) THEN   !V66
              IF(I.EQ.1) I4TEMP = 0        !V66
              IF(I.EQ.2) I4TEMP = 0        !V66
              IF(I.EQ.3) I4TEMP = 24676527 !V66
            ENDIF                          !V66
            MESTAB(IND+0) = I1TEMP(4)
            MESTAB(IND+1) = I1TEMP(3)
            MESTAB(IND+2) = I1TEMP(2)
            MESTAB(IND+3) = I1TEMP(1)
            IND = IND + 4
410     CONTINUE
C
C SEND ESTIMATED / FINAL PRIZES
C
         DRAWCDC = LKKDAT(CURDRW, GIDX)
         DIVBITMAP = IAND(DIVBITMAP, SENDJOKDIV)
         CALL ESTPRIZES(MESTAB, SHRPOS, DRAWCDC, DIVBITMAP, ESTPRZPOS)    
C
C END OF JOKER GAME
C
        OUTLEN = IND - 1
        GOTO 9000

C
C BUILD SUPER DOUBLE TYPE RESULTS REPORT
C
500     CONTINUE
        NAME_LEN = DBLNMS_LEN - 2
        IF(LDBSTS(GIDX).LT.GAMENV) THEN
           TRABUF(TERR) = RNIN
           GOTO 10000
        ENDIF
C
C SETS OF WINNERS
C
        TOT_WIN = LDBCMB(GIDX)
        MESTAB(IND) = TOT_WIN
        IND = IND + 1

C
C SHARE BITMAP
C
        I4TEMP = 0
        IF(LDBSTS(GIDX).EQ.GAMENV) I4TEMP = I4TEMP + '80'X
        IF(LDBSTS(GIDX).GE.GFINAL) I4TEMP = I4TEMP + '40'X
        IF(LDBSTS(GIDX).EQ.GAMCAN) I4TEMP = I4TEMP + '04'X
        IF(LDBSTS(GIDX).LE.GAMBFD) I4TEMP = I4TEMP + '08'X
        IF(TOT_WIN.EQ.0) I4TEMP = I4TEMP + '02'X   !NO WINNERS
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C MESSAGE OPTION BITMAP
C
        I4TEMP = 0
        IF(KGNUM.GT.0) I4TEMP = I4TEMP + '80'
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C CDC DATE
C
        I4TEMP = LDBDAT(GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND = IND + 2
C
C WEEK NUMBER
C
        CDC = LDBDAT(GIDX)
        CALL FIGWEK(CDC-WEEK_OFFSET,WEEK,YEAR2)
        MESTAB(IND+0) = MOD(YEAR2,100)
        MESTAB(IND+1) = WEEK
        IND = IND + 2
C
C # WINNERS SENT
C
        NR_WIN_IND = IND
        IND = IND + 1
C
C MORE FLAG
C
        MESTAB(IND) = 'FF'X ! LAST SEGMENT
        FRST_WIN = MAX(SEGNO*6,1)
        IF(SEGNO.EQ.0) THEN
            LAST_WIN = MIN(5,TOT_WIN)
        ELSE
            LAST_WIN = MIN(FRST_WIN+5,TOT_WIN)
        ENDIF
        IF(LDBSTS(GIDX).NE.GAMCAN.AND.LDBSTS(GIDX).NE.GAMREF) THEN
            IF(FRST_WIN.GT.TOT_WIN) THEN
               TRABUF(TERR)=INVL
               GOTO 10000
            ENDIF
        ENDIF
        IF(LAST_WIN.LT.TOT_WIN) MESTAB(IND) = '80'X ! MORE SEGMENTS...

        I4TEMP = LAST_WIN-FRST_WIN+1
        MESTAB(NR_WIN_IND) = ISHFT(I4TEMP,4)

        IF(LDBSTS(GIDX).EQ.GAMREF.OR.LDBSTS(GIDX).EQ.GAMCAN) THEN
            MESTAB(IND) = 'FF'X
            MESTAB(NR_WIN_IND) = 0
        ENDIF
        IND = IND + 1
        IF(SEGNO.GT.0) THEN
            MESTAB(IND) = 0
            IND = IND + 1
            MESTAB(IND) = 0
            IND = IND + 1
            MESTAB(IND) = NAME_LEN
            IND = IND + 1
            MESTAB(IND) = NAME_LEN
            IND = IND + 1
            GOTO 510
        ENDIF

C
C EVENT NAME FIELD LENGTH
C
        MESTAB(IND) = DBLENM_LEN
        IND = IND + 1
C
C EVENT DESCRIPTION FIELD LENGTH
C
C***    MESTAB(IND) = DBLDES_LEN  !do not send to terminal
        MESTAB(IND) = 0
        IND = IND + 1
C
C EVENT ROW 1 FIELD LENGTH
C
        MESTAB(IND) = NAME_LEN
        IND = IND + 1
C
C EVENT ROW 2 FIELD LENGTH
C
        MESTAB(IND) = NAME_LEN
        IND = IND + 1
C
C EVENT NAME
C
        CALL MOVBYT(LDBENM(1,GIDX),1,MESTAB,IND,DBLENM_LEN)
        IND = IND + DBLENM_LEN
C
C EVENT DESCRIPTION
C
C***    CALL MOVBYT(LDBDES(1,GIDX),1,MESTAB,IND,DBLDES_LEN)
C***    IND = IND + DBLDES_LEN
C
C SUPER DOUBLE WINNING ROW NUMBER AND ODDS
C
510     CONTINUE    
        IF(MESTAB(NR_WIN_IND).EQ.0) GOTO 520
C
        DO I=FRST_WIN,LAST_WIN
           RW1 = LDBWIN(1,I,GIDX)            !---- Winner
           RW2 = LDBWIN(2,I,GIDX)            !---- Second
           MESTAB(IND) = RW1
           IND = IND + 1
           MESTAB(IND) = RW2
           IND = IND + 1

           CALL MOVBYT(LDBNMS(1,RW1,GIDX),1,MESTAB,IND,NAME_LEN)
           IND = IND + NAME_LEN

           CALL MOVBYT(LDBNMS(1,RW2,GIDX),1,MESTAB,IND,NAME_LEN)
           IND = IND + NAME_LEN

           I4TEMP = LDBODS(I,GIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDDO
C
520     CONTINUE
C
C KICKER NUMBER (IF PRESENT)
C
        IF(KGNUM.GT.0) THEN
           I4TEMP = LKKWIN(KGIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDIF
C
        OUTLEN = IND -1
        GOTO 9000 

C
C
C BUILD SCORE GAME TYPE RESULTS REPORT
C
600     CONTINUE
        IF(LSCSTS(GIDX).LT.GAMENV) THEN
           TRABUF(TERR)=RNIN
           GOTO 10000
        ENDIF
C
C SETS OF WINNING NUMBERS SENT
C
        MESTAB(IND) = 1
        IND = IND + 1
C
C SHARE BITMAP 
C
        I4TEMP = 0
        IF(LSCSTS(GIDX).EQ.GAMDON) I4TEMP = I4TEMP + '80'X
        IF(LSCSTS(GIDX).EQ.GFINAL) I4TEMP = I4TEMP + '40'X
        IF(LSCSTS(GIDX).EQ.GAMCAN) I4TEMP = I4TEMP + '04'X
        IF(LSCSTS(GIDX).NE.GAMCAN.AND.LSCWIN(1,GIDX).LT.0) 
     *     I4TEMP = I4TEMP + '02'X
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C MESSAGE OPTION BITMAP
C
        I4TEMP = 0
        IF(KGNUM.GT.0) I4TEMP = I4TEMP + '80'X
        I4TEMP = I4TEMP + '20'X                 !WINNING ODDS PRESENT
        I4TEMP = I4TEMP + '10'X                 !WINNING SCORE PRESENT
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C CDC DATE
C
        I4TEMP = LSCDAT(GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND = IND + 2
C
C WEEK NUMBER
C
        CDC = LSCDAT(GIDX)
        CALL FIGWEK(CDC-WEEK_OFFSET,WEEK,YEAR2)
        MESTAB(IND+0) = MOD(YEAR2,100)
        MESTAB(IND+1) = WEEK
        IND = IND + 2
C
C # WINNING NUMBERS , # BONUS NUMBERS
C
        MESTAB(IND+0) = '10'X  !
        IND = IND + 1
C
C TEAM NAME FIELD LENGHT
C
        MESTAB(IND) = 14
        IND = IND + 1
C
C HOME TEAM NAME
C
        CALL MOVBYT(LSCNM1(1,GIDX),1,MESTAB,IND,14)
        IND = IND + 14
C
C AWAY TEAM NAME
C
        CALL MOVBYT(LSCNM2(1,GIDX),1,MESTAB,IND,14)
        IND = IND + 14
C
C WINNING SCORE HOME TEAM
C
        I4TEMP = LSCWIN(1,GIDX)
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C WINNING SCORE AWAY TEAM
C
        I4TEMP = LSCWIN(2,GIDX)
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C WINNING ODDS
C
        I4TEMP = LSCODS(GIDX)
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND = IND + 4
C
C SET KICKER NUMBER (IF PRESENT)
C
        IF(KGNUM.GT.0) THEN
           I4TEMP = LKKWIN(KGIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDIF
C
        OUTLEN = IND - 1
        GOTO 9000
C
C
C BUILD WINNERS TIP GAME TYPE RESULTS REPORT
C
700     CONTINUE
        NAME_LEN = WNMS_LEN-2      ! as in LODTXT
        IF(LWISTS(GIDX).LT.GAMENV) THEN
           TRABUF(TERR) = RNIN
           GOTO 10000
        ENDIF
C
C SETS OF WINNERS
C
        WIN1 = 0
        DO 705 I=1,4
           IF(LWIWIN(I,GIDX).GT.0) WIN1 = WIN1 + 1
705     CONTINUE
        I4TEMP = WIN1
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C SHARE BITMAP
C
        I4TEMP = 0
        IF(LWISTS(GIDX).EQ.GAMENV) I4TEMP = I4TEMP + '80'X
        IF(LWISTS(GIDX).GE.GFINAL) I4TEMP = I4TEMP + '40'X
        IF(LWISTS(GIDX).EQ.GAMCAN) I4TEMP = I4TEMP + '04'X
        IF(LWISTS(GIDX).LE.GAMBFD) I4TEMP = I4TEMP + '08'X
        IF(WIN1.LT.1)              I4TEMP = I4TEMP + '02'X   !NO WINNERS
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C MESSAGE OPTION BITMAP
C
        I4TEMP = 0
        IF(KGNUM.GT.0) I4TEMP = I4TEMP + '80'
        IF(WIN1.GT.0)  THEN
           I4TEMP = I4TEMP + '40'X                     !WINNING ROWS PRESENT
           I4TEMP = I4TEMP + '20'X                     !WINNING ODDS PRESENT
           I4TEMP = I4TEMP + '04'X                     !TEXT PRESENT
        ENDIF
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C CDC DATE
C
        I4TEMP = LWIDAT(GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND = IND + 2
C
C WEEK NUMBER
C
        CDC = LWIDAT(GIDX)
        CALL FIGWEK(CDC-WEEK_OFFSET,WEEK,YEAR2)
        MESTAB(IND+0) = MOD(YEAR2,100)
        MESTAB(IND+1) = WEEK
        IND = IND + 2
C
C # WINNERS SENT
C
        I4TEMP = ISHFT(WIN1,4)
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C EVENT NAME FIELD LENGTH
C
        MESTAB(IND) = WENM_LEN
        IND = IND + 1
C
C EVENT DESCRIPTION FIELD LENGTH
C
        MESTAB(IND) = WDES_LEN
        IND = IND + 1
C
C EVENT NAME FIELD LENGTH
C
        MESTAB(IND) = NAME_LEN
        IND = IND + 1
C
C EVENT NAME
C
        CALL MOVBYT(LWIENM(1,GIDX),1,MESTAB,IND,WENM_LEN)
        IND = IND + WENM_LEN
C
C EVENT DESCRIPTION
C
        CALL MOVBYT(LWIDES(1,GIDX),1,MESTAB,IND,WDES_LEN)
        IND = IND + WDES_LEN
C
C WIN TIP WINNING ROW NUMBER AND ODDS
C
        IF(WIN1.GT.0) THEN
           DO 710 I = 1 , WIN1
              I4TEMP = LWIWIN(I,GIDX)
              RW1 = I4TEMP
              MESTAB(IND) = I1TEMP(1)
              IND = IND + 1
              CALL MOVBYT(LWINMS(1,RW1,GIDX),1,MESTAB,IND,NAME_LEN)
              IND = IND + NAME_LEN
              I4TEMP = LWIODS(I,GIDX)
              MESTAB(IND+0) = I1TEMP(4)
              MESTAB(IND+1) = I1TEMP(3)
              MESTAB(IND+2) = I1TEMP(2)
              MESTAB(IND+3) = I1TEMP(1)
              IND = IND + 4
710        CONTINUE
        ENDIF
C
C KICKER NUMBER (IF PRESENT)
C
        IF(KGNUM.GT.0) THEN
           I4TEMP = LKKWIN(KGIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDIF
C
        OUTLEN = IND -1
        GOTO 9000 
C
C
C BUILD TOTO SELECT GAME TYPE RESULTS REPORT
C
800     CONTINUE
C
C SETS OF WINNING NUMBERS SENT
C
        LOGGAM_TTSREC = .FALSE.
C
        IF(GTYP.EQ.TTSL.AND.DRAW.EQ.DAFDRW(GNUM)) THEN
           I2DATE(VCDC) = DTSBSD
        ELSE  
           I2DATE(VCDC) = LTSBSD(GIDX)
        ENDIF
        CALL CDATE(I2DATE)
        BEGDOW = I2DATE(VDOW)
        IF(GTYP.EQ.TTSL.AND.DRAW.EQ.DAFDRW(GNUM)) THEN
           I2DATE(VCDC) = DTSESD
        ELSE
           I2DATE(VCDC) = LTSESD(GIDX)
        ENDIF
        CALL CDATE(I2DATE)
        ENDDOW = I2DATE(VDOW)
        IF(ENDDOW.EQ.SUNDAY .OR. ENDDOW.EQ.MONDAY) THEN
           IF(DRAW.GT.1 .AND.
     *         .NOT.(BEGDOW.EQ.MONDAY .OR. BEGDOW.EQ.TUESDAY)) THEN
              CALL READW(GAMFDB(1,GNUM),DRAW-1,TTSREC,ST)
              CALL LOGGAM(TTSL,GIDX,TTSREC,LTSSTS)
              NUM_ACTIVE_TTSL = 2
           ELSE
              NUM_ACTIVE_TTSL = 1
           ENDIF
        ELSE
           CALL READW(GAMFDB(1,GNUM),DRAW+1,TTSREC,ST)
           IF(TTSSTS.GE.GAMOPN) THEN
              LOGGAM_TTSREC = .TRUE.
              NUM_ACTIVE_TTSL = 2
           ELSE
              NUM_ACTIVE_TTSL = 1
           ENDIF
        ENDIF

        MESTAB(IND) = NUM_ACTIVE_TTSL
        IND = IND + 1
C
C LOOP ON NUMBER OF ACTIVE TOTO SELECT GAMES
C
        DO J =1,NUM_ACTIVE_TTSL
           IF(J.EQ.2) THEN
              IF(LOGGAM_TTSREC) THEN
                 CALL LOGGAM(TTSL,GIDX,TTSREC,LTSSTS)
              ELSEIF(.NOT.SKIPIT) THEN
                 CALL LOGGAM(TTSL,GIDX,DTSREC,LTSSTS)
              ELSE
                 CALL FASTMOV(TSLSTS,LTSSTS,LTSRECLEN)
              ENDIF
           ENDIF
C
C SHARE BITMAP 
C
           IF(J.EQ.1) THEN
              IF(GTYP.EQ.TTSL.AND..NOT.SKIPIT) 
     *           CALL LOGGAM(TTSL,GIDX,DTSREC,LTSSTS)

              I4TEMP = 0
              IF(LTSSTS(GIDX).EQ.GAMDON) I4TEMP = I4TEMP + '80'X
              IF(LTSSTS(GIDX).EQ.GFINAL) I4TEMP = I4TEMP + '40'X
              IF(LTSSTS(GIDX).EQ.GAMCAN) I4TEMP = I4TEMP + '04'X
              IF(LTSSTS(GIDX).LE.GAMBFD) I4TEMP = I4TEMP + '08'X
              MESTAB(IND) = I1TEMP(1)
              IND = IND + 1
C
C MESSAGE OPTION BITMAP
C
              I4TEMP = 0
              IF(KGNUM.GT.0) I4TEMP = I4TEMP + '80'X
              MESTAB(IND) = I1TEMP(1)
              IND = IND + 1
C
C DRAW CDC DATE
C
              I4TEMP = LTSDAT(1,GIDX)
              MESTAB(IND+0) = I1TEMP(2)
              MESTAB(IND+1) = I1TEMP(1)
              IND = IND + 2
C
C WEEK NUMBER
C
              CDC = LTSESD(GIDX)
              CALL FIGWEK(CDC-WEEK_OFFSET,WEEK,YEAR2)
              MESTAB(IND+0) = MOD(YEAR2,100)
              MESTAB(IND+1) = WEEK
              IND = IND + 2

           ENDIF
C
C # WINNING NUMBERS, # BONUS NUMBERS
C
           I4TEMP = LTSRWS(GIDX) 
           MESTAB(IND) = I1TEMP(1)
           IND = IND + 1
C
C TOTO SELECT WINNING NUMBERS
C
           LEN = LTSRWS(GIDX)
           IF(MOD(LEN,2).NE.0) LEN = LEN + 1
           DO 810 I=1,LEN,2
              WIN1 = LTSWIN(I,GIDX)
              IF(LTSSTA(I,GIDX).EQ.GAMCAN) WIN1 = ROWCAN
              IF(LTSSTA(I,GIDX).EQ.GAMBFD) WIN1 = ROWCLS
              IF(LTSSTA(I,GIDX).EQ.GAMREF) WIN1 = ROWREF
              IF(I+1.GT.LTSRWS(GIDX)) THEN
                 WIN2 = 0
              ELSE
                 WIN2 = LTSWIN(I+1,GIDX)
                 IF(LTSSTA(I+1,GIDX).EQ.GAMCAN) WIN2 = ROWCAN
                 IF(LTSSTA(I+1,GIDX).EQ.GAMBFD) WIN2 = ROWCLS
                 IF(LTSSTA(I+1,GIDX).EQ.GAMREF) WIN2 = ROWREF
              ENDIF
              WINNER = ISHFT(WIN1,4) + WIN2
              MESTAB(IND) = ZEXT(WINNER)
              IND = IND + 1
810        CONTINUE
C
C START CDC DATE
C
           I4TEMP = LTSBSD(GIDX)
           MESTAB(IND+0) = I1TEMP(2)
           MESTAB(IND+1) = I1TEMP(1)
           IND = IND + 2
C
C END CDC DATE
C
           I4TEMP = LTSESD(GIDX)
           MESTAB(IND+0) = I1TEMP(2)
           MESTAB(IND+1) = I1TEMP(1)
           IND = IND + 2

        ENDDO

C
C SET KICKER NUMBER (IF PRESENT)
C
        IF(KGNUM.GT.0) THEN
           I4TEMP = LKKWIN(GIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDIF
C
        OUTLEN = IND - 1
        GOTO 9000
C
C BUILD RESULTS GAME TYPE RESULTS REPORT
C
900     CONTINUE
        IF(LTGSTS(GIDX).LT.GAMENV) THEN
           TRABUF(TERR)=RNIN
           GOTO 10000
        ENDIF
C
C SETS OF WINNING NUMBERS SENT
C
        MESTAB(IND) = 1
        IND = IND + 1
C
C SHARE BITMAP 
C
        I4TEMP = 0
        IF(LTGSTS(GIDX).GE.GAMENV) I4TEMP = '01'X
        IF(LTGSTS(GIDX).GE.GFINAL) I4TEMP = '40'X
        IF(LTGSTS(GIDX).EQ.GAMCAN) I4TEMP = '04'X
        IF(LTGSTS(GIDX).LE.GAMBFD) I4TEMP = '08'X
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C MESSAGE OPTION BITMAP
C
        I4TEMP = 0
        IF(KGNUM.GT.0) I4TEMP = '80'X           !KICKER NUMBER PRESENT
        I4TEMP = I4TEMP + '08'X                 !DIVISIONS PRESENT
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C NUMBER OF DIVISIONS
C
        I4TEMP = LTTDIV(GIDX)
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C BIT MAP OF DIVISIONS SENT
C
        I4TEMP = 0
        DO I=1,LTTDIV(GIDX)
           IF(LTGMAT(I,GIDX).NE.0) I4TEMP = I4TEMP + ALLDIV(I)
	ENDDO
        DIVBITMAP = I4TEMP
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND = IND +2
C
C FREE FOR ESTIMATED / FREE BIT MAP
C
        ESTPRZPOS = IND
        IND = IND + 2
C
C CDC DATE
C
        I4TEMP = LTGDAT(CURDRW, GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
C WEEK NUMBER
C
        CDC = LTGESD(GIDX)
        CALL FIGWEK(CDC,WEEK,YEAR2)
        MESTAB(IND+0) = MOD(YEAR2, 100)
        MESTAB(IND+1) = WEEK
        IND = IND + 2
C
C NEXT BYTE HAS # WINNING NUMBERS, # BONUS NUMBERS
C
        I4TEMP = ISHFT(LTGMAX(GIDX),4) + 0
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C SPORTS WINNING NUMBERS
C
        LEN=LTGMAX(GIDX)
        DO I=1,LEN
           WIN1=LTGWIN(1,I,GIDX)
           WIN2=LTGWIN(2,I,GIDX)
           WINNER=ISHFT(WIN1,4)+WIN2
           MESTAB(IND) = ZEXT( WINNER )
           IND=IND+1
	ENDDO
C
C SET KICKER NUMBER
C
        IF(KGNUM.GT.0) THEN
           I4TEMP = LKKWIN(KGIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND=IND+4
        ENDIF
C
C SET SHARE COUNT AND AMOUNT
C
        SHRPOS = IND
        DO I=1,LTTDIV(GIDX)
           IF(LTGMAT(I,GIDX).NE.0) THEN
             I4TEMP = LTGTSR(I,GIDX)
             MESTAB(IND+0) = I1TEMP(4)
             MESTAB(IND+1) = I1TEMP(3)
             MESTAB(IND+2) = I1TEMP(2)
             MESTAB(IND+3) = I1TEMP(1)
             IND=IND+4
C
             I4TEMP = LTGSHV(I,GIDX)
             MESTAB(IND+0) = I1TEMP(4)
             MESTAB(IND+1) = I1TEMP(3)
             MESTAB(IND+2) = I1TEMP(2)
             MESTAB(IND+3) = I1TEMP(1)
             IND=IND+4
           ENDIF
	ENDDO
C
C SET ESTIMATED / FINAL PRIZE BIT MAP
C
        DRAWCDC = LTGDAT(CURDRW, GIDX)
        CALL ESTPRIZES(MESTAB, SHRPOS, DRAWCDC, DIVBITMAP, ESTPRZPOS)
C
C END FOR RESULTS GAMES
C
        OUTLEN=IND - 1
        GOTO 9000
C
C
C BUILD Super SCORE GAME TYPE RESULTS REPORT
C
1100    CONTINUE
        IF(LSSSTS(GIDX).LT.GAMENV) THEN
           TRABUF(TERR)=RNIN
           GOTO 10000
        ENDIF
C
C SETS OF WINNING NUMBERS SENT
C
        MESTAB(IND) = 1
        IND = IND + 1
C
C SHARE BITMAP 
C
        I4TEMP = 0
        IF(LSSSTS(GIDX).EQ.GAMDON) I4TEMP = I4TEMP + '80'X
        IF(LSSSTS(GIDX).EQ.GFINAL) I4TEMP = I4TEMP + '40'X
        IF(LSSSTS(GIDX).EQ.GAMCAN) I4TEMP = I4TEMP + '04'X
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C MESSAGE OPTION BITMAP
C
        I4TEMP = 0
        IF(KGNUM.GT.0) I4TEMP = I4TEMP + '80'X
        I4TEMP = I4TEMP + '20'X                 !WINNING ODDS PRESENT
        I4TEMP = I4TEMP + '10'X                 !WINNING SCORE PRESENT
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C CDC DATE
C
        I4TEMP = LSSDAT(GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND = IND + 2
C
C WEEK NUMBER
C
        CDC = LSSDAT(GIDX)
        CALL FIGWEK(CDC-WEEK_OFFSET,WEEK,YEAR2)
        MESTAB(IND+0) = MOD(YEAR2,100)
        MESTAB(IND+1) = WEEK
        IND = IND + 2
C
C # WINNING SETS , # BONUS NUMBERS
C       
        TOT_WIN = 0
C
        IF(LSSSTS(GIDX).NE.GAMCAN.AND.LSSSTS(GIDX).NE.GAMREF) THEN
          IF(LSSEST(1,GIDX).NE.GAMNUL) TOT_WIN = TOT_WIN + 1
          IF(LSSEST(2,GIDX).NE.GAMNUL) TOT_WIN = TOT_WIN + 1
          IF(LSSEST(3,GIDX).NE.GAMNUL) TOT_WIN = TOT_WIN + 1
        ENDIF
C
        MESTAB(IND+0) = ISHFT(TOT_WIN,4) ! no bonus numbers for TSSC
        IND = IND + 1
C
C LENGTH OF MASTER EVENT NAME
C
        MESTAB(IND) = SSNMS_LEN
        IND = IND + 1
C
C MASTER EVENT NAME
C
        CALL MOVBYT(LSSMNM(1,GIDX),1,MESTAB,IND,SSNMS_LEN)
        IND = IND + SSNMS_LEN
C
C Send the length of the set names and set names
C
        DO I=1,3
          IF(LSSEST(I,GIDX).NE.GAMNUL) THEN
             LENGTH = SSNMS_LEN
             MESTAB(IND) = LENGTH
             IND = IND + 1
             CALL MOVBYT(LSSSNM(1,I,GIDX),1,MESTAB,IND,LENGTH)
             IND = IND + LENGTH
          ENDIF
        ENDDO
C
C Send set scores if set status != GAMNUL
C  
        DO I=1,3
          IF(LSSEST(I,GIDX).NE.GAMNUL) THEN
            MESTAB(IND) = LSSWIN(1,I,GIDX)
            IND = IND + 1
            MESTAB(IND) = LSSWIN(2,I,GIDX)
            IND = IND + 1
          ENDIF
        ENDDO
C
C WINNING ODDS
C
        I4TEMP = LSSODS(GIDX)
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND = IND + 4
C
C SET KICKER NUMBER (IF PRESENT)
C
        IF(KGNUM.GT.0) THEN
           I4TEMP = LKKWIN(KGIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDIF
C
        OUTLEN = IND - 1
        GOTO 9000
C
C PASSIVE RESULTS
C
1600    CONTINUE
        IF  (LPASTS(INDEMI,GIDX).LT.GAMENV) THEN
            TRABUF(TERR) = RNIN
            GOTO 10000
        ENDIF
C
C SETS OF WINNING NUMBERS SENT (1)
C
        MESTAB(IND) = 1
        IND = IND + 1
C
C DRAW CDC DATE
C
        CALL HOST_TO_TERM(MESTAB(IND), LPAESD(INDEMI, GIDX), 2)
        IND = IND + 2
C
C WEEK AND YEAR
C
	I4TEMP = LPADRAW(INDEMI,GIDX)
	CALL GETPASDRW(I4TEMP,WEEK,YEAR)
C
        MESTAB(IND) = WEEK
        IND = IND + 1
        MESTAB(IND) = (YEAR - 2000)      ! YEAR IS IN YYYY FORMAT
        IND = IND + 1
C
C SEND WINNING FRACTION (IF POP. EMISSION OR CLASSIC SPECIAL)
C
        IF  ( GIDX.EQ.PSBPOP                                      .OR.
     *        (GIDX.EQ.PSBCLA .AND. LPAEMT(INDEMI,GIDX).EQ.EM_EXT)    ) THEN
            MESTAB(IND) = LPAWSER(INDEMI,GIDX)
        ELSE
            MESTAB(IND) = 0
        ENDIF
        IND = IND + 1
C
C SEND WINNING NUMBERS WITH 5 DIGITS, WINNING SEQUENCE NUMBERS WITH 4 DIGITS,
C WINNING SEQUENCE NUMBERS WITH 3, AND WINNING SEQUENCE NUMBERS WITH 2 DIGITS
C
      DO 6000 FIDX = SIZEOF(FATOR)/4, 2, -1
C
C SET PRIZE TYPE AND HOW MANY BYTES TO SEND TO TERMINAL
C
        PRZTYP = PR_SEQ  !"SEQUENCIA" PRIZE TYPE FOR WINNING NUMBERS WITH 4, 3 AND 2 DIGITS
        NUMOFBYTES = 2   !2 BYTES FOR WINNING NUMBERS WITH 4, 3 AND 2 DIGITS
        IF(FIDX .EQ. SIZEOF(FATOR)/4) THEN
          PRZTYP = PR_DIG  !"DIGITOS" PRIZE TYPE FOR WINNING NUMBERS WITH 5 DIGITS
          NUMOFBYTES = 4   !4 BYTES FOR WINNING NUMBERS WITH 5 DIGITS
        ENDIF
C
C REMEMBER POSITION FOR NUMBER OF WINNERS
C
        NUMWIN = 0
        NUMWINPOS = IND
        IND = IND + 1
C
C WINNING NUMBERS
C
         DO 5000 INDPAS = 1, PAGDIV
           NUMOFWINS = LPAWNUM(INDPAS, INDEMI, GIDX)
           IF(NUMOFWINS .EQ. 0) GOTO 5000 
           DIVTYP = LPATYP(INDPAS, INDEMI, GIDX)
           NUMDIG = LPADIG(INDPAS, INDEMI, GIDX)
           IF(DIVTYP .EQ. PRZTYP .AND. NUMDIG .EQ. FATOR(FIDX)) THEN
             NUMWIN = NUMWIN + NUMOFWINS
             DO I = 1, NUMOFWINS
               I4TEMP = LPAWIN(I, INDPAS, INDEMI, GIDX)
               CALL HOST_TO_TERM(MESTAB(IND), I4TEMP, NUMOFBYTES)
               IND = IND + NUMOFBYTES
             ENDDO
           ENDIF
5000     CONTINUE
C
C SED NUMBER OF WINNERS NUMBERS SEND TO TERMINAL
C
        MESTAB(NUMWINPOS) = NUMWIN
C
6000  CONTINUE
C
C SET PASSIVE END OF REPORT
C
        OUTLEN = IND - 1
        GOTO 9000
C
C BUILD BINGO GAME TYPE RESULTS REPORT
C
1200    CONTINUE
        IF(LBNSTS(GIDX).LT.GAMENV) THEN
           TRABUF(TERR)=RNIN
           GOTO 10000
        ENDIF
C
C SETS OF WINNING NUMBERS SENT
C
        IF(LBNDIV(BGOBAB,GIDX).NE.0) THEN
           TRABUF(TERR)=RNIN                 !old AB-bingo not sent
           GOTO 10000
        ELSEIF(LBNNSP(GIDX).NE.0) THEN
           MESTAB(IND) = 5                   !5-phase Bingo
        ELSE
           MESTAB(IND) = 2                   !2-phase Bingo
        ENDIF
        IND = IND + 1
C
C BINGO FULL HOUSE, PHASE 1
C -------------------------
C FIND DIVISION NUMBERS FOR LOTTERY LIST OF DIVISIONS
C
        CALL FASTSET(0,FHLOTD,BGOLOT)
        DO I=1,BGODIV
           IF(LBNDNR(I,GIDX).NE.0) FHLOTD(LBNDNR(I,GIDX))=I
        ENDDO
C
C LOOP OVER SUBPHASES
C
        IF(LBNNSP(GIDX).GT.0) THEN
           BSUBPH=LBNNSP(GIDX)-1
        ELSE
           BSUBPH=1
        ENDIF 

        DO SPH=1,BSUBPH
C
C SHARE BITMAP
C
           I4TEMP = 0
           IF(LBNSTS(GIDX).GE.GAMENV) I4TEMP = '01'X
           IF(LBNSTS(GIDX).GE.GFINAL) I4TEMP = '40'X
           IF(LBNSTS(GIDX).EQ.GAMCAN) I4TEMP = '04'X
           IF(LBNSTS(GIDX).LE.GAMBFD) I4TEMP = '08'X
           MESTAB(IND) = I1TEMP(1)
           IND = IND + 1
C
C MESSAGE OPTION BITMAP
C
           I4TEMP = 0
           IF(KGNUM.GT.0) I4TEMP = '80'X           !KICKER NUMBER PRESENT
           I4TEMP = I4TEMP + '08'X                 !DIVISIONS PRESENT
           MESTAB(IND) = I1TEMP(1)
           IND = IND + 1
C
C NUMBER OF BITMAP-MATCH DIVISIONS
C
           DIVMAP = 0
           IF(BSUBPH.EQ.1) THEN
              DO I=1,12
                 IF(FHLOTD(I).NE.0) DIVMAP = DIVMAP + FHDIV(I)
              ENDDO
           ELSE
              DO I=2*SPH-1,2*SPH
                 IF(FHLOTD(I).NE.0) DIVMAP = DIVMAP + FHDIV(I)
              ENDDO
           ENDIF
           CALL BITCNT(DIVMAP,4,I4TEMP)
           MESTAB(IND) = I1TEMP(1)
           IND = IND + 1
C
C BIT MAP OF DIVISIONS SENT
C
           I4TEMP = DIVMAP 
           MESTAB(IND+0) = I1TEMP(2)
           MESTAB(IND+1) = I1TEMP(1)
           IND = IND +2
C
C CDC DATE
C
           I4TEMP = LBNDAT(1,GIDX)
           MESTAB(IND+0) = I1TEMP(2)
           MESTAB(IND+1) = I1TEMP(1)
           IND=IND+2
C
C WEEK NUMBER
C
           CDC = LBNDAT(1,GIDX)
           CALL FIGWEK(CDC,WEEK,YEAR2)
           MESTAB(IND+0) = MOD(YEAR2,100)
           MESTAB(IND+1) = WEEK
           IND = IND + 2
C
C NEXT BYTE HAS # WINNING NUMBERS
C
           IF(BSUBPH.EQ.1) THEN
              I4TEMP = LBNPHS(1,GIDX)        !number of #'s in phase 1
           ELSE
              I4TEMP = LBNSPH(SPH,GIDX)	     !number of #'s in subphase SPH 
	      IF(SPH.GT.1) I4TEMP = I4TEMP - LBNSPH(SPH-1,GIDX)
           ENDIF
           MESTAB(IND) = I1TEMP(1)
           IND = IND + 1
C
C BINGO WINNING NUMBERS
C
           L=1
           IF(BSUBPH.EQ.1) THEN
              LEN=LBNPHS(1,GIDX)                   !#'s in phase 1
           ELSE
              LEN=LBNSPH(SPH,GIDX)                 !#'s in subphase SPH   
              IF(SPH.GT.1) L=LBNSPH(SPH-1,GIDX)+1
           ENDIF
           DO I=L,LEN
              WIN1=LBNWIN(I,GIDX)
              MESTAB(IND) = ZEXT( WIN1 )
              IND=IND+1
           ENDDO
C
C SET KICKER NUMBER
C
           IF(KGNUM.GT.0) THEN
              I4TEMP = LKKWIN(KGIDX)
              MESTAB(IND+0) = I1TEMP(4)
              MESTAB(IND+1) = I1TEMP(3)
              MESTAB(IND+2) = I1TEMP(2)
              MESTAB(IND+3) = I1TEMP(1)
              IND=IND+4
           ENDIF
C
C SET SHARE COUNT AND AMOUNT
C
           DO I=1,12             
              IF(FHLOTD(I).GT.0) THEN
                 IF(BSUBPH.EQ.1 .OR.
     *              BSUBPH.GT.1 .AND. (I.EQ.2*SPH-1.OR.I.EQ.2*SPH)) THEN
C
                    I4TEMP = LBNSHR(FHLOTD(I),BGOFHS,GIDX)
                    MESTAB(IND+0) = I1TEMP(4)
                    MESTAB(IND+1) = I1TEMP(3)
                    MESTAB(IND+2) = I1TEMP(2)
                    MESTAB(IND+3) = I1TEMP(1)
                    IND=IND+4
C
                    I4TEMP = LBNSHV(FHLOTD(I),BGOFHS,GIDX)
                    MESTAB(IND+0) = I1TEMP(4)
                    MESTAB(IND+1) = I1TEMP(3)
                    MESTAB(IND+2) = I1TEMP(2)
                    MESTAB(IND+3) = I1TEMP(1)
                    IND=IND+4
C
                 ENDIF
              ENDIF
           ENDDO

        ENDDO                                      !end of phase1 
C
C BINGO FULL HOUSE, PHASE 2
C -------------------------
C
C SHARE BITMAP
C
        I4TEMP = 0
        IF(LBNSTS(GIDX).GE.GAMENV) I4TEMP = '01'X
        IF(LBNSTS(GIDX).GE.GFINAL) I4TEMP = '40'X
        IF(LBNSTS(GIDX).EQ.GAMCAN) I4TEMP = '04'X
        IF(LBNSTS(GIDX).LE.GAMBFD) I4TEMP = '08'X
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C MESSAGE OPTION BITMAP
C
        I4TEMP = 0
        IF(KGNUM.GT.0) I4TEMP = '80'X           !KICKER NUMBER PRESENT
        I4TEMP = I4TEMP + '08'X                 !DIVISIONS PRESENT
        IF(LBNMAT(BGOMAXMAP+1,BGOFHS,GIDX).NE.0)
     *     I4TEMP = I4TEMP + '02'X              !WORST #'S DRAWN PRESENT
        IF(LBNMAT(BGOMAXMAP+2,BGOFHS,GIDX).NE.0)
     *     I4TEMP = I4TEMP + '01'X              !SECOND WORST #'S DRAWN PRESENT
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C NUMBER OF DIVISIONS
C
        DIVMAP = 0
        DO I=13,20
           IF(FHLOTD(I).NE.0) DIVMAP = DIVMAP + FHDIV(I-12)
        ENDDO
        CALL BITCNT(DIVMAP,4,I4TEMP)
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C BIT MAP OF DIVISIONS SENT
C
        I4TEMP = DIVMAP
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND = IND +2
C
C NUMBERS DRAWN FOR WORST
C
        IF(LBNMAT(BGOMAXMAP+1,BGOFHS,GIDX).NE.0) THEN
           I4TEMP = LBNWST(GIDX)
           MESTAB(IND) = I1TEMP(1)
           IND = IND + 1
        ENDIF
C
C NUMBERS DRAWN FOR SECOND WORST
C
        IF(LBNMAT(BGOMAXMAP+2,BGOFHS,GIDX).NE.0) THEN
           I4TEMP = LBNWS2(GIDX)
           MESTAB(IND) = I1TEMP(1)
           IND = IND + 1
        ENDIF  
C
C CDC DATE
C
        I4TEMP = LBNDAT(1,GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
C WEEK NUMBER
C
        CDC = LBNDAT(1,GIDX)
        CALL FIGWEK(CDC,WEEK,YEAR2)
        MESTAB(IND+0) = MOD(YEAR2,100)
        MESTAB(IND+1) = WEEK
        IND = IND + 2
C
C NEXT BYTE HAS # WINNING NUMBERS
C
        IF(BSUBPH.EQ.1) THEN
           L=LBNPHS(1,GIDX)
           LEN=LBNPHS(2,GIDX) - L             !number of #'s in phase 2
        ELSE
           L=LBNSPH(LBNNSP(GIDX)-1,GIDX)
           LEN = LBNSPH(LBNNSP(GIDX),GIDX) - L
        ENDIF
        I4TEMP = LEN
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C BINGO WINNING NUMBERS
C
        IF(LEN.GT.0) THEN
           DO I=L+1,L+LEN
              WIN1=LBNWIN(I,GIDX)
              MESTAB(IND) = ZEXT( WIN1 )
              IND=IND+1
           ENDDO
        ENDIF
C
C SET KICKER NUMBER
C
        IF(KGNUM.GT.0) THEN
           I4TEMP = LKKWIN(KGIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND=IND+4
        ENDIF
C
C SET SHARE COUNT AND AMOUNT
C
        DO I=13,20             
           IF(FHLOTD(I).NE.0) THEN
              I4TEMP = LBNSHR(FHLOTD(I),BGOFHS,GIDX)
              MESTAB(IND+0) = I1TEMP(4)
              MESTAB(IND+1) = I1TEMP(3)
              MESTAB(IND+2) = I1TEMP(2)
              MESTAB(IND+3) = I1TEMP(1)
              IND=IND+4
C
              I4TEMP = LBNSHV(FHLOTD(I),BGOFHS,GIDX)
              MESTAB(IND+0) = I1TEMP(4)
              MESTAB(IND+1) = I1TEMP(3)
              MESTAB(IND+2) = I1TEMP(2)
              MESTAB(IND+3) = I1TEMP(1)
              IND=IND+4
           ENDIF
        ENDDO
C
C
        OUTLEN=IND - 1
        GOTO 9000

C
C BUILD TODAYS COUPLE TYPE RESULTS REPORT
C
1300    CONTINUE
        NAME_LEN = CPLNMS_LEN - 2
        IF(LCPSTS(GIDX).LT.GAMENV) THEN
           TRABUF(TERR) = RNIN
           GOTO 10000
        ENDIF
C
C SETS OF WINNERS
C
        TOT_WIN = LCPCMB(GIDX)
        MESTAB(IND) = TOT_WIN
        IND = IND + 1

C
C SHARE BITMAP
C
        I4TEMP = 0
        IF(LCPSTS(GIDX).EQ.GAMENV) I4TEMP = I4TEMP + '80'X
        IF(LCPSTS(GIDX).GE.GFINAL) I4TEMP = I4TEMP + '40'X
        IF(LCPSTS(GIDX).EQ.GAMCAN) I4TEMP = I4TEMP + '04'X
        IF(LCPSTS(GIDX).LE.GAMBFD) I4TEMP = I4TEMP + '08'X
        IF(TOT_WIN.EQ.0) I4TEMP = I4TEMP + '02'X   !NO WINNERS
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C MESSAGE OPTION BITMAP
C
        I4TEMP = 0
        IF(KGNUM.GT.0) I4TEMP = I4TEMP + '80'
        IF(TOT_WIN.GT.0)  THEN
           I4TEMP = I4TEMP + '40'X                     !WINNING ROWS PRESENT
           I4TEMP = I4TEMP + '20'X                     !WINNING ODDS PRESENT
           I4TEMP = I4TEMP + '04'X                     !TEXT PRESENT
        ENDIF
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C CDC DATE
C
        I4TEMP = LCPDAT(GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND = IND + 2
C
C WEEK NUMBER
C
        CDC = LCPDAT(GIDX)
        CALL FIGWEK(CDC-WEEK_OFFSET,WEEK,YEAR2)
        MESTAB(IND+0) = MOD(YEAR2,100)
        MESTAB(IND+1) = WEEK
        IND = IND + 2
C
C # WINNERS SENT
C
        NR_WIN_IND = IND
        IND = IND + 1
C
C MORE FLAG
C
        MESTAB(IND) = 'FF'X ! LAST SEGMENT
        FRST_WIN = MAX(SEGNO*6,1)
        IF(SEGNO.EQ.0) THEN
            LAST_WIN = MIN(5,TOT_WIN)
        ELSE
            LAST_WIN = MIN(FRST_WIN+5,TOT_WIN)
        ENDIF
        IF(LCPSTS(GIDX).NE.GAMCAN.AND.LCPSTS(GIDX).NE.GAMREF) THEN
            IF(FRST_WIN.GT.TOT_WIN) THEN
               TRABUF(TERR)=INVL
               GOTO 10000
            ENDIF
        ENDIF
        IF(LAST_WIN.LT.TOT_WIN) MESTAB(IND) = '80'X ! MORE SEGMENTS...

        I4TEMP = LAST_WIN-FRST_WIN+1
        MESTAB(NR_WIN_IND) = ISHFT(I4TEMP,4)

        IF(LCPSTS(GIDX).EQ.GAMREF.OR.LCPSTS(GIDX).EQ.GAMCAN) THEN
            MESTAB(IND) = 'FF'X
            MESTAB(NR_WIN_IND) = 0
        ENDIF
        IND = IND + 1
       IF(SEGNO.GT.0) THEN
            MESTAB(IND) = 0
            IND = IND + 1
            MESTAB(IND) = 0
            IND = IND + 1
            MESTAB(IND) = 0
            IND = IND + 1
            MESTAB(IND) = 0
            IND = IND + 1
            MESTAB(IND) = NAME_LEN
            IND = IND + 1
            MESTAB(IND) = NAME_LEN
            IND = IND + 1
            GOTO 1310
        ENDIF
C
C EVENT 1 NAME FIELD LENGTH
C
        MESTAB(IND) = CPLENM_LEN
        IND = IND + 1
C
C EVENT 1 DESCRIPTION FIELD LENGTH
C
C***    MESTAB(IND) = CPLDES_LEN   !do not send to terminal
        MESTAB(IND) = 0
        IND = IND + 1

C
C EVENT 2 NAME FIELD LENGTH
C
        MESTAB(IND) = CPLENM_LEN
        IND = IND + 1
C
C EVENT 2 DESCRIPTION FIELD LENGTH
C
C***    MESTAB(IND) = CPLDES_LEN   !do not send to terminal
        MESTAB(IND) = 0
        IND = IND + 1
C
C EVENT 1 WINNING ROW FIELD LENGTH
C
        MESTAB(IND) = NAME_LEN
        IND = IND + 1

C
C EVENT 2 WINNING ROW FIELD LENGTH
C
        MESTAB(IND) = NAME_LEN
        IND = IND + 1
C
C EVENT 1 NAME
C
        CALL MOVBYT(LCPENM(1,1,GIDX),1,MESTAB,IND,CPLENM_LEN)
        IND = IND + CPLENM_LEN
C
C EVENT 1 DESCRIPTION
C
C***    CALL MOVBYT(LCPDES(1,1,GIDX),1,MESTAB,IND,CPLDES_LEN)
C***    IND = IND + CPLDES_LEN

C
C EVENT 2 NAME
C
        CALL MOVBYT(LCPENM(1,2,GIDX),1,MESTAB,IND,CPLENM_LEN)
        IND = IND + CPLENM_LEN
C
C EVENT 2 DESCRIPTION
C
C***    CALL MOVBYT(LCPDES(1,2,GIDX),1,MESTAB,IND,CPLDES_LEN)
C***    IND = IND + CPLDES_LEN
C
C TODAYS COUPLE WINNING ROW NUMBER AND ODDS
C
1310    CONTINUE
        IF(MESTAB(NR_WIN_IND).EQ.0) GOTO 1320
C
        DO I=FRST_WIN,LAST_WIN
           RW1 = LCPWIN(1,I,GIDX)            !---- Winner Event 1
           RW2 = LCPWIN(2,I,GIDX)            !---- Winner Event 2
           MESTAB(IND) = RW1
           IND = IND + 1
           MESTAB(IND) = RW2-MAXCPLRW/2
           IND = IND + 1
           CALL MOVBYT(LCPNMS(1,RW1,GIDX),1,MESTAB,IND,NAME_LEN)
           IND = IND + NAME_LEN
           CALL MOVBYT(LCPNMS(1,RW2,GIDX),1,MESTAB,IND,NAME_LEN)
           IND = IND + NAME_LEN
           I4TEMP = LCPODS(I,GIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDDO
C
1320    CONTINUE
C
C KICKER NUMBER (IF PRESENT)
C
        IF(KGNUM.GT.0) THEN
           I4TEMP = LKKWIN(KGIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDIF
C
        OUTLEN = IND -1
        GOTO 9000 

C
C BUILD Super Triple TYPE RESULTS REPORT
C
1400    CONTINUE

        IF(LSTSTS(GIDX).LT.GAMENV) THEN
           TRABUF(TERR) = RNIN
           GOTO 10000
        ENDIF
C
C SETS OF WINNERS
C
        TOT_WIN = LSTCMB(GIDX)
        MESTAB(IND) = TOT_WIN
        IND = IND + 1
C
C SHARE BITMAP
C
        I4TEMP = 0
        IF(LSTSTS(GIDX).EQ.GAMENV) I4TEMP = I4TEMP + '80'X
        IF(LSTSTS(GIDX).EQ.GFINAL) I4TEMP = I4TEMP + '40'X
        IF(LSTSTS(GIDX).EQ.GAMCAN.OR.LSTSTS(GIDX).EQ.GAMREF) 
     *          I4TEMP = I4TEMP + '04'X
        IF(LSTSTS(GIDX).LE.GAMBFD) I4TEMP = I4TEMP + '08'X
        IF(TOT_WIN.EQ.0) I4TEMP = I4TEMP + '02'X   !NO WINNERS
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C MESSAGE OPTION BITMAP
C
        I4TEMP = 0
        IF(KGNUM.GT.0) I4TEMP = I4TEMP + '80'
        IF(TOT_WIN.GT.0)  THEN
           I4TEMP = I4TEMP + '40'X                     !WINNING ROWS PRESENT
           I4TEMP = I4TEMP + '20'X                     !WINNING ODDS PRESENT
           I4TEMP = I4TEMP + '04'X                     !TEXT PRESENT
        ENDIF
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C CDC DATE
C
        I4TEMP = LSTDAT(GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND = IND + 2
C
C WEEK NUMBER
C
        CDC = LSTDAT(GIDX)
        CALL FIGWEK(CDC-WEEK_OFFSET,WEEK,YEAR2)
        MESTAB(IND+0) = MOD(YEAR2,100)
        MESTAB(IND+1) = WEEK
        IND = IND + 2
C
C # WINNERS SENT
C
        NR_WIN_IND = IND
        IND = IND + 1
C
C MORE FLAG
C
        MESTAB(IND) = 'FF'X ! LAST SEGMENT
        FRST_WIN = MAX(SEGNO*4,1)
        IF(SEGNO.EQ.0) THEN
            LAST_WIN = MIN(3,TOT_WIN)
        ELSE
            LAST_WIN = MIN(FRST_WIN+3,TOT_WIN)
        ENDIF
        IF(LSTSTS(GIDX).NE.GAMCAN.AND.LSTSTS(GIDX).NE.GAMREF) THEN
            IF(FRST_WIN.GT.TOT_WIN) THEN
               TRABUF(TERR)=INVL
               GOTO 10000
            ENDIF
        ENDIF
        IF(LAST_WIN.LT.TOT_WIN) MESTAB(IND) = '80'X ! MORE SEGMENTS...
        I4TEMP = LAST_WIN-FRST_WIN+1
        MESTAB(NR_WIN_IND) = ISHFT(I4TEMP,4)
        IF(LSTSTS(GIDX).EQ.GAMREF.OR.LSTSTS(GIDX).EQ.GAMCAN) THEN
            MESTAB(IND) = 'FF'X
            MESTAB(NR_WIN_IND) = 0
        ENDIF
        IND = IND + 1
        IF(SEGNO.GT.0) THEN
            MESTAB(IND) = 0
            IND = IND + 1
            MESTAB(IND) = 0
            IND = IND + 1
            MESTAB(IND) = STRNMS_LEN
            IND = IND + 1
            MESTAB(IND) = STRNMS_LEN
            IND = IND + 1
            MESTAB(IND) = STRNMS_LEN
            IND = IND + 1
            GOTO 1410
        ENDIF
C
C EVENT NAME FIELD LENGTH
C
        MESTAB(IND) = STRENM_LEN
        IND = IND + 1
C
C EVENT DESCRIPTION FIELD LENGTH
C
C        MESTAB(IND) = STRDES_LEN  !do not send to terminal
        MESTAB(IND) = 0
        IND = IND + 1
C
C EVENT ROW 1 FIELD LENGTH
C
        MESTAB(IND) = STRNMS_LEN
        IND = IND + 1
C
C EVENT ROW 2 FIELD LENGTH
C
        MESTAB(IND) = STRNMS_LEN
        IND = IND + 1
C
C EVENT ROW 3 FIELD LENGTH
C
        MESTAB(IND) = STRNMS_LEN
        IND = IND + 1
C
C EVENT NAME
C
        CALL MOVBYT(LSTENM(1,GIDX),1,MESTAB,IND,STRENM_LEN)
        IND = IND + STRENM_LEN
C
C EVENT DESCRIPTION
C
C        CALL MOVBYT(LSTDES(1,GIDX),1,MESTAB,IND,STRDES_LEN)
C        IND = IND + STRDES_LEN
C
C SUPER TRIPLE WINNING ROW NUMBER AND ODDS
C
1410    CONTINUE
        IF(MESTAB(NR_WIN_IND).EQ.0) GOTO 1420
C
C Competitor names 
C       
        DO I=FRST_WIN,LAST_WIN
           RW1 = LSTWIN(1,I,GIDX)   ! FIRST
           RW2 = LSTWIN(2,I,GIDX)   ! SECOND    
           RW3 = LSTWIN(3,I,GIDX)   ! THIRD
           MESTAB(IND) = RW1
           IND = IND + 1
           MESTAB(IND) = RW2
           IND = IND + 1
           MESTAB(IND) = RW3
           IND = IND + 1
           CALL MOVBYT(LSTNMS(1,RW1,GIDX),1,MESTAB,IND,STRNMS_LEN)
           IND = IND + STRNMS_LEN
           CALL MOVBYT(LSTNMS(1,RW2,GIDX),1,MESTAB,IND,STRNMS_LEN)
           IND = IND + STRNMS_LEN
           CALL MOVBYT(LSTNMS(1,RW3,GIDX),1,MESTAB,IND,STRNMS_LEN)
           IND = IND + STRNMS_LEN
           I4TEMP = LSTODS(I,GIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4                   
        ENDDO
1420    CONTINUE
C
C KICKER NUMBER (IF PRESENT)
C
        IF(KGNUM.GT.0) THEN
           I4TEMP = LKKWIN(KGIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDIF
C
        OUTLEN = IND -1
        GOTO 9000 
C
C BUILD Todays Trio TYPE RESULTS REPORT
C
1500    CONTINUE

        IF(LTRSTS(GIDX).LT.GAMENV) THEN
           TRABUF(TERR) = RNIN
           GOTO 10000
        ENDIF
C
C SETS OF WINNERS
C
        TOT_WIN = LTRCMB(GIDX)
        MESTAB(IND) = TOT_WIN
        IND = IND + 1
C
C SHARE BITMAP
C
        I4TEMP = 0
        IF(LTRSTS(GIDX).EQ.GAMENV) I4TEMP = I4TEMP + '80'X
        IF(LTRSTS(GIDX).EQ.GFINAL) I4TEMP = I4TEMP + '40'X
        IF(LTRSTS(GIDX).EQ.GAMCAN.OR.LTRSTS(GIDX).EQ.GAMREF) 
     *          I4TEMP = I4TEMP + '04'X
        IF(LTRSTS(GIDX).LE.GAMBFD) I4TEMP = I4TEMP + '08'X
        IF(WIN1.EQ.0.AND.WIN2.EQ.0) I4TEMP = I4TEMP + '02'X   !NO WINNERS
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C MESSAGE OPTION BITMAP
C
        I4TEMP = 0
        IF(KGNUM.GT.0) I4TEMP = I4TEMP + '80'
        IF(WIN1.GT.0.OR.WIN2.GT.0)  THEN
           I4TEMP = I4TEMP + '40'X                     !WINNING ROWS PRESENT
           I4TEMP = I4TEMP + '20'X                     !WINNING ODDS PRESENT
           I4TEMP = I4TEMP + '04'X                     !TEXT PRESENT
        ENDIF
        MESTAB(IND) = I1TEMP(1)
        IND = IND + 1
C
C CDC DATE
C
        I4TEMP = LTRDAT(GIDX)
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND = IND + 2
C
C WEEK NUMBER
C
        CDC = LTRDAT(GIDX)
        CALL FIGWEK(CDC-WEEK_OFFSET,WEEK,YEAR2)
        MESTAB(IND+0) = MOD(YEAR2,100)
        MESTAB(IND+1) = WEEK
        IND = IND + 2
C
C # WINNERS SENT
C
        NR_WIN_IND = IND
        IND = IND + 1
C
C More Flag
C
        MESTAB(IND) = 'FF'X     ! last segment
        FRST_WIN = MAX(SEGNO*4-1,1)
        IF(SEGNO.EQ.0) THEN
            LAST_WIN = MIN(2,TOT_WIN)
        ELSE
            LAST_WIN = MIN(FRST_WIN+4-1,TOT_WIN)
        ENDIF
        IF(LTRSTS(GIDX).NE.GAMCAN.AND.LTRSTS(GIDX).NE.GAMREF) THEN
            IF(FRST_WIN.GT.TOT_WIN) THEN
               TRABUF(TERR)=INVL
               GOTO 10000
            ENDIF
        ENDIF
        IF(LAST_WIN.LT.TOT_WIN) MESTAB(IND) = '80'X  ! more segments...
        IND = IND + 1
        I4TEMP = LAST_WIN-FRST_WIN+1
        MESTAB(NR_WIN_IND) = ISHFT(I4TEMP,4)
        IF(LTRSTS(GIDX).EQ.GAMREF.OR.LTRSTS(GIDX).EQ.GAMCAN) THEN
            MESTAB(IND-1) = 'FF'X
            MESTAB(NR_WIN_IND) = 0
        ENDIF
C
        IF(SEGNO.GT.0) THEN
          MESTAB(IND) = 0
          IND = IND + 1
          DO I=1,3
            MESTAB(IND) = 0
            IND = IND + 1
          ENDDO
          MESTAB(IND) = TRPNMS_LEN
          IND = IND + 1
          GOTO 1510
        ENDIF
C
C MASTER EVENT NAME FIELD LENGTH
C
        MESTAB(IND) = TRPENM_LEN-1
        IND = IND + 1
C
C CONTESTS NAME FIELD LENGTH
C
        DO I=1,3
          IF(LTREST(I,GIDX).EQ.GAMNUL) THEN 
            LENX(I) = 0
          ELSE
            LENX(I) = TRPENM_LEN-1
          ENDIF
          MESTAB(IND) = LENX(I)
          IND = IND + 1
        ENDDO
C
C COMPETITOR NAME LENGTH
C
        MESTAB(IND) = TRPNMS_LEN
        IND = IND + 1
C
C Master event name
C
        CALL MOVBYT(LTRMNM(1,GIDX),1,MESTAB,IND,TRPENM_LEN-1)
        IND = IND + TRPENM_LEN-1
C
C Contest names
C
        DO I=1,3
            CALL MOVBYT(LTRENM(1,I,GIDX),1,MESTAB,IND,LENX(I))
            IND = IND + LENX(I)
        ENDDO

1510    CONTINUE
        IF(MESTAB(NR_WIN_IND).EQ.0) GOTO 1521
C
C Competitor names 
C       
        DO 1520 I=FRST_WIN,LAST_WIN
           MESTAB(IND) = LTRWIN(1,I,GIDX)
           IND = IND + 1
           MESTAB(IND) = LTRWIN(2,I,GIDX)
           IND = IND + 1
           MESTAB(IND) = LTRWIN(3,I,GIDX)
           IND = IND + 1
           DO K = 1, 3
              I4TEMP = LTRWIN(K,I,GIDX)
              IF(I4TEMP.GT.0) THEN
                 CALL MOVBYT(LTRNMS(1,I4TEMP,K,GIDX),1,MESTAB,IND,TRPNMS_LEN)
              ENDIF
              IND = IND + TRPNMS_LEN
           ENDDO
           I4TEMP = LTRODS(I,GIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4          
1520    CONTINUE
1521    CONTINUE
C
C KICKER NUMBER (IF PRESENT)
C
        IF(KGNUM.GT.0) THEN
           I4TEMP = LKKWIN(KGIDX)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDIF
C
        OUTLEN = IND -1
        GOTO 9000 
C
C ERROR IN REPORT REQUEST FROM TERMINAL
C
10000   CONTINUE
        TRABUF(TSTAT)=REJT
        MESTAB(2) = ERRTYP
        MESTAB(5) = TRABUF(TERR)
        OUTLEN=6
C
C CALCULATE CHECKSUM FOR MESSAGE BACK TO TERMINAL
C
9000    CONTINUE
        I4CCITT = TRABUF(TCHK)
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
        CHKLEN = OUTLEN - 1
        CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)

        RETURN
        END

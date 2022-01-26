C
C SUBROUTINE TRALOG
C
C V60 08-DEZ-2020 SCML New Terminals Project - Olimpo
C V59 04-MAR-2016 SCML M16 PROJECT: added new fields to EM transactions.
C V58 05-MAR-2014 SCML Added support to PLACARD Project - IGS
C V57 24-OCT-2013 SCML Added TIVMT,TIPLIDTYP,TINIBBA2,TINIBCD,TIPLCARD,TINIBBB
C                      TINIBBO,TINIBBA1,TINETPRZ and TIVDESCR to instant
C                      validation, regarding new bank validation mode.
C V56 23-SEP-2013 SCML TVPLIDTYP added
C V55 19-MAY-2011 FJG  FIX LOGBUF(16)
C V54 12-APR-2011 FJG  ACCENTURE MERGE FOR EM2
C V53 06-JAN-2011 FJG  MILLENNIUM MXSRV
C V52 22-NOV-2010 MAC  LUCKY NUMBER
C                 FJG  TWEMSER/TWEMCHK replaced by TWLNKSER/TWLNKCHK
C V51 11-NOV-2010 FJG  EUROMILLIONS VALIDATION PRIZE OVER 42M
C V50 14-APR-2010 RXK  Week,year added for ePassive, NIB and Player Card fixed
C V49 16-MAR-2010 RXK  Changes for ePassive
C V48 25-MAR-2009 MMO  ADDED TWEMSER/TWEMCHK JOKER/EM.
C V47 06-JUN-2005 FRP  Modify for IPS Distribution.
C V46 27-OCT-2003 FRP  Modify for Batch2 Totobola Changes.
C V43 02-MAY-2001 JHR  ADDED MONDAY FLAG ( TWLMFI )
C V42 20-MAR-2001 UXN  TCMD changed to be 2 log records.
C V41 15-FEB-2001 UXN  3 digit instant game # changes. IVAL changed.
C V40 14-FEB-2001 UXN  Game type/index flag removed.
C V39 29-NOV-2000 UXN  INCLUDED TTGL
C V38 15-JAN-2001 EPH  OP AMOUNT (TVOPPAY + TVKOPPAY)
C V37 12-DEC-2000 CS   ADDED PASSIVE LOTTERY.
C V36 29-SEP-2000 UXN  TAGTINF added. TVCWT added to validation record.
C                      IFSESON added.
C V35 28-FEB-2000 RXK  TWADDFW added for Lotto and Jokeri.
C V34 01-FEB-2000 UXN  TNFRAC added. Fractions changed. TGAMTYP2 removed.
C V33 13-OCT-1999 RXK  World Tour added.
C V32 07-JUN-1999 RXK  Fraction flag for oddset games set.
C V31 10-MAY-1999 UXN  Triple changed to Trio. TWTTBMAX changed from 24 to 27
C                      TWDBBMAX changed from 10 to 12, TWCPBMAX changed from
C                      10 to 12.
C                      SUPER TRIPLE ADDED.
C V30 18-MAR-1999 RXK  Gtyp+gind in 5+3 bits change.
C V29 05-FEB-1999 UXN  Jokeri game index checking added.
C V28 20-AUG-1998 RXK  New JOKER added.
C V27 17-JUN-1997 RXK  Changes for Bingo Fullhouse
C V26 11-FEB-1997 RXK  IMNU=instant supply message, IORD=instant games names
C                      request message
C V25 06-FEB-1997 RXK  Instant validation section changed
C V24 05-DEC-1996 HXK  Updated for Finland IPS pre-release
C V23 17-APR-1996 HXK  Release of Finland for X.25, Telephone Betting,
C                      Instant Pass Thru Phase 1
C V22 06-JAN-1996 HXK  Fix for logging of Double / Couple
C V21 04-JAN-1996 HXK  Further changes for Double / Couple batch
C V20 17-DEC-1995 HXK  Changed calculation for TWAMT for Double, Couple system
C                      bets
C V19 23-NOV-1995 PXB  Couple and Double games added
C V18 09-MAY-1995 PXB  Added Rank Flag and coupon type id.
C V17 24-OCT-1994 HXK  Removed Kicker logic for Bingo
C V16 19-OCT-1994 HXK  Set Bingo record size = 2
C V15 02-OCT-1994 HXK  Added Bingo
C V14 08-JAN-1994 HXK  change to handle amounts for score system bets
C V13 14-SEP-1993 GXA  Added Retreiving of TCDC_SOLD from offset loged.
C V12 10-SEP-1993 GXA  Changed TWBNK to TVBNK in Validation transactions.
C V11 23-AUG-1993 GXA  Changed Toto Select record layout, inorder to fit amount
C                      bet. Used for system bets. Record could extend to two
C                      log records if bank info usd
C V10 26-JUL-1993 GXA  Added System Type and Number to Toto Select one board
C                      bets.
C V09 16-JUL-1993 GXA  Removed Discount Amt from Lotto,Sports,Spede and Ravi
C                      due to increased wager amounts due to Free systems.
C                      (No space in log record).
C V08 26-MAY-1993 GXA  Released for Finland Dec Conversion / Oddset.
C V07 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                      DEC Baseline
C V06 19-AUG-1992 GCAN ADDED KICKER TO TOTO SELECT AND SIPLIFIED TOTO SELECT
C                      BOARD DATA IN RECORD.
C V05 07-AUG-1992 GCAN FIXED SCORE AMOUNT BET.
C V04 26-JAN-1992 GCAN FIXED WIN TIP AMOUNT BET.
C V03 13-DEC-1991 DAS  NEW X2X CODE
C V02 07-OCT-1991 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C
C SUBROUTINE TO ENCODE INTERNAL TRANSACTION FORMAT
C TO LOG RECORD.
C
C
C CALLING SEQUENCE:
C     CALL TRALOG(TRABUF,LOGBUF)
C INPUT
C     TRALOG - INTERNAL TRANSACTION FORMAT
C OUTPUT
C     LOGBUF - TRANSACTION LOG BUFFER
C
C
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C CONFIDENTIAL PROPRIETARY INFORMATION
C This item is the property of GTECH Corporation, W. Greenwich, Rhode
C Island, and contains confidential and trade secret information.  It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH.  Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published or disclosed, in whold or in part, directly
C or indirectly, except as expressly authorized by an officer of
C GTECH pursuant to written agreement.
C COPYRIGHT 2000 GTECH CORPORATION.  ALL RIGHTS RESERVED.
C
C-------------------------------------------------------
! transaction twag
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TRALOG(TRABUF,LOGBUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:X2XPTL.DEF'
        INCLUDE 'INCLIB:X2STMES.DEF'
        INCLUDE 'INCLIB:X2FEMES.DEF'
C
C
        INTEGER*4 BRDOFF, BETIND, LOGOFF, TRAIND
        INTEGER*4 LOGBUF(LREC*3)
        INTEGER*4 BUFIDX,IND,GNUM,GTYP
        BYTE      BUFF(120)
C
        INTEGER*4 I4TEMP
        INTEGER*2 I2TEMP(2)
        BYTE      I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
        INTEGER*4   I,J,X
        INTEGER*4 I4TEMP_AUX
        INTEGER*2 I2TEMP_AUX(2)
        BYTE      I1TEMP_AUX(4)
        EQUIVALENCE (I4TEMP_AUX,I2TEMP_AUX,I1TEMP_AUX)
C        INTEGER*4   AUXTWCOLMSERL
C
C
C
C ENCODE TRANSACTION HEADER INFORMATION
C
        LOGBUF(1) = TRABUF(TSER)
C
C REMEMBER BYTES ARE NUMBERED AS 3,2,1,0 - SO SOME THINGS LOOK BACKWARDS
C BUT THEY REALLY ARE NOT
C
        I2TEMP(1) = TRABUF(TCDC)
        I2TEMP(2) = TRABUF(TTER)
        LOGBUF(2) = I4TEMP

        I4TEMP    = TRABUF(TAGT)
        I1TEMP(4) = TRABUF(TNFRAC)
        LOGBUF(3) = I4TEMP
C
        I4TEMP    = TRABUF(TTIM)
        I1TEMP(4) = TRABUF(TTSTCS)
        LOGBUF(4) = I4TEMP
C
        I2TEMP(1) = TRABUF(TCHK)
        I1TEMP(3) = TRABUF(TERR)
        I1TEMP(4) = TRABUF(TGAM)
        LOGBUF(5) = I4TEMP
C
        I1TEMP(1) = TRABUF(TSTAT)
        I1TEMP(2) = ISHFT(TRABUF(TTYP),4) +
     *              IAND(TRABUF(TTRN),'0F'X)

        X = IAND(TRABUF(TFIL),7)

        IF(TRABUF(TINTRA).NE.0) X = X + 8

        I1TEMP(3) = ISHFT(TRABUF(TSIZE),4) + X
        I1TEMP(4) = ISHFT(TRABUF(TGAMTYP),3) +
     *              IAND(TRABUF(TGAMIND),'07'X)
        LOGBUF(6) = I4TEMP

        X         = IAND(TRABUF(TTKID), '7F'X)
        IF(TRABUF(TFAMTFLG).EQ.1) X = IOR(X,'80'X)
        I1TEMP(1) = X
        I1TEMP(2) = TRABUF(TFRAC)
        I1TEMP(3) = TRABUF(TSUBERR)
        I1TEMP(4) = TRABUF(TCDC) - TRABUF(TCDC_SOLD)
        LOGBUF(7) = I4TEMP
C
C PROJECT EURO MIL
C
        IF (TRABUF(TTYP) .EQ. TEUR) THEN
           I4TEMP = 0
           I1TEMP(1) = TRABUF(TEUTYP)
           I2TEMP(2) = TRABUF(TEUCHK)
           LOGBUF(8) = I4TEMP

           LOGBUF(9) = TRABUF(TEUSER)
           LOGBUF(10) = TRABUF(TEUMESSQ)

           IF (TRABUF(TEUTYP) .EQ. TWAG) THEN

              I1TEMP(1) = TRABUF (TEUWBEGW)
              I1TEMP(2) = TRABUF (TEUWBEGY)
              I1TEMP(3) = TRABUF (TEUWENDW)
              I1TEMP(4) = TRABUF (TEUWENDY)
              LOGBUF(11) = I4TEMP

              I1TEMP(1) = TRABUF (TEUWDUR)
              I1TEMP(2) = TRABUF (TEUWNBET)
              I2TEMP(2) = TRABUF (TEUWQP)
              LOGBUF(12) = I4TEMP

              I1TEMP(1) = TRABUF (TEUWNMK)
              I1TEMP(2) = TRABUF (TEUWNST)
              I2TEMP(2) = 0
              LOGBUF(13) = I4TEMP

              I1TEMP(1) = TRABUF (TEUWTIMEH)
              I1TEMP(2) = TRABUF (TEUWTIMEM)
              I1TEMP(3) = TRABUF (TEUWTIMES)
              I1TEMP(4) = 0
              LOGBUF(14) = I4TEMP

              I2TEMP(1) = TRABUF (TEUWOFS1)
              I2TEMP(2) = TRABUF (TEUWOFS2)
              LOGBUF(15) = I4TEMP
C
C DRAW INDICATOR
C
              I1TEMP(1) = TRABUF (TEUWDRWIND)
              I1TEMP(2) = 0
              I2TEMP(2) = 0
              LOGBUF(16) = I4TEMP

              LOGOFF = 17
              BRDOFF = 15

              CALL FASTMOV(TRABUF(TEUWBOARD), LOGBUF(LOGOFF), BRDOFF)
C----+---+-------------+------------------------------------------------
C V59|BEG| M16 PROJECT | EM WAGER NEW FIELDS
C----+---+-------------+------------------------------------------------
C              GOTO 9000
              IF(TRABUF(TSIZE).EQ.2) GOTO 9000
C
              I4TEMP     = 0
              IF(TRABUF(TEUW_KIWFL).NE.0)  I1TEMP(1) = I1TEMP(1) + '80'X        !EM KICKER FLAG                    (1 BIT )
              IF(TRABUF(TEUW_SMWFL).NE.0)  I1TEMP(2) = I1TEMP(2) + '08'X        !EM PARTICIPATING IN SM GAME FLAG  (1 BIT )
              IF(TRABUF(TEUW_SHWFL).NE.0)  I1TEMP(2) = I1TEMP(2) + '04'X        !EM PARTICIPATING IN SoM GAME FLAG (1 BIT )
              I1TEMP(3)  = TRABUF(TEUW_EUWCH)                                   !EM WAGERING CHANNEL               (1 BYTE)
              LOGBUF(33) = I4TEMP
C
              LOGBUF(34) = TRABUF(TEUW_PLNIF)                                   !PORTUGUESE PLAYER NIF (4 BYTES)
C
              I4TEMP     = TRABUF(TEUW_SMWSN)                                   !SM WAGER SERIAL NUMBER (3 BYTES)
              I1TEMP(4)  = TRABUF(TEUW_SMWCD)                                   !SM WAGER CHECK DIGITS  (1 BYTE )
              LOGBUF(35) = I4TEMP
C
              I1TEMP(1)  = TRABUF(TEUW_SMWDN)                                   !SM WAGER DRAW NUMBER       (1 BYTE)
              I1TEMP(2)  = TRABUF(TEUW_SMWDY)                                   !SM WAGER DRAW YEAR         (1 BYTE)
              I1TEMP(3)  = TRABUF(TEUW_SMWOF)                                   !OFFSET TO SM DRAW CDC DATE (1 BYTE)
              I1TEMP(4)  = 0
              LOGBUF(36) = I4TEMP
C
              I2TEMP(1)  = TRABUF(TEUW_SMWTB)                                   !SM WAGER TOTAL BETS (2 BYTES)
              I2TEMP(2)  = 0
              LOGBUF(37) = I4TEMP
C
              LOGBUF(38) = TRABUF(TEUW_SMWB1)                                   !SM WAGER BEGIN NUMBER (PART 1 - 4 CHARS)
C
              I4TEMP     = TRABUF(TEUW_SMWB2)                                   !SM WAGER BEGIN NUMBER (PART 2 - 3 BYTES)
              I1TEMP(4)  = 0
              LOGBUF(39) = I4TEMP
C
              LOGBUF(40) = TRABUF(TEUW_SMWE1)                                   !SM WAGER END NUMBER (PART 1 - 4 CHARS)
C
              I4TEMP     = TRABUF(TEUW_SMWE2)                                   !SM WAGER END NUMBER (PART 2 - 3 BYTES)
              I1TEMP(4)  = 0
              LOGBUF(41) = I4TEMP
C
              I1TEMP(1)  = TRABUF(TEUW_SHWDN)                                   !SoM WAGER DRAW NUMBER       (1 BYTE)
              I1TEMP(2)  = TRABUF(TEUW_SHWDY)                                   !SoM WAGER DRAW YEAR         (1 BYTE)
              I1TEMP(3)  = TRABUF(TEUW_SHWOF)                                   !OFFSET TO SoM DRAW CDC DATE (1 BYTE)
              I1TEMP(4)  = 0
              LOGBUF(42) = I4TEMP
C
              I2TEMP(1)  = TRABUF(TEUW_SHWTB)                                   !SoM WAGER TOTAL BETS (2 BYTES)
              I2TEMP(2)  = 0
              LOGBUF(43) = I4TEMP
C
              LOGBUF(44) = TRABUF(TEUW_SHWB1)                                   !SoM WAGER BEGIN NUMBER (PART 1 - 4 CHARS)
C
              I4TEMP     = TRABUF(TEUW_SHWB2)                                   !SoM WAGER BEGIN NUMBER (PART 2 - 3 BYTES)
              I1TEMP(4)  = 0
              LOGBUF(45) = I4TEMP
C
              LOGBUF(46) = TRABUF(TEUW_SHWE1)                                   !SoM WAGER END NUMBER (PART 1 - 4 CHARS)
C
              I4TEMP     = TRABUF(TEUW_SHWE2)                                   !SoM WAGER END NUMBER (PART 2 - 3 BYTES)
              I1TEMP(4)  = 0
              LOGBUF(47) = I4TEMP
C
              GOTO 9000
C----+---+-------------+------------------------------------------------
C V59|END| M16 PROJECT | EM WAGER NEW FIELDS
C----+---+-------------+------------------------------------------------
           ENDIF

           IF (TRABUF(TEUTYP) .EQ. TCAN) THEN

              I2TEMP(1)  = TRABUF (TEUCWJUL)
              I1TEMP(3)  = TRABUF (TEUCWCKD)
              LOGBUF(11) = I4TEMP

              LOGBUF(12) = TRABUF(TEUCWSER)

              I1TEMP(1)  = TRABUF (TEUCTIMEH)
              I1TEMP(2)  = TRABUF (TEUCTIMEM)
              I1TEMP(3)  = TRABUF (TEUCTIMES)
              I1TEMP(4)  = TRABUF (TEUCST)
              LOGBUF(13) = I4TEMP

              LOGBUF(14) = TRABUF (TEUCAM)
C----+---+-------------+------------------------------------------------
C V59|BEG| M16 PROJECT | EM CANCELLATION NEW FIELDS
C----+---+-------------+------------------------------------------------
              I4TEMP     = 0
              IF(TRABUF(TEUC_SMCFL).NE.0)  I1TEMP(1) = I1TEMP(1) + '80'X        !SM CANCELLATION DATA PRESENT FLAG (1 BIT)
              IF(TRABUF(TEUC_SHCFL).NE.0)  I1TEMP(1) = I1TEMP(1) + '40'X        !SoM CANCELLATION FLAG             (1 BIT)
              LOGBUF(17) = I4TEMP
C
              I4TEMP     = TRABUF(TEUC_SMWSN)                                   !SM WAGER EXTERNAL SERIAL NUMBER (3 BYTES)
              I1TEMP(4)  = TRABUF(TEUC_SMWCD)                                   !SM WAGER CHECK DIGITS           (1 BYTE )
              LOGBUF(18) = I4TEMP
C
              I4TEMP     = TRABUF(TEUC_SMCSN)                                   !SM CANCELLATION EXTERNAL SERIAL NUMBER (3 BYTES)
              I1TEMP(4)  = TRABUF(TEUC_SMCCD)                                   !SM CANCELLATION CHECK DIGITS           (1 BYTE )
              LOGBUF(19) = I4TEMP
C
              LOGBUF(20) = TRABUF(TEUC_SMWCA)                                   !SM WAGER CANCELLATION AMOUNT (WAGER UNITS) (4 BYTES
)
C----+---+-------------+------------------------------------------------
C V59|END| M16 PROJECT | EM CANCELLATION NEW FIELDS
C----+---+-------------+------------------------------------------------
              GOTO 9000
            ENDIF

            IF (TRABUF(TEUTYP) .EQ. TVAL) THEN

              I1TEMP(1)  = TRABUF (TEUVSBT)
              I1TEMP(2)  = TRABUF (TEUVWCKD)
              I2TEMP(2)  = TRABUF (TEUVWJUL)
              LOGBUF(11) = I4TEMP

              LOGBUF(12) = TRABUF(TEUVWSER)

              I1TEMP(1)  = TRABUF (TEUVTIMEH)
              I1TEMP(2)  = TRABUF (TEUVTIMEM)
              I1TEMP(3)  = TRABUF (TEUVTIMES)
              I1TEMP(4)  = TRABUF (TEUVST)
              LOGBUF(13) = I4TEMP

              LOGBUF(14) = TRABUF (TEUVCAM)
              LOGBUF(15) = TRABUF (TEUVRAM)
C+++++++++++++V51+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              I1TEMP(1)  = TRABUF(TEUVCAMH)
C----+------------------------------------------------------------------
C V57| Adding new validation messages fields
C----+------------------------------------------------------------------
C             I1TEMP(2)  = 0
              I1TEMP(2)  = TRABUF(TEUVPLIDTYP)
C----+------------------------------------------------------------------
C V57| Adding new validation messages fields
C----+------------------------------------------------------------------
              I1TEMP(3)  = TRABUF(TEUVRAMH)
              I1TEMP(4)  = 0
              LOGBUF(16) = I4TEMP
C+++++++++++++V51+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C----+------------------------------------------------------------------
C V57| Adding new validation messages fields
C----+------------------------------------------------------------------
              LOGBUF(17) = TRABUF(TEUVPLCARD)

              I2TEMP(1)  = TRABUF(TEUVNIBBB)
              I2TEMP(2)  = TRABUF(TEUVNIBBO)
              LOGBUF(18) = I4TEMP

              LOGBUF(19) = TRABUF(TEUVNIBBA1)

              I1TEMP(1)  = TRABUF(TEUVNIBBA2)
              I1TEMP(2)  = TRABUF(TEUVNIBCD)
              LOGBUF(20) = I4TEMP
C----+------------------------------------------------------------------
C V57| Adding new validation messages fields
C----+------------------------------------------------------------------
C----+---+-------------+------------------------------------------------
C V59|BEG| M16 PROJECT | EM VALIDATION NEW FIELDS
C----+---+-------------+------------------------------------------------
              I4TEMP      = 0
              IF(TRABUF(TEUV_NIFFL).NE.0)  I1TEMP(1) = I1TEMP(1) + '80'X        !PLAYER NIF PRESENT FLAG               (1 BIT)
              IF(TRABUF(TEUV_NIFCF).NE.0)  I1TEMP(1) = I1TEMP(1) + '40'X        !PLAYER NIF CONFIRMATION REQUIRED FLAG (1 BIT)
              IF(TRABUF(TEUV_SHVFL).NE.0)  I1TEMP(1) = I1TEMP(1) + '20'X        !SoM VALIDATION FLAG                   (1 BIT)
              LOGBUF(21)  = I4TEMP
C
              LOGBUF(22)  = TRABUF(TEUV_PLNIF)                                  !PORTUGUESE PLAYER NIF (4 BYTES)
C----+---+-------------+------------------------------------------------
C V59|END| M16 PROJECT | EM VALIDATION NEW FIELDS
C----+---+-------------+------------------------------------------------

              IF (TRABUF (TEUVST) .EQ. 11) THEN
C                LOGBUF(16) USED FOR TEUVCAMH AND TEUVRAMH. SEE ABOVE
C                I1TEMP(3) = TRABUF(TEUEVWCKD)
C                LOGBUF(16) = I4TEMP

                 LOGBUF(17) = TRABUF(TEUEVWSER)

                 I1TEMP(1) = TRABUF (TEUVEBEGW)
                 I1TEMP(2) = TRABUF (TEUVEBEGY)
                 I1TEMP(3) = TRABUF (TEUVEENDW)
                 I1TEMP(4) = TRABUF (TEUVEENDY)
                 LOGBUF(18) = I4TEMP

                 I1TEMP(1) = TRABUF (TEUVEDUR)
                 I1TEMP(2) = TRABUF (TEUVENBET)
                 I2TEMP(2) = TRABUF (TEUVEQP)
                 LOGBUF(19) = I4TEMP

                 I1TEMP(1) = TRABUF (TEUVENMK)
                 I1TEMP(2) = TRABUF (TEUVENST)
                 I2TEMP(2) = 0
                 LOGBUF(20) = I4TEMP

                 I1TEMP(1) = TRABUF (TEUVETIMEH)
                 I1TEMP(2) = TRABUF (TEUVETIMEM)
                 I1TEMP(3) = TRABUF (TEUVETIMES)
                 I1TEMP(4) = TRABUF(TEUEVWCKD)
                 LOGBUF(21) = I4TEMP

                 I2TEMP(1) = TRABUF (TEUVEOFS1)
                 I2TEMP(2) = TRABUF (TEUVEOFS2)
                 LOGBUF(22) = I4TEMP

                 LOGOFF = 23
                 BRDOFF = 9

                 CALL FASTMOV(TRABUF(TEUVEBOARD), LOGBUF(LOGOFF), BRDOFF)

              ENDIF

              GOTO 9000
            ENDIF
        ENDIF
C----+------------------------------------------------------------------
C V58| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF (TRABUF(TTYP) .EQ. TIGS) THEN
C----+------------------------------------------------------------------
C    |
C    |
C V58| Handling IGS transaction header
C    |
C    |
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V58| Handling IGS transaction header: IGS TRANSACTION TYPE
C----+------------------------------------------------------------------
            LOGBUF(8)  = TRABUF (TIGS_TTYP) ! (1 byte)
C----+------------------------------------------------------------------
C V58| Handling IGS transaction header: MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
C----+------------------------------------------------------------------
            LOGBUF(9)  = TRABUF (TIGS_XREF) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling IGS transaction header: IGS ERROR CODE DESCRIPTION, SYSTEM CODE WHERE ERROR OCCURRED
C----+------------------------------------------------------------------
            LOGBUF(10) = TRABUF (TIGS_XERR + 0) ! (9 bytes)
            LOGBUF(11) = TRABUF (TIGS_XERR + 1)
            I4TEMP     = 0
            I1TEMP(1)  = TRABUF (TIGS_XERR + 2)
            I1TEMP(2)  = TRABUF (TIGS_SERR) ! (1 byte)
            LOGBUF(12) = I4TEMP
C----+------------------------------------------------------------------
C    |
C    |
C V58| Handling wager transactions
C    |
C    |
C----+------------------------------------------------------------------
            IF (TRABUF(TIGS_TTYP) .EQ. IGSWAG) THEN
C----+------------------------------------------------------------------
C V58| Handling wager transactions: TERMINAL MESSAGE ID (LOW 4 BYTES)
C----+------------------------------------------------------------------
                LOGBUF(13) = TRABUF (TIGSW_MIDL) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling wager transactions: TERMINAL MESSAGE ID (HIGH 4 BYTES)
C----+------------------------------------------------------------------
                LOGBUF(14) = TRABUF (TIGSW_MIDH) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling wager transactions: ABP GAME ID
C----+------------------------------------------------------------------
                LOGBUF(15) = TRABUF (TIGSW_XGID) ! (1 byte)
C----+------------------------------------------------------------------
C V58| Handling wager transactions: SUBTYPE ID
C    | - Be careful when placing information here, because bytes #4 of -
C    | - words #16,#32,#48 are used for record extension               -
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I2TEMP(1)  = TRABUF (TIGSW_STID) ! (2 bytes)
                LOGBUF(16) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling wager transactions: UNIT STAKE OF THE BET
C----+------------------------------------------------------------------
                LOGBUF(17) = TRABUF (TIGSW_USTK) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling wager transactions: TOTAL BETS/NUMBER OF SELECTIONS (MAX = 8)
C----+------------------------------------------------------------------
                LOGBUF(18) = TRABUF (TIGSW_TBET) ! (1 byte)
C----+------------------------------------------------------------------
C V58| Handling wager transactions: BET REFERENCE DATE (YYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSW_WRDY) ! (1 byte)
                I1TEMP(2)  = TRABUF (TIGSW_WRDM) ! (1 byte)
                I1TEMP(3)  = TRABUF (TIGSW_WRDD) ! (1 byte)
                LOGBUF(19) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling wager transactions: BET REFERENCE GAME
C----+------------------------------------------------------------------
                LOGBUF(20) = TRABUF (TIGSW_WRGM) ! (1 byte)
C----+------------------------------------------------------------------
C V58| Handling wager transactions: BET REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
C----+------------------------------------------------------------------
                LOGBUF(21) = TRABUF (TIGSW_WRSL) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling wager transactions: BET REFERENCE SERIAL NUMBER (HIGH ONE BYTE)
C----+------------------------------------------------------------------
                LOGBUF(22) = TRABUF (TIGSW_WRSH) ! (1 byte)
C----+------------------------------------------------------------------
C V58| Handling wager transactions: BET REFERENCE CHECK DIGITS
C----+------------------------------------------------------------------
                LOGBUF(23) = TRABUF (TIGSW_WRCD) ! (2 bytes)
C----+------------------------------------------------------------------
C V58| Handling wager transactions: BET CREATION DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I2TEMP(1)  = TRABUF (TIGSW_WCDY) ! (2 bytes)
                I1TEMP(3)  = TRABUF (TIGSW_WCDM) ! (1 byte)
                I1TEMP(4)  = TRABUF (TIGSW_WCDD) ! (1 byte)
                LOGBUF(24) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling wager transactions: BET CREATION TIME (HHMISS)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSW_WCTH) ! (1 byte)
                I1TEMP(2)  = TRABUF (TIGSW_WCTM) ! (1 byte)
                I1TEMP(3)  = TRABUF (TIGSW_WCTS) ! (1 byte)
                LOGBUF(25) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling wager transactions: BET LAST EVENT DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I2TEMP(1)  = TRABUF (TIGSW_LEDY) ! (2 bytes)
                I1TEMP(3)  = TRABUF (TIGSW_LEDM) ! (1 byte)
                I1TEMP(4)  = TRABUF (TIGSW_LEDD) ! (1 byte)
                LOGBUF(26) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling wager transactions: BET TOTAL STAKE
C----+------------------------------------------------------------------
                LOGBUF(27) = TRABUF (TIGSW_TSTK) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling wager transactions: BET MAXIMUM POSSIBLE RETURNS
C----+------------------------------------------------------------------
                LOGBUF(28) = TRABUF (TIGSW_MAXR) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling wager transactions: PLAYER NIF NUMBER
C----+------------------------------------------------------------------
                LOGBUF(29) = TRABUF (TIGSW_PNIF) ! (4 bytes)
                GOTO 9000
            ENDIF
C----+------------------------------------------------------------------
C    |
C    |
C V58| Handling cancellation transactions
C    |
C    |
C----+------------------------------------------------------------------
            IF (TRABUF(TIGS_TTYP) .EQ. IGSCAN) THEN
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: TERMINAL MESSAGE ID (LOW 4 BYTES)
C----+------------------------------------------------------------------
                LOGBUF(13) = TRABUF (TIGSC_MIDL) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: TERMINAL MESSAGE ID (HIGH 4 BYTES)
C----+------------------------------------------------------------------
                LOGBUF(14) = TRABUF (TIGSC_MIDH) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: BET REFERENCE DATE (YYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSC_WRDY) ! (1 byte)
                I1TEMP(2)  = TRABUF (TIGSC_WRDM) ! (1 byte)
                I1TEMP(3)  = TRABUF (TIGSC_WRDD) ! (1 byte)
                LOGBUF(15) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: BET REFERENCE GAME
C    | - Be careful when placing information here, because bytes #4 of -
C    | - words #16,#32,#48 are used for record extension               -
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSC_WRGM) ! (1 bytes)
                LOGBUF(16) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: BET REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
C----+------------------------------------------------------------------
                LOGBUF(17) = TRABUF (TIGSC_WRSL) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: BET REFERENCE SERIAL NUMBER (HIGH ONE BYTE)
C----+------------------------------------------------------------------
                LOGBUF(18) = TRABUF (TIGSC_WRSH) ! (1 byte)
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: BET REFERENCE CHECK DIGITS
C----+------------------------------------------------------------------
                LOGBUF(19) = TRABUF (TIGSC_WRCD) ! (2 bytes)
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: CANCEL REFERENCE GAME
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(2)  = TRABUF (TIGSC_CRGM) ! (1 byte)
                LOGBUF(20) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: CANCEL REFERENCE DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I2TEMP(1)  = TRABUF (TIGSC_CRDY) ! (2 bytes)
                I1TEMP(3)  = TRABUF (TIGSC_CRDM) ! (1 byte)
                I1TEMP(4)  = TRABUF (TIGSC_CRDD) ! (1 byte)
                LOGBUF(21) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: CANCEL REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
C----+------------------------------------------------------------------
                LOGBUF(22) = TRABUF (TIGSC_CRSL) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: CANCEL REFERENCE SERIAL NUMBER (HIGH ONE BYTE)
C----+------------------------------------------------------------------
                LOGBUF(23) = TRABUF (TIGSC_CRSH) ! (1 byte)
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: CANCEL REFERENCE CHECK DIGITS
C----+------------------------------------------------------------------
                LOGBUF(24) = TRABUF (TIGSC_CRCD) ! (2 bytes)
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: WAGER CANCEL DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I2TEMP(1)  = TRABUF (TIGSC_WCDY) ! (2 bytes)
                I1TEMP(3)  = TRABUF (TIGSC_WCDM) ! (1 byte)
                I1TEMP(4)  = TRABUF (TIGSC_WCDD) ! (1 byte)
                LOGBUF(25) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: WAGER CANCEL TIME (HHMISS)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSC_WCTH) ! (1 byte)
                I1TEMP(2)  = TRABUF (TIGSC_WCTM) ! (1 byte)
                I1TEMP(3)  = TRABUF (TIGSC_WCTS) ! (1 byte)
                LOGBUF(26) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling cancellation transactions: CANCEL AMOUNT (WAGER UNITS)
C----+------------------------------------------------------------------
                LOGBUF(27) = TRABUF (TIGSC_CAMT) ! (4 bytes)
                GOTO 9000
            ENDIF
C----+------------------------------------------------------------------
C    |
C    |
C V58| Handling validation transactions
C    |
C    |
C----+------------------------------------------------------------------
            IF (TRABUF(TIGS_TTYP) .EQ. IGSVAL) THEN
C----+------------------------------------------------------------------
C V58| Handling validation transactions: TERMINAL MESSAGE ID (LOW 4 BYTES)
C----+------------------------------------------------------------------
                LOGBUF(13) = TRABUF (TIGSV_MIDL) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling validation transactions: TERMINAL MESSAGE ID (HIGH 4 BYTES)
C----+------------------------------------------------------------------
                LOGBUF(14) = TRABUF (TIGSV_MIDH) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling validation transactions: BET REFERENCE DATE (YYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSV_WRDY) ! (1 byte)
                I1TEMP(2)  = TRABUF (TIGSV_WRDM) ! (1 byte)
                I1TEMP(3)  = TRABUF (TIGSV_WRDD) ! (1 byte)
                LOGBUF(15) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling validation transactions: BET REFERENCE GAME
C    | - Be careful when placing information here, because bytes #4 of -
C    | - words #16,#32,#48 are used for record extension               -
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSV_WRGM) ! (1 bytes)
                LOGBUF(16) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling validation transactions: BET REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
C----+------------------------------------------------------------------
                LOGBUF(17) = TRABUF (TIGSV_WRSL) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling validation transactions: BET REFERENCE SERIAL NUMBER (HIGH ONE BYTE)
C----+------------------------------------------------------------------
                LOGBUF(18) = TRABUF (TIGSV_WRSH) ! (1 byte)
C----+------------------------------------------------------------------
C V58| Handling validation transactions: BET REFERENCE CHECK DIGITS
C----+------------------------------------------------------------------
                LOGBUF(19) = TRABUF (TIGSV_WRCD) ! (2 bytes)
C----+------------------------------------------------------------------
C V58| Handling validation transactions: PAYMENT MODE
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(2)  = TRABUF (TIGSV_PMOD) ! (1 byte)
                LOGBUF(20) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling validation transactions: WAGER VALIDATION DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I2TEMP(1)  = TRABUF (TIGSV_WVDY) ! (2 bytes)
                I1TEMP(3)  = TRABUF (TIGSV_WVDM) ! (1 byte)
                I1TEMP(4)  = TRABUF (TIGSV_WVDD) ! (1 byte)
                LOGBUF(21) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling validation transactions: WAGER VALIDATION TIME (HHMISS)
C    |                                   PLAYER NIF NUMBER CONFIRMATION NEEDED FLAG
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSV_WVTH) ! (1 byte)
                I1TEMP(2)  = TRABUF (TIGSV_WVTM) ! (1 byte)
                I1TEMP(3)  = TRABUF (TIGSV_WVTS) ! (1 byte)
                I1TEMP(4)  = TRABUF (TIGSV_FNIF) ! (1 byte)
                LOGBUF(22) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling validation transactions: TOTAL PRIZE AMOUNT (VALIDATION UNITS)
C----+------------------------------------------------------------------
                LOGBUF(23) = TRABUF (TIGSV_TPRZ) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling validation transactions: TOTAL TAX AMOUNT (VALIDATION UNITS)
C----+------------------------------------------------------------------
                LOGBUF(24) = TRABUF (TIGSV_TTAX) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling validation transactions: TOTAL TAXNET PRIZE AMOUNT (VALIDATION UNITS)
C----+------------------------------------------------------------------
                LOGBUF(25) = TRABUF (TIGSV_NPRZ) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling validation transactions: PLAYER NIF NUMBER
C----+------------------------------------------------------------------
                LOGBUF(26) = TRABUF (TIGSV_PNIF) ! (4 bytes)
                GOTO 9000
            ENDIF
C----+------------------------------------------------------------------
C    |
C    |
C V58| Handling payment transactions
C    |
C    |
C----+------------------------------------------------------------------
            IF (TRABUF(TIGS_TTYP) .EQ. IGSPAY) THEN
C----+------------------------------------------------------------------
C V58| Handling payment transactions: TERMINAL MESSAGE ID (LOW 4 BYTES)
C----+------------------------------------------------------------------
                LOGBUF(13) = TRABUF (TIGSP_MIDL) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling payment transactions: TERMINAL MESSAGE ID (HIGH 4 BYTES)
C----+------------------------------------------------------------------
                LOGBUF(14) = TRABUF (TIGSP_MIDH) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling payment transactions: BET REFERENCE DATE (YYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSP_WRDY) ! (1 byte)
                I1TEMP(2)  = TRABUF (TIGSP_WRDM) ! (1 byte)
                I1TEMP(3)  = TRABUF (TIGSP_WRDD) ! (1 byte)
                LOGBUF(15) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling payment transactions: BET REFERENCE GAME
C    | - Be careful when placing information here, because bytes #4 of -
C    | - words #16,#32,#48 are used for record extension               -
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSP_WRGM) ! (1 byte)
                LOGBUF(16) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling payment transactions: BET REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
C----+------------------------------------------------------------------
                LOGBUF(17) = TRABUF (TIGSP_WRSL) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling payment transactions: BET REFERENCE SERIAL NUMBER (HIGH ONE BYTE),
C                                     PAYMENT MODE,
C    |                                BET REFERENCE CHECK DIGITS
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSP_WRSH) ! (1 byte)
                I1TEMP(2)  = TRABUF (TIGSP_PMOD) ! (1 byte)
                I2TEMP(2)  = TRABUF (TIGSP_WRCD) ! (2 bytes)
                LOGBUF(18) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling payment transactions: PLAYER ID
C----+------------------------------------------------------------------
                LOGBUF(19) = TRABUF (TIGSP_PYID) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling payment transactions: PLAYER NIB, PLAYER ID TYPE
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I2TEMP(1)  = TRABUF (TIGSP_NIBB) ! (2 bytes)  NIB Branch
                I2TEMP(2)  = TRABUF (TIGSP_NIBO) ! (2 bytes)  NIB Office
                LOGBUF(20) = I4TEMP
                LOGBUF(21) = TRABUF (TIGSP_NIA1) ! (4 bytes)  NIB Account Number Part 1
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSP_NIA2) ! (1 byte)   NIB Account Number Part 2
                I1TEMP(2)  = TRABUF (TIGSP_NICD) ! (1 byte)   NIB Check digits
                I1TEMP(3)  = TRABUF (TIGSP_IDTY) ! (1 byte)   Player Id Type
                LOGBUF(22) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling payment transactions: PAYMENT REFERENCE DATE (YYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSP_PRDY) ! (1 byte)
                I1TEMP(2)  = TRABUF (TIGSP_PRDM) ! (1 byte)
                I1TEMP(3)  = TRABUF (TIGSP_PRDD) ! (1 byte)
                LOGBUF(23) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling payment transactions: PAYMENT REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
C----+------------------------------------------------------------------
                LOGBUF(24) = TRABUF (TIGSP_PRSL) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling payment transactions: PAYMENT REFERENCE SERIAL NUMBER (HIGH ONE BYTE),
C    |                                PAYMENT REFERENCE GAME,
C    |                                PAYMENT REFERENCE CHECK DIGITS
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSP_PRGM) ! (1 byte)
                I1TEMP(2)  = TRABUF (TIGSP_PRSH) ! (1 byte)
                I2TEMP(2)  = TRABUF (TIGSP_PRCD) ! (2 bytes)
                LOGBUF(25) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling payment transactions: PRIZE PAYMENT DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I2TEMP(1)  = TRABUF (TIGSP_PPDY) ! (2 bytes)
                I1TEMP(3)  = TRABUF (TIGSP_PPDM) ! (1 byte)
                I1TEMP(4)  = TRABUF (TIGSP_PPDD) ! (1 byte)
                LOGBUF(26) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling payment transactions: PRIZE PAYMENT TIME (HHMISS)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSP_PPTH) ! (1 byte)
                I1TEMP(2)  = TRABUF (TIGSP_PPTM) ! (1 byte)
                I1TEMP(3)  = TRABUF (TIGSP_PPTS) ! (1 byte)
                LOGBUF(27) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling payment transactions: TOTAL PRIZE AMOUNT (PAYMENT UNITS)
C----+------------------------------------------------------------------
                LOGBUF(28) = TRABUF (TIGSP_TPRZ) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling payment transactions: TOTAL TAX AMOUNT (PAYMENT UNITS)
C----+------------------------------------------------------------------
                LOGBUF(29) = TRABUF (TIGSP_TTAX) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling payment transactions: TOTAL TAXNET PRIZE AMOUNT (PAYMENT UNITS)
C----+------------------------------------------------------------------
                LOGBUF(30) = TRABUF (TIGSP_NPRZ) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling payment transactions: PLAYER NIF NUMBER
C----+------------------------------------------------------------------
                LOGBUF(31) = TRABUF (TIGSP_PNIF) ! (4 bytes)
                GOTO 9000
            ENDIF
C----+------------------------------------------------------------------
C    |
C    |
C V58| Handling game programme report transactions
C    |
C    |
C----+------------------------------------------------------------------
            IF (TRABUF(TIGS_TTYP) .EQ. IGSREP) THEN
C----+------------------------------------------------------------------
C V58| Handling game programme report transactions: TERMINAL MESSAGE ID (LOW 4 BYTES)
C----+------------------------------------------------------------------
                LOGBUF(13) = TRABUF (TIGSR_MIDL) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling game programme report transactions: TERMINAL MESSAGE ID (HIGH 4 BYTES)
C----+------------------------------------------------------------------
                LOGBUF(14) = TRABUF (TIGSR_MIDH) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling game programme report transactions: MEDIA ID
C----+------------------------------------------------------------------
                LOGBUF(15) = TRABUF (TIGSR_MEID) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling game programme report transactions: SEGMENT NUMBER REQUESTED
C    | - Be careful when placing information here, because bytes #4 of -
C    | - words #16,#32,#48 are used for record extension               -
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSR_SEGN) ! (1 byte)
                LOGBUF(16) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling game programme report transactions: PROGRAMME TEMPLATE ID
C----+------------------------------------------------------------------
                LOGBUF(17) = TRABUF (TIGSR_PTID) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling game programme report transactions: PROGRAMME REPORT DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I2TEMP(1)  = TRABUF (TIGSR_PRDY) ! (2 bytes)
                I1TEMP(3)  = TRABUF (TIGSR_PRDM) ! (1 byte)
                I1TEMP(4)  = TRABUF (TIGSR_PRDD) ! (1 byte)
                LOGBUF(18) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling game programme report transactions: PROGRAMME REPORT TIME (HHMISS)
C----+------------------------------------------------------------------
                I4TEMP     = 0
                I1TEMP(1)  = TRABUF (TIGSR_PRTH) ! (1 byte)
                I1TEMP(2)  = TRABUF (TIGSR_PRTM) ! (1 byte)
                I1TEMP(3)  = TRABUF (TIGSR_PRTS) ! (1 byte)
                LOGBUF(19) = I4TEMP
C----+------------------------------------------------------------------
C V58| Handling game programme report transactions: TOTAL SEGMENTS
C----+------------------------------------------------------------------
                LOGBUF(20) = TRABUF (TIGSR_TSEG) ! (1 byte)
C----+------------------------------------------------------------------
C V58| Handling game programme report transactions: MEDIA VERSION (LOW 4 BYTES)
C----+------------------------------------------------------------------
                LOGBUF(21) = TRABUF (TIGSR_MVRL) ! (4 bytes)
C----+------------------------------------------------------------------
C V58| Handling game programme report transactions: MEDIA VERSION (HIGH 4 BYTES)
C----+------------------------------------------------------------------
                LOGBUF(22) = TRABUF (TIGSR_MVRH) ! (4 bytes)
                GOTO 9000
            ENDIF
            GOTO 9000
        ENDIF
C----+------------------------------------------------------------------
C V58| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
C ENCODE PASSIVE GAMES
C
        IF(TRABUF(TGAMTYP).EQ.TPAS) THEN

           IF(TRABUF(TTYP).EQ.TWAG .OR. TRABUF(TTYP).EQ.TCAN .OR.
     *        TRABUF(TTYP).EQ.TINC)  THEN
C
              LOGBUF(8)  = TRABUF(TWCSER)
C
              LOGBUF(9)  = TRABUF(TWAMT)
C
              I2TEMP(1)  = TRABUF(TWBEG)
              I2TEMP(2)  = TRABUF(TWCTER)
              LOGBUF(10) = I4TEMP
C
              I2TEMP(1)  = TRABUF(TWQPF)
              I1TEMP(4)  = TRABUF(TWNBET)
              LOGBUF(11) = I4TEMP
C
              I1TEMP(1)  = TRABUF(TWTKC)
              I1TEMP(2)  = TRABUF(TWVSTS)
              I1TEMP(3)  = TRABUF(TWDUR)
              I1TEMP(4)  = TRABUF(TWEPOP)
              LOGBUF(12) = I4TEMP

              I1TEMP(1)  = TRABUF(TWEPWK)
              I1TEMP(2)  = TRABUF(TWEPYR)
              LOGBUF(13) = I4TEMP

              IF(TRABUF(TWEPOP).EQ.EPASSAL) THEN

                 LOGBUF(14) = TRABUF(TWEPSN)

                 I1TEMP(1) = TRABUF(TWEPSF)
                 I1TEMP(2) = TRABUF(TWEPSS)
                 LOGBUF(15) = I4TEMP

              ELSEIF(TRABUF(TWEPOP).EQ.EPASRES.OR.TRABUF(TWEPOP).EQ.EPASREL)
     *        THEN

                  LOGBUF(14) = TRABUF(TWEPSD)

                  LOGBUF(15) = TRABUF(TWEPRM)

                  I1TEMP(1)  = TRABUF(TWEPNE)
                  I1TEMP(2)  = TRABUF(TWEPNF)
                  I1TEMP(3)  = TRABUF(TWEPNR)
                  LOGBUF(16) = I4TEMP
C
C BEGIN CONTINUATION RECORD 1
C
                  LOGBUF(17) = TRABUF(TWEPRES1)
                  LOGBUF(18) = TRABUF(TWEPRES2)
                  LOGBUF(19) = TRABUF(TWEPRES3)

                  IF(TRABUF(TWEPOP).EQ.EPASREL) GOTO 9000

                  I1TEMP(1)  = TRABUF(TWEPNFR1)
                  I1TEMP(2)  = TRABUF(TWEPNFR2)
                  I1TEMP(3)  = TRABUF(TWEPNFR3)
                  LOGBUF(20) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPSER1_1)
                  I1TEMP(2)  = TRABUF(TWEPSER1_2)
                  I1TEMP(3)  = TRABUF(TWEPSER1_3)
                  I1TEMP(4)  = TRABUF(TWEPSER1_4)
                  LOGBUF(21) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPSER1_5)
                  I1TEMP(2)  = TRABUF(TWEPSER1_6)
                  I1TEMP(3)  = TRABUF(TWEPSER1_7)
                  I1TEMP(4)  = TRABUF(TWEPSER1_8)
                  LOGBUF(22) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPSER1_9)
                  I1TEMP(2)  = TRABUF(TWEPSER1_10)
                  I1TEMP(3)  = TRABUF(TWEPFRC1_1)
                  I1TEMP(4)  = TRABUF(TWEPFRC1_2)
                  LOGBUF(23) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPFRC1_3)
                  I1TEMP(2)  = TRABUF(TWEPFRC1_4)
                  I1TEMP(3)  = TRABUF(TWEPFRC1_5)
                  I1TEMP(4)  = TRABUF(TWEPFRC1_6)
                  LOGBUF(24) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPFRC1_7)
                  I1TEMP(2)  = TRABUF(TWEPFRC1_8)
                  I1TEMP(3)  = TRABUF(TWEPFRC1_9)
                  I1TEMP(4)  = TRABUF(TWEPFRC1_10)
                  LOGBUF(25) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPSER2_1)
                  I1TEMP(2)  = TRABUF(TWEPSER2_2)
                  I1TEMP(3)  = TRABUF(TWEPSER2_3)
                  I1TEMP(4)  = TRABUF(TWEPSER2_4)
                  LOGBUF(26) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPSER2_5)
                  I1TEMP(2)  = TRABUF(TWEPSER2_6)
                  I1TEMP(3)  = TRABUF(TWEPSER2_7)
                  I1TEMP(4)  = TRABUF(TWEPSER2_8)
                  LOGBUF(27) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPSER2_9)
                  I1TEMP(2)  = TRABUF(TWEPSER2_10)
                  I1TEMP(3)  = TRABUF(TWEPFRC2_1)
                  I1TEMP(4)  = TRABUF(TWEPFRC2_2)
                  LOGBUF(28) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPFRC2_3)
                  I1TEMP(2)  = TRABUF(TWEPFRC2_4)
                  I1TEMP(3)  = TRABUF(TWEPFRC2_5)
                  I1TEMP(4)  = TRABUF(TWEPFRC2_6)
                  LOGBUF(29) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPFRC2_7)
                  I1TEMP(2)  = TRABUF(TWEPFRC2_8)
                  I1TEMP(3)  = TRABUF(TWEPFRC2_9)
                  I1TEMP(4)  = TRABUF(TWEPFRC2_10)
                  LOGBUF(30) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPSER3_1)
                  I1TEMP(2)  = TRABUF(TWEPSER3_2)
                  I1TEMP(3)  = TRABUF(TWEPSER3_3)
                  I1TEMP(4)  = TRABUF(TWEPSER3_4)
                  LOGBUF(31) = I4TEMP

                  IF(TRABUF(TSIZE).EQ.2) GOTO 9000
C
C BEGIN CONTINUATION RECORD 2
C
                  I1TEMP(1)  = TRABUF(TWEPSER3_5)
                  I1TEMP(2)  = TRABUF(TWEPSER3_6)
                  I1TEMP(3)  = TRABUF(TWEPSER3_7)
                  I1TEMP(4)  = TRABUF(TWEPSER3_8)
                  LOGBUF(33) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPSER3_9)
                  I1TEMP(2)  = TRABUF(TWEPSER3_10)
                  I1TEMP(3)  = TRABUF(TWEPFRC3_1)
                  I1TEMP(4)  = TRABUF(TWEPFRC3_2)
                  LOGBUF(34) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPFRC3_3)
                  I1TEMP(2)  = TRABUF(TWEPFRC3_4)
                  I1TEMP(3)  = TRABUF(TWEPFRC3_5)
                  I1TEMP(4)  = TRABUF(TWEPFRC3_6)
                  LOGBUF(35) = I4TEMP

                  I1TEMP(1)  = TRABUF(TWEPFRC3_7)
                  I1TEMP(2)  = TRABUF(TWEPFRC3_8)
                  I1TEMP(3)  = TRABUF(TWEPFRC3_9)
                  I1TEMP(4)  = TRABUF(TWEPFRC3_10)
                  LOGBUF(36) = I4TEMP

              ENDIF
              GOTO 9000

          ENDIF

          IF(TRABUF(TTYP).EQ.TRET) THEN

          I4TEMP      =  IOR(TRABUF(TPRETYP),ISHFT(TRABUF(TPTCK),4))
          I4TEMP      =  ISHFT(I4TEMP,24) +
     *                   IAND (TRABUF(TPNUM1), '00FFFFFF'X)
          LOGBUF(8)   =  I4TEMP

          LOGBUF(9)   =  TRABUF(TPKEY1)
          LOGBUF(10)  =  TRABUF(TPPAY1)

          I2TEMP(1)   =  TRABUF(TPEMIS1)
          I1TEMP(3)   =  TRABUF(TPSER1)
          I1TEMP(4)   =  TRABUF(TPTEN1)
          LOGBUF(11)  =  I4TEMP

          I4TEMP      =  TRABUF(TPNUM2)
          !I1TEMP(4) was used for TWAG->TRET transformation
          I1TEMP(4)   =  0
          LOGBUF(12)  =  I4TEMP

          LOGBUF(13)  =  TRABUF(TPKEY2)
          LOGBUF(14)  =  TRABUF(TPPAY2)

          I2TEMP(1)   =  TRABUF(TPEMIS2)
          I1TEMP(3)   =  TRABUF(TPSER2)
          I1TEMP(4)   =  TRABUF(TPTEN2)
          LOGBUF(15)  =  I4TEMP

          I4TEMP      = TRABUF(TPOFFTER)
          I1TEMP(3)   = IAND(TRABUF(TPSTS1),'0F'X) + ISHFT(TRABUF(TPSTS2),4)
          LOGBUF(16)  = I4TEMP

          IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
C PASSIVE UNSOLD TICKETS CONTINUATION RECORD 1
C
          LOGBUF(17)  =  TRABUF(TPKEY3)
          LOGBUF(18)  =  TRABUF(TPPAY3)

          I2TEMP(1)   =  TRABUF(TPEMIS3)
          I1TEMP(3)   =  TRABUF(TPSER3)
          I1TEMP(4)   =  TRABUF(TPTEN3)
          LOGBUF(19)  =  I4TEMP

          LOGBUF(20)  =  TRABUF(TPKEY4)
          LOGBUF(21)  =  TRABUF(TPPAY4)

          I2TEMP(1)   =  TRABUF(TPEMIS4)
          I1TEMP(3)   =  TRABUF(TPSER4)
          I1TEMP(4)   =  TRABUF(TPTEN4)
          LOGBUF(22)  =  I4TEMP

          LOGBUF(23)  =  TRABUF(TPKEY5)
          LOGBUF(24)  =  TRABUF(TPPAY5)

          I2TEMP(1)   =  TRABUF(TPEMIS5)
          I1TEMP(3)   =  TRABUF(TPSER5)
          I1TEMP(4)   =  TRABUF(TPTEN5)
          LOGBUF(25)  =  I4TEMP

          LOGBUF(26)  =  TRABUF(TPKEY6)
          LOGBUF(27)  =  TRABUF(TPPAY6)

          I2TEMP(1)   =  TRABUF(TPEMIS6)
          I1TEMP(3)   =  TRABUF(TPSER6)
          I1TEMP(4)   =  TRABUF(TPTEN6)
          LOGBUF(28)  =  I4TEMP

          I4TEMP      =  0
          I1TEMP(4)   =  IAND(TRABUF(TPSTS3),'0F'X) + ISHFT(TRABUF(TPSTS4),4)
          I4TEMP      =  I4TEMP +
     *                   IAND (TRABUF(TPNUM3),'00FFFFFF'X)
          LOGBUF(29)  =  I4TEMP

          I4TEMP      =  0
          I1TEMP(4)   =  IAND(TRABUF(TPSTS5),'0F'X) + ISHFT(TRABUF(TPSTS6),4)
          I4TEMP      =  I4TEMP +
     *                   IAND (TRABUF(TPNUM4),'00FFFFFF'X)
          LOGBUF(30)  =  I4TEMP

          I4TEMP      =  TRABUF(TPNUM5)
          I1TEMP(4)   =  TRABUF(TPFRCNT)
          LOGBUF(31)  =  I4TEMP

          LOGBUF(32)  =  TRABUF(TPNUM6)

          IF(TRABUF(TSIZE).EQ.2) GOTO 9000
C
C PASSIVE  UNSOLD TICKETS CONTINUATION RECORD 2
C
          LOGBUF(33)  =  TRABUF(TPKEY7)
          LOGBUF(34)  =  TRABUF(TPPAY7)

          I2TEMP(1)   =  TRABUF(TPEMIS7)
          I1TEMP(3)   =  TRABUF(TPSER7)
          I1TEMP(4)   =  TRABUF(TPTEN7)
          LOGBUF(35)  =  I4TEMP

          LOGBUF(36)  =  TRABUF(TPKEY8)
          LOGBUF(37)  =  TRABUF(TPPAY8)

          I2TEMP(1)   =  TRABUF(TPEMIS8)
          I1TEMP(3)   =  TRABUF(TPSER8)
          I1TEMP(4)   =  TRABUF(TPTEN8)
          LOGBUF(38)  =  I4TEMP

          LOGBUF(39)  =  TRABUF(TPKEY9)
          LOGBUF(40)  =  TRABUF(TPPAY9)

          I2TEMP(1)   =  TRABUF(TPEMIS9)
          I1TEMP(3)   =  TRABUF(TPSER9)
          I1TEMP(4)   =  TRABUF(TPTEN9)
          LOGBUF(41)  =  I4TEMP

          LOGBUF(42)  =  TRABUF(TPKEY10)
          LOGBUF(43)  =  TRABUF(TPPAY10)

          I2TEMP(1)   =  TRABUF(TPEMIS10)
          I1TEMP(3)   =  TRABUF(TPSER10)
          I1TEMP(4)   =  TRABUF(TPTEN10)
          LOGBUF(44)  =  I4TEMP

          I4TEMP      =  0
          I1TEMP(4)   =  IAND(TRABUF(TPSTS7),'0F'X) + ISHFT(TRABUF(TPSTS8),4)
          I4TEMP      =  I4TEMP +
     *                   IAND (TRABUF(TPNUM7),'00FFFFFF'X)
          LOGBUF(45)  =  I4TEMP

          I4TEMP      =  0
          I1TEMP(4)   =  IAND(TRABUF(TPSTS9),'0F'X) + ISHFT(TRABUF(TPSTS10),4)
          I4TEMP      =  I4TEMP +
     *                   IAND (TRABUF(TPNUM8),'00FFFFFF'X)
          LOGBUF(46)  =  I4TEMP

          LOGBUF(47)  =  TRABUF(TPNUM9)
          LOGBUF(48)  =  TRABUF(TPNUM10)

          GOTO 9000
          ENDIF
        ENDIF
C
C WAGERS/CANCELLATIONS/DELETIONS
C
        IF(TRABUF(TTYP).EQ.TWAG .OR.
     *     TRABUF(TTYP).EQ.TCAN .OR.
     *     TRABUF(TTYP).EQ.TINC) THEN
C
C LOTTO/SPORTS/SPEDEN
C
          IF(TRABUF(TGAMTYP).EQ.TLTO.OR.TRABUF(TGAMTYP).EQ.TSPT.OR. !TLTO <=> TOTOLOTO | TSPT <=> TOTOBOLA
     *    TRABUF(TGAMTYP).EQ.TTGL) THEN ! TTGL <=> TOTOGOLO (No Longer In Use) IGNORE
C
             LOGBUF(8)  = TRABUF(TWCSER)
C
             LOGBUF(9)  = TRABUF(TWAMT)
C
             I2TEMP(1)  = TRABUF(TWBEG)
             I2TEMP(2)  = TRABUF(TWCTER)
             LOGBUF(10) = I4TEMP
C
             I2TEMP(1)  = TRABUF(TWQPF)
             I2TEMP(2)  = TRABUF(TWSIMP)
             LOGBUF(11) = I4TEMP
C
             I1TEMP(1)  = TRABUF(TWSYSN)
             I1TEMP(2)  = TRABUF(TWVSTS)
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
             IF(TRABUF(TWCOLMCOMF_TLTO) .EQ. 1) THEN
C               can insert in the most higher bit since the higher nible used to save const 1 and previously max 5 that is 0101

               I1TEMP(3)  = TRABUF(TWDUR) + '10'X
             ELSE IF(TRABUF(TCOLMCOMF_TLTO) .EQ. 1) THEN
               I1TEMP(3)  = TRABUF(TWDUR) + '20'X
             ELSE
               I1TEMP(3)  = TRABUF(TWDUR)
             ENDIF
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------

             I1TEMP(4)  = TRABUF(TWSYST)
             LOGBUF(12) = I4TEMP
C
             I4TEMP = 0
             I1TEMP(1)  = TRABUF(TWNMRK)
             IF(TRABUF(TWMFLG)  .NE. 0)    I1TEMP(2) = I1TEMP(2) + '80'X
             IF(TRABUF(TWKGME)  .NE. 0)    I1TEMP(2) = I1TEMP(2) + '40'X
             IF(TRABUF(TWKFLG)  .NE. 0)    I1TEMP(2) = I1TEMP(2) + '20'X
             IF(TRABUF(TWKFLG2) .NE. 0)    I1TEMP(2) = I1TEMP(2) + '10'X
             IF(TRABUF(TWFFLG)  .NE. 0)    I1TEMP(2) = I1TEMP(2) + '08'X
             IF(TRABUF(TWADDFW) .NE. 0)    I1TEMP(2) = I1TEMP(2) + '04'X
             IF(TRABUF(TWLMFI)  .NE. 0)    I1TEMP(2) = I1TEMP(2) + '02'X
             IF(TRABUF(TGAMTYP) .EQ. TSPT) I1TEMP(2) = I1TEMP(2) + '01'X
C
             I1TEMP(3)  = TRABUF(TWCDCOFF)
             I1TEMP(4)  = TRABUF(TWMFRAC)
             LOGBUF(13) = I4TEMP
C

             I4TEMP     = TRABUF(TWBNKID)
             I1TEMP(4)  = TRABUF(TWWEQP)
             LOGBUF(14) = I4TEMP
C
             LOGBUF(15) = TRABUF(TWBNKNM)
C
             I1TEMP(1)  = TRABUF(TWSRW)
             IF (TRABUF(TGAMTYP).EQ.TLTO) THEN                      !V52...
               I1TEMP(2)  = IAND(TRABUF(TWNBET),'0F'X) + ISHFT(TRABUF(TWLUCK),4)
             ELSE
               I1TEMP(2)  = IAND(TRABUF(TWNBET),'0F'X) + ISHFT(TRABUF(TWSPFRG),4)
             ENDIF                                                  !...V52
             I1TEMP(3)  = TRABUF(TWTKC)
             LOGBUF(16) = I4TEMP
C
             IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
C BEGIN CONTINUATION RECORD 1
C
             IF(TRABUF(TGAMTYP) .EQ. TSPT) THEN
               LOGBUF(18) = TRABUF(TWCEBM)  ! SPORTS GAME CANCELATION EVENTS BITMAP
             ELSE
               LOGBUF(18) = 0
             ENDIF
C
             IF(TRABUF(TGAMTYP) .EQ. TSPT) THEN
                LOGOFF = 19     ! NEW OFFSET VERSION FOR SPORTS GAMES
                BRDOFF = 13
             ELSE
                LOGOFF = 17     ! HOLE RECORD FOR BORAD DATA OLD VERSION
                BRDOFF = 15     ! IF WE DO NOT PLAY KICKER. KEEP OLD OFFSET FOR LOTO GAMES
             ENDIF
C
             IF(TRABUF(TWKGME).NE.0) THEN !TWKGME <=> KICKER GAME #
                LOGBUF(17) = TRABUF(TWKICK)
C               LOGBUF(18) = TRABUF(TWKICK2)  ! NOT USED IN "SCML"
C
                I2TEMP(1) = TRABUF(TWKAMT)
                I1TEMP(3) = TRABUF(TWKGME)
                I1TEMP(4) = TRABUF(TWKDUR)
                LOGBUF(19)= I4TEMP
C
                I2TEMP(1) = TRABUF(TWKBEG)
                I2TEMP(2) = 0
                LOGBUF(20)= I4TEMP
C
                LOGOFF = 21
                BRDOFF = 11                 !WE LOSE SPACE FOR BOARD DATA
C                                           !TO KICKER STORAGE.
             ENDIF
C
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C nota: confirmar se n�o comessar no LOGBUF 19
C pois TWCEBM no TSPT(SPORTS) est� ocupar a posi��o 18 ....
C----+------------------------------------------------------------------
            IF(TRABUF(TWCOLMCOMF_TLTO) .EQ. 1) THEN
               LOGBUF(17) = TRABUF(TWCOLMSERL_TLTO)
               LOGBUF(18) = TRABUF(TWCOLMSERM_TLTO)
               LOGBUF(19) = TRABUF(TWCOLMMIDL_TLTO)
               I1TEMP(1) = TRABUF(TWCOLMSERH_TLTO)
               I1TEMP(2) = TRABUF(TWCOLMMIDH_TLTO)
               I1TEMP(3) = TRABUF(TWCOLMCOMF_TLTO)
               I1TEMP(4) = 0
               LOGBUF(20) = I4TEMP

C WE LOSE SPACE FOR BOARD DATA
C TO USE FOR NEW TERMINALS 2021
               LOGOFF = 21
               BRDOFF = 11
            ENDIF

            IF(TRABUF(TCOLMCOMF_TLTO) .EQ. 1) THEN
               LOGBUF(17) = TRABUF(TCOLMSERL_TLTO)
               LOGBUF(18) = TRABUF(TCOLMSERM_TLTO)
               LOGBUF(19) = TRABUF(TCOLMMIDL_TLTO)
               I1TEMP(1) = TRABUF(TCOLMSERH_TLTO)
               I1TEMP(2) = TRABUF(TCOLMMIDH_TLTO)
               I1TEMP(3) = TRABUF(TCOLMCOMF_TLTO)
               I1TEMP(4) = 0
               LOGBUF(20) = I4TEMP

C WE LOSE SPACE FOR BOARD DATA
C TO USE FOR NEW TERMINALS 2021
               LOGOFF = 21
               BRDOFF = 11
            ENDIF
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------

             CALL FASTMOV(TRABUF(TWBORD), LOGBUF(LOGOFF), BRDOFF)
C
             IF(TRABUF(TSIZE).EQ.2) GOTO 9000
C
C BEGIN CONTINUATION RECORD 2
C
             LOGOFF = LOGOFF + BRDOFF + 1

             CALL FASTMOV(TRABUF(TWBORD+BRDOFF), LOGBUF(LOGOFF), 15)
             GOTO 9000
C
          ENDIF
C
C
C ENCODE KICKER (ONLY) WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TKIK) THEN
             LOGBUF(8) = TRABUF(TWCSER)
C
             I2TEMP(1) = TRABUF(TWAMT)
             I2TEMP(2) = TRABUF(TWDAMT)
             LOGBUF(9) = I4TEMP
C
             I2TEMP(1) = TRABUF(TWBEG)
             I2TEMP(2) = TRABUF(TWCTER)
             LOGBUF(10)= I4TEMP
C
             LOGBUF(11) = TRABUF(TWKICK)
C
             LOGBUF(12) = TRABUF(TWKICK2)
C
             I1TEMP(1) = TRABUF(TWDUR)
             I1TEMP(2) = TRABUF(TWVSTS)
             I1TEMP(3) = 0
             IF(TRABUF(TWQPF).NE.0)   I1TEMP(3) = I1TEMP(3) + '80'X
             IF(TRABUF(TWKFLG).NE.0)  I1TEMP(3) = I1TEMP(3) + '20'X
             IF(TRABUF(TWKFLG2).NE.0) I1TEMP(3) = I1TEMP(3) + '10'X
             IF(TRABUF(TWFFLG).NE.0)  I1TEMP(3) = I1TEMP(3) + '08'X
             IF(TRABUF(TWADDFW).NE.0) I1TEMP(3) = I1TEMP(3) + '04'X
             I1TEMP(4)  = TRABUF(TWTKC)
             LOGBUF(13) = I4TEMP
C
             I4TEMP     = TRABUF(TWBNKID)
             I1TEMP(4)  = TRABUF(TWNBET)
             LOGBUF(14) = I4TEMP
C
             LOGBUF(15) = TRABUF(TWBNKNM)
C
             I2TEMP(1) = TRABUF(TWQPF)
             I1TEMP(3) = TRABUF(TWSYST)
             I1TEMP(4) = 0
             LOGBUF(16)= I4TEMP
C
             I4TEMP     = TRABUF(TWLNKSER)
             I1TEMP(4)  = TRABUF(TWLNKCHK)
             LOGBUF(29) = I4TEMP
             GOTO 9000
C
          ENDIF
C
C NUMBERS WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TNBR) THEN

            LOGBUF(8)  = TRABUF(TWCSER)

            I2TEMP(1)  = TRABUF(TWAMT)
            I2TEMP(2)  = TRABUF(TWDAMT)
            LOGBUF(9)  = I4TEMP

            I2TEMP(1)  = TRABUF(TWBEG)
            I2TEMP(2)  = TRABUF(TWCTER)
            LOGBUF(10) = I4TEMP

            I2TEMP(1)  = TRABUF(TWNNUM1)
            I2TEMP(2)  = TRABUF(TWNNUM2)
            LOGBUF(11) = I4TEMP

            I2TEMP(1)  = TRABUF(TWNNUM3)
            I2TEMP(2)  = TRABUF(TWNNUM4)
            LOGBUF(12) = I4TEMP

            I2TEMP(1)  = TRABUF(TWNAMT1)
            I2TEMP(2)  = TRABUF(TWNAMT2)
            LOGBUF(13) = I4TEMP

            I2TEMP(1)  = TRABUF(TWNAMT3)
            I2TEMP(2)  = TRABUF(TWNAMT4)
            LOGBUF(14) = I4TEMP

            I1TEMP(1)  = TRABUF(TWNPOL1)
            I1TEMP(2)  = TRABUF(TWNPOL2)
            I1TEMP(3)  = TRABUF(TWNPOL3)
            I1TEMP(4)  = TRABUF(TWNPOL4)
            LOGBUF(15) = I4TEMP
C
            I1TEMP(1)  = TRABUF(TWNBET)
            I1TEMP(2)  = TRABUF(TWTKC)
            I1TEMP(3)  = TRABUF(TWQPF)
            LOGBUF(16) = I4TEMP
            IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
            I1TEMP(1)=0
            IF(TRABUF(TWQPF).NE.0)  I1TEMP(1) = I1TEMP(1) + '80'X
            IF(TRABUF(TWNAND).NE.0) I1TEMP(1) = I1TEMP(1) + '40'X
            I1TEMP(2)  = TRABUF(TWNTYP)
            I1TEMP(3)  = TRABUF(TWVSTS)
            I1TEMP(4)  = TRABUF(TWDUR)
            LOGBUF(17) = I4TEMP
C
            I2TEMP(1)  = TRABUF(TWNNUM5)
            I2TEMP(2)  = TRABUF(TWNNUM6)
            LOGBUF(18) = I4TEMP
C
            I2TEMP(1)  = TRABUF(TWNNUM7)
            I2TEMP(2)  = TRABUF(TWNNUM8)
            LOGBUF(19) = I4TEMP
C
            I2TEMP(1)  = TRABUF(TWNNUM9)
            I2TEMP(2)  = TRABUF(TWNNUM10)
            LOGBUF(20) = I4TEMP
C
            I2TEMP(1)  = TRABUF(TWNAMT5)
            I2TEMP(2)  = TRABUF(TWNAMT6)
            LOGBUF(21) = I4TEMP
C
            I2TEMP(1)  = TRABUF(TWNAMT7)
            I2TEMP(2)  = TRABUF(TWNAMT8)
            LOGBUF(22) = I4TEMP
C
            I2TEMP(1)  = TRABUF(TWNAMT9)
            I2TEMP(2)  = TRABUF(TWNAMT10)
            LOGBUF(23) = I4TEMP
C
            I1TEMP(1)  = TRABUF(TWNPOL5)
            I1TEMP(2)  = TRABUF(TWNPOL6)
            I1TEMP(3)  = TRABUF(TWNPOL7)
            I1TEMP(4)  = TRABUF(TWNPOL8)
            LOGBUF(24) = I4TEMP
C
            I1TEMP(1)  = TRABUF(TWNPOL9)
            I1TEMP(2)  = TRABUF(TWNPOL10)
            I2TEMP(2)  = TRABUF(TWQPF)
            LOGBUF(25) = I4TEMP
            GOTO 9000
          ENDIF
C
C SCORE WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TSCR) THEN
C
             LOGBUF(8) = TRABUF(TWCSER)
C
             I2TEMP(1) = TRABUF(TWDAMT)
             I2TEMP(2) = TRABUF(TWBEG )
             LOGBUF(9) = I4TEMP
C
             I2TEMP(1) = TRABUF(TWSAMT)
             I2TEMP(2) = TRABUF(TWSAMT+TWSBLEN)
             LOGBUF(10)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWSAMT+TWSBLEN*2)
             I1TEMP(3) = TRABUF(TWSSCR1)
             I1TEMP(4) = TRABUF(TWSSCR2)
             LOGBUF(11)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWSSCR1+TWSBLEN)
             I1TEMP(2) = TRABUF(TWSSCR2+TWSBLEN)
             I1TEMP(3) = TRABUF(TWSSCR1+TWSBLEN*2)
             I1TEMP(4) = TRABUF(TWSSCR2+TWSBLEN*2)
             LOGBUF(12)= I4TEMP
C
             I4TEMP = TRABUF(TWBNKID)
             I1TEMP(4) = TRABUF(TWNBET)
             LOGBUF(13)= I4TEMP
C
             LOGBUF(14) = TRABUF(TWBNKNM)
C
             I1TEMP(1) = TRABUF(TWSYST)
             I1TEMP(2) = TRABUF(TWSYSN)
             I1TEMP(3) = TRABUF(TWVSTS)
             I1TEMP(4) = 0
             IF(TRABUF(TWFFLG).NE.0) I1TEMP(4) = I1TEMP(4) + '08'X
             IF(TRABUF(TWQPF).NE.0)  I1TEMP(4) = I1TEMP(4) + '80'X
             LOGBUF(15) = I4TEMP
C
             I4TEMP = 0
             I2TEMP(1) = TRABUF(TWCTER)
             I1TEMP(3) = TRABUF(TWTKC)
             LOGBUF(16)= I4TEMP
C
             IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
C SCORE CONTINUATION RECORD 1
C
             I2TEMP(1) = TRABUF(TWSAMT+TWSBLEN*3)
             I2TEMP(2) = TRABUF(TWSAMT+TWSBLEN*4)
             LOGBUF(17)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWSAMT+TWSBLEN*5)
             I2TEMP(2) = TRABUF(TWSAMT+TWSBLEN*6)
             LOGBUF(18)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWSAMT+TWSBLEN*7)
             I2TEMP(2) = TRABUF(TWSAMT+TWSBLEN*8)
             LOGBUF(19)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWSAMT+TWSBLEN*9)
             I1TEMP(3) = TRABUF(TWSSCR1+TWSBLEN*3)
             I1TEMP(4) = TRABUF(TWSSCR2+TWSBLEN*3)
             LOGBUF(20)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWSSCR1+TWSBLEN*4)
             I1TEMP(2) = TRABUF(TWSSCR2+TWSBLEN*4)
             I1TEMP(3) = TRABUF(TWSSCR1+TWSBLEN*5)
             I1TEMP(4) = TRABUF(TWSSCR2+TWSBLEN*5)
             LOGBUF(21)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWSSCR1+TWSBLEN*6)
             I1TEMP(2) = TRABUF(TWSSCR2+TWSBLEN*6)
             I1TEMP(3) = TRABUF(TWSSCR1+TWSBLEN*7)
             I1TEMP(4) = TRABUF(TWSSCR2+TWSBLEN*7)
             LOGBUF(22)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWSSCR1+TWSBLEN*8)
             I1TEMP(2) = TRABUF(TWSSCR2+TWSBLEN*8)
             I1TEMP(3) = TRABUF(TWSSCR1+TWSBLEN*9)
             I1TEMP(4) = TRABUF(TWSSCR2+TWSBLEN*9)
             LOGBUF(23)= I4TEMP
C
             GOTO 9000
C
          ENDIF
C
C WINNERS TIP WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TWIT) THEN
C
             LOGBUF(8) = TRABUF(TWCSER)
C
             I2TEMP(1) = TRABUF(TWDAMT)
             I2TEMP(2) = TRABUF(TWBEG )
             LOGBUF(9) = I4TEMP
C
             I2TEMP(1) = TRABUF(TWWAMT)
             I2TEMP(2) = TRABUF(TWWAMT+TWWBLEN)
             LOGBUF(10)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWWAMT+TWWBLEN*2)
             I1TEMP(3) = TRABUF(TWWROW)
             I1TEMP(4) = TRABUF(TWWROW+TWWBLEN)
             LOGBUF(11)= I4TEMP
C
             I4TEMP = TRABUF(TWBNKID)
             I1TEMP(4) = TRABUF(TWWROW+TWWBLEN*2)
             LOGBUF(12)= I4TEMP
C
             LOGBUF(13) = TRABUF(TWBNKNM)
C
             I1TEMP(1) = TRABUF(TWWCOUPID)
             I1TEMP(2) = 0
             I1TEMP(3) = TRABUF(TWNBET)
             I1TEMP(4) = TRABUF(TWSYST)
             LOGBUF(14)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWSYSN)
             I1TEMP(2) = TRABUF(TWVSTS)
             I2TEMP(2) = TRABUF(TWCTER)
             LOGBUF(15)= I4TEMP
C
             I1TEMP(1) = 0
             IF(TRABUF(TWQPF).NE.0)  I1TEMP(1) = I1TEMP(1) + '80'X
             IF(TRABUF(TWFFLG).NE.0) I1TEMP(1) = I1TEMP(1) + '08'X
             I1TEMP(2) = TRABUF(TWTKC)
             I1TEMP(3) = 0
             LOGBUF(16)= I4TEMP
C
             IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
C WINNERS TIP CONTINUATION RECORD 1
C
             I2TEMP(1) = TRABUF(TWWAMT+TWWBLEN*3)
             I2TEMP(2) = TRABUF(TWWAMT+TWWBLEN*4)
             LOGBUF(17)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWWAMT+TWWBLEN*5)
             I2TEMP(2) = TRABUF(TWWAMT+TWWBLEN*6)
             LOGBUF(18)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWWAMT+TWWBLEN*7)
             I2TEMP(2) = TRABUF(TWWAMT+TWWBLEN*8)
             LOGBUF(19)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWWAMT+TWWBLEN*9)
             I1TEMP(3) = TRABUF(TWWROW+TWWBLEN*3)
             I1TEMP(4) = TRABUF(TWWROW+TWWBLEN*4)
             LOGBUF(20)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWWROW+TWWBLEN*5)
             I1TEMP(2) = TRABUF(TWWROW+TWWBLEN*6)
             I1TEMP(3) = TRABUF(TWWROW+TWWBLEN*7)
             I1TEMP(4) = TRABUF(TWWROW+TWWBLEN*8)
             LOGBUF(21)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWWROW+TWWBLEN*9)
             I1TEMP(2) = 0
             I2TEMP(2) = 0
             LOGBUF(22)= I4TEMP
             GOTO 9000
          ENDIF
C
C SUPER DOUBLE WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TDBL) THEN
C
             LOGBUF(8) = TRABUF(TWCSER)
C
             I4TEMP    = 0
             I1TEMP(1) = TRABUF(TWDBCOUPID)
             I2TEMP(2) = TRABUF(TWBEG )
             LOGBUF(9) = I4TEMP
C
             I2TEMP(1) = TRABUF(TWDBAMT)
             I2TEMP(2) = TRABUF(TWDBAMT+TWDBBLEN)
             LOGBUF(10)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWDBAMT+TWDBBLEN*2)
             I1TEMP(3) = TRABUF(TWDBROW1)
             I1TEMP(4) = TRABUF(TWDBROW2)
             LOGBUF(11)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWDBROW1+TWDBBLEN)
             I1TEMP(2) = TRABUF(TWDBROW2+TWDBBLEN)
             I1TEMP(3) = TRABUF(TWDBROW1+TWDBBLEN*2)
             I1TEMP(4) = TRABUF(TWDBROW2+TWDBBLEN*2)
             LOGBUF(12)= I4TEMP
C
             I4TEMP = TRABUF(TWBNKID)
             I1TEMP(4) = TRABUF(TWNBET)
             LOGBUF(13)= I4TEMP
C
             LOGBUF(14) = TRABUF(TWBNKNM)
C
             I1TEMP(1) = TRABUF(TWSYST)
             I1TEMP(2) = TRABUF(TWSYSN)
             I1TEMP(3) = TRABUF(TWVSTS)
             I1TEMP(4) = 0
             IF(TRABUF(TWQPF).NE.0)  I1TEMP(4) = I1TEMP(4) + '80'X
             IF(TRABUF(TWFFLG).NE.0) I1TEMP(4) = I1TEMP(4) + '08'X
             LOGBUF(15) = I4TEMP
C
             I4TEMP = 0
             I2TEMP(1) = TRABUF(TWCTER)
             I1TEMP(3) = TRABUF(TWTKC)
             LOGBUF(16)= I4TEMP
C
             IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
C SUPER DOUBLE CONTINUATION RECORD 1
C
             I2TEMP(1) = TRABUF(TWDBAMT+TWDBBLEN*3)
             I2TEMP(2) = TRABUF(TWDBAMT+TWDBBLEN*4)
             LOGBUF(17)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWDBAMT+TWDBBLEN*5)
             I2TEMP(2) = TRABUF(TWDBAMT+TWDBBLEN*6)
             LOGBUF(18)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWDBAMT+TWDBBLEN*7)
             I2TEMP(2) = TRABUF(TWDBAMT+TWDBBLEN*8)
             LOGBUF(19)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWDBAMT+TWDBBLEN*9)
             I1TEMP(3) = TRABUF(TWDBROW1+TWDBBLEN*3)
             I1TEMP(4) = TRABUF(TWDBROW2+TWDBBLEN*3)
             LOGBUF(20)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWDBROW1+TWDBBLEN*4)
             I1TEMP(2) = TRABUF(TWDBROW2+TWDBBLEN*4)
             I1TEMP(3) = TRABUF(TWDBROW1+TWDBBLEN*5)
             I1TEMP(4) = TRABUF(TWDBROW2+TWDBBLEN*5)
             LOGBUF(21)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWDBROW1+TWDBBLEN*6)
             I1TEMP(2) = TRABUF(TWDBROW2+TWDBBLEN*6)
             I1TEMP(3) = TRABUF(TWDBROW1+TWDBBLEN*7)
             I1TEMP(4) = TRABUF(TWDBROW2+TWDBBLEN*7)
             LOGBUF(22)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWDBROW1+TWDBBLEN*8)
             I1TEMP(2) = TRABUF(TWDBROW2+TWDBBLEN*8)
             I1TEMP(3) = TRABUF(TWDBROW1+TWDBBLEN*9)
             I1TEMP(4) = TRABUF(TWDBROW2+TWDBBLEN*9)
             LOGBUF(23)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWDBAMT+TWDBBLEN*10)
             I2TEMP(2) = TRABUF(TWDBAMT+TWDBBLEN*11)
             LOGBUF(24)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWDBROW1+TWDBBLEN*10)
             I1TEMP(2) = TRABUF(TWDBROW2+TWDBBLEN*10)
             I1TEMP(3) = TRABUF(TWDBROW1+TWDBBLEN*11)
             I1TEMP(4) = TRABUF(TWDBROW2+TWDBBLEN*11)
             LOGBUF(25)= I4TEMP

             GOTO 9000
C
          ENDIF
C
C TODAYS COUPLE WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TCPL) THEN
C
             LOGBUF(8) = TRABUF(TWCSER)
C
             I4TEMP    = 0
             I1TEMP(1) = TRABUF(TWCPCOUPID)
             I2TEMP(2) = TRABUF(TWBEG )
             LOGBUF(9) = I4TEMP
C
             I2TEMP(1) = TRABUF(TWCPAMT)
             I2TEMP(2) = TRABUF(TWCPAMT+TWCPBLEN)
             LOGBUF(10)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWCPAMT+TWCPBLEN*2)
             I1TEMP(3) = TRABUF(TWCPROW1)
             I1TEMP(4) = TRABUF(TWCPROW2)
             LOGBUF(11)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWCPROW1+TWCPBLEN)
             I1TEMP(2) = TRABUF(TWCPROW2+TWCPBLEN)
             I1TEMP(3) = TRABUF(TWCPROW1+TWCPBLEN*2)
             I1TEMP(4) = TRABUF(TWCPROW2+TWCPBLEN*2)
             LOGBUF(12)= I4TEMP
C
             I4TEMP = TRABUF(TWBNKID)
             I1TEMP(4) = TRABUF(TWNBET)
             LOGBUF(13)= I4TEMP
C
             LOGBUF(14) = TRABUF(TWBNKNM)
C
             I1TEMP(1) = TRABUF(TWSYST)
             I1TEMP(2) = TRABUF(TWSYSN)
             I1TEMP(3) = TRABUF(TWVSTS)
             I1TEMP(4) = 0
             IF(TRABUF(TWQPF).NE.0)  I1TEMP(4) = I1TEMP(4) + '80'X
             IF(TRABUF(TWFFLG).NE.0) I1TEMP(4) = I1TEMP(4) + '08'X
             LOGBUF(15) = I4TEMP
C
             I4TEMP = 0
             I2TEMP(1) = TRABUF(TWCTER)
             I1TEMP(3) = TRABUF(TWTKC)
             LOGBUF(16)= I4TEMP
C
             IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
C TODAYS COUPLE CONTINUATION RECORD 1
C
             I2TEMP(1) = TRABUF(TWCPAMT+TWCPBLEN*3)
             I2TEMP(2) = TRABUF(TWCPAMT+TWCPBLEN*4)
             LOGBUF(17)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWCPAMT+TWCPBLEN*5)
             I2TEMP(2) = TRABUF(TWCPAMT+TWCPBLEN*6)
             LOGBUF(18)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWCPAMT+TWCPBLEN*7)
             I2TEMP(2) = TRABUF(TWCPAMT+TWCPBLEN*8)
             LOGBUF(19)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWCPAMT+TWCPBLEN*9)
             I1TEMP(3) = TRABUF(TWCPROW1+TWCPBLEN*3)
             I1TEMP(4) = TRABUF(TWCPROW2+TWCPBLEN*3)
             LOGBUF(20)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWCPROW1+TWCPBLEN*4)
             I1TEMP(2) = TRABUF(TWCPROW2+TWCPBLEN*4)
             I1TEMP(3) = TRABUF(TWCPROW1+TWCPBLEN*5)
             I1TEMP(4) = TRABUF(TWCPROW2+TWCPBLEN*5)
             LOGBUF(21)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWCPROW1+TWCPBLEN*6)
             I1TEMP(2) = TRABUF(TWCPROW2+TWCPBLEN*6)
             I1TEMP(3) = TRABUF(TWCPROW1+TWCPBLEN*7)
             I1TEMP(4) = TRABUF(TWCPROW2+TWCPBLEN*7)
             LOGBUF(22)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWCPROW1+TWCPBLEN*8)
             I1TEMP(2) = TRABUF(TWCPROW2+TWCPBLEN*8)
             I1TEMP(3) = TRABUF(TWCPROW1+TWCPBLEN*9)
             I1TEMP(4) = TRABUF(TWCPROW2+TWCPBLEN*9)
             LOGBUF(23)= I4TEMP
C
             I2TEMP(1) = TRABUF(TWCPAMT+TWCPBLEN*10)
             I2TEMP(2) = TRABUF(TWCPAMT+TWCPBLEN*11)
             LOGBUF(24)= I4TEMP
C
             I1TEMP(1) = TRABUF(TWCPROW1+TWCPBLEN*10)
             I1TEMP(2) = TRABUF(TWCPROW2+TWCPBLEN*10)
             I1TEMP(3) = TRABUF(TWCPROW1+TWCPBLEN*11)
             I1TEMP(4) = TRABUF(TWCPROW2+TWCPBLEN*11)
             LOGBUF(25)= I4TEMP
C
             GOTO 9000
          ENDIF
C
C TOTO SELECT WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TTSL) THEN
C
             LOGBUF(8) = TRABUF(TWCSER)
C
             I2TEMP(1) = TRABUF(TWDAMT)
             I2TEMP(2) = TRABUF(TWBEG )
             LOGBUF(9) = I4TEMP
C
             I2TEMP(1) = TRABUF(TWTAMT1)
             I1TEMP(3) = TRABUF(TWTKC)
             I1TEMP(4) = 0
             LOGBUF(10)= I4TEMP
C
C FIRST BOARD OF BOARD DATA
C
             LOGOFF = 11
             DO 500 J = 0,TRABUF(TWTSEL1)-1,2
                TRAIND = TWTBLEN*J
                I1TEMP(1) = TRABUF(TWTROW1+TRAIND)
                I1TEMP(2) = IAND(TRABUF(TWTPOL1+TRAIND),15)+
     *                      ISHFT(TRABUF(TWTSTS1+TRAIND),4)
C
                TRAIND = TWTBLEN*(J+1)
                I1TEMP(3) = TRABUF(TWTROW1+TRAIND)
                I1TEMP(4) = IAND(TRABUF(TWTPOL1+TRAIND),15)+
     *                      ISHFT(TRABUF(TWTSTS1+TRAIND),4)
                LOGBUF(LOGOFF) = I4TEMP
                LOGOFF = LOGOFF + 1
500          CONTINUE
C
             LOGBUF(14) = TRABUF(TWAMT)
C
             I1TEMP(1) = TRABUF(TWVSTS)
             I1TEMP(2) = TRABUF(TWSYSN)
             I1TEMP(3) = 0
             IF(TRABUF(TWQPF).NE.0)  I1TEMP(3) = I1TEMP(3) + '80'X
             IF(TRABUF(TWFFLG).NE.0) I1TEMP(3) = I1TEMP(3) + '08'X
              I1TEMP(4) = TRABUF(TWSYST)
             LOGBUF(15) = I4TEMP
C
             I2TEMP(1) = TRABUF(TWCTER)
             I1TEMP(3) = ISHFT(TRABUF(TWNBET),4)
             I1TEMP(3) = I1TEMP(3) + IAND(TRABUF(TWTSEL1),15)
             LOGBUF(16) = I4TEMP
C
             IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
C TOTO SELECT CONTINUATION RECORD 1
C

             DO 1000 I = 1,TRABUF(TWNBET)-1              !FOR BOARDS 2 AND UP
                BETIND= (I*TWTBLEN*TWTRMAX) + (I*TWTHLEN)
                BRDOFF = I * 5
                I2TEMP(1) = TRABUF(TWTAMT1+BETIND)
                I2TEMP(2) = 0
                LOGBUF(12+BRDOFF) = I4TEMP
C
                LOGOFF = BRDOFF
                DO 1010 J = 0,TRABUF(TWTSEL1+BETIND)-1,2  !TWO SELECTIONS AT TIME
                   TRAIND = BETIND + TWTBLEN*J
                   I1TEMP(1) = TRABUF(TWTROW1+TRAIND)
                   I1TEMP(2) = IAND(TRABUF(TWTPOL1+TRAIND),15)+
     *                        ISHFT(TRABUF(TWTSTS1+TRAIND),4)
C
                   TRAIND = BETIND + TWTBLEN*(J+1)
                   I1TEMP(3) = TRABUF(TWTROW1+TRAIND)
                   I1TEMP(4) = IAND(TRABUF(TWTPOL1+TRAIND),15)+
     *                        ISHFT(TRABUF(TWTSTS1+TRAIND),4)
C
                   LOGBUF(13+LOGOFF) = I4TEMP
                   LOGOFF = LOGOFF + 1
1010            CONTINUE
C
                I1TEMP(1) = TRABUF(TWTSEL1+BETIND)
                I1TEMP(2) = TRABUF(TWSYST)
                I1TEMP(3) = TRABUF(TWSYSN)
                I1TEMP(4) = 0
                LOGBUF(16+BRDOFF) = I4TEMP
1000         CONTINUE
C
             LOGBUF(31) = TRABUF(TWBNKNM)
C
             LOGBUF(32) = TRABUF(TWBNKID)
C
             GOTO 9000
          ENDIF
C
C BINGO
C
          IF(TRABUF(TGAMTYP).EQ.TBNG) THEN
C
             LOGBUF(8)  = TRABUF(TWCSER)
C
             LOGBUF(9)  = TRABUF(TWAMT)
C
             I2TEMP(1)  = TRABUF(TWBEG)
             I2TEMP(2)  = TRABUF(TWCTER)
             LOGBUF(10) = I4TEMP
C
             I2TEMP(1)  = TRABUF(TWQPF)
             I2TEMP(2)  = TRABUF(TWSIMP)
             LOGBUF(11) = I4TEMP
C
             I1TEMP(1)  = TRABUF(TWSYSN)
             I1TEMP(2)  = TRABUF(TWVSTS)
             I1TEMP(3)  = TRABUF(TWDUR)
             I1TEMP(4)  = TRABUF(TWSYST)
             LOGBUF(12) = I4TEMP
C
             I4TEMP = 0
             I1TEMP(1)  = TRABUF(TWNMRK)
             IF(TRABUF(TWMFLG).NE.0)  I1TEMP(2) = I1TEMP(2) + '80'X
             IF(TRABUF(TWKFLG).NE.0)  I1TEMP(2) = I1TEMP(2) + '20'X
             IF(TRABUF(TWKFLG2).NE.0) I1TEMP(2) = I1TEMP(2) + '10'X
             IF(TRABUF(TWFFLG).NE.0)  I1TEMP(2) = I1TEMP(2) + '08'X
             I1TEMP(3)  = TRABUF(TWCDCOFF)
             I1TEMP(4)  = TRABUF(TWMFRAC)
             LOGBUF(13) = I4TEMP
C
             LOGBUF(14) = TRABUF(TWBNKID)
C
             LOGBUF(15) = TRABUF(TWBNKNM)
C
             I4TEMP = 0
             I1TEMP(1)  = TRABUF(TWSRW)
             I1TEMP(2)  = TRABUF(TWNBET)
             I1TEMP(3)  = TRABUF(TWTKC)
             LOGBUF(16) = I4TEMP
C
C
C BEGIN CONTINUATION RECORD 1
C
             DO I=1,3
                DO J=1,5
                   X=(I-1)*5+(J-1)
                   IF(MOD(X,2).EQ.0) THEN
                      I2TEMP(1) = TRABUF(TWBBFH1+X)
                      I2TEMP(2) = TRABUF(TWBBFH1+X+1)
                      LOGBUF(17+X/2) = I4TEMP              ! 17...24
                   ENDIF
                ENDDO
             ENDDO

             LOGBUF(25) = TRABUF(TWBSED)
             LOGBUF(26) = TRABUF(TWBLUK)
C
             I4TEMP     = 0
             I2TEMP(1)  = TRABUF(TWBBAS)
             LOGBUF(27) = I4TEMP
C
             GOTO 9000
          ENDIF
C
C TODAY'S TRIO WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TTRP) THEN
C
             LOGBUF(8) = TRABUF(TWCSER)
C
             I2TEMP(1) = TRABUF(TWDAMT)
             I2TEMP(2) = TRABUF(TWBEG )
             LOGBUF(9) = I4TEMP
C
             I4TEMP    = TRABUF(TWTTAMT)
             I1TEMP(4) = TRABUF(TWSYST)
             LOGBUF(10)= I4TEMP

             I1TEMP(1) = TRABUF(TWTTMA)
             I1TEMP(2) = TRABUF(TWTTMB)
             I1TEMP(3) = TRABUF(TWTTMC)
             I1TEMP(4) = TRABUF(TWTTBET)
             LOGBUF(11)= I4TEMP

             I1TEMP(1) = TRABUF(TWTTBET+1)
             I1TEMP(2) = TRABUF(TWTTBET+2)
             I2TEMP(2) = TRABUF(TWCTER)
             LOGBUF(12)=I4TEMP

             I4TEMP    = TRABUF(TWBNKID)
             I1TEMP(4) = TRABUF(TWVSTS)
             LOGBUF(13)=I4TEMP

             LOGBUF(14) = TRABUF(TWBNKNM)

             I2TEMP(1) = TRABUF(TWSYSN)
             I2TEMP(2) = 0
             LOGBUF(15)=I4TEMP
C
             I1TEMP(1) = 0
             IF(TRABUF(TWQPF).NE.0)  I1TEMP(1) = I1TEMP(1) + '80'X
             IF(TRABUF(TWFFLG).NE.0) I1TEMP(1) = I1TEMP(1) + '08'X
             I1TEMP(2) = TRABUF(TWTKC)
             I1TEMP(3) = TRABUF(TWTTCOUPID)
             LOGBUF(16)= I4TEMP
C
             IF(TRABUF(TWSYST).EQ.NOSYS) GOTO 9000
C
C TODAY'S TRIO CONTINUATION RECORD 1
C
             I1TEMP(1) = TRABUF(TWTTBET+3)
             I1TEMP(2) = TRABUF(TWTTBET+4)
             I1TEMP(3) = TRABUF(TWTTBET+5)
             I1TEMP(4) = TRABUF(TWTTBET+6)
             LOGBUF(17)= I4TEMP

             I1TEMP(1) = TRABUF(TWTTBET+7)
             I1TEMP(2) = TRABUF(TWTTBET+8)
             I1TEMP(3) = TRABUF(TWTTBET+9)
             I1TEMP(4) = TRABUF(TWTTBET+10)
             LOGBUF(18)= I4TEMP

             I1TEMP(1) = TRABUF(TWTTBET+11)
             I1TEMP(2) = TRABUF(TWTTBET+12)
             I1TEMP(3) = TRABUF(TWTTBET+13)
             I1TEMP(4) = TRABUF(TWTTBET+14)
             LOGBUF(19)= I4TEMP

             I1TEMP(1) = TRABUF(TWTTBET+15)
             I1TEMP(2) = TRABUF(TWTTBET+16)
             I1TEMP(3) = TRABUF(TWTTBET+17)
             I1TEMP(4) = TRABUF(TWTTBET+18)
             LOGBUF(20)= I4TEMP

             I1TEMP(1) = TRABUF(TWTTBET+19)
             I1TEMP(2) = TRABUF(TWTTBET+20)
             I1TEMP(3) = TRABUF(TWTTBET+21)
             I1TEMP(4) = TRABUF(TWTTBET+22)
             LOGBUF(21)= I4TEMP

             I1TEMP(1) = TRABUF(TWTTBET+23)
             I1TEMP(2) = TRABUF(TWTTBET+24)
             I1TEMP(3) = TRABUF(TWTTBET+25)
             I1TEMP(4) = TRABUF(TWTTBET+26)
             LOGBUF(22)= I4TEMP

             GOTO 9000
          ENDIF
C
C SUPERSCORE WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TSSC) THEN
C
             LOGBUF(8) = TRABUF(TWCSER)
C
             I2TEMP(1) = TRABUF(TWDAMT)
             I2TEMP(2) = TRABUF(TWBEG )
             LOGBUF(9) = I4TEMP
C
             I4TEMP    = TRABUF(TWSSAMT)
             I1TEMP(4) = TRABUF(TWSYST)
             LOGBUF(10)= I4TEMP

             I1TEMP(1) = ISHFT(TRABUF(TWSSHM1),4) +
     *              IAND(TRABUF(TWSSAW1),'0F'X)
             I1TEMP(2) = ISHFT(TRABUF(TWSSHM2),4) +
     *              IAND(TRABUF(TWSSAW2),'0F'X)
             I1TEMP(3) = ISHFT(TRABUF(TWSSHM3),4) +
     *              IAND(TRABUF(TWSSAW3),'0F'X)
             I1TEMP(4) = TRABUF(TWSSBET)
             LOGBUF(11)= I4TEMP

             I1TEMP(1) = TRABUF(TWSSBET+1)
             I1TEMP(2) = TRABUF(TWSSBET+2)
             I1TEMP(3) = TRABUF(TWSSBET+3)
             I1TEMP(4) = TRABUF(TWSSBET+4)
             LOGBUF(12)= I4TEMP

             I1TEMP(1) = TRABUF(TWSSBET+5)
             I1TEMP(2) = TRABUF(TWVSTS)
             I2TEMP(2) = TRABUF(TWCTER)
             LOGBUF(13)=I4TEMP

             I4TEMP    = TRABUF(TWBNKID)
             I1TEMP(4) = 0
             LOGBUF(14)=I4TEMP

             LOGBUF(15) = TRABUF(TWBNKNM)
C
             I1TEMP(1) = 0
             IF(TRABUF(TWQPF).NE.0)  I1TEMP(1) = I1TEMP(1) + '80'X
             IF(TRABUF(TWFFLG).NE.0) I1TEMP(1) = I1TEMP(1) + '08'X
             I1TEMP(2) = TRABUF(TWTKC)
             I1TEMP(3) = TRABUF(TWSSCOUPID)
             LOGBUF(16)= I4TEMP
C
             IF(TRABUF(TWSYST).EQ.NOSYS) GOTO 9000
C
C SUPERSCORE CONTINUATION RECORD 1
C
             LOGBUF(17) = TRABUF(TWSYSN)

             I1TEMP(1) = TRABUF(TWSSBET+6)
             I1TEMP(2) = TRABUF(TWSSBET+7)
             I1TEMP(3) = TRABUF(TWSSBET+8)
             I1TEMP(4) = TRABUF(TWSSBET+9)
             LOGBUF(18)= I4TEMP

             I1TEMP(1) = TRABUF(TWSSBET+10)
             I1TEMP(2) = TRABUF(TWSSBET+11)
             I1TEMP(3) = TRABUF(TWSSBET+12)
             I1TEMP(4) = TRABUF(TWSSBET+13)
             LOGBUF(19)= I4TEMP

             I1TEMP(1) = TRABUF(TWSSBET+14)
             I1TEMP(2) = TRABUF(TWSSBET+15)
             I1TEMP(3) = TRABUF(TWSSBET+16)
             I1TEMP(4) = TRABUF(TWSSBET+17)
             LOGBUF(20)= I4TEMP

             I1TEMP(1) = TRABUF(TWSSBET+18)
             I1TEMP(2) = TRABUF(TWSSBET+19)
             I1TEMP(3) = TRABUF(TWSSBET+20)
             I1TEMP(4) = TRABUF(TWSSBET+21)
             LOGBUF(21)= I4TEMP

             I1TEMP(1) = TRABUF(TWSSBET+22)
             I1TEMP(2) = TRABUF(TWSSBET+23)
             I1TEMP(3) = TRABUF(TWSSBET+24)
             I1TEMP(4) = TRABUF(TWSSBET+25)
             LOGBUF(22)= I4TEMP

             I1TEMP(1) = TRABUF(TWSSBET+26)
             I1TEMP(2) = TRABUF(TWSSBET+27)
             I1TEMP(3) = TRABUF(TWSSBET+28)
             I1TEMP(4) = TRABUF(TWSSBET+29)
             LOGBUF(23)= I4TEMP

             GOTO 9000
          ENDIF
C
C SUPER TRIPLE WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TSTR) THEN
C
             LOGBUF(8) = TRABUF(TWCSER)
C
             I2TEMP(1) = TRABUF(TWDAMT)
             I2TEMP(2) = TRABUF(TWBEG )
             LOGBUF(9) = I4TEMP
C
             I4TEMP    = TRABUF(TWSTAMT)
             I1TEMP(4) = TRABUF(TWSYST)
             LOGBUF(10)= I4TEMP

             I1TEMP(1) = TRABUF(TWSTM1)
             I1TEMP(2) = TRABUF(TWSTM2)
             I1TEMP(3) = TRABUF(TWSTM3)
             I1TEMP(4) = TRABUF(TWSTBET)
             LOGBUF(11)= I4TEMP

             I1TEMP(1) = TRABUF(TWSTBET+1)
             I1TEMP(2) = TRABUF(TWSTBET+2)
             I2TEMP(2) = TRABUF(TWCTER)
             LOGBUF(12)=I4TEMP

             I4TEMP    = TRABUF(TWBNKID)
             I1TEMP(4) = TRABUF(TWVSTS)
             LOGBUF(13)=I4TEMP

             LOGBUF(14) = TRABUF(TWBNKNM)

             I2TEMP(1) = TRABUF(TWSYSN)
             I2TEMP(2) = 0
             LOGBUF(15)=I4TEMP
C
             I1TEMP(1) = 0
             IF(TRABUF(TWQPF).NE.0)  I1TEMP(1) = I1TEMP(1) + '80'X
             IF(TRABUF(TWFFLG).NE.0) I1TEMP(1) = I1TEMP(1) + '08'X
             I1TEMP(2) = TRABUF(TWTKC)
             I1TEMP(3) = TRABUF(TWSTCOUPID)
             LOGBUF(16)= I4TEMP
C
             IF(TRABUF(TWSYST).EQ.NOSYS) GOTO 9000
C
C SUPER TRIPLE CONTINUATION RECORD 1
C
             I1TEMP(1) = TRABUF(TWSTBET+3)
             I1TEMP(2) = TRABUF(TWSTBET+4)
             I1TEMP(3) = TRABUF(TWSTBET+5)
             I1TEMP(4) = TRABUF(TWSTBET+6)
             LOGBUF(17)= I4TEMP

             I1TEMP(1) = TRABUF(TWSTBET+7)
             I1TEMP(2) = TRABUF(TWSTBET+8)
             I1TEMP(3) = TRABUF(TWSTBET+9)
             I1TEMP(4) = TRABUF(TWSTBET+10)
             LOGBUF(18)= I4TEMP

             I1TEMP(1) = TRABUF(TWSTBET+11)
             I1TEMP(2) = TRABUF(TWSTBET+12)
             I1TEMP(3) = TRABUF(TWSTBET+13)
             I1TEMP(4) = TRABUF(TWSTBET+14)
             LOGBUF(19)= I4TEMP

             I1TEMP(1) = TRABUF(TWSTBET+15)
             I1TEMP(2) = TRABUF(TWSTBET+16)
             I1TEMP(3) = TRABUF(TWSTBET+17)
             I1TEMP(4) = 0
             LOGBUF(20)= I4TEMP

             GOTO 9000
          ENDIF
C
        ENDIF
C
C ENCODE VALIDATION BODY
C
        IF( (TRABUF(TTYP).EQ.TVAL.OR.TRABUF(TTYP).EQ.TREF) .AND.
     *       TRABUF(TGAMTYP).NE.TPAS.OR.
     *       TRABUF(TTYP).EQ.TVAL.AND.
     *       TRABUF(TGAMTYP).EQ.TPAS.AND.TRABUF(TVEPVAL).NE.0 )  THEN
C
           LOGBUF(8)  = TRABUF(TVSER) !WAGER SERIAL NUMBER
           LOGBUF(9)  = TRABUF(TVEXC) !EXCHANGE TICKET SERIAL #
           LOGBUF(10) = TRABUF(TVPAY) !AMOUNT PAID
           LOGBUF(11) = TRABUF(TVKPAY) !KICKER AMOUNT PAID

           I4TEMP     = TRABUF(TVREF) !REFUND AMOUNT
           I1TEMP(4)  = TRABUF(TVEPVAL) !NEW PASSIVE VALIDATION LAYOUT
           LOGBUF(12) = I4TEMP
C
           I2TEMP(1)  = TRABUF(TVCDC) !WAGER CDC
           I2TEMP(2)  = TRABUF(TVWCDC) !LAST WINNING CDC (WINSEL CDC)
           LOGBUF(13) = I4TEMP
C
           I2TEMP(1)  = TRABUF(TVWKCDC) !KICKER WINSEL CDC
           I2TEMP(2)  = 0
           LOGBUF(14) = I4TEMP
C
           I2TEMP(1)  = TRABUF(TVSTER) !WAGER SELL TERMINAL
           I1TEMP(3)  = TRABUF(TVCWT) !CHECKWRITER MESSAGE
           I1TEMP(4)  = TRABUF(TVTYPE) !VALIDATION TYPE
           LOGBUF(15) = I4TEMP
C
           I1TEMP(1)  = TRABUF(TVCODE) !VALIDATION TYPE CODE
           I1TEMP(2)  = TRABUF(TVKGME) !KICKER GAME NUMBER
           I1TEMP(3)  = TRABUF(TVSTS) !VALIDATION STATUS
           LOGBUF(16) = I4TEMP
C
C VALIDATION CONTINUATION RECORD 1
C
           IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
           LOGBUF(17) = TRABUF(TVBNKID) !BANK ID NUMBER
           LOGBUF(18) = TRABUF(TVBNKNUM) !BANK ACCOUNT NUMBER
           LOGBUF(19) = TRABUF(TVOPPAY) !AMOUNT IN OP'S FOR MAIN GAME     !V38
           LOGBUF(20) = TRABUF(TVKOPPAY) !AMOUNT IN OP'S FOR KICKER      !V34    !V38

C----+------------------------------------------------------------------
C V57| Adding new validation messages fields
C----+------------------------------------------------------------------
           IF(TRABUF(TGAMTYP).NE.TPAS) THEN
                LOGBUF(21) = TRABUF(TVPLCARD) !PLAYER CARD/TELEPHONE CONTACT NUMBER

                I2TEMP(1)   =  TRABUF(TVNIBBB) !BANK BRANCH
                I2TEMP(2)   =  TRABUF(TVNIBBO) !BANK OFFICE
                LOGBUF(22)  =  I4TEMP

                LOGBUF(23)  =  TRABUF(TVNIBBA1) !BANK ACCOUNT

                I1TEMP(1)   =  TRABUF(TVNIBBA2) !BANK ACCOUNT
                I1TEMP(2)   =  TRABUF(TVNIBCD) !CHECK DIGITS
                I1TEMP(3)   =  TRABUF(TVPLIDTYP) !PLAYER ID TYPE: 0 - TELEPHONE NUMBER /  1 - PLAYER CARD !V45
                I1TEMP(4)   =  0                 !V56
                LOGBUF(24)  =  I4TEMP

C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
                LOGBUF(25) = TRABUF(TVOLMSERL_TLTO)
                LOGBUF(26) = TRABUF(TVOLMSERM_TLTO)
                LOGBUF(28) = TRABUF(TVOLMMIDL_TLTO)
                I1TEMP(1)  = TRABUF(TVOLMSERH_TLTO)
                I1TEMP(2)  = TRABUF(TVOLMMIDH_TLTO)
                I1TEMP(3)  = TRABUF(TVOLMCOMF_TLTO)
                I1TEMP(4)  = 0
                LOGBUF(27) = I4TEMP
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------

                GOTO 9000
           ENDIF
C----+------------------------------------------------------------------
C V57| Adding new validation messages fields
C----+------------------------------------------------------------------

           I4TEMP      =  ISHFT(TRABUF(TPTCK),28) +
     *                    IAND (TRABUF(TPNUM1), '00FFFFFF'X)
           LOGBUF(21)  =  I4TEMP

           LOGBUF(22)  =  TRABUF(TPKEY1)
           LOGBUF(23)  =  TRABUF(TPPAY1)

           I2TEMP(1)   =  TRABUF(TPEMIS1)
           I1TEMP(3)   =  TRABUF(TPSER1)
           I1TEMP(4)   =  TRABUF(TPTEN1)
           LOGBUF(24)  =  I4TEMP

           I1TEMP(1)   =  TRABUF(TPSTS1)
           I1TEMP(2)   =  TRABUF(TVEPTYP)
           I2TEMP(2)   =  TRABUF(TPOFFTER)
           LOGBUF(25)  =  I4TEMP

           I1TEMP(1)   =  TRABUF(TVEPWK)
           I1TEMP(2)   =  TRABUF(TVEPYR)
C           I2TEMP(2)   =  0 !V56
           I1TEMP(3)   =  TRABUF(TVPLIDTYP) !V56
           I1TEMP(4)   =  0                 !V56
           LOGBUF(26)  =  I4TEMP

           LOGBUF(27) = TRABUF(TVPLCARD)

           I2TEMP(1)   =  TRABUF(TVNIBBB)
           I2TEMP(2)   =  TRABUF(TVNIBBO)
           LOGBUF(28)  =  I4TEMP

           LOGBUF(29)  =  TRABUF(TVNIBBA1)

           I1TEMP(1)   =  TRABUF(TVNIBBA2)
           I1TEMP(2)   =  TRABUF(TVNIBCD)
           LOGBUF(30)  =  I4TEMP
C
           GOTO 9000
        ENDIF
C
C PASSIVE LOTTERY "BUNCH" VALIDATION
C
        IF(TRABUF(TTYP).EQ.TVAL .AND. TRABUF(TGAMTYP).EQ.TPAS.AND.
     *     TRABUF(TVEPVAL).EQ.0) THEN

          I4TEMP      =  IOR(TRABUF(TPRETYP),ISHFT(TRABUF(TPTCK),4))
          I4TEMP      =  ISHFT(I4TEMP,24) +
     *                   IAND (TRABUF(TPNUM1), '00FFFFFF'X)
          LOGBUF(8)   =  I4TEMP

          LOGBUF(9)   =  TRABUF(TPKEY1)
          LOGBUF(10)  =  TRABUF(TPPAY1)

          I2TEMP(1)   =  TRABUF(TPEMIS1)
          I1TEMP(3)   =  TRABUF(TPSER1)
          I1TEMP(4)   =  TRABUF(TPTEN1)
          LOGBUF(11)  =  I4TEMP

          I4TEMP      =  TRABUF(TPNUM2)
          I1TEMP(4)   =  TRABUF(TVEPVAL)
          LOGBUF(12)  =  I4TEMP

          LOGBUF(13)  =  TRABUF(TPKEY2)
          LOGBUF(14)  =  TRABUF(TPPAY2)

          I2TEMP(1)   =  TRABUF(TPEMIS2)
          I1TEMP(3)   =  TRABUF(TPSER2)
          I1TEMP(4)   =  TRABUF(TPTEN2)
          LOGBUF(15)  =  I4TEMP

          I4TEMP      = TRABUF(TPOFFTER)
          I1TEMP(3)   = IAND(TRABUF(TPSTS1),'0F'X) + ISHFT(TRABUF(TPSTS2),4)
          LOGBUF(16)  = I4TEMP
C
C PASSIVE CONTINUATION RECORD 1
C
          LOGBUF(17)  =  TRABUF(TPKEY3)
          LOGBUF(18)  =  TRABUF(TPPAY3)

          I2TEMP(1)   =  TRABUF(TPEMIS3)
          I1TEMP(3)   =  TRABUF(TPSER3)
          I1TEMP(4)   =  TRABUF(TPTEN3)
          LOGBUF(19)  =  I4TEMP

          LOGBUF(20)  =  TRABUF(TPKEY4)
          LOGBUF(21)  =  TRABUF(TPPAY4)

          I2TEMP(1)   =  TRABUF(TPEMIS4)
          I1TEMP(3)   =  TRABUF(TPSER4)
          I1TEMP(4)   =  TRABUF(TPTEN4)
          LOGBUF(22)  =  I4TEMP

          LOGBUF(23)  =  TRABUF(TPKEY5)
          LOGBUF(24)  =  TRABUF(TPPAY5)

          I2TEMP(1)   =  TRABUF(TPEMIS5)
          I1TEMP(3)   =  TRABUF(TPSER5)
          I1TEMP(4)   =  TRABUF(TPTEN5)
          LOGBUF(25)  =  I4TEMP

          LOGBUF(26)  =  TRABUF(TPKEY6)
          LOGBUF(27)  =  TRABUF(TPPAY6)

          I2TEMP(1)   =  TRABUF(TPEMIS6)
          I1TEMP(3)   =  TRABUF(TPSER6)
          I1TEMP(4)   =  TRABUF(TPTEN6)
          LOGBUF(28)  =  I4TEMP

          I4TEMP      =  0
          I1TEMP(4)   =  IAND(TRABUF(TPSTS3),'0F'X) + ISHFT(TRABUF(TPSTS4),4)
          I4TEMP      =  I4TEMP +
     *                   IAND (TRABUF(TPNUM3),'00FFFFFF'X)
          LOGBUF(29)  =  I4TEMP

          I4TEMP      =  0
          I1TEMP(4)   =  IAND(TRABUF(TPSTS5),'0F'X) + ISHFT(TRABUF(TPSTS6),4)
          I4TEMP      =  I4TEMP +
     *                   IAND (TRABUF(TPNUM4),'00FFFFFF'X)
          LOGBUF(30)  =  I4TEMP

          LOGBUF(31)  =  TRABUF(TPNUM5)
          LOGBUF(32)  =  TRABUF(TPNUM6)
C
C PASSIVE CONTINUATION RECORD 2
C
          LOGBUF(33)  =  TRABUF(TPKEY7)
          LOGBUF(34)  =  TRABUF(TPPAY7)

          I2TEMP(1)   =  TRABUF(TPEMIS7)
          I1TEMP(3)   =  TRABUF(TPSER7)
          I1TEMP(4)   =  TRABUF(TPTEN7)
          LOGBUF(35)  =  I4TEMP

          LOGBUF(36)  =  TRABUF(TPKEY8)
          LOGBUF(37)  =  TRABUF(TPPAY8)

          I2TEMP(1)   =  TRABUF(TPEMIS8)
          I1TEMP(3)   =  TRABUF(TPSER8)
          I1TEMP(4)   =  TRABUF(TPTEN8)
          LOGBUF(38)  =  I4TEMP

          LOGBUF(39)  =  TRABUF(TPKEY9)
          LOGBUF(40)  =  TRABUF(TPPAY9)

          I2TEMP(1)   =  TRABUF(TPEMIS9)
          I1TEMP(3)   =  TRABUF(TPSER9)
          I1TEMP(4)   =  TRABUF(TPTEN9)
          LOGBUF(41)  =  I4TEMP

          LOGBUF(42)  =  TRABUF(TPKEY10)
          LOGBUF(43)  =  TRABUF(TPPAY10)

          I2TEMP(1)   =  TRABUF(TPEMIS10)
          I1TEMP(3)   =  TRABUF(TPSER10)
          I1TEMP(4)   =  TRABUF(TPTEN10)
          LOGBUF(44)  =  I4TEMP

          I4TEMP      =  0
          I1TEMP(4)   =  IAND(TRABUF(TPSTS7),'0F'X) + ISHFT(TRABUF(TPSTS8),4)
          I4TEMP      =  I4TEMP +
     *                   IAND (TRABUF(TPNUM7),'00FFFFFF'X)
          LOGBUF(45)  =  I4TEMP

          I4TEMP      =  0
          I1TEMP(4)   =  IAND(TRABUF(TPSTS9),'0F'X) + ISHFT(TRABUF(TPSTS10),4)
          I4TEMP      =  I4TEMP +
     *                   IAND (TRABUF(TPNUM8),'00FFFFFF'X)
          LOGBUF(46)  =  I4TEMP

          LOGBUF(47)  =  TRABUF(TPNUM9)
          LOGBUF(48)  =  TRABUF(TPNUM10)

          GOTO 9000
        ENDIF
C
C ENCODE INSTANTS
C
        IF(TRABUF(TTYP).EQ.TCRS) THEN
          LOGBUF(7)  = TRABUF(TIXRF)
C
          I4TEMP     = 0
          I1TEMP(1)  = TRABUF(TITYP)
          I1TEMP(2)  = TRABUF(TIERR)
          LOGBUF(16) = I4TEMP
C
          IF(TRABUF(TITYP).EQ.IVAL) THEN
C
            I4TEMP     = LOGBUF(16)
            I1TEMP(3)  = ISHFT(TRABUF(TIIND),4)+
     *                   IAND(TRABUF(TIBCH),'0F'X)
            LOGBUF(16) = I4TEMP
C
!-------->>V57 -------------------------------------------------------------------
!            I2TEMP(1) = TRABUF(TIVALM)
            I1TEMP(1)  = TRABUF(TIVMT)
            I1TEMP(2)  = TRABUF(TIVALM)
!-------- V57<<-------------------------------------------------------------------
            I2TEMP(2) = TRABUF(TIVALT)
            LOGBUF(8) = I4TEMP
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C New Terminal Project TIVENV only uses 2 digits = 1 byte (99<256)
C            LOGBUF(9) = TRABUF(TIVENV)
            IF(TRABUF(TVOLMCOMF_IL) .EQ. 1) THEN
              I4TEMP = TRABUF(TVOLMSERL_IL)
              I1TEMP(4) = I1TEMP(3)
              I1TEMP(3) = I1TEMP(2)
              I1TEMP(2) = I1TEMP(1)
              I1TEMP(1) = TRABUF(TIVENV)

              I1TEMP(1)  = IOR( ISHFT( TRABUF(TVOLMCOMF_IL), 7),
     *                       IAND( I1TEMP(1),'7F'X) )

              LOGBUF(9) = I4TEMP
            ELSE
              LOGBUF(9) = TRABUF(TIVENV)
            ENDIF
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C
            I4TEMP = TRABUF(TIVAGT)
            I1TEMP(4) = TRABUF(TIVTYP)
            LOGBUF(10) = I4TEMP
C
            IF(TRABUF(TIVMT) .EQ. IRVMT) THEN !OLD LAYOUT
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C beginof - the remaining 6 bytes of Olimpo serial (giving the total of the 9 bytes)
C HAVES MORE THAN 4 TICKETS TO VALIDATE THEN 3 SEGMENTS IN USE
               IF(TRABUF(TVOLMCOMF_IL) .EQ. 1) THEN
                  IF(TRABUF(TIBCH) .GE. 4) THEN
                          I4TEMP = TRABUF(TVOLMSERL_IL)
                          I1TEMP_AUX(1) = I1TEMP(4)
                          I4TEMP = TRABUF(TVOLMSERM_IL)
                          I1TEMP(4) = 0
                          I1TEMP(3) = I1TEMP(2)
                          I1TEMP(2) = I1TEMP(1)
                          I1TEMP(1) = I1TEMP_AUX(1)
                          LOGBUF(32) = I4TEMP

                          I4TEMP = TRABUF(TVOLMSERM_IL)
                          I1TEMP_AUX(1) = I1TEMP(3)
                          I1TEMP_AUX(2) = I1TEMP(4)
                          I1TEMP_AUX(3) = TRABUF(TVOLMSERH_IL)
                          I1TEMP(1) = I1TEMP_AUX(1)
                          I1TEMP(2) = I1TEMP_AUX(2)
                          I1TEMP(3) = I1TEMP_AUX(3)
                          I1TEMP(4) = 0
                          LOGBUF(48) = I4TEMP
C HAVES MORE THA  N 1 TICKETS TO VALIDATE THEN 2 SEGMENTS IN USE
                  ELSEIF(TRABUF(TIBCH) .GT. 1) THEN
                          I4TEMP = TRABUF(TVOLMSERL_IL)
                          I1TEMP_AUX(1) = I1TEMP(4)
                          I4TEMP = TRABUF(TVOLMSERM_IL)
                          I1TEMP(4) = 0
                          I1TEMP(3) = I1TEMP(2)
                          I1TEMP(2) = I1TEMP(1)
                          I1TEMP(1) = I1TEMP_AUX(1)
                          LOGBUF(32) = I4TEMP

                          I4TEMP = TRABUF(TVOLMSERM_IL)
                          I1TEMP_AUX(1) = I1TEMP(3)
                          I1TEMP_AUX(2) = I1TEMP(4)
                          I1TEMP_AUX(3) = TRABUF(TVOLMSERH_IL)
                          I1TEMP(1) = I1TEMP_AUX(1)
                          I1TEMP(2) = I1TEMP_AUX(2)
                          I1TEMP(3) = I1TEMP_AUX(3)
                          I1TEMP(4) = 0
                          LOGBUF(31) = I4TEMP
C HAVES 1 TICKET  S TO VALIDATE THEN 1 SEGMENT IN USE BUT NO SPACE FREE (LOGBUF)... USES TWO SEGMENTS TO HAVE SPACE

                  ELSE
                          I4TEMP = TRABUF(TVOLMSERL_IL)
                          I1TEMP_AUX(1) = I1TEMP(4)
                          I4TEMP = TRABUF(TVOLMSERM_IL)
                          I1TEMP(4) = I1TEMP(3)
                          I1TEMP(3) = I1TEMP(2)
                          I1TEMP(2) = I1TEMP(1)
                          I1TEMP(1) = I1TEMP_AUX(1)
                          LOGBUF(17) = I4TEMP

                          I4TEMP = TRABUF(TVOLMSERM_IL)
                          I1TEMP_AUX(1) = I1TEMP(4)
                          I1TEMP_AUX(2) = TRABUF(TVOLMSERH_IL)
                          I1TEMP(1) = I1TEMP_AUX(1)
                          I1TEMP(2) = I1TEMP_AUX(2)
                          I1TEMP(3) = 0
                          I1TEMP(4) = 0
                          LOGBUF(18) = I4TEMP

                          I4TEMP = TRABUF(TVOLMMIDL_IL)
                          LOGBUF(19) = I4TEMP

                          I4TEMP = TRABUF(TVOLMMIDH_IL)
                          LOGBUF(20) = I4TEMP

                  ENDIF
               ENDIF
C endof - the remaining 6 bytes of Olimpo serial (giving the total of the 9 bytes)
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C
              BUFIDX = 11
              DO 2000 X = 0, TRABUF(TIBCH)-1
                I4TEMP     = IAND(TRABUF(TIPCK1+X),'00FFFFFF'X)
                I1TEMP(4)  = TRABUF(TISTS1+X)
                LOGBUF(BUFIDX)  = I4TEMP
                BUFIDX = BUFIDX + 1
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C                IF(X .EQ. 0) THEN
C                 COMMUNICATIONS CHANNEL FLAG (OLM or not from OLM)
C                  I4TEMP  = IOR( ISHFT( TRABUF(TVOLMCOMF_IL+X), 30),
C     *                         IAND(  TRABUF(TIVRN1+X),'03FFFFFFF'X) )
C                  LOGBUF(BUFIDX) = I4TEMP
C                ELSE
C
                  LOGBUF(BUFIDX) = TRABUF(TIVRN1+X)
C                ENDIF
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C V60                LOGBUF(BUFIDX) = TRABUF(TIVRN1+X)
                BUFIDX         = BUFIDX + 1
C
                I2TEMP(1)  = TRABUF(TILTX1+X)
                I2TEMP(2)  = IOR( ISHFT( TRABUF(TIPCKSTS1+X), 12),
     *                            IAND(  TRABUF(TIGAM1+X),'0FFF'X) )
                LOGBUF(BUFIDX) = I4TEMP
                BUFIDX = BUFIDX + 1
C
                I2TEMP(1)      = TRABUF(TITIM1+X)/2
                I2TEMP(2)      = TRABUF(TICDC1+X)
                LOGBUF(BUFIDX) = I4TEMP
                BUFIDX         = BUFIDX + 1
C
                LOGBUF(BUFIDX) = TRABUF(TIPRZ1+X)
                BUFIDX = BUFIDX + 1

                IF (MOD(BUFIDX,16).EQ.0) BUFIDX = BUFIDX + 1
2000          CONTINUE
C
C IF TRABUF(TIVTYP) IT'S IVTP_NCP TRABUF(TIBCH) SHOULD BE ALWAYS ONE
C
              IF(TRABUF(TIVTYP) .EQ. IVTP_NCP) THEN
                CALL FASTMOV(TRABUF(TIVDESCR), LOGBUF(17), 5)
              ENDIF

            ELSEIF(TRABUF(TIVMT) .EQ. IBVMT) THEN ! NEW BANK VALIDATION MODE LAYOUT
!-------->>V57 -------------------------------------------------------------------
                ! TRABUF(TIBCH) IS ALWAYS ONE
              I4TEMP     = IAND(TRABUF(TIPCK1),'00FFFFFF'X)
              I1TEMP(4)  = TRABUF(TISTS1)
              LOGBUF(11) = I4TEMP
C
              LOGBUF(12) = TRABUF(TIVRN1)
C
              I2TEMP(1)  = TRABUF(TILTX1)
              I2TEMP(2)  = IOR(ISHFT(TRABUF(TIPCKSTS1),12),
     *                         IAND(TRABUF(TIGAM1),'0FFF'X))
              LOGBUF(13) = I4TEMP
C
              I2TEMP(1)  = TRABUF(TITIM1)/2
              I2TEMP(2)  = TRABUF(TICDC1)
              LOGBUF(14) = I4TEMP
C
              LOGBUF(15) = TRABUF(TIPRZ1)
C
C             LOGBUF(16) ALREADY IN USE! DON'T USE IT!
C
              I4TEMP = 0
              I1TEMP(1)  = TRABUF(TIPLIDTYP) !PLAYER ID TYPE
              I1TEMP(2)  = TRABUF(TINIBBA2)  !NIB ACCOUNT NUMBER (PART 2)
              I1TEMP(3)  = TRABUF(TINIBCD)   !NIB CHECK DIGITS
              LOGBUF(17) = I4TEMP
C
              LOGBUF(18) = TRABUF(TIPLCARD)  !PLAYER ID
C
              I2TEMP(1)  =  TRABUF(TINIBBB)  !NIB BRANCH
              I2TEMP(2)  =  TRABUF(TINIBBO)  !NIB OFFICE
              LOGBUF(19) =  I4TEMP
C
              LOGBUF(20) =  TRABUF(TINIBBA1) !NIB ACCOUNT NUMBER (PART 1)
C
              LOGBUF(21) = TRABUF(TINETPRZ)  !NET PRIZE AMOUNT
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C beginof - the remaining 6 bytes of Olimpo serial (giving the total of the 9 bytes)
              IF(TRABUF(TVOLMCOMF_IL) .EQ. 1) THEN
                I4TEMP = TRABUF(TVOLMSERL_IL)
                I1TEMP_AUX(1) = I1TEMP(4)
                I4TEMP = TRABUF(TVOLMSERM_IL)
                I1TEMP(4) = I1TEMP(3)
                I1TEMP(3) = I1TEMP(2)
                I1TEMP(2) = I1TEMP(1)
                I1TEMP(1) = I1TEMP_AUX(1)
                LOGBUF(27) = I4TEMP

                I4TEMP = TRABUF(TVOLMSERM_IL)
                I1TEMP_AUX(1) = I1TEMP(4)
                I1TEMP_AUX(2) = TRABUF(TVOLMSERH_IL)
                I1TEMP(1) = I1TEMP_AUX(1)
                I1TEMP(2) = I1TEMP_AUX(2)
                I1TEMP(3) = 0
                I1TEMP(4) = 0
                LOGBUF(28) = I4TEMP

                I4TEMP = TRABUF(TVOLMMIDL_IL)
                LOGBUF(29) = I4TEMP
                I1TEMP(1) = TRABUF(TVOLMMIDH_IL)
                I1TEMP(2) = 0
                I1TEMP(3) = 0
                I1TEMP(4) = 0
                LOGBUF(30) = I4TEMP
              ENDIF
C endof - the remaining 6 bytes of Olimpo serial (giving the total of the 9 bytes)
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------

C
              IF(TRABUF(TIVTYP) .EQ. IVBM_NOPRZ) THEN
C               LOGBUF(22), LOGBUF(23), LOGBUF(24), LOGBUF(25), LOGBUF(26)
                CALL FASTMOV(TRABUF(TIVDESCR), LOGBUF(22), 5)
              ENDIF
!-------- V57<<-------------------------------------------------------------------
            ENDIF
C
          ELSE IF(TRABUF(TITYP).EQ.IISS) THEN  ! V12 !INSTANT ISSUE
C
            LOGBUF(8)  = TRABUF(TIREP)
C
            I1TEMP(1)  = TRABUF(TINUM)
            I1TEMP(2)  = 0
            I1TEMP(3)  = 0
            I1TEMP(4)  = 0
            LOGBUF(10) = I4TEMP
C
            BUFIDX=11
            DO X = 0, TRABUF(TINUM)- 1
C
                I2TEMP(1) = TRABUF(TIGAM+X) !(TIGAM=30)            !INSTANT GAME NUMBER
                I2TEMP(2) = 0
                LOGBUF(BUFIDX) = I4TEMP! LOGBUF(11)
C
                IF (MOD(BUFIDX+1,16).EQ.0) THEN
                    BUFIDX = BUFIDX + 2
                ELSE
                    BUFIDX = BUFIDX + 1
                ENDIF
C
                I4TEMP    = TRABUF(TIPCK+X)
                I1TEMP(4) = TRABUF(TIRES+X)
                LOGBUF(BUFIDX) = I4TEMP !LOGBUF(12) or LOGBUF(13)
                BUFIDX=BUFIDX+1
C
            END DO
C
          ELSE IF(TRABUF(TITYP).EQ.ILOT) THEN
C
            LOGBUF(8)  = TRABUF(TLREP)
C
            I1TEMP(1)  = TRABUF(TLCLS)
            I1TEMP(2)  = 0
            I2TEMP(2)  = TRABUF(TLGAM)
            LOGBUF(9)  = I4TEMP
C
            I2TEMP(1)  = TRABUF(TLSTR)
            I2TEMP(2)  = TRABUF(TLEND)
            LOGBUF(10) = I4TEMP
C
            I4TEMP     = TRABUF(TLPCK)
            I1TEMP(4)  = 0
            LOGBUF(11) = I4TEMP
C
            LOGBUF(12)  = TRABUF(TLAMT)
C
            LOGBUF(13)  = TRABUF(TLCOM)

C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C           Begin - Olimpo Serial Number & MessageID & Communication Flag
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
              I4TEMP = TRABUF(TGOLMSERL_IL)
              LOGBUF(14) = I4TEMP
              I4TEMP = TRABUF(TGOLMSERM_IL)
              LOGBUF(15) = I4TEMP
              I4TEMP = TRABUF(TGOLMSERH_IL)
              LOGBUF(17) = I4TEMP
              I4TEMP = TRABUF(TGOLMMIDL_IL)
              LOGBUF(18) = I4TEMP
              I4TEMP = TRABUF(TGOLMMIDH_IL)
              LOGBUF(19) = I4TEMP
              I4TEMP = TRABUF(TGOLMCOMF_IL)
              LOGBUF(20) = I4TEMP
            ENDIF
C           End - Olimpo Serial Number & MessageID & Communication Flag
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
          ELSE IF(TRABUF(TITYP).EQ.ICAR) THEN
C
            LOGBUF(8)  = TRABUF(TCREP)
C
            I1TEMP(1)  = TRABUF(TCCLS)
            I1TEMP(2)  = 0
            I2TEMP(2)  = TRABUF(TCGAM)
            LOGBUF(9)  = I4TEMP
C
            LOGBUF(10) = TRABUF(TCCAR)
C
            LOGBUF(11) = TRABUF(TCSTA)
C
            LOGBUF(12) = TRABUF(TCEND)
C
            LOGBUF(13) = TRABUF(TCCNT)
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C           Begin - Olimpo Serial Number & MessageID & Communication Flag
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
              I4TEMP = TRABUF(TGOLMSERL_IL)
              LOGBUF(14) = I4TEMP
              I4TEMP = TRABUF(TGOLMSERM_IL)
              LOGBUF(15) = I4TEMP
              I4TEMP = TRABUF(TGOLMSERH_IL)
              LOGBUF(17) = I4TEMP
              I4TEMP = TRABUF(TGOLMMIDL_IL)
              LOGBUF(18) = I4TEMP
              I4TEMP = TRABUF(TGOLMMIDH_IL)
              LOGBUF(19) = I4TEMP
              I4TEMP = TRABUF(TGOLMCOMF_IL)
              LOGBUF(20) = I4TEMP
            ENDIF
C           End - Olimpo Serial Number & MessageID & Communication Flag
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C
          ELSE IF(TRABUF(TITYP).EQ.IQTA.OR.
     *            TRABUF(TITYP).EQ.IINV.OR.
     *            TRABUF(TITYP).EQ.ISET) THEN
C
            I2TEMP(1)  = IOR( ISHFT(TRABUF(TRGAM),4), TRABUF(TRCLS) )
            I2TEMP(2)  = TRABUF(TRNXT1)
            LOGBUF(8)  = I4TEMP
C
            LOGBUF(9)  = TRABUF(TRNXT2)
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C           Begin - Olimpo Serial Number & MessageID & Communication Flag
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1 .AND. TRABUF(TITYP).EQ.IQTA) THEN
              I4TEMP = TRABUF(TGOLMSERL_IL)
              LOGBUF(10) = I4TEMP
              I4TEMP = TRABUF(TGOLMSERM_IL)
              LOGBUF(11) = I4TEMP
              I4TEMP = TRABUF(TGOLMSERH_IL)
              LOGBUF(12) = I4TEMP
              I4TEMP = TRABUF(TGOLMMIDL_IL)
              LOGBUF(13) = I4TEMP
              I4TEMP = TRABUF(TGOLMMIDH_IL)
              LOGBUF(14) = I4TEMP
              I4TEMP = TRABUF(TGOLMCOMF_IL)
              LOGBUF(15) = I4TEMP
            ENDIF
C           End - Olimpo Serial Number & MessageID & Communication Flag
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C
          ELSE IF(TRABUF(TITYP).EQ.IFIN) THEN
C
            I1TEMP(1)  = TRABUF(TRTYP)
            I1TEMP(2)  = TRABUF(TRSUB)
            I1TEMP(3)  = 0
            I1TEMP(4)  = 0
            LOGBUF(8)  = I4TEMP
C
            LOGBUF(9)  = TRABUF(TRCHN)
C
            LOGBUF(10) = TRABUF(TRCON)
C
            LOGBUF(11) = TRABUF(TRCON1)
C
            LOGBUF(12) = TRABUF(TRCON2)
C
            LOGBUF(13) = TRABUF(TRCON3)
C
          ELSE IF(TRABUF(TITYP).EQ.IORD) THEN
C
            I4TEMP     = 0
            I2TEMP(1)  = TRABUF(TGPGAM)
            I2TEMP(2)  = TRABUF(TGPNXT)
            LOGBUF(8)  = I4TEMP
C
            LOGBUF(9)  = TRABUF(TGPAGT)
C
            LOGBUF(10) = TRABUF(TGPRCL)
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C           Begin - Olimpo Serial Number & MessageID & Communication Flag
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
              I4TEMP = TRABUF(TGOLMSERL_IL)
              LOGBUF(11) = I4TEMP
              I4TEMP = TRABUF(TGOLMSERM_IL)
              LOGBUF(12) = I4TEMP
              I4TEMP = TRABUF(TGOLMSERH_IL)
              LOGBUF(13) = I4TEMP
              I4TEMP = TRABUF(TGOLMMIDL_IL)
              LOGBUF(14) = I4TEMP
              I4TEMP = TRABUF(TGOLMMIDH_IL)
              LOGBUF(15) = I4TEMP
              I4TEMP     = LOGBUF(16)
              I1TEMP(3)  = TRABUF(TGOLMCOMF_IL)
              LOGBUF(16) = I4TEMP
C              I4TEMP = TRABUF(TGOLMCOMF_IL)
C              LOGBUF(17) = I4TEMP
            ENDIF
C           End - Olimpo Serial Number & MessageID & Communication Flag
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C
          ELSE IF(TRABUF(TITYP).EQ.IMNU) THEN
C
            I4TEMP     = LOGBUF(16)
            I1TEMP(3)  = TRABUF(TIBCH)
            LOGBUF(16) = I4TEMP
C
            LOGBUF(8)  = TRABUF(TSORD)
C
            LOGBUF(9)  = TRABUF(TSINF)
C
            IND=1
            DO X = 0, TRABUF(TIBCH)-1
C
                GNUM = IAND(TRABUF(TSGAM+X),'0FFF'X)
                GNUM = GNUM*1000 + TRABUF(TSQTY+X)
                GTYP = ISHFT(TRABUF(TSGAM+X),-12)
C
                I4TEMP = 0
                I4TEMP = ISHFT(GTYP,20) + IAND(GNUM,'000FFFFF'X)
C
                IF(TSBIT(TRABUF(TSSTK1),X)) I4TEMP = IOR(I4TEMP,'00800000'X)
C
                CALL MOVBYT(I4TEMP,1,BUFF,IND,3)
                IND=IND+3
            END DO
C
C----+------------------------------------------------------------------
C V60| begin New Terminals Project - Olimpo
C----+------------------------------------------------------------------
            IF(TRABUF(TGOLMCOMF_IL).NE.1) THEN
               CALL MOVBYT(BUFF(1),1,LOGBUF(10),1,24)
            ELSE IF(TRABUF(TGOLMCOMF_IL).EQ.1 .AND. TRABUF(TIBCH).LE.3) THEN
               CALL MOVBYT(BUFF(1),1,LOGBUF(10),1,9)
            ELSE IF(TRABUF(TGOLMCOMF_IL).EQ.1 .AND. TRABUF(TIBCH).GT.3) THEN
               CALL MOVBYT(BUFF(1),1,LOGBUF(10),1,24)
            ENDIF

            IF(TRABUF(TGOLMCOMF_IL).NE.1 .AND. TRABUF(TIBCH).GE.9) THEN
               CALL MOVBYT(BUFF(25),1,LOGBUF(17),1,60)
            ELSE IF(TRABUF(TGOLMCOMF_IL).EQ.1 .AND. TRABUF(TIBCH).GT.3 .AND. TRABUF(TIBCH).LE.24) THEN
               CALL MOVBYT(BUFF(25),1,LOGBUF(17),1,48)
            ELSE IF(TRABUF(TGOLMCOMF_IL).EQ.1 .AND. TRABUF(TIBCH).GT.24) THEN
               CALL MOVBYT(BUFF(25),1,LOGBUF(17),1,60)
            ENDIF

            IF(TRABUF(TGOLMCOMF_IL).NE.1 .AND. TRABUF(TIBCH).GE.29) THEN
               CALL MOVBYT(BUFF(85),1,LOGBUF(33),1,36)
            ELSE IF(TRABUF(TGOLMCOMF_IL).EQ.1 .AND. TRABUF(TIBCH).GT.24) THEN
               CALL MOVBYT(BUFF(85),1,LOGBUF(33),1,36)
            ENDIF
C
C            CALL MOVBYT(BUFF(1),1,LOGBUF(10),1,24)      V60
C
C            IF(TRABUF(TIBCH).GE.9)                      V60
C     *        CALL MOVBYT(BUFF(25),1,LOGBUF(17),1,60)   V60
C            IF(TRABUF(TIBCH).GE.29)                     V60
C     *        CALL MOVBYT(BUFF(85),1,LOGBUF(33),1,36)   V60
C
C
C           Begin - Olimpo Serial Number & MessageID & Communication Flag
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
CCCCCCCCCCCCCCCCCCCCCCCCC THIRD SEGMENT CCCCCCCCCCCCCCCCCCCCCCCCC
               IF(TRABUF(TIBCH).GT.24) THEN
                  I4TEMP = TRABUF(TGOLMSERL_IL)
                  LOGBUF(45) = I4TEMP
                  I4TEMP = TRABUF(TGOLMSERM_IL)
                  LOGBUF(46) = I4TEMP
                  I4TEMP = TRABUF(TGOLMMIDL_IL)
                  LOGBUF(47) = I4TEMP

                  I1TEMP(1) = TRABUF(TGOLMCOMF_IL)
                  I1TEMP(2) = TRABUF(TGOLMSERH_IL)
                  I1TEMP(3) = TRABUF(TGOLMMIDH_IL)
                  I1TEMP(4) = 0
                  LOGBUF(48) = I4TEMP
CCCCCCCCCCCCCCCCCCCCCCCCC SECOND SEGMENT CCCCCCCCCCCCCCCCCCCCCCCCC
               ELSE IF(TRABUF(TIBCH).LE.24 .AND. TRABUF(TIBCH).GT.3) THEN
                  I4TEMP = TRABUF(TGOLMSERL_IL)
                  LOGBUF(29) = I4TEMP
                  I4TEMP = TRABUF(TGOLMSERM_IL)
                  LOGBUF(30) = I4TEMP
                  I4TEMP = TRABUF(TGOLMMIDL_IL)
                  LOGBUF(31) = I4TEMP

                  I1TEMP(1) = TRABUF(TGOLMCOMF_IL)
                  I1TEMP(2) = TRABUF(TGOLMSERH_IL)
                  I1TEMP(3) = TRABUF(TGOLMMIDH_IL)
                  I1TEMP(4) = 0
                  LOGBUF(32) = I4TEMP
CCCCCCCCCCCCCCCCCCCCCCCCC FIRST SEGMENT CCCCCCCCCCCCCCCCCCCCCCCCC
               ELSE IF(TRABUF(TIBCH).LE.3) THEN   
                  I4TEMP = LOGBUF(12)
                  I1TEMP(2) = TRABUF(TGOLMSERH_IL)
                  I1TEMP(3) = TRABUF(TGOLMMIDH_IL)
                  I1TEMP(4) = TRABUF(TGOLMCOMF_IL)
                  LOGBUF(12) = I4TEMP
                  I4TEMP = TRABUF(TGOLMSERL_IL)
                  LOGBUF(13) = I4TEMP
                  I4TEMP = TRABUF(TGOLMSERM_IL)
                  LOGBUF(14) = I4TEMP
                  I4TEMP = TRABUF(TGOLMMIDL_IL)
                  LOGBUF(15) = I4TEMP
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
               ENDIF
            ENDIF
C           End - Olimpo Serial Number & MessageID & Communication Flag
C----+------------------------------------------------------------------
C V60| end New Terminals Project - Olimpo
C----+------------------------------------------------------------------
            GOTO 9000
C
          ELSE IF(TRABUF(TITYP).EQ.ICNF) THEN
C
            I2TEMP(1)  = TRABUF(TIINV1)
            I2TEMP(2)  = TRABUF(TIINV2)
            LOGBUF(8)  = I4TEMP
C
            LOGBUF(9)  = TRABUF(TIINV3)
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C           Begin - Olimpo Serial Number & MessageID & Communication Flag
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
               I4TEMP = TRABUF(TGOLMSERL_IL)
               LOGBUF(10) = I4TEMP
               I4TEMP = TRABUF(TGOLMSERM_IL)
               LOGBUF(11) = I4TEMP
               I4TEMP = TRABUF(TGOLMSERH_IL)
               LOGBUF(12) = I4TEMP
               I4TEMP = TRABUF(TGOLMMIDL_IL)
               LOGBUF(13) = I4TEMP
               I4TEMP = TRABUF(TGOLMMIDH_IL)
               LOGBUF(14) = I4TEMP
               I4TEMP = TRABUF(TGOLMCOMF_IL)
               LOGBUF(15) = I4TEMP
            ENDIF
C           End - Olimpo Serial Number & MessageID & Communication Flag
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C
          ELSE IF(TRABUF(TITYP).EQ.IOACT) THEN
C
            I2TEMP(1)  = TRABUF(TOINV1)
            I2TEMP(2)  = TRABUF(TOINV2)
            LOGBUF(8)  = I4TEMP
C
            LOGBUF(9)  = TRABUF(TOINV3)
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C           Begin - Olimpo Serial Number & MessageID & Communication Flag
            I4TEMP = TRABUF(TGOLMSERL_IL)
            LOGBUF(10) = I4TEMP
            I4TEMP = TRABUF(TGOLMSERM_IL)
            LOGBUF(11) = I4TEMP
            I4TEMP = TRABUF(TGOLMSERH_IL)
            LOGBUF(12) = I4TEMP
            I4TEMP = TRABUF(TGOLMMIDL_IL)
            LOGBUF(13) = I4TEMP
            I4TEMP = TRABUF(TGOLMMIDH_IL)
            LOGBUF(14) = I4TEMP
            I4TEMP = TRABUF(TGOLMCOMF_IL)
            LOGBUF(15) = I4TEMP
C           End - Olimpo Serial Number & MessageID & Communication Flag
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C
          ELSE IF(TRABUF(TITYP).EQ.ISON.OR.
     *            TRABUF(TITYP).EQ.ISOF) THEN
C
            LOGBUF(8)  = TRABUF(TIOLD)
            LOGBUF(9)  = TRABUF(TINEW)
            LOGBUF(10) = TRABUF(TISGN)
            LOGBUF(11) = TRABUF(TIGVT1)
            LOGBUF(12) = TRABUF(TIGVT2)
            LOGBUF(13) = TRABUF(TIGVT3)

            I1TEMP(1)  = TRABUF(TICLS)
            I1TEMP(2)  = TRABUF(TIACT)
            I1TEMP(3)  = TRABUF(TISTS)
            I1TEMP(4)  = 0
            LOGBUF(14) = I4TEMP

            LOGBUF(15) = TRABUF(TISTN)
C
          ELSE IF(TRABUF(TITYP).EQ.IEST) THEN
C
            LOGBUF(8)  = TRABUF(TISFT)
            LOGBUF(10) = TRABUF(TIPHONE1_1)
            LOGBUF(11) = TRABUF(TIPHONE1_2)
            LOGBUF(12) = TRABUF(TIPHONE1_3)
            LOGBUF(13) = TRABUF(TIPHONE2_1)
            LOGBUF(14) = TRABUF(TIPHONE2_2)
            LOGBUF(15) = TRABUF(TIPHONE2_3)
C
            I4TEMP = TRABUF(TIRSTTIM)
            I1TEMP(4) = TRABUF(TIRSTFLG)
            LOGBUF(9) = I4TEMP
C
            I4TEMP    = LOGBUF(16)
            I1TEMP(3) = TRABUF(TICHKSUM)
            LOGBUF(16)= I4TEMP
C
C begin continuation record 1
C
            I2TEMP(1) = TRABUF(TIMINCB)
            LOGBUF(17)= I4TEMP
C
          ELSE IF(TRABUF(TITYP).EQ.IGTB) THEN
C
            LOGBUF(8)  = TRABUF(TIGMC)
            LOGBUF(9)  = TRABUF(TIGMN+0)
            LOGBUF(10) = TRABUF(TIGMN+1)
            LOGBUF(11) = TRABUF(TIGMN+2)
            LOGBUF(12) = TRABUF(TIGMN+3)
            LOGBUF(13) = TRABUF(TIGMN+4)
C
          ELSE IF(TRABUF(TITYP).EQ.IFSESON) THEN !V09
C
            I1TEMP(1)  = TRABUF(TIFSETYP)
            I1TEMP(2)  = TRABUF(TIFSERSLT)
            I2TEMP(2)  = TRABUF(TIFSEOFF)
            LOGBUF(8)  = I4TEMP
C
            LOGBUF(9)  = TRABUF(TIFSEREP)
            I4TEMP     = 0
            I2TEMP(1)  = TRABUF(TIFSECLS)
            LOGBUF(10) = I4TEMP
C
          ENDIF
          GOTO 9000
        ENDIF
C
C
C ENCODE SPECIAL SERVICE BODY
C
        IF(TRABUF(TTYP).EQ.TSPE) THEN
           IF(TRABUF(TSFUN).EQ.TSX2X) THEN
C
              LOGBUF(8)=TRABUF(TXIDX)
C
              I1TEMP(1) = TRABUF(TXPTL)
              I1TEMP(2) = TRABUF(TXLAY)
              I2TEMP(2) = TRABUF(TXSTN)
              LOGBUF(9) = I4TEMP
C
              I4TEMP    = 0
              I1TEMP(1) = TRABUF(TXSAP)
              LOGBUF(10)= I4TEMP
C
              IF(TRABUF(TXLAY).EQ.X2X_TRATYP_GLO) THEN
C
              ELSE IF(TRABUF(TXLAY).EQ.X2X_TRATYP_XPORT) THEN
                 I4TEMP    = LOGBUF(10)
                 I1TEMP(2) = TRABUF(TXTFEID)
                 I1TEMP(3) = TRABUF(TXTDSAP)
                 I1TEMP(4) = TRABUF(TXTBTYP)
                 LOGBUF(10)= I4TEMP
C
              ELSE IF(TRABUF(TXLAY).EQ.X2X_TRATYP_FE) THEN
                 I4TEMP    = LOGBUF(10)
                 I1TEMP(2) = TRABUF(TXFPID)
                 I1TEMP(3) = TRABUF(TXFSSAP)
                 I1TEMP(4) = TRABUF(TXFMDUT)
                 LOGBUF(10)= I4TEMP
C
                 LOGBUF(11)= TRABUF(TXFDAD1)
                 LOGBUF(12)= TRABUF(TXFDAD2)
C
                 IF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_CMD) THEN
                    I4TEMP    = ISHFT(TRABUF(TXFCC),8)
                    I1TEMP(1) = TRABUF(TXFCFID)
                    LOGBUF(13)= I4TEMP
C
                 ELSE IF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_ACK) THEN
C
                 ELSE IF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_ERR) THEN
                    I4TEMP    = 0
                    I1TEMP(1) = TRABUF(TXFDEC)
                    LOGBUF(13)= I4TEMP
C
                 ELSE IF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_ALARM) THEN
                    I4TEMP    = ISHFT(TRABUF(TXFLMC),8)
                    I1TEMP(1) = TRABUF(TXFLFID)
                    LOGBUF(13)= I4TEMP
C
                 ELSE IF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_UP) THEN
                    LOGBUF(13)= TRABUF(TXFALT1)
                    LOGBUF(14)= TRABUF(TXFALT2)
                    IF(TRABUF(TXPTL).EQ.X2ERR_GOODVS) THEN
                      I4TEMP     = ISHFT(TRABUF(TXFVS),8)
                      LOGBUF(15) = I4TEMP
                    ENDIF
C
                 ENDIF
C
              ELSE IF(TRABUF(TXLAY).EQ.X2X_TRATYP_STTN) THEN
                 I4TEMP    = LOGBUF(10)
                 I1TEMP(2) = TRABUF(TXSPID)
                 I1TEMP(3) = TRABUF(TXSSDTU)
                 LOGBUF(10)= I4TEMP
C
                 IF(TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_CMD_UP) THEN
                    I4TEMP    = LOGBUF(10)
                    I1TEMP(4) = TRABUF(TXSCC)
                    LOGBUF(10)= I4TEMP
C
                    I4TEMP    = 0
                    I2TEMP(1) = TRABUF(TXSSNUM)
                    LOGBUF(11)= I4TEMP
C
                 ELSE IF((TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_RESET).OR.
     *                (TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_RESET2)) THEN
                    I4TEMP    = LOGBUF(10)
                    I1TEMP(4) = TRABUF(TXSSTYP)
                    LOGBUF(10)= I4TEMP
C
                    I4TEMP    = 0
                    I2TEMP(1) = TRABUF(TXSSPHID)
                    LOGBUF(11)= I4TEMP
C
                 ENDIF
              ENDIF
C
C ONLINE AGENT UPDATES
C
           ELSE IF(TRABUF(TSFUN).EQ.TAGTINF) THEN
              I1TEMP(1)  = TRABUF(TSNEW)
              LOGBUF(8)  = I4TEMP

              CALL MOVBYT(TRABUF(TSDT1),1,LOGBUF(8),2,31)
              CALL MOVBYT(TRABUF(TSDT1),32,LOGBUF(17),1,LREC*4-1)
              CALL MOVBYT(TRABUF(TSDT1),95,LOGBUF(33),1,LREC*4-1)
C
C MX LOG TRANSACTIONS
C
           ELSE IF(TRABUF(TSFUN).EQ.TMXL) THEN                  !MXSRV
              CALL FASTMOV(TRABUF(TMXL_RPCTAG),LOGBUF(8),8)     !MXSRV
              CALL FASTMOV(TRABUF(TMXL_RPCTAG+8),LOGBUF(17),8)  !MXSRV
              CALL FASTMOV(TRABUF(TMXL_DATA),LOGBUF(25),7)      !MXSRV
              CALL FASTMOV(TRABUF(TMXL_DATA+7),LOGBUF(33),15)   !MXSRV
              I4TEMP     = 0                                    !MXSRV
              I1TEMP(3)  = TRABUF(TMXL_ERCODE)                  !MXSRV
              LOGBUF(16) = I4TEMP                               !MXSRV
              I4TEMP     = 0                                    !MXSRV
              I2TEMP(1)  = TRABUF(TMXL_DATLEN)                  !MXSRV
              LOGBUF(32) = I4TEMP                               !MXSRV
           ELSE
              LOGBUF( 8) = TRABUF(TSOLD)
              LOGBUF( 9) = TRABUF(TSNEW)
              LOGBUF(10) = TRABUF(TSDT1)
              LOGBUF(11) = TRABUF(TSDT2)
              LOGBUF(12) = TRABUF(TSDT3)
              LOGBUF(13) = TRABUF(TSDT4)
              LOGBUF(14) = TRABUF(TSDT5)
              LOGBUF(15) = TRABUF(TSDT6)
C
              I4TEMP     = 0
              I1TEMP(3)  = TRABUF(TSSGN)
              LOGBUF(16) = I4TEMP
              IF(TRABUF(TSIZE).GT.1)
     *          CALL FASTMOV(TRABUF(TSDT7),LOGBUF(17),15) !17+15=32 <-> LOGBUF(32)
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C              LOGBUF(18) = TRABUF(TSDT8)
C              LOGBUF(19) = TRABUF(TSDT9)
C              LOGBUF(20) = TRABUF(TSDT10)
C              LOGBUF(21) = TRABUF(TSDT11)
C              LOGBUF(22) = TRABUF(TSDT12)
C              LOGBUF(23) = TRABUF(TSDT13)
C----+------------------------------------------------------------------
C V60| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
           ENDIF
           I4TEMP     = LOGBUF(16)
           I2TEMP(1)  = TRABUF(TSFUN)
           LOGBUF(16) = I4TEMP
           GOTO 9000
        ENDIF
C
C ENCODE COMMAND BODY
C
        IF(TRABUF(TTYP).EQ.TCMD) THEN
          LOGBUF( 8) = TRABUF(TCMOLD)
          LOGBUF( 9) = TRABUF(TCMNEW)
          LOGBUF(10) = TRABUF(TCMDT1)
          LOGBUF(11) = TRABUF(TCMDT2)
          LOGBUF(12) = TRABUF(TCMDT3)
          LOGBUF(13) = TRABUF(TCMDT4)
          LOGBUF(14) = TRABUF(TCMSRC)
C
          LOGBUF(15) = TRABUF(TCMTER)
C
          I2TEMP(1)  = TRABUF(TCMNUM)
          I1TEMP(3)  = TRABUF(TCMTYP)
C                                       !BYTE RESERVED FOR RTYP
          LOGBUF(16) = I4TEMP
C
C CONTINUATION RECORD 2
C
          LOGBUF(17) = TRABUF(TCMDT5)
          LOGBUF(18) = TRABUF(TCMLIN)
C
          GOTO 9000
        ENDIF
C
9000    CONTINUE
C
C SET LOGGER RECORD TYPES
C
        IF( TRABUF(TSIZE).EQ.3 ) THEN
          I4TEMP     = LOGBUF(16)
          I1TEMP(4)  = LONE
          LOGBUF(16) = I4TEMP
C
          I4TEMP     = LOGBUF(32)
          I1TEMP(4)  = LTWO
          LOGBUF(32) = I4TEMP
C
          I4TEMP     = LOGBUF(48)
          I1TEMP(4)  = LEND
          LOGBUF(48) = I4TEMP
C
          LOGBUF(1) = IOR(LOGBUF(LSER), '80000000'X)
C
        ELSE IF( TRABUF(TSIZE).EQ.2 ) THEN
          I4TEMP     = LOGBUF(16)
          I1TEMP(4)  = LONE
          LOGBUF(16) = I4TEMP
C
          I4TEMP     = LOGBUF(32)
          I1TEMP(4)  = LEND
          LOGBUF(32) = I4TEMP
C
          LOGBUF(1) = IOR(LOGBUF(LSER), '40000000'X)

        ELSE
          I4TEMP     = LOGBUF(16)
          I1TEMP(4)  = LREG
          LOGBUF(16) = I4TEMP

        ENDIF
C
        RETURN
        END

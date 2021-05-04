C
C SUBROUTINE LOGTRA
C
C LOGTRA.FOR
C
C V59 10-DEZ-2020 SCML New Terminals Project - Olimpo
C V58 04-MAR-2916 SCML M16 PROJECT:
C                      Added new fields to Euromillions transactions.
C V57 05-MAR-2014 SCML Added support to PLACARD Project - IGS
C V56 24-OCT-2013 SCML Added TIVMT,TIPLIDTYP,TINIBBA2,TINIBCD,TIPLCARD,TINIBBB
C                      TINIBBO,TINIBBA1,TINETPRZ and TIVDESCR to instant
C                      validation, regarding new bank validation mode.
C V55 23-SEP-2013 SCML TVPLIDTYP added
C V54 19-MAY-2011 FJG  FIX LOGBUF(16) 
C V53 12-APR-2011 FJG  ACCENTURE MERGE FOR EM2
C V52 06-JAN-2011 FJG  MILLENNIUM MXSRV
C V51 22-NOV-2010 MAC  LUCKY NUMBER
C                 FJG  TWEMSER/TWEMCHK replaced by TWLNKSER/TWLNKCHK
C V50 11-NOV-2010 FJG  EUROMILLIONS VALIDATION PRIZE OVER 42M
C V49 14-APR-2010 RXK  Week,year added for ePassive, NIB and Player Card fixed
C V48 16-MAR-2010 RXK  Changes for ePassive
C V47 25-MAR-2009 MMO  ADDED TWEMSER/TWEMCHK JOKER/EM.
C V46 06-JUN-2005 FRP  Modify for IPS Distribution.
C V45 27-OCT-2003 FRP  Modify for Batch2 Totobola Changes.
C V44 22-JUL-2003 FRP  Fix bug for TIVAGT: recover from I4TEMP,
C                      not from LOGBUF(10).
C V43 02-MAY-2001 JHR  ADDED MONDAY FLAG ( TWLMFI )
C V42 20-MAR-2001 UXN  TCMD size changed to 2 log records.
C V41 15-FEB-2001 UXN  3 digit instant game # changes.
C V40 14-FEB-2001 UXN  Game type/index flag removed
C V39 12-JAN-2001 EPH  ADDED TVOPPAY TVKOPPAY
C V38 12-DEC-2000 CS   ADDED PASSIVE LOTTERY.
C V37 29-SEP-2000 UXN  TAGTINF added. TVCWT added for validation record.
C                      IFSESON added.
C V36 28-FEB-2000 RXK  TWADDFW added for Lotto and Jokeri.
C V35 01-FEB-2000 UXN  Fractions changed, TGAMTYP2 removed.
C V34 13-OCT-1999 RXK  World Tour added.
C V33 07-JUN-1999 RXK  Fraction flag for oddset games set.
C V32 03-JUN-1999 UXN  Fix for Super Double and Todays Couple TWAMT.
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
C SUBROUTINE TO DECODE LOG RECORD TO TRANSACTION
C INTERNAL FORMAT.
C
C
C CALLING SEQUENCE:
C     CALL LOGTRA(TRABUF,LOGBUF)
C INPUT
C     LOGBUF - TRANSACTION LOG RECORD
C OUTPUT
C     TRALOG - INTERNAL TRANSACTION FORMAT
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
        SUBROUTINE LOGTRA(TRABUF,LOGBUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:X2XPTL.DEF'
        INCLUDE 'INCLIB:X2FEMES.DEF'
        INCLUDE 'INCLIB:X2STMES.DEF'
C
        INTEGER*4 LOGBUF(LREC*3)
        INTEGER*4 BRDOFF, BETIND, LOGOFF, TRAIND
        INTEGER*4 BUFIDX,IND,GNUM,GTYP
        BYTE      BUFF(120)
C
        INTEGER*4 I4TEMP
        INTEGER*2 I2TEMP(2)
        BYTE      I1TEMP(4)
        EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
C
C        INTEGER*4 I4TEMP_AUX
C        INTEGER*2 I2TEMP_AUX(2)
C        BYTE      I1TEMP_AUX(4)
C        EQUIVALENCE (I4TEMP_AUX,I2TEMP_AUX,I1TEMP_AUX)
C        BYTE      CHCOM_FLAG

        INTEGER*4 I,J,X,KIND, BDATA_VER
        INTEGER*4 OPREQ   
C
        LOGICAL     KIKFLG                        !KICKER FLAG (IS IT LOGGED?)
C
C
C
C CLEAR OUT ALL OF TRABUF - THIS PREVENTS MANY ERRORS FROM CREEPING IN
C
        CALL FASTSET( 0, TRABUF, TRALEN )
C
C DECODE TRANSACTION HEADER INFORMATION
        TRABUF(TSER)     = IAND(LOGBUF(1),'3FFFFFFF'X)
C
        I4TEMP           = LOGBUF(2)
        TRABUF(TCDC)     = ZEXT( I2TEMP(1) ) 
        TRABUF(TTER)     = I2TEMP(2) 

        TRABUF(TAGT)     = IAND(LOGBUF(3),'00FFFFFF'X) 
        TRABUF(TNFRAC)   = IAND(ISHFT(LOGBUF(3),-24),'000000FF'X)      
        TRABUF(TTIM)     = IAND(LOGBUF(4),'00FFFFFF'X) 

        TRABUF(TTSTCS)   = IAND(ISHFT(LOGBUF(4),-24),'000000FF'X) 

        I4TEMP           = LOGBUF(5)
        TRABUF(TCHK)     = ZEXT(I2TEMP(1)) 
        TRABUF(TERR)     = ZEXT(I1TEMP(3)) 
        X                = ZEXT(I1TEMP(4)) 
        TRABUF(TGAM)     = IAND (X,'7F'X ) 
C
        I4TEMP           = LOGBUF(6)
        TRABUF(TSTAT)    = ZEXT(I1TEMP(1)) 
        X                = ZEXT(I1TEMP(2)) 

        TRABUF(TTYP)     = ISHFT( X, -4 ) 
        TRABUF(TTRN)     = IAND (X,'0F'X ) 
        X                = ZEXT (I1TEMP(3)) 

        TRABUF(TSIZE)    = ISHFT( X, -4 ) 
        IF( IAND( X, 8) .NE. 0 ) TRABUF(TINTRA) = 1 

        TRABUF(TFIL)     = IAND ( X, '07'X )
        X                = ZEXT(I1TEMP(4)) 

        TRABUF(TGAMTYP)  = ISHFT( X, -3 ) 
        TRABUF(TGAMIND)  = IAND ( X,'07'X ) 

        I4TEMP = LOGBUF(7)
        X                = ZEXT( I1TEMP(1) )
        TRABUF(TTKID)    = IAND( X, '7F'X)
        TRABUF(TFAMTFLG) = ISHFT(X, -7) 
        TRABUF(TFRAC)    = ZEXT( I1TEMP(2) )
        TRABUF(TSUBERR)  = ZEXT( I1TEMP(3) )
        TRABUF(TCDC_SOLD)= TRABUF(TCDC) - ZEXT( I1TEMP(4) )
C
C PROJECT EURO MIL
C
        IF (TRABUF(TTYP) .EQ. TEUR) THEN ! PUT INTO TRABUF VALUES THAT ARE IN TMF
            I4TEMP = LOGBUF(8) 
            TRABUF(TEUTYP) = ZEXT(I1TEMP(1)) ! TRANSACTION TYPE - EURO MILHOES
            TRABUF(TEUCHK) = ZEXT(I2TEMP(2)) ! TRANSACTION CHECK DIGITS - EURO MILHOES
            
            TRABUF(TEUSER) = LOGBUF(9) ! TRANSACTION SERIAL - EURO MILHOES
            TRABUF(TEUMESSQ) = LOGBUF(10) ! MESSAGEQ SEQ NUMBER
            
            IF (TRABUF(TEUTYP) .EQ. TWAG) THEN 
              
              I4TEMP = LOGBUF(11)
              TRABUF (TEUWBEGW) = ZEXT(I1TEMP(1))
              TRABUF (TEUWBEGY) = ZEXT(I1TEMP(2))
              TRABUF (TEUWENDW) = ZEXT(I1TEMP(3))
              TRABUF (TEUWENDY) = ZEXT(I1TEMP(4))

	      I4TEMP = LOGBUF(12)
              TRABUF (TEUWDUR)  = ZEXT(I1TEMP(1))
              TRABUF (TEUWNBET) = ZEXT(I1TEMP(2))
              TRABUF (TEUWQP)   = ZEXT(I2TEMP(2))
              
              I4TEMP = LOGBUF(13)
              TRABUF (TEUWNMK) = ZEXT(I1TEMP(1))
              TRABUF (TEUWNST) = ZEXT(I1TEMP(2))
              
              I4TEMP = LOGBUF(14)
              TRABUF (TEUWTIMEH) = ZEXT(I1TEMP(1))
              TRABUF (TEUWTIMEM) = ZEXT(I1TEMP(2))
              TRABUF (TEUWTIMES) = ZEXT(I1TEMP(3))

              I4TEMP = LOGBUF(15) 
              TRABUF (TEUWOFS1) = I2TEMP(1)
              TRABUF (TEUWOFS2) = I2TEMP(2)
C
C DRAW INDICATOR
C
              I4TEMP = LOGBUF(16)
              TRABUF (TEUWDRWIND) = I1TEMP(1)

              LOGOFF = 17
              BRDOFF = 15               
              
              CALL FASTMOV(LOGBUF(LOGOFF), TRABUF(TEUWBOARD), BRDOFF)
C----+---+-------------+------------------------------------------------
C V58|BEG| M16 PROJECT | EM WAGER NEW FIELDS
C----+---+-------------+------------------------------------------------
C              GOTO 9000
              IF(TRABUF(TSIZE).EQ.2) GOTO 9000
C
              I4TEMP             = LOGBUF(33)
              X                  = ZEXT(I1TEMP(1))
              IF(IAND(X,'80'X).NE.0) TRABUF(TEUW_KIWFL) = 1                     !EM KICKER FLAG (1 BIT)
              X                  = ZEXT(I1TEMP(2))
              IF(IAND(X,'08'X).NE.0) TRABUF(TEUW_SMWFL) = 1                     !EM PARTICIPATING IN SM GAME FLAG  (1 BIT)
              IF(IAND(X,'04'X).NE.0) TRABUF(TEUW_SHWFL) = 1                     !EM PARTICIPATING IN SoM GAME FLAG (1 BIT)
              TRABUF(TEUW_EUWCH) = ZEXT(I1TEMP(3))                              !EM WAGERING CHANNEL               (1 BYTE)
C
              TRABUF(TEUW_PLNIF) = LOGBUF(34)                                   !PORTUGUESE PLAYER NIF (4 BYTES)
C
              TRABUF(TEUW_SMWSN) = IAND(LOGBUF(35),'00FFFFFF'X)                 !SM WAGER EXTERNAL SERIAL NUMBER (3 BYTES)
              TRABUF(TEUW_SMWCD) = IAND(ISHFT(LOGBUF(35),-24),'000000FF'X)      !SM WAGER CHECK DIGITS  (1 BYTE )
C
              I4TEMP             = LOGBUF(36)
              TRABUF(TEUW_SMWDN) = ZEXT(I1TEMP(1))                              !SM WAGER DRAW NUMBER       (1 BYTE)
              TRABUF(TEUW_SMWDY) = ZEXT(I1TEMP(2))                              !SM WAGER DRAW YEAR         (1 BYTE)
              TRABUF(TEUW_SMWOF) = ZEXT(I1TEMP(3))                              !OFFSET TO SM DRAW CDC DATE (1 BYTE)
C
              I4TEMP             = LOGBUF(37)
              TRABUF(TEUW_SMWTB) = ZEXT(I2TEMP(1))                              !SM WAGER TOTAL BETS (2 BYTES)
C
              TRABUF(TEUW_SMWB1) = LOGBUF(38)                                   !SM WAGER BEGIN NUMBER (PART 1 - 4 CHARS)
C
              TRABUF(TEUW_SMWB2) = IAND(LOGBUF(39),'00FFFFFF'X)                 !SM WAGER BEGIN NUMBER (PART 2 - 3 BYTES)
C
              TRABUF(TEUW_SMWE1) = LOGBUF(40)                                   !SM WAGER END NUMBER (PART 1 - 4 CHARS)
C
              TRABUF(TEUW_SMWE2) = IAND(LOGBUF(41),'00FFFFFF'X)                 !SM WAGER END NUMBER (HIGH 3 BYTES)
C
              I4TEMP             = LOGBUF(42)
              TRABUF(TEUW_SHWDN) = ZEXT(I1TEMP(1))                              !SoM WAGER DRAW NUMBER       (1 BYTE)
              TRABUF(TEUW_SHWDY) = ZEXT(I1TEMP(2))                              !SoM WAGER DRAW YEAR         (1 BYTE)
              TRABUF(TEUW_SHWOF) = ZEXT(I1TEMP(3))                              !OFFSET TO SoM DRAW CDC DATE (1 BYTE)
C
              I4TEMP             = LOGBUF(43)
              TRABUF(TEUW_SHWTB) = ZEXT(I2TEMP(1))                              !SoM WAGER TOTAL BETS (2 BYTES)
C
              TRABUF(TEUW_SHWB1) = LOGBUF(44)                                   !SoM WAGER BEGIN NUMBER (PART 1 - 4 CHARS)
C
              TRABUF(TEUW_SHWB2) = IAND(LOGBUF(45),'00FFFFFF'X)                 !SoM WAGER BEGIN NUMBER (PART 2 - 3 BYTES)
C
              TRABUF(TEUW_SHWE1) = LOGBUF(46)                                   !SoM WAGER END NUMBER (PART 1 - 4 CHARS)
C
              TRABUF(TEUW_SHWE2) = IAND(LOGBUF(47),'00FFFFFF'X)                 !SoM WAGER END NUMBER (PART 2 - 3 BYTES)
C
              GOTO 9000
C----+---+-------------+------------------------------------------------
C V58|END| M16 PROJECT | EM WAGER NEW FIELDS
C----+---+-------------+------------------------------------------------
           ENDIF
            
           IF (TRABUF(TEUTYP) .EQ. TCAN) THEN 
              
              I4TEMP = LOGBUF(11)
              TRABUF (TEUCWJUL) = ZEXT(I2TEMP(1))
              TRABUF (TEUCWCKD)	= ZEXT(I1TEMP(3))
              
              TRABUF(TEUCWSER)  = LOGBUF(12)
              
              I4TEMP = LOGBUF(13)
              TRABUF (TEUCTIMEH) = ZEXT(I1TEMP(1))
              TRABUF (TEUCTIMEM) = ZEXT(I1TEMP(2))
              TRABUF (TEUCTIMES) = ZEXT(I1TEMP(3))
              TRABUF (TEUCST)    = ZEXT(I1TEMP(4))           
              TRABUF (TEUCAM) = LOGBUF(14)
C----+---+-------------+------------------------------------------------
C V58|BEG| M16 PROJECT | EM CANCELLATION NEW FIELDS
C----+---+-------------+------------------------------------------------
              I4TEMP             = LOGBUF(17)
              X                  = ZEXT(I1TEMP(1))
              IF(IAND(X,'80'X).NE.0) TRABUF(TEUC_SMCFL) = 1                     !SM CANCELLATION DATA PRESENT FLAG (1 BIT)
              IF(IAND(X,'40'X).NE.0) TRABUF(TEUC_SHCFL) = 1                     !SoM CANCELLATION FLAG             (1 BIT)
C
              TRABUF(TEUC_SMWSN) = IAND(LOGBUF(18),'00FFFFFF'X)                 !SM WAGER EXTERNAL SERIAL NUMBER (3 BYTES)
              TRABUF(TEUC_SMWCD) = IAND(ISHFT(LOGBUF(18),-24),'000000FF'X)      !SM WAGER CHECK DIGITS           (1 BYTE )
C
              TRABUF(TEUC_SMCSN) = IAND(LOGBUF(19),'00FFFFFF'X)                 !SM CANCELLATION EXTERNAL SERIAL NUMBER (3 BYTES)
              TRABUF(TEUC_SMCCD) = IAND(ISHFT(LOGBUF(19),-24),'000000FF'X)      !SM CANCELLATION CHECK DIGITS           (1 BYTE )
C
              TRABUF(TEUC_SMWCA) = LOGBUF(20)                                   !SM WAGER CANCELLATION AMOUNT (WAGER UNITS) (4 BYTES)
C----+---+-------------+------------------------------------------------
C V58|END| M16 PROJECT | EM CANCELLATION NEW FIELDS
C----+---+-------------+------------------------------------------------
              GOTO 9000
            ENDIF            

            IF (TRABUF(TEUTYP) .EQ. TVAL) THEN 
              
              I4TEMP = LOGBUF(11)
              TRABUF (TEUVSBT)  = ZEXT(I1TEMP(1))
              TRABUF (TEUVWCKD) = ZEXT(I1TEMP(2))
              TRABUF (TEUVWJUL)	= ZEXT(I2TEMP(2))
              
              TRABUF(TEUVWSER) = LOGBUF(12)

              I4TEMP = LOGBUF(13)
              TRABUF (TEUVTIMEH) = ZEXT(I1TEMP(1))
              TRABUF (TEUVTIMEM) = ZEXT(I1TEMP(2))
              TRABUF (TEUVTIMES) = ZEXT(I1TEMP(3))
              TRABUF (TEUVST) = ZEXT(I1TEMP(4)) 
              
              TRABUF (TEUVCAM) = LOGBUF(14)
              TRABUF (TEUVRAM) = LOGBUF(15)
C+++++++++++++V50+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++              
              I4TEMP = LOGBUF(16)
              TRABUF(TEUVCAMH) = ZEXT(I1TEMP(1))
C----+------------------------------------------------------------------
C V56| Adding new validation messages' fields
C----+------------------------------------------------------------------
              TRABUF(TEUVPLIDTYP) = ZEXT(I1TEMP(2))
C----+------------------------------------------------------------------
C V56| Adding new validation messages' fields
C----+------------------------------------------------------------------
              TRABUF(TEUVRAMH) = ZEXT(I1TEMP(3))

C----+------------------------------------------------------------------
C V56| Adding new validation messages' fields
C----+------------------------------------------------------------------
              TRABUF(TEUVPLCARD) = LOGBUF(17)  
        
              I4TEMP          =  LOGBUF(18)
              TRABUF(TEUVNIBBB) = ZEXT( I2TEMP(1) )
              TRABUF(TEUVNIBBO) = ZEXT( I2TEMP(2) )
        
              TRABUF(TEUVNIBBA1) =  LOGBUF(19)           
        
              I4TEMP           =  LOGBUF(20)
              TRABUF(TEUVNIBBA2) = ZEXT( I1TEMP(1) )  
              TRABUF(TEUVNIBCD)  = ZEXT( I1TEMP(2) )
C----+------------------------------------------------------------------
C V56| Adding new validation messages' fields
C----+------------------------------------------------------------------

C----+---+-------------+------------------------------------------------
C V58|BEG| M16 PROJECT | EM CANCELLATION NEW FIELDS
C----+---+-------------+------------------------------------------------
              I4TEMP             = LOGBUF(21)
              X                  = ZEXT(I1TEMP(1))
              IF(IAND(X,'80'X).NE.0) TRABUF(TEUV_NIFFL) = 1                     !PLAYER NIF PRESENT FLAG               (1 BIT)
              IF(IAND(X,'40'X).NE.0) TRABUF(TEUV_NIFCF) = 1                     !PLAYER NIF CONFIRMATION REQUIRED FLAG (1 BIT)
              IF(IAND(X,'20'X).NE.0) TRABUF(TEUV_SHVFL) = 1                     !SoM VALIDATION FLAG                   (1 BIT)
C
              TRABUF(TEUV_PLNIF) = LOGBUF(22)                                   !PORTUGUESE PLAYER NIF (4 BYTES)
C----+---+-------------+------------------------------------------------
C V58|END| M16 PROJECT | EM CANCELLATION NEW FIELDS
C----+---+-------------+------------------------------------------------

C+++++++++++++V50+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              IF (TRABUF (TEUVST) .EQ. 11) THEN
C                LOGBUF(16) USED FOR TEUVCAMH AND TEUVRAMH. SEE ABOVE      	
C              	 I4TEMP = LOGBUF(16)
C              	 TRABUF(TEUEVWCKD) = I1TEMP(3)
              	 
              	 TRABUF(TEUEVWSER) = LOGBUF(17)
                 
                 I4TEMP = LOGBUF(18)
                 TRABUF (TEUVEBEGW) = I1TEMP(1)
                 TRABUF (TEUVEBEGY) = I1TEMP(2)
                 TRABUF (TEUVEENDW) = I1TEMP(3)
                 TRABUF (TEUVEENDY) = I1TEMP(4)	
                 
                 I4TEMP = LOGBUF(19)
                 TRABUF (TEUVEDUR)  = I1TEMP(1)
                 TRABUF (TEUVENBET) = I1TEMP(2)
                 TRABUF (TEUVEQP)   = I2TEMP(2)
                 
                 I4TEMP = LOGBUF(20)
                 TRABUF (TEUVENMK) = I1TEMP(1)
                 TRABUF (TEUVENST) = I1TEMP(2)
                 
                 I4TEMP = LOGBUF(21)
                 TRABUF (TEUVETIMEH) = I1TEMP(1)
                 TRABUF (TEUVETIMEM) = I1TEMP(2)
                 TRABUF (TEUVETIMES) = I1TEMP(3)
                 TRABUF (TEUEVWCKD)  = I1TEMP(4)
                 
                 I4TEMP = LOGBUF(22)
                 TRABUF (TEUVEOFS1) = I2TEMP(1)
                 TRABUF (TEUVEOFS2) = I2TEMP(2)
                 
                 LOGOFF = 23
                 BRDOFF = 9              
                 
                 CALL FASTMOV(LOGBUF(LOGOFF), TRABUF(TEUVEBOARD), BRDOFF)              	 
              	 
              ENDIF
              
              GOTO 9000
            ENDIF            
        
            GOTO 9000
        ENDIF
C----+------------------------------------------------------------------
C V57| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF (TRABUF(TTYP) .EQ. TIGS) THEN 
C----+------------------------------------------------------------------
C    |
C    |
C V57| Handling IGS transaction header
C    |
C    |
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V57| Handling IGS transaction header: IGS TRANSACTION TYPE 
C----+------------------------------------------------------------------
            TRABUF (TIGS_TTYP) = LOGBUF(8)  ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling IGS transaction header: MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
C----+------------------------------------------------------------------
            TRABUF (TIGS_XREF) = LOGBUF(9)  ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling IGS transaction header: IGS ERROR CODE DESCRIPTION, SYSTEM CODE WHERE ERROR OCCURRED
C----+------------------------------------------------------------------
            TRABUF (TIGS_XERR + 0) = LOGBUF(10) ! (9 bytes) 28
            TRABUF (TIGS_XERR + 1) = LOGBUF(11) ! 29
            I4TEMP = LOGBUF(12) 
            TRABUF (TIGS_XERR + 2) = ZEXT(I1TEMP(1)) ! (1 byte) 30            
            TRABUF (TIGS_SERR)     = ZEXT(I1TEMP(2)) ! (1 byte)
C----+------------------------------------------------------------------
C    |
C    |
C V57| Handling wager transactions
C    |
C    |
C----+------------------------------------------------------------------
            IF (TRABUF(TIGS_TTYP) .EQ. IGSWAG) THEN
C----+------------------------------------------------------------------
C V57| Handling wager transactions: TERMINAL MESSAGE ID (LOW 4 BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSW_MIDL) = LOGBUF(13) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: TERMINAL MESSAGE ID (HIGH 4 BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSW_MIDH) = LOGBUF(14) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: ABP GAME ID
C----+------------------------------------------------------------------
                TRABUF (TIGSW_XGID) = LOGBUF(15) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: SUBTYPE ID
C    | - Be careful when placing information here, because bytes #4 of -
C    | - words #16,#32,#48 are used for record extension               -
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(16) 
                TRABUF (TIGSW_STID) = I2TEMP(1)  ! (2 bytes)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: UNIT STAKE OF THE BET
C----+------------------------------------------------------------------  
                TRABUF (TIGSW_USTK) = LOGBUF(17) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: TOTAL BETS/NUMBER OF SELECTIONS (MAX = 8)
C----+------------------------------------------------------------------
                TRABUF (TIGSW_TBET) = LOGBUF(18) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: BET REFERENCE DATE (YYMMDD)
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(19)                
                TRABUF (TIGSW_WRDY) = ZEXT(I1TEMP(1)) ! (1 byte)                                    
                TRABUF (TIGSW_WRDM) = ZEXT(I1TEMP(2)) ! (1 byte)                                      
                TRABUF (TIGSW_WRDD) = ZEXT(I1TEMP(3)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: BET REFERENCE GAME
C----+------------------------------------------------------------------                              
                TRABUF (TIGSW_WRGM) = LOGBUF(20) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: BET REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
C----+------------------------------------------------------------------     
                TRABUF (TIGSW_WRSL) = LOGBUF(21) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: BET REFERENCE SERIAL NUMBER (HIGH ONE BYTE)
C----+------------------------------------------------------------------      
                TRABUF (TIGSW_WRSH) = LOGBUF(22) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: BET REFERENCE CHECK DIGITS
C----+------------------------------------------------------------------                       
                TRABUF (TIGSW_WRCD) = LOGBUF(23) ! (2 bytes)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: BET CREATION DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(24)                                   
                TRABUF (TIGSW_WCDY) = ZEXT(I2TEMP(1)) ! (2 bytes)                                      
                TRABUF (TIGSW_WCDM) = ZEXT(I1TEMP(3)) ! (1 byte)               
                TRABUF (TIGSW_WCDD) = ZEXT(I1TEMP(4)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: BET CREATION TIME (HHMISS)
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(25)                                       
                TRABUF (TIGSW_WCTH) = ZEXT(I1TEMP(1)) ! (1 byte)                     
                TRABUF (TIGSW_WCTM) = ZEXT(I1TEMP(2)) ! (1 byte)                                     
                TRABUF (TIGSW_WCTS) = ZEXT(I1TEMP(3)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: BET LAST EVENT DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(26)                 
                TRABUF (TIGSW_LEDY) = ZEXT(I2TEMP(1)) ! (2 bytes)
                TRABUF (TIGSW_LEDM) = ZEXT(I1TEMP(3)) ! (1 byte)
                TRABUF (TIGSW_LEDD) = ZEXT(I1TEMP(4)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: BET TOTAL STAKE
C----+------------------------------------------------------------------
                TRABUF (TIGSW_TSTK) = LOGBUF(27) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: BET MAXIMUM POSSIBLE RETURNS
C----+------------------------------------------------------------------                    
                TRABUF (TIGSW_MAXR) = LOGBUF(28) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: PLAYER NIF NUMBER
C----+------------------------------------------------------------------                           
                TRABUF (TIGSW_PNIF) = LOGBUF(29) ! (4 bytes)
                GOTO 9000
            ENDIF
C----+------------------------------------------------------------------
C    |
C    |
C V57| Handling cancellation transactions
C    |
C    |
C----+------------------------------------------------------------------
            IF (TRABUF(TIGS_TTYP) .EQ. IGSCAN) THEN
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: TERMINAL MESSAGE ID (LOW 4 BYTES)
C----+------------------------------------------------------------------               
                TRABUF (TIGSC_MIDL) = LOGBUF(13) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: TERMINAL MESSAGE ID (HIGH 4 BYTES) -> Terminal Number               
C----+------------------------------------------------------------------              
                TRABUF (TIGSC_MIDH) = LOGBUF(14) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: BET REFERENCE DATE (YYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = LOGBUF(15)                
                TRABUF (TIGSC_WRDY) = ZEXT(I1TEMP(1)) ! (1 byte)                             
                TRABUF (TIGSC_WRDM) = ZEXT(I1TEMP(2)) ! (1 byte)                                      
                TRABUF (TIGSC_WRDD) = ZEXT(I1TEMP(3)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: BET REFERENCE GAME
C    | - Be careful when placing information here, because bytes #4 of -
C    | - words #16,#32,#48 are used for record extension               -
C----+------------------------------------------------------------------
                I4TEMP     = LOGBUF(16)
                TRABUF (TIGSC_WRGM) = ZEXT(I1TEMP(1)) ! (1 bytes)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: BET REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
C----+------------------------------------------------------------------     
                TRABUF (TIGSC_WRSL) = LOGBUF(17) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: BET REFERENCE SERIAL NUMBER (HIGH ONE BYTE)      
C----+------------------------------------------------------------------
                TRABUF (TIGSC_WRSH) = LOGBUF(18) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: BET REFERENCE CHECK DIGITS
C----+------------------------------------------------------------------
                TRABUF (TIGSC_WRCD) = LOGBUF(19) ! (2 bytes)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: CANCEL STATUS, CANCEL REFERENCE GAME
C----+------------------------------------------------------------------                       
                I4TEMP     = LOGBUF(20)
                TRABUF (TIGSC_CRGM) = ZEXT(I1TEMP(2)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: CANCEL REFERENCE DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = LOGBUF(21)
                TRABUF (TIGSC_CRDY) = ZEXT(I2TEMP(1)) ! (2 bytes)
                TRABUF (TIGSC_CRDM) = ZEXT(I1TEMP(3)) ! (1 byte)
                TRABUF (TIGSC_CRDD) = ZEXT(I1TEMP(4)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: CANCEL REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSC_CRSL) = LOGBUF(22) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: CANCEL REFERENCE SERIAL NUMBER (HIGH ONE BYTE)
C----+------------------------------------------------------------------
                TRABUF (TIGSC_CRSH) = LOGBUF(23) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: CANCEL REFERENCE CHECK DIGITS
C----+------------------------------------------------------------------
                TRABUF (TIGSC_CRCD) = LOGBUF(24) ! (2 bytes)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: WAGER CANCEL DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = LOGBUF(25)
                TRABUF (TIGSC_WCDY) = ZEXT(I2TEMP(1)) ! (2 bytes)
                TRABUF (TIGSC_WCDM) = ZEXT(I1TEMP(3)) ! (1 byte)
                TRABUF (TIGSC_WCDD) = ZEXT(I1TEMP(4)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: WAGER CANCEL TIME (HHMISS)
C----+------------------------------------------------------------------
                I4TEMP     = LOGBUF(26)
                TRABUF (TIGSC_WCTH) = ZEXT(I1TEMP(1)) ! (1 byte)
                TRABUF (TIGSC_WCTM) = ZEXT(I1TEMP(2)) ! (1 byte)
                TRABUF (TIGSC_WCTS) = ZEXT(I1TEMP(3)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling cancellation transactions: CANCEL AMOUNT (WAGER UNITS)
C----+------------------------------------------------------------------
                TRABUF (TIGSC_CAMT) = LOGBUF(27) ! (4 bytes)
                GOTO 9000
            ENDIF
C----+------------------------------------------------------------------
C    |
C    |
C V57| Handling validation transactions
C    |
C    |
C----+------------------------------------------------------------------
            IF (TRABUF(TIGS_TTYP) .EQ. IGSVAL) THEN
C----+------------------------------------------------------------------
C V57| Handling validation transactions: TERMINAL MESSAGE ID (LOW 4 BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSV_MIDL) = LOGBUF(13) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling validation transactions: TERMINAL MESSAGE ID (HIGH 4 BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSV_MIDH) = LOGBUF(14) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling validation transactions: BET REFERENCE DATE (YYMMDD)
C----+------------------------------------------------------------------
                I4TEMP     = LOGBUF(15) 
                TRABUF (TIGSV_WRDY) = ZEXT(I1TEMP(1)) ! (1 byte)
                TRABUF (TIGSV_WRDM) = ZEXT(I1TEMP(2)) ! (1 byte)
                TRABUF (TIGSV_WRDD) = ZEXT(I1TEMP(3)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling validation transactions: BET REFERENCE GAME
C    | - Be careful when placing information here, because bytes #4 of -
C    | - words #16,#32,#48 are used for record extension               -
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(16) 
                TRABUF (TIGSV_WRGM) = ZEXT(I1TEMP(1)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling validation transactions: BET REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSV_WRSL) = LOGBUF(17) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling validation transactions: BET REFERENCE SERIAL NUMBER (HIGH ONE BYTE)
C----+------------------------------------------------------------------
                TRABUF (TIGSV_WRSH) = LOGBUF(18) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling validation transactions: BET REFERENCE CHECK DIGITS
C----+------------------------------------------------------------------
                TRABUF (TIGSV_WRCD) = LOGBUF(19) ! (2 bytes)
C----+------------------------------------------------------------------
C V57| Handling validation transactions: PAYMENT MODE
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(20)
                TRABUF (TIGSV_PMOD) = I1TEMP(2) ! (1 byte) (must preserve sign)
C----+------------------------------------------------------------------
C V57| Handling validation transactions: WAGER VALIDATION DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP  = LOGBUF(21) 
                TRABUF (TIGSV_WVDY) = ZEXT(I2TEMP(1)) ! (2 bytes)
                TRABUF (TIGSV_WVDM) = ZEXT(I1TEMP(3)) ! (1 byte)
                TRABUF (TIGSV_WVDD) = ZEXT(I1TEMP(4)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling validation transactions: WAGER VALIDATION TIME (HHMISS)
C    |                                   PLAYER NIF NUMBER CONFIRMATION NEEDED FLAG
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(22) 
                TRABUF (TIGSV_WVTH) = ZEXT(I1TEMP(1)) ! (1 byte)
                TRABUF (TIGSV_WVTM) = ZEXT(I1TEMP(2)) ! (1 byte)
                TRABUF (TIGSV_WVTS) = ZEXT(I1TEMP(3)) ! (1 byte)
                TRABUF (TIGSV_FNIF) = I1TEMP(4)       ! (1 byte) (must preserve sign)
C----+------------------------------------------------------------------
C V57| Handling validation transactions: TOTAL PRIZE AMOUNT (VALIDATION UNITS)
C----+------------------------------------------------------------------
                TRABUF (TIGSV_TPRZ) = LOGBUF(23) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling validation transactions: TOTAL TAX AMOUNT (VALIDATION UNITS)
C----+------------------------------------------------------------------
                TRABUF (TIGSV_TTAX) = LOGBUF(24) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling validation transactions: TOTAL TAXNET PRIZE AMOUNT (VALIDATION UNITS)
C----+------------------------------------------------------------------
                TRABUF (TIGSV_NPRZ) = LOGBUF(25) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: PLAYER NIF NUMBER
C----+------------------------------------------------------------------
                TRABUF (TIGSV_PNIF) = LOGBUF(26) ! (4 bytes)
                GOTO 9000
            ENDIF
C----+------------------------------------------------------------------
C    |
C    |
C V57| Handling payment transactions
C    |
C    |
C----+------------------------------------------------------------------
            IF (TRABUF(TIGS_TTYP) .EQ. IGSPAY) THEN
C----+------------------------------------------------------------------
C V57| Handling payment transactions: TERMINAL MESSAGE ID (LOW 4 BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSP_MIDL) = LOGBUF(13) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: TERMINAL MESSAGE ID (HIGH 4 BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSP_MIDH) = LOGBUF(14) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: BET REFERENCE DATE (YYMMDD)
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(15) 
                TRABUF (TIGSP_WRDY) = ZEXT(I1TEMP(1)) ! (1 byte)
                TRABUF (TIGSP_WRDM) = ZEXT(I1TEMP(2)) ! (1 byte)
                TRABUF (TIGSP_WRDD) = ZEXT(I1TEMP(3)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: BET REFERENCE GAME
C    | - Be careful when placing information here, because bytes #4 of -
C    | - words #16,#32,#48 are used for record extension               -
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(16)
                TRABUF (TIGSP_WRGM) = ZEXT(I1TEMP(1)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: BET REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSP_WRSL) = LOGBUF(17) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: BET REFERENCE SERIAL NUMBER (HIGH ONE BYTE),
C                                     PAYMENT MODE,
C    |                                BET REFERENCE CHECK DIGITS
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(18) 
                TRABUF (TIGSP_WRSH) = ZEXT(I1TEMP(1)) ! (1 byte)
                TRABUF (TIGSP_PMOD) = ZEXT(I1TEMP(2)) ! (1 byte)
                TRABUF (TIGSP_WRCD) = ZEXT(I2TEMP(2)) ! (2 bytes)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: PLAYER ID
C----+------------------------------------------------------------------
                TRABUF (TIGSP_PYID) = LOGBUF(19) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: PLAYER NIB, PLAYER ID TYPE
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(20) 
                TRABUF (TIGSP_NIBB) = ZEXT(I2TEMP(1)) ! (2 bytes)  NIB Branch
                TRABUF (TIGSP_NIBO) = ZEXT(I2TEMP(2)) ! (2 bytes)  NIB Office

                TRABUF (TIGSP_NIA1) = LOGBUF(21) ! (4 bytes)  NIB Account Number Part 1
                
                I4TEMP = LOGBUF(22) 
                TRABUF (TIGSP_NIA2) = ZEXT(I1TEMP(1)) ! (1 byte)   NIB Account Number Part 2
                TRABUF (TIGSP_NICD) = ZEXT(I1TEMP(2)) ! (1 byte)   NIB Check digits
                TRABUF (TIGSP_IDTY) = ZEXT(I1TEMP(3)) ! (1 byte)   Player Id Type
C----+------------------------------------------------------------------
C V57| Handling payment transactions: PAYMENT REFERENCE DATE (YYMMDD)
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(23) 
                TRABUF (TIGSP_PRDY) = ZEXT(I1TEMP(1)) ! (1 byte)
                TRABUF (TIGSP_PRDM) = ZEXT(I1TEMP(2)) ! (1 byte)
                TRABUF (TIGSP_PRDD) = ZEXT(I1TEMP(3)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: PAYMENT REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSP_PRSL) = LOGBUF(24) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: PAYMENT REFERENCE SERIAL NUMBER (HIGH ONE BYTE),
C    |                                PAYMENT REFERENCE GAME,
C    |                                PAYMENT REFERENCE CHECK DIGITS
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(25)
                TRABUF (TIGSP_PRGM) = ZEXT(I1TEMP(1)) ! (1 byte)
                TRABUF (TIGSP_PRSH) = ZEXT(I1TEMP(2)) ! (1 byte)
                TRABUF (TIGSP_PRCD) = ZEXT(I2TEMP(2)) ! (2 bytes)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: PRIZE PAYMENT DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(26) 
                TRABUF (TIGSP_PPDY) = ZEXT(I2TEMP(1)) ! (2 bytes)
                TRABUF (TIGSP_PPDM) = ZEXT(I1TEMP(3)) ! (1 byte)
                TRABUF (TIGSP_PPDD) = ZEXT(I1TEMP(4)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: PRIZE PAYMENT TIME (HHMISS)
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(27) 
                TRABUF (TIGSP_PPTH) = ZEXT(I1TEMP(1)) ! (1 byte)
                TRABUF (TIGSP_PPTM) = ZEXT(I1TEMP(2)) ! (1 byte)
                TRABUF (TIGSP_PPTS) = ZEXT(I1TEMP(3)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: TOTAL PRIZE AMOUNT (PAYMENT UNITS)
C----+------------------------------------------------------------------
                TRABUF (TIGSP_TPRZ) = LOGBUF(28) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: TOTAL TAX AMOUNT (PAYMENT UNITS)
C----+------------------------------------------------------------------
                TRABUF (TIGSP_TTAX) = LOGBUF(29) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling payment transactions: TOTAL TAXNET PRIZE AMOUNT (PAYMENT UNITS)
C----+------------------------------------------------------------------
                TRABUF (TIGSP_NPRZ) = LOGBUF(30) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling wager transactions: PLAYER NIF NUMBER
C----+------------------------------------------------------------------
                TRABUF (TIGSP_PNIF) = LOGBUF(31) ! (4 bytes)
                GOTO 9000
            ENDIF
C----+------------------------------------------------------------------
C    |
C    |
C V57| Handling game programme report transactions
C    |
C    |
C----+------------------------------------------------------------------
            IF (TRABUF(TIGS_TTYP) .EQ. IGSREP) THEN
C----+------------------------------------------------------------------
C V57| Handling game programme report transactions: TERMINAL MESSAGE ID (LOW 4 BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSR_MIDL) = LOGBUF(13) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling game programme report transactions: TERMINAL MESSAGE ID (HIGH 4 BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSR_MIDH) = LOGBUF(14) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling game programme report transactions: MEDIA ID
C----+------------------------------------------------------------------
                TRABUF (TIGSR_MEID) = LOGBUF(15) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling game programme report transactions: SEGMENT NUMBER REQUESTED
C    | - Be careful when placing information here, because bytes #4 of -
C    | - words #16,#32,#48 are used for record extension               -
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(16)
                TRABUF (TIGSR_SEGN) = ZEXT(I1TEMP(1)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling game programme report transactions: PROGRAMME TEMPLATE ID
C----+------------------------------------------------------------------
                TRABUF (TIGSR_PTID) = LOGBUF(17) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling game programme report transactions: PROGRAMME REPORT DATE (YYYYMMDD)
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(18) 
                TRABUF (TIGSR_PRDY) = ZEXT(I2TEMP(1)) ! (2 bytes)
                TRABUF (TIGSR_PRDM) = ZEXT(I1TEMP(3)) ! (1 byte)
                TRABUF (TIGSR_PRDD) = ZEXT(I1TEMP(4)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling game programme report transactions: PROGRAMME REPORT TIME (HHMISS)
C----+------------------------------------------------------------------
                I4TEMP = LOGBUF(19)
                TRABUF (TIGSR_PRTH) = ZEXT(I1TEMP(1)) ! (1 byte)
                TRABUF (TIGSR_PRTM) = ZEXT(I1TEMP(2)) ! (1 byte)
                TRABUF (TIGSR_PRTS) = ZEXT(I1TEMP(3)) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling game programme report transactions: TOTAL SEGMENTS
C----+------------------------------------------------------------------
                TRABUF (TIGSR_TSEG) = LOGBUF(20) ! (1 byte)
C----+------------------------------------------------------------------
C V57| Handling game programme report transactions: MEDIA VERSION (LOW 4 BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSR_MVRL) = LOGBUF(21) ! (4 bytes)
C----+------------------------------------------------------------------
C V57| Handling game programme report transactions: MEDIA VERSION (HIGH 4 BYTES)
C----+------------------------------------------------------------------
                TRABUF (TIGSR_MVRH) = LOGBUF(22) ! (4 bytes)
                GOTO 9000
            ENDIF
            GOTO 9000
        ENDIF
C----+------------------------------------------------------------------
C V57| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
C DECODE PASSIVE GAMES
C
        IF(TRABUF(TGAMTYP).EQ.TPAS) THEN

	  I4TEMP = LOGBUF(12)
          IF(TRABUF(TTYP).EQ.TVAL) THEN
             TRABUF(TVEPVAL) = I1TEMP(4)
          ELSEIF(TRABUF(TTYP).EQ.TWAG) THEN
             OPREQ = I1TEMP(4)
             IF(OPREQ.EQ.0) THEN
                TRABUF(TTYP) = TRET
                IF(TRABUF(TSTAT).EQ.VOID) TRABUF(TSTAT) = GOOD
             ENDIF
          ENDIF
        ENDIF

        IF(TRABUF(TGAMTYP).EQ.TPAS. AND.
     *    (TRABUF(TTYP).EQ.TWAG .OR. TRABUF(TTYP).EQ.TCAN .OR.
     *     TRABUF(TTYP).EQ.TINC)) THEN

              TRABUF(TWCSER)   = LOGBUF(8)
C
              TRABUF(TWAMT)    = LOGBUF(9)
C
              I4TEMP           = LOGBUF(10)
              TRABUF(TWBEG)    = ZEXT( I2TEMP(1) )
              TRABUF(TWCTER)   = ZEXT( I2TEMP(2) )
C
              I4TEMP           = LOGBUF(11)
              TRABUF(TWQPF)    = ZEXT( I2TEMP(1) )        
              TRABUF(TWNBET)   = ZEXT( I1TEMP(4) )
C
              I4TEMP           = LOGBUF(12)
              TRABUF(TWTKC)    = ZEXT( I1TEMP(1) )
              TRABUF(TWVSTS)   = ZEXT( I1TEMP(2) )
              TRABUF(TWDUR)    = ZEXT( I1TEMP(3) )
              TRABUF(TWEPOP)   = ZEXT( I1TEMP(4) )

              I4TEMP           = LOGBUF(13)
              TRABUF(TWEPWK)   = ZEXT( I1TEMP(1) )
              TRABUF(TWEPYR)   = ZEXT( I1TEMP(2) )

              IF(TRABUF(TWEPOP).EQ.EPASSAL) THEN

                 TRABUF(TWEPSN)   = LOGBUF(14)

                 I4TEMP           = LOGBUF(15) 
                 TRABUF(TWEPSF)   = ZEXT( I1TEMP(1) )
                 TRABUF(TWEPSS)   = ZEXT( I1TEMP(2) )

                 TRABUF(TWEND) = TRABUF(TWBEG)+TRABUF(TWDUR)-1
                 TRABUF(TWTOT) = (TRABUF(TWDUR) * TRABUF(TWAMT))+
     *                           TRABUF(TWTKC)

              ELSEIF(TRABUF(TWEPOP).EQ.EPASRES.OR.TRABUF(TWEPOP).EQ.EPASREL)
     *        THEN

                 TRABUF(TWEPSD)   = LOGBUF(14)

                 TRABUF(TWEPRM)   = LOGBUF(15) 
       
                 I4TEMP           = LOGBUF(16)
                 TRABUF(TWEPNE)   = ZEXT( I1TEMP(1) )
                 TRABUF(TWEPNF)   = ZEXT( I1TEMP(2) )
                 TRABUF(TWEPNR)   = ZEXT( I1TEMP(3) )
C
C BEGIN CONTINUATION RECORD 1
C
                 TRABUF(TWEPRES1)   = LOGBUF(17)
                 TRABUF(TWEPRES2)   = LOGBUF(18)
                 TRABUF(TWEPRES3)   = LOGBUF(19)

                 IF(TRABUF(TWEPOP).EQ.EPASREL) GOTO 9000

                 I4TEMP             = LOGBUF(20)
                 TRABUF(TWEPNFR1)   = ZEXT( I1TEMP(1) ) 
                 TRABUF(TWEPNFR2)   = ZEXT( I1TEMP(2) ) 
                 TRABUF(TWEPNFR3)   = ZEXT( I1TEMP(3) ) 

                 I4TEMP             = LOGBUF(21)
                 TRABUF(TWEPSER1_1) = ZEXT( I1TEMP(1) ) 
                 TRABUF(TWEPSER1_2) = ZEXT( I1TEMP(2) ) 
                 TRABUF(TWEPSER1_3) = ZEXT( I1TEMP(3) ) 
                 TRABUF(TWEPSER1_4) = ZEXT( I1TEMP(4) ) 

                 I4TEMP             = LOGBUF(22)
                 TRABUF(TWEPSER1_5) = ZEXT( I1TEMP(1) ) 
                 TRABUF(TWEPSER1_6) = ZEXT( I1TEMP(2) ) 
                 TRABUF(TWEPSER1_7) = ZEXT( I1TEMP(3) ) 
                 TRABUF(TWEPSER1_8) = ZEXT( I1TEMP(4) ) 

                 I4TEMP             = LOGBUF(23)
                 TRABUF(TWEPSER1_9) = ZEXT( I1TEMP(1) ) 
                 TRABUF(TWEPSER1_10)= ZEXT( I1TEMP(2) ) 
                 TRABUF(TWEPFRC1_1) = ZEXT( I1TEMP(3) )
                 TRABUF(TWEPFRC1_2) = ZEXT( I1TEMP(4) )

                 I4TEMP             = LOGBUF(24)
                 TRABUF(TWEPFRC1_3) = ZEXT( I1TEMP(1) )
                 TRABUF(TWEPFRC1_4) = ZEXT( I1TEMP(2) )
                 TRABUF(TWEPFRC1_5) = ZEXT( I1TEMP(3) )
                 TRABUF(TWEPFRC1_6) = ZEXT( I1TEMP(4) )

                 I4TEMP             = LOGBUF(25)
                 TRABUF(TWEPFRC1_7) = ZEXT( I1TEMP(1) )
                 TRABUF(TWEPFRC1_8) = ZEXT( I1TEMP(2) )
                 TRABUF(TWEPFRC1_9) = ZEXT( I1TEMP(3) )
                 TRABUF(TWEPFRC1_10)= ZEXT( I1TEMP(4) )

                 I4TEMP             = LOGBUF(26)
                 TRABUF(TWEPSER2_1) = ZEXT( I1TEMP(1) ) 
                 TRABUF(TWEPSER2_2) = ZEXT( I1TEMP(2) ) 
                 TRABUF(TWEPSER2_3) = ZEXT( I1TEMP(3) ) 
                 TRABUF(TWEPSER2_4) = ZEXT( I1TEMP(4) ) 

                 I4TEMP             = LOGBUF(27)
                 TRABUF(TWEPSER2_5) = ZEXT( I1TEMP(1) ) 
                 TRABUF(TWEPSER2_6) = ZEXT( I1TEMP(2) ) 
                 TRABUF(TWEPSER2_7) = ZEXT( I1TEMP(3) ) 
                 TRABUF(TWEPSER2_8) = ZEXT( I1TEMP(4) ) 

                 I4TEMP             = LOGBUF(28)
                 TRABUF(TWEPSER2_9) = ZEXT( I1TEMP(1) ) 
                 TRABUF(TWEPSER2_10)= ZEXT( I1TEMP(2) ) 
                 TRABUF(TWEPFRC2_1) = ZEXT( I1TEMP(3) )
                 TRABUF(TWEPFRC2_2) = ZEXT( I1TEMP(4) )

                 I4TEMP             = LOGBUF(29)
                 TRABUF(TWEPFRC2_3) = ZEXT( I1TEMP(1) )
                 TRABUF(TWEPFRC2_4) = ZEXT( I1TEMP(2) )
                 TRABUF(TWEPFRC2_5) = ZEXT( I1TEMP(3) )
                 TRABUF(TWEPFRC2_6) = ZEXT( I1TEMP(4) )

                 I4TEMP             = LOGBUF(30)
                 TRABUF(TWEPFRC2_7) = ZEXT( I1TEMP(1) )
                 TRABUF(TWEPFRC2_8) = ZEXT( I1TEMP(2) )
                 TRABUF(TWEPFRC2_9) = ZEXT( I1TEMP(3) )
                 TRABUF(TWEPFRC2_10)= ZEXT( I1TEMP(4) )

                 I4TEMP             = LOGBUF(31)
                 TRABUF(TWEPSER3_1) = ZEXT( I1TEMP(1) ) 
                 TRABUF(TWEPSER3_2) = ZEXT( I1TEMP(2) ) 
                 TRABUF(TWEPSER3_3) = ZEXT( I1TEMP(3) ) 
                 TRABUF(TWEPSER3_4) = ZEXT( I1TEMP(4) ) 
C
C BEGIN CONTINUATION RECORD 2
C
                 I4TEMP             = LOGBUF(33)
                 TRABUF(TWEPSER3_5) = ZEXT( I1TEMP(1) ) 
                 TRABUF(TWEPSER3_6) = ZEXT( I1TEMP(2) ) 
                 TRABUF(TWEPSER3_7) = ZEXT( I1TEMP(3) ) 
                 TRABUF(TWEPSER3_8) = ZEXT( I1TEMP(4) ) 

                 I4TEMP             = LOGBUF(34)
                 TRABUF(TWEPSER3_9) = ZEXT( I1TEMP(1) ) 
                 TRABUF(TWEPSER3_10)= ZEXT( I1TEMP(2) ) 
                 TRABUF(TWEPFRC3_1) = ZEXT( I1TEMP(3) )
                 TRABUF(TWEPFRC3_2) = ZEXT( I1TEMP(4) )

                 I4TEMP             = LOGBUF(35)
                 TRABUF(TWEPFRC3_3) = ZEXT( I1TEMP(1) )
                 TRABUF(TWEPFRC3_4) = ZEXT( I1TEMP(2) )
                 TRABUF(TWEPFRC3_5) = ZEXT( I1TEMP(3) )
                 TRABUF(TWEPFRC3_6) = ZEXT( I1TEMP(4) )

                 I4TEMP             = LOGBUF(36)
                 TRABUF(TWEPFRC3_7) = ZEXT( I1TEMP(1) )
                 TRABUF(TWEPFRC3_8) = ZEXT( I1TEMP(2) )
                 TRABUF(TWEPFRC3_9) = ZEXT( I1TEMP(3) )
                 TRABUF(TWEPFRC3_10)= ZEXT( I1TEMP(4) )

              ENDIF
              GOTO 9000   

        ENDIF

        IF(TRABUF(TTYP).EQ.TRET) THEN        !RETURNS OF PPASSIVE TICKETS

	  I4TEMP         =  LOGBUF(8)
	  TRABUF(TPTCK)  =  ISHFT (I1TEMP(4),-4)
	  TRABUF(TPRETYP)=  IAND (I1TEMP(4), '0F'X)
          TRABUF(TPNUM1) =  IAND(I4TEMP,'00FFFFFF'X)

	  TRABUF(TPKEY1) =  LOGBUF(9)
	  TRABUF(TPPAY1) =  LOGBUF(10)

          I4TEMP         =  LOGBUF(11)
	  TRABUF(TPEMIS1)=  ZEXT( I2TEMP(1) )	  
	  TRABUF(TPSER1) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN1) =  ZEXT( I1TEMP(4) )	  

	  I4TEMP         =  LOGBUF(12)
          TRABUF(TPNUM2) =  IAND(I4TEMP,'00FFFFFF'X)
          ! OPREQ is in I1TEMP(4), this is decoded above  

	  TRABUF(TPKEY2) =  LOGBUF(13)
	  TRABUF(TPPAY2) =  LOGBUF(14)

          I4TEMP         =  LOGBUF(15)
	  TRABUF(TPEMIS2)=  ZEXT( I2TEMP(1) )	  
	  TRABUF(TPSER2) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN2) =  ZEXT( I1TEMP(4) )	  

	  I4TEMP         =  LOGBUF(16)
	  TRABUF(TPOFFTER)= ZEXT( I2TEMP(1) )
	  X              =  ZEXT( I1TEMP(3) )
	  TRABUF(TPSTS1) =  IAND (X , '0F'X)
	  TRABUF(TPSTS2) =  ISHFT(X, -4)

          IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
C  *** PASSIVE UNSOLD TICKETS CONTINUATION RECORD 1 ***
C
	  TRABUF(TPKEY3) =  LOGBUF(17)	  
	  TRABUF(TPPAY3) =  LOGBUF(18)	  

	  I4TEMP         =  LOGBUF(19)
	  TRABUF(TPEMIS3)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER3) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN3) =  ZEXT( I1TEMP(4) )	  

	  TRABUF(TPKEY4) =  LOGBUF(20)	  
	  TRABUF(TPPAY4) =  LOGBUF(21)	  

	  I4TEMP         =  LOGBUF(22)
	  TRABUF(TPEMIS4)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER4) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN4) =  ZEXT( I1TEMP(4) )	  

	  TRABUF(TPKEY5) =  LOGBUF(23)	  
	  TRABUF(TPPAY5) =  LOGBUF(24)	  

	  I4TEMP         =  LOGBUF(25)
	  TRABUF(TPEMIS5)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER5) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN5) =  ZEXT( I1TEMP(4) )	  

	  TRABUF(TPKEY6) =  LOGBUF(26)	  
	  TRABUF(TPPAY6) =  LOGBUF(27)	  

	  I4TEMP         =  LOGBUF(28)
	  TRABUF(TPEMIS6)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER6) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN6) =  ZEXT( I1TEMP(4) )	  

	  I4TEMP         =  LOGBUF(29)
          TRABUF(TPNUM3) =  IAND(I4TEMP,'00FFFFFF'X)
	  X              =  ZEXT( I1TEMP(4) )
	  TRABUF(TPSTS3) =  IAND (X , '0F'X)
	  TRABUF(TPSTS4) =  ISHFT(X, -4)

	  I4TEMP         =  LOGBUF(30)
          TRABUF(TPNUM4) =  IAND(I4TEMP,'00FFFFFF'X)
	  X              =  ZEXT( I1TEMP(4) )
	  TRABUF(TPSTS5) =  IAND (X , '0F'X)
	  TRABUF(TPSTS6) =  ISHFT(X, -4)

	  I4TEMP         =  LOGBUF(31)
          TRABUF(TPNUM5) =  IAND(I4TEMP,'00FFFFFF'X)
          TRABUF(TPFRCNT)=  ZEXT( I1TEMP(4) )

	  I4TEMP         =  LOGBUF(32)
          TRABUF(TPNUM6) =  IAND(I4TEMP,'00FFFFFF'X)

          IF(TRABUF(TSIZE).EQ.2) GOTO 9000
C
C  *** PASSIVE UNSOLD TICKETS CONTINUATION RECORD 2 ***
C
	  TRABUF(TPKEY7) =  LOGBUF(33)	  
	  TRABUF(TPPAY7) =  LOGBUF(34)	  

	  I4TEMP         =  LOGBUF(35)
	  TRABUF(TPEMIS7)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER7) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN7) =  ZEXT( I1TEMP(4) )	  

	  TRABUF(TPKEY8) =  LOGBUF(36)	  
	  TRABUF(TPPAY8) =  LOGBUF(37)	  

	  I4TEMP         =  LOGBUF(38)
	  TRABUF(TPEMIS8)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER8) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN8) =  ZEXT( I1TEMP(4) )	  

	  TRABUF(TPKEY9) =  LOGBUF(39)	  
	  TRABUF(TPPAY9) =  LOGBUF(40)	  

	  I4TEMP         =  LOGBUF(41)
	  TRABUF(TPEMIS9)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER9) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN9) =  ZEXT( I1TEMP(4) )	  

	  TRABUF(TPKEY10) =  LOGBUF(42)	  
	  TRABUF(TPPAY10) =  LOGBUF(43)	  

	  I4TEMP          =  LOGBUF(44)
	  TRABUF(TPEMIS10)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER10) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN10) =  ZEXT( I1TEMP(4) )	  

	  I4TEMP          =  LOGBUF(45)
          TRABUF(TPNUM7)  =  IAND(I4TEMP,'00FFFFFF'X)
	  X               =  ZEXT( I1TEMP(4) )
	  TRABUF(TPSTS7)  =  IAND (X , '0F'X)
	  TRABUF(TPSTS8)  =  ISHFT(X, -4)

	  I4TEMP          =  LOGBUF(46)
          TRABUF(TPNUM8)  =  IAND(I4TEMP,'00FFFFFF'X)
	  X               =  ZEXT( I1TEMP(4) )
	  TRABUF(TPSTS9)  =  IAND (X , '0F'X)
	  TRABUF(TPSTS10) =  ISHFT(X, -4)

	  I4TEMP          =  LOGBUF(47)
          TRABUF(TPNUM9)  =  IAND(I4TEMP,'00FFFFFF'X)

	  I4TEMP          =  LOGBUF(48)
          TRABUF(TPNUM10) =  IAND(I4TEMP,'00FFFFFF'X)

      	  GOTO 9000

	ENDIF
C
C DECODE WAGERS/CANCELLATIONS/DELETIONS
C
        IF(TRABUF(TTYP).EQ.TWAG .OR.
     *     TRABUF(TTYP).EQ.TCAN .OR.
     *     TRABUF(TTYP).EQ.TINC) THEN

C
C DECODE LOTTO/SPORTS/SPEDEN WAGER BODY
C
           IF(TRABUF(TGAMTYP).EQ.TLTO.OR.TRABUF(TGAMTYP).EQ.TSPT 
     *     .OR.TRABUF(TGAMTYP).EQ.TTGL) THEN 
C
              TRABUF(TWCSER)   = LOGBUF(8)
C 
              TRABUF(TWAMT)    = LOGBUF(9)
C
              I4TEMP           = LOGBUF(10)
              TRABUF(TWBEG)    = ZEXT( I2TEMP(1) )
              TRABUF(TWCTER)   = ZEXT( I2TEMP(2) )
C 
              I4TEMP           = LOGBUF(11)
              TRABUF(TWQPF)    = ZEXT( I2TEMP(1) )
              TRABUF(TWSIMP)   = ZEXT( I2TEMP(2) )
C 
C              I4TEMP           = LOGBUF(12)
C              TRABUF(TWSYSN)   = ZEXT( I1TEMP(1) )
C              TRABUF(TWVSTS)   = ZEXT( I1TEMP(2) ) 
C              TRABUF(TWDUR)    = ZEXT( I1TEMP(3) )
C              TRABUF(TWSYST)   = ZEXT( I1TEMP(4) )
C 
              BDATA_VER = 0
              KIKFLG = .FALSE.
              I4TEMP           = LOGBUF(13)
              TRABUF(TWNMRK)   = ZEXT( I1TEMP(1) )
              X                = ZEXT( I1TEMP(2) )
              IF(IAND(X,'80'X).NE.0) TRABUF(TWMFLG) = 1
              IF(IAND(X,'40'X).NE.0) KIKFLG = .TRUE.
              IF(IAND(X,'20'X).NE.0) TRABUF(TWKFLG) = 1
              IF(IAND(X,'10'X).NE.0) TRABUF(TWKFLG2) = 1
              IF(IAND(X,'08'X).NE.0) TRABUF(TWFFLG) = 1
              IF(IAND(X,'04'X).NE.0) TRABUF(TWADDFW) = 1
              IF(IAND(X,'02'X).NE.0) TRABUF(TWLMFI) = 1
              IF(IAND(X,'01'X).NE.0) BDATA_VER = 1
              TRABUF(TWCDCOFF) = ZEXT( I1TEMP(3) )
              TRABUF(TWMFRAC)  = ZEXT( I1TEMP(4) )
C
              TRABUF(TWBNKID)  = IAND(LOGBUF(14),'00FFFFFF'X)
              TRABUF(TWWEQP)   = IAND(ISHFT(LOGBUF(14),-24),'000000FF'X)
C
              TRABUF(TWBNKNM)  = LOGBUF(15)
C
              I4TEMP = LOGBUF(16)
              TRABUF(TWSRW)    = ZEXT( I1TEMP(1) )
	      X                = ZEXT( I1TEMP(2) )
	      TRABUF(TWNBET)   = IAND (X , '0F'X)
              IF (TRABUF(TGAMTYP).EQ.TLTO) THEN                      !V51...
                TRABUF(TWLUCK)  = ISHFT(X, -4)
              ELSE
                TRABUF(TWSPFRG) = ISHFT(X, -4)
              ENDIF                                                  !...V51
              TRABUF(TWTKC)    = ZEXT( I1TEMP(3) )
C
C BEGIN CONTINUATION RECORD 1
C
              IF(TRABUF(TGAMTYP) .EQ. TSPT) THEN
                TRABUF(TWCEBM) = LOGBUF(18)  ! SPORTS GAME CANCELATION EVENTS BITMAP
              ELSE
                TRABUF(TWCEBM) = 0
              ENDIF
C
              IF(BDATA_VER .EQ. 0) THEN    ! CHECK BOARD DATA VERSION
                 LOGOFF = 17               ! HOLE RECORD FOR BORAD DATA OLD VERSION
                 BRDOFF = 15               ! IF WE DO NOT PLAY KICKER.
              ELSE
                 LOGOFF = 19               ! NEW VERSION TO SAVE "TWCEBM" ON SPORTS GAMES
                 BRDOFF = 13
              ENDIF
C
              IF(KIKFLG) THEN
                 TRABUF(TWKICK)  = LOGBUF(17)
C                TRABUF(TWKICK2) = LOGBUF(18)  ! NOT USED IN "SCML"
C
                 I4TEMP = LOGBUF(19)
                 TRABUF(TWKAMT)  = ZEXT( I2TEMP(1) )
                 TRABUF(TWKGME)  = ZEXT( I1TEMP(3) )
                 TRABUF(TWKDUR)  = ZEXT( I1TEMP(4) )
C
                 I4TEMP = LOGBUF(20)
                 TRABUF(TWKBEG)  = ZEXT( I2TEMP(1) )
C
                 LOGOFF = 21                    
                 BRDOFF = 11                    !WE LOOSE SPACE FOR BOARD DATA
C                                               !TO KICKER STORAGE.
              ENDIF
C
C   

C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------   
              I4TEMP           = LOGBUF(12)
              TRABUF(TWSYSN)   = ZEXT( I1TEMP(1) )
              TRABUF(TWVSTS)   = ZEXT( I1TEMP(2) ) 
              
              IF(ISHFT(ZEXT(I1TEMP(3)),-4) .EQ. 1) THEN
                TRABUF(TWDUR)    = ZEXT( IAND(I1TEMP(3),15) )

                TRABUF(TWCOLMSERL_TLTO) = LOGBUF(17) 
                TRABUF(TWCOLMSERM_TLTO) = LOGBUF(18)
                TRABUF(TWCOLMMIDL_TLTO) = LOGBUF(19)
                I4TEMP = LOGBUF(20) 
                TRABUF(TWCOLMSERH_TLTO) = I1TEMP(1)  
                TRABUF(TWCOLMMIDH_TLTO) = I1TEMP(2)
                TRABUF(TWCOLMCOMF_TLTO) = I1TEMP(3)

C WE LOSE SPACE FOR BOARD DATA
C TO USE FOR NEW TERMINALS 2021                  
                LOGOFF = 21
                BRDOFF = 11                 
              ELSE
                TRABUF(TWDUR)    = ZEXT( I1TEMP(3) )
              ENDIF
              TRABUF(TWSYST)   = ZEXT( I1TEMP(4) )              
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------                   
              CALL FASTMOV(LOGBUF(LOGOFF), TRABUF(TWBORD), BRDOFF)
C
C           *** NOW SET CALCULATED FIELDS ***
C
              TRABUF(TWEND) = TRABUF(TWBEG)+TRABUF(TWDUR)-1
              TRABUF(TWTOT) = (TRABUF(TWDUR) * TRABUF(TWAMT))+
     *                        TRABUF(TWTKC)
              IF(TRABUF(TWKGME).NE.0)THEN
                 TRABUF(TWKEND) = TRABUF(TWKBEG)+ TRABUF(TWKDUR)-1
                 TRABUF(TWTOT)  = TRABUF(TWTOT) +
     *                            TRABUF(TWKAMT)*TRABUF(TWKDUR) 
              ENDIF
              CALL FASTSET(TRABUF(TWNMRK),TRABUF(TWNMRK+1),11)
C
              IF(TRABUF(TSIZE).EQ.2) GOTO 9000
C
C           *** CONTINUATION RECORD 2 ***
C
              CALL FASTMOV(LOGBUF(33), TRABUF(TWBORD+BRDOFF), 15)
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------ 

C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------               
              GOTO 9000
           ENDIF
C
C DECODE KICKER (ONLY) WAGER BODY
C
           IF(TRABUF(TGAMTYP).EQ.TKIK) THEN
C 
             TRABUF(TWCSER)  = LOGBUF(8)
C 
             I4TEMP          = LOGBUF(9)
             TRABUF(TWAMT)   = ZEXT( I2TEMP(1) )
             TRABUF(TWDAMT)  = ZEXT( I2TEMP(2) )
C 
             I4TEMP          = LOGBUF(10)
             TRABUF(TWBEG)   = ZEXT( I2TEMP(1) )
             TRABUF(TWCTER)  = ZEXT( I2TEMP(2) )
C 
             TRABUF(TWKICK)  = LOGBUF(11)
             TRABUF(TWKICK2) = LOGBUF(12)
             I4TEMP          = LOGBUF(13)
             TRABUF(TWDUR)   = ZEXT( I1TEMP(1) )
             TRABUF(TWVSTS)  = ZEXT( I1TEMP(2) )
             X               = ZEXT( I1TEMP(3) )
             IF(IAND(X,'80'X).NE.0) TRABUF(TWQPF) = 1
             IF(IAND(X,'20'X).NE.0) TRABUF(TWKFLG) = 1
             IF(IAND(X,'10'X).NE.0) TRABUF(TWKFLG2)= 1
             IF(IAND(X,'08'X).NE.0) TRABUF(TWFFLG) = 1
             IF(IAND(X,'04'X).NE.0) TRABUF(TWADDFW) = 1
             TRABUF(TWTKC)   = ZEXT( I1TEMP(4) )
C
             TRABUF(TWBNKID) = IAND(LOGBUF(14),'00FFFFFF'X)
             TRABUF(TWNBET)  = IAND(ISHFT(LOGBUF(14),-24),'000000FF'X)
C
             TRABUF(TWBNKNM) = LOGBUF(15)
C
             I4TEMP          = LOGBUF(16)
             TRABUF(TWQPF)   = ZEXT( I2TEMP(1) )
             TRABUF(TWSYST)  = ZEXT( I1TEMP(3) )
	     KIND            = TRABUF(TGAMIND)
	     
	     TRABUF(TWLNKSER)= IAND(LOGBUF(29),'00FFFFFF'X)
             TRABUF(TWLNKCHK)= IAND(ISHFT(LOGBUF(29),-24),'000000FF'X)
C
C           *** NOW SET CALCULATED FIELDS AND SOME KICKER FIELDS ***
C
             TRABUF(TWKGME)  = TRABUF(TGAM)
             TRABUF(TWKBEG)  = TRABUF(TWBEG)
             TRABUF(TWEND)   = TRABUF(TWBEG)+TRABUF(TWDUR)-1
             TRABUF(TWKEND)  = TRABUF(TWEND)
             TRABUF(TWKDUR)  = TRABUF(TWDUR)   
             TRABUF(TWKAMT)  = TRABUF(TWAMT)
             TRABUF(TWTOT)   = TRABUF(TWDUR) * TRABUF(TWAMT) +
     *                         TRABUF(TWTKC)
             GOTO 9000
          ENDIF
C
C NUMBERS WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TNBR) THEN
C 
            TRABUF(TWCSER)  = LOGBUF(8)
C
            I4TEMP          = LOGBUF(9)
            TRABUF(TWAMT)   = ZEXT( I2TEMP(1) )
            TRABUF(TWDAMT)  = ZEXT( I2TEMP(2) )
 
            I4TEMP          = LOGBUF(10)
            TRABUF(TWBEG)   = ZEXT( I2TEMP(1) )
            TRABUF(TWCTER)  = ZEXT( I2TEMP(2) )
C
            I4TEMP          = LOGBUF(11)
            TRABUF(TWNNUM1) = ZEXT( I2TEMP(1) )
            TRABUF(TWNNUM2) = ZEXT( I2TEMP(2) )
C
            I4TEMP          = LOGBUF(12)
            TRABUF(TWNNUM3) = ZEXT( I2TEMP(1) )
            TRABUF(TWNNUM4) = ZEXT( I2TEMP(2) )
C
            I4TEMP          = LOGBUF(13)
            TRABUF(TWNAMT1) = ZEXT( I2TEMP(1) )
            TRABUF(TWNAMT2) = ZEXT( I2TEMP(2) )
C
            I4TEMP          = LOGBUF(14)
            TRABUF(TWNAMT3) = ZEXT( I2TEMP(1) )
            TRABUF(TWNAMT4) = ZEXT( I2TEMP(2) )
C
            I4TEMP          = LOGBUF(15)
            TRABUF(TWNPOL1) = ZEXT( I1TEMP(1) )
            TRABUF(TWNPOL2) = ZEXT( I1TEMP(2) )
            TRABUF(TWNPOL3) = ZEXT( I1TEMP(3) )
            TRABUF(TWNPOL4) = ZEXT( I1TEMP(4) )
C
            I4TEMP          = LOGBUF(16)
            TRABUF(TWNBET)  = ZEXT( I1TEMP(1) )
            TRABUF(TWTKC)   = ZEXT( I1TEMP(2) )
            TRABUF(TWQPF)   = ZEXT( I1TEMP(3) )
C
            IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
            I4TEMP          = LOGBUF(17)
            X               = ZEXT( I1TEMP(1) )
            IF(IAND(X,'80'X).NE.0) TRABUF(TWQPF)  = 1
            IF(IAND(X,'40'X).NE.0) TRABUF(TWNAND) = 1
            TRABUF(TWNTYP)  = ZEXT( I1TEMP(2) )
            TRABUF(TWVSTS)  = ZEXT( I1TEMP(3) )
            TRABUF(TWDUR)   = ZEXT( I1TEMP(4) )
C
            TRABUF(TWEND)   = TRABUF(TWBEG)+TRABUF(TWDUR)-1
            TRABUF(TWTOT)   = (TRABUF(TWAMT) * TRABUF(TWDUR)) +
     *                        TRABUF(TWTKC)
C
            I4TEMP          = LOGBUF(18)
            TRABUF(TWNNUM5) = ZEXT( I2TEMP(1) )
            TRABUF(TWNNUM6) = ZEXT( I2TEMP(2) )
C
            I4TEMP          = LOGBUF(19)
            TRABUF(TWNNUM7) = ZEXT( I2TEMP(1) )
            TRABUF(TWNNUM8) = ZEXT( I2TEMP(2) )
C
            I4TEMP          = LOGBUF(20)
            TRABUF(TWNNUM9) = ZEXT( I2TEMP(1) )
            TRABUF(TWNNUM10)= ZEXT( I2TEMP(2) )
C
            I4TEMP          = LOGBUF(21)
            TRABUF(TWNAMT5) = ZEXT( I2TEMP(1) )
            TRABUF(TWNAMT6) = ZEXT( I2TEMP(2) )
C
            I4TEMP          = LOGBUF(22)
            TRABUF(TWNAMT7) = ZEXT( I2TEMP(1) )
            TRABUF(TWNAMT8) = ZEXT( I2TEMP(2) )
C
            I4TEMP          = LOGBUF(23)
            TRABUF(TWNAMT9) = ZEXT( I2TEMP(1) )
            TRABUF(TWNAMT10)= ZEXT( I2TEMP(2) )
C
            I4TEMP          = LOGBUF(24)
            TRABUF(TWNPOL5) = ZEXT( I1TEMP(1) )
            TRABUF(TWNPOL6) = ZEXT( I1TEMP(2) )
            TRABUF(TWNPOL7) = ZEXT( I1TEMP(3) )
            TRABUF(TWNPOL8) = ZEXT( I1TEMP(4) )
C
            I4TEMP          = LOGBUF(25)
            TRABUF(TWNPOL9) = ZEXT( I1TEMP(1) )
            TRABUF(TWNPOL10)= ZEXT( I1TEMP(2) )
            TRABUF(TWQPF   )= ZEXT( I2TEMP(2) )
C
            GOTO 9000
          ENDIF
C
C SCORE WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TSCR) THEN
C
             TRABUF(TWCSER)  = LOGBUF(8)
C
             I4TEMP=LOGBUF(9)
             TRABUF(TWDAMT)  = ZEXT( I2TEMP(1) )
             TRABUF(TWBEG )  = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(10)
             TRABUF(TWSAMT )  = ZEXT( I2TEMP(1) )
             TRABUF(TWSAMT+TWSBLEN) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(11)
             TRABUF(TWSAMT+TWSBLEN*2) = ZEXT( I2TEMP(1) )
             TRABUF(TWSSCR1) = ZEXT( I1TEMP(3) )
             TRABUF(TWSSCR2) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(12)
             TRABUF(TWSSCR1+TWSBLEN) = ZEXT( I1TEMP(1) )
             TRABUF(TWSSCR2+TWSBLEN) = ZEXT( I1TEMP(2) )
             TRABUF(TWSSCR1+TWSBLEN*2) = ZEXT( I1TEMP(3) )
             TRABUF(TWSSCR2+TWSBLEN*2) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(13)
             TRABUF(TWNBET) = ZEXT( I1TEMP(4) )
             I1TEMP(4) = 0
             TRABUF(TWBNKID) = I4TEMP
C
             TRABUF(TWBNKNM) = LOGBUF(14)
C
             I4TEMP = LOGBUF(15)
             TRABUF(TWSYST) = ZEXT( I1TEMP(1) )
             TRABUF(TWSYSN) = ZEXT( I1TEMP(2) )
             TRABUF(TWVSTS) = ZEXT( I1TEMP(3) )
             X = ZEXT( I1TEMP(4) )
             IF(IAND(X,'80'X).NE.0) TRABUF(TWQPF)  = 1
             IF(IAND(X,'08'X).NE.0) TRABUF(TWFFLG) = 1
C
             I4TEMP = LOGBUF(16)
             TRABUF(TWCTER) = ZEXT( I2TEMP(1) )
             TRABUF(TWTKC) = ZEXT(I1TEMP(3))
C
C SET CALCULATED FIELDS
C
             TRABUF(TWEND) = TRABUF(TWBEG)      
             TRABUF(TWDUR) = 1
             IF(TRABUF(TWSYST).EQ.FULSYS) THEN
                TRABUF(TWAMT) = TRABUF(TWSAMT)*TRABUF(TWSYSN)
             ELSE
                TRABUF(TWAMT) = TRABUF(TWSAMT)+TRABUF(TWSAMT+TWSBLEN)+
     *                          TRABUF(TWSAMT+TWSBLEN*2)
             ENDIF
             TRABUF(TWTOT) = (TRABUF(TWAMT) * TRABUF(TWDUR)) + 
     *                        TRABUF(TWTKC)
C
             IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
             I4TEMP = LOGBUF(17)
             TRABUF(TWSAMT+TWSBLEN*3) = ZEXT( I2TEMP(1) )
             TRABUF(TWSAMT+TWSBLEN*4) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(18)
             TRABUF(TWSAMT+TWSBLEN*5) = ZEXT( I2TEMP(1) )
             TRABUF(TWSAMT+TWSBLEN*6) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(19)
             TRABUF(TWSAMT+TWSBLEN*7) = ZEXT( I2TEMP(1) )
             TRABUF(TWSAMT+TWSBLEN*8) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(20)
             TRABUF(TWSAMT+TWSBLEN*9) = ZEXT( I2TEMP(1) )
             TRABUF(TWSSCR1+TWSBLEN*3) = ZEXT( I1TEMP(3) )
             TRABUF(TWSSCR2+TWSBLEN*3) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(21)
             TRABUF(TWSSCR1+TWSBLEN*4) = ZEXT( I1TEMP(1) )
             TRABUF(TWSSCR2+TWSBLEN*4) = ZEXT( I1TEMP(2) )
             TRABUF(TWSSCR1+TWSBLEN*5) = ZEXT( I1TEMP(3) )
             TRABUF(TWSSCR2+TWSBLEN*5) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(22) 
             TRABUF(TWSSCR1+TWSBLEN*6) = ZEXT( I1TEMP(1) )
             TRABUF(TWSSCR2+TWSBLEN*6) = ZEXT( I1TEMP(2) )
             TRABUF(TWSSCR1+TWSBLEN*7) = ZEXT( I1TEMP(3) )
             TRABUF(TWSSCR2+TWSBLEN*7) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(23)
             TRABUF(TWSSCR1+TWSBLEN*8) = ZEXT( I1TEMP(1) )
             TRABUF(TWSSCR2+TWSBLEN*8) = ZEXT( I1TEMP(2) )
             TRABUF(TWSSCR1+TWSBLEN*9) = ZEXT( I1TEMP(3) )
             TRABUF(TWSSCR2+TWSBLEN*9) = ZEXT( I1TEMP(4) )
C
C SET CALCULATED FIELDS
C
             TRABUF(TWAMT)=TRABUF(TWAMT)+
     *                     TRABUF(TWSAMT+TWSBLEN*3) +
     *                     TRABUF(TWSAMT+TWSBLEN*4) +
     *                     TRABUF(TWSAMT+TWSBLEN*5) +
     *                     TRABUF(TWSAMT+TWSBLEN*6) +
     *                     TRABUF(TWSAMT+TWSBLEN*7) +
     *                     TRABUF(TWSAMT+TWSBLEN*8) +
     *                     TRABUF(TWSAMT+TWSBLEN*9) 
             TRABUF(TWTOT) = (TRABUF(TWAMT) * TRABUF(TWDUR)) + 
     *                        TRABUF(TWTKC)
             GOTO 9000
          ENDIF
C
C WINNERS TIP WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TWIT) THEN
C
             TRABUF(TWCSER)  = LOGBUF(8)
C
             I4TEMP = LOGBUF(9)
             TRABUF(TWDAMT)  = ZEXT( I2TEMP(1))
             TRABUF(TWBEG )  = ZEXT( I2TEMP(2))
C
             I4TEMP = LOGBUF(10)
             TRABUF(TWWAMT )  = ZEXT( I2TEMP(1) )
             TRABUF(TWWAMT+TWWBLEN) = ZEXT( I2TEMP(2) ) 
C
             I4TEMP = LOGBUF(11)
             TRABUF(TWWAMT+TWWBLEN*2) = ZEXT( I2TEMP(1) )
             TRABUF(TWWROW) = ZEXT( I1TEMP(3) )
             TRABUF(TWWROW+TWWBLEN) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(12)
             TRABUF(TWWROW+TWWBLEN*2) = ZEXT( I1TEMP(4) )
             I1TEMP(4) = 0
             TRABUF(TWBNKID) = I4TEMP
C
             I4TEMP = LOGBUF(13)
             TRABUF(TWBNKNM) = I4TEMP
C
             I4TEMP = LOGBUF(14)
             TRABUF(TWWCOUPID) = ZEXT( I1TEMP(1) )
             TRABUF(TWNBET) = ZEXT( I1TEMP(3) )
             TRABUF(TWSYST) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(15)
             TRABUF(TWSYSN) = ZEXT( I1TEMP(1) )
             TRABUF(TWVSTS) = ZEXT( I1TEMP(2) )
             TRABUF(TWCTER) = ZEXT( I2TEMP(2) )
C
             I4TEMP=LOGBUF(16)
             X = ZEXT( I1TEMP(1) )
             IF(IAND(X,'80'X).NE.0) TRABUF(TWQPF)  = 1
             IF(IAND(X,'08'X).NE.0) TRABUF(TWFFLG) = 1
             TRABUF(TWTKC) = ZEXT( I1TEMP(2) )
C
C SET CALCULATED FIELDS
C
             TRABUF(TWAMT) = TRABUF(TWWAMT) + TRABUF(TWWAMT+TWWBLEN) +
     *                      TRABUF(TWWAMT+TWWBLEN*2) 
             TRABUF(TWEND) = TRABUF(TWBEG)
             TRABUF(TWDUR) = 1
             TRABUF(TWTOT) = (TRABUF(TWAMT) * TRABUF(TWDUR)) + 
     *                        TRABUF(TWTKC)
C
             IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
             I4TEMP = LOGBUF(17)
             TRABUF(TWWAMT+TWWBLEN*3) = ZEXT( I2TEMP(1) )
             TRABUF(TWWAMT+TWWBLEN*4) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(18)
             TRABUF(TWWAMT+TWWBLEN*5) = ZEXT( I2TEMP(1) )
             TRABUF(TWWAMT+TWWBLEN*6) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(19)
             TRABUF(TWWAMT+TWWBLEN*7) = ZEXT( I2TEMP(1) )
             TRABUF(TWWAMT+TWWBLEN*8) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(20)
             TRABUF(TWWAMT+TWWBLEN*9) = ZEXT( I2TEMP(1) )     
             TRABUF(TWWROW+TWWBLEN*3) = ZEXT( I1TEMP(3) )
             TRABUF(TWWROW+TWWBLEN*4) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(21)
             TRABUF(TWWROW+TWWBLEN*5) = ZEXT( I1TEMP(1) )
             TRABUF(TWWROW+TWWBLEN*6) = ZEXT( I1TEMP(2) )
             TRABUF(TWWROW+TWWBLEN*7) = ZEXT( I1TEMP(3) )
             TRABUF(TWWROW+TWWBLEN*8) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(22)
             TRABUF(TWWROW+TWWBLEN*9) = ZEXT( I1TEMP(1) )
C
C SET CALCULATED FIELDS
C
             TRABUF(TWAMT) = TRABUF(TWAMT)+
     *                     TRABUF(TWWAMT+TWWBLEN*3) +
     *                     TRABUF(TWWAMT+TWWBLEN*4) +
     *                     TRABUF(TWWAMT+TWWBLEN*5) +
     *                     TRABUF(TWWAMT+TWWBLEN*6) +
     *                     TRABUF(TWWAMT+TWWBLEN*7) +
     *                     TRABUF(TWWAMT+TWWBLEN*8) +
     *                     TRABUF(TWWAMT+TWWBLEN*9)
             TRABUF(TWTOT) = (TRABUF(TWAMT) * TRABUF(TWDUR)) + 
     *                        TRABUF(TWTKC)
             GOTO 9000
          ENDIF
C
C SUPER DOUBLE WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TDBL) THEN
C
             TRABUF(TWCSER)  = LOGBUF(8)
C
             I4TEMP=LOGBUF(9)
             TRABUF(TWDBCOUPID) = ZEXT( I1TEMP(1) )
             TRABUF(TWBEG )  = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(10)
             TRABUF(TWDBAMT )  = ZEXT( I2TEMP(1) )
             TRABUF(TWDBAMT+TWDBBLEN) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(11)
             TRABUF(TWDBAMT+TWDBBLEN*2) = ZEXT( I2TEMP(1) )
             TRABUF(TWDBROW1) = ZEXT( I1TEMP(3) )
             TRABUF(TWDBROW2) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(12)
             TRABUF(TWDBROW1+TWDBBLEN) = ZEXT( I1TEMP(1) )
             TRABUF(TWDBROW2+TWDBBLEN) = ZEXT( I1TEMP(2) )
             TRABUF(TWDBROW1+TWDBBLEN*2) = ZEXT( I1TEMP(3) )
             TRABUF(TWDBROW2+TWDBBLEN*2) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(13)
             TRABUF(TWNBET) = ZEXT( I1TEMP(4) )
             I1TEMP(4) = 0
             TRABUF(TWBNKID) = I4TEMP
C
             TRABUF(TWBNKNM) = LOGBUF(14)
C
             I4TEMP = LOGBUF(15)
             TRABUF(TWSYST) = ZEXT( I1TEMP(1) )
             TRABUF(TWSYSN) = ZEXT( I1TEMP(2) )
             TRABUF(TWVSTS) = ZEXT( I1TEMP(3) )
             X = ZEXT( I1TEMP(4) )
             IF(IAND(X,'80'X).NE.0) TRABUF(TWQPF)  = 1
             IF(IAND(X,'08'X).NE.0) TRABUF(TWFFLG) = 1
C
             I4TEMP = LOGBUF(16)
             TRABUF(TWCTER) = ZEXT( I2TEMP(1) )
             TRABUF(TWTKC) = ZEXT(I1TEMP(3))
C
C SET CALCULATED FIELDS
C
             TRABUF(TWEND) = TRABUF(TWBEG)      
             TRABUF(TWDUR) = 1
             IF(TRABUF(TWSYST).EQ.FULSYS) THEN
                TRABUF(TWAMT) = TRABUF(TWDBAMT)*TRABUF(TWSYSN)
             ELSE
                TRABUF(TWAMT) = TRABUF(TWDBAMT)+
     *                          TRABUF(TWDBAMT+TWDBBLEN)+
     *                          TRABUF(TWDBAMT+TWDBBLEN*2)
             ENDIF
C
             TRABUF(TWTOT) = (TRABUF(TWAMT) * TRABUF(TWDUR)) + 
     *                        TRABUF(TWTKC)
C
             IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
             I4TEMP = LOGBUF(17)
             TRABUF(TWDBAMT+TWDBBLEN*3) = ZEXT( I2TEMP(1) )
             TRABUF(TWDBAMT+TWDBBLEN*4) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(18)
             TRABUF(TWDBAMT+TWDBBLEN*5) = ZEXT( I2TEMP(1) )
             TRABUF(TWDBAMT+TWDBBLEN*6) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(19)
             TRABUF(TWDBAMT+TWDBBLEN*7) = ZEXT( I2TEMP(1) )
             TRABUF(TWDBAMT+TWDBBLEN*8) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(20)
             TRABUF(TWDBAMT+TWDBBLEN*9) = ZEXT( I2TEMP(1) )
             TRABUF(TWDBROW1+TWDBBLEN*3) = ZEXT( I1TEMP(3) )
             TRABUF(TWDBROW2+TWDBBLEN*3) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(21)
             TRABUF(TWDBROW1+TWDBBLEN*4) = ZEXT( I1TEMP(1) )
             TRABUF(TWDBROW2+TWDBBLEN*4) = ZEXT( I1TEMP(2) )
             TRABUF(TWDBROW1+TWDBBLEN*5) = ZEXT( I1TEMP(3) )
             TRABUF(TWDBROW2+TWDBBLEN*5) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(22) 
             TRABUF(TWDBROW1+TWDBBLEN*6) = ZEXT( I1TEMP(1) )
             TRABUF(TWDBROW2+TWDBBLEN*6) = ZEXT( I1TEMP(2) )
             TRABUF(TWDBROW1+TWDBBLEN*7) = ZEXT( I1TEMP(3) )
             TRABUF(TWDBROW2+TWDBBLEN*7) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(23)
             TRABUF(TWDBROW1+TWDBBLEN*8) = ZEXT( I1TEMP(1) )
             TRABUF(TWDBROW2+TWDBBLEN*8) = ZEXT( I1TEMP(2) )
             TRABUF(TWDBROW1+TWDBBLEN*9) = ZEXT( I1TEMP(3) )
             TRABUF(TWDBROW2+TWDBBLEN*9) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(24)
             TRABUF(TWDBAMT+TWDBBLEN*10) = ZEXT( I2TEMP(1) )
             TRABUF(TWDBAMT+TWDBBLEN*11) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(25)
             TRABUF(TWDBROW1+TWDBBLEN*10) = ZEXT( I1TEMP(1) )
             TRABUF(TWDBROW2+TWDBBLEN*10) = ZEXT( I1TEMP(2) )
             TRABUF(TWDBROW1+TWDBBLEN*11) = ZEXT( I1TEMP(3) )
             TRABUF(TWDBROW2+TWDBBLEN*11) = ZEXT( I1TEMP(4) )
C
C SET CALCULATED FIELDS
C
	     DO I=3, TRABUF(TWNBET)-1
                TRABUF(TWAMT)=TRABUF(TWAMT)+TRABUF(TWDBAMT+TWDBBLEN*I)
	     ENDDO
             TRABUF(TWTOT) = (TRABUF(TWAMT) * TRABUF(TWDUR)) + 
     *                        TRABUF(TWTKC)
             GOTO 9000
          ENDIF
C
C TODAYS COUPLE WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TCPL) THEN
C
             TRABUF(TWCSER)  = LOGBUF(8)
C
             I4TEMP=LOGBUF(9)
             TRABUF(TWCPCOUPID) = ZEXT( I1TEMP(1) )
             TRABUF(TWBEG )  = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(10)
             TRABUF(TWCPAMT )  = ZEXT( I2TEMP(1) )
             TRABUF(TWCPAMT+TWCPBLEN) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(11)
             TRABUF(TWCPAMT+TWCPBLEN*2) = ZEXT( I2TEMP(1) )
             TRABUF(TWCPROW1) = ZEXT( I1TEMP(3) )
             TRABUF(TWCPROW2) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(12)
             TRABUF(TWCPROW1+TWCPBLEN) = ZEXT( I1TEMP(1) )
             TRABUF(TWCPROW2+TWCPBLEN) = ZEXT( I1TEMP(2) )
             TRABUF(TWCPROW1+TWCPBLEN*2) = ZEXT( I1TEMP(3) )
             TRABUF(TWCPROW2+TWCPBLEN*2) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(13)
             TRABUF(TWNBET) = ZEXT( I1TEMP(4) )
             I1TEMP(4) = 0
             TRABUF(TWBNKID) = I4TEMP
C
             TRABUF(TWBNKNM) = LOGBUF(14)
C
             I4TEMP = LOGBUF(15)
             TRABUF(TWSYST) = ZEXT( I1TEMP(1) )
             TRABUF(TWSYSN) = ZEXT( I1TEMP(2) )
             TRABUF(TWVSTS) = ZEXT( I1TEMP(3) )
             X = ZEXT( I1TEMP(4) )
             IF(IAND(X,'80'X).NE.0) TRABUF(TWQPF)  = 1
             IF(IAND(X,'08'X).NE.0) TRABUF(TWFFLG) = 1
C
             I4TEMP = LOGBUF(16)
             TRABUF(TWCTER) = ZEXT( I2TEMP(1) )
             TRABUF(TWTKC) = ZEXT(I1TEMP(3))
C
C SET CALCULATED FIELDS
C
             TRABUF(TWEND) = TRABUF(TWBEG)      
             TRABUF(TWDUR) = 1
             IF(TRABUF(TWSYST).EQ.FULSYS) THEN
                TRABUF(TWAMT) = TRABUF(TWCPAMT)*TRABUF(TWSYSN)
             ELSE
                TRABUF(TWAMT) = TRABUF(TWCPAMT)+
     *                          TRABUF(TWCPAMT+TWCPBLEN)+
     *                          TRABUF(TWCPAMT+TWCPBLEN*2)
             ENDIF
             TRABUF(TWTOT) = (TRABUF(TWAMT) * TRABUF(TWDUR)) + 
     *                        TRABUF(TWTKC)
C
             IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
             I4TEMP = LOGBUF(17)
             TRABUF(TWCPAMT+TWCPBLEN*3) = ZEXT( I2TEMP(1) )
             TRABUF(TWCPAMT+TWCPBLEN*4) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(18)
             TRABUF(TWCPAMT+TWCPBLEN*5) = ZEXT( I2TEMP(1) )
             TRABUF(TWCPAMT+TWCPBLEN*6) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(19)
             TRABUF(TWCPAMT+TWCPBLEN*7) = ZEXT( I2TEMP(1) )
             TRABUF(TWCPAMT+TWCPBLEN*8) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(20)
             TRABUF(TWCPAMT+TWCPBLEN*9) = ZEXT( I2TEMP(1) )
             TRABUF(TWCPROW1+TWCPBLEN*3) = ZEXT( I1TEMP(3) )
             TRABUF(TWCPROW2+TWCPBLEN*3) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(21)
             TRABUF(TWCPROW1+TWCPBLEN*4) = ZEXT( I1TEMP(1) )
             TRABUF(TWCPROW2+TWCPBLEN*4) = ZEXT( I1TEMP(2) )
             TRABUF(TWCPROW1+TWCPBLEN*5) = ZEXT( I1TEMP(3) )
             TRABUF(TWCPROW2+TWCPBLEN*5) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(22) 
             TRABUF(TWCPROW1+TWCPBLEN*6) = ZEXT( I1TEMP(1) )
             TRABUF(TWCPROW2+TWCPBLEN*6) = ZEXT( I1TEMP(2) )
             TRABUF(TWCPROW1+TWCPBLEN*7) = ZEXT( I1TEMP(3) )
             TRABUF(TWCPROW2+TWCPBLEN*7) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(23)
             TRABUF(TWCPROW1+TWCPBLEN*8) = ZEXT( I1TEMP(1) )
             TRABUF(TWCPROW2+TWCPBLEN*8) = ZEXT( I1TEMP(2) )
             TRABUF(TWCPROW1+TWCPBLEN*9) = ZEXT( I1TEMP(3) )
             TRABUF(TWCPROW2+TWCPBLEN*9) = ZEXT( I1TEMP(4) )
C
             I4TEMP = LOGBUF(24)
             TRABUF(TWCPAMT+TWCPBLEN*10) = ZEXT( I2TEMP(1) )
             TRABUF(TWCPAMT+TWCPBLEN*11) = ZEXT( I2TEMP(2) )
C
             I4TEMP = LOGBUF(25)
             TRABUF(TWCPROW1+TWCPBLEN*10) = ZEXT( I1TEMP(1) )
             TRABUF(TWCPROW2+TWCPBLEN*10) = ZEXT( I1TEMP(2) )
             TRABUF(TWCPROW1+TWCPBLEN*11) = ZEXT( I1TEMP(3) )
             TRABUF(TWCPROW2+TWCPBLEN*11) = ZEXT( I1TEMP(4) )
C
C SET CALCULATED FIELDS
C
	     DO I=3,TRABUF(TWNBET)-1
                TRABUF(TWAMT)=TRABUF(TWAMT)+TRABUF(TWCPAMT+TWCPBLEN*I)
             ENDDO
             TRABUF(TWTOT) = (TRABUF(TWAMT) * TRABUF(TWDUR)) + 
     *                        TRABUF(TWTKC)
             GOTO 9000
          ENDIF
C
C TOTO SELECT WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TTSL) THEN
C
             TRABUF(TWCSER)  = LOGBUF(8)
C
             I4TEMP=LOGBUF(9)
             TRABUF(TWDAMT)  = ZEXT( I2TEMP(1))
             TRABUF(TWBEG )  = ZEXT( I2TEMP(2))
C
             I4TEMP = LOGBUF(10)
             TRABUF(TWTAMT1) = ZEXT( I2TEMP(1) )
             TRABUF(TWTKC) = ZEXT( I1TEMP(3) )
C
             I4TEMP = LOGBUF(16)
             TRABUF(TWCTER) = ZEXT(I2TEMP(1))
             X = ZEXT( I1TEMP(3) )
             TRABUF(TWTSEL1) = IAND(X,15)
             TRABUF(TWNBET) = ISHFT(X,-4)
C
             LOGOFF = 11                                !FIRST BOARD ONLY
             DO 500 J = 0,TRABUF(TWTSEL1)-1,2           !TWO SELECTIONS AT TIME
                TRAIND = TWTBLEN*J
                I4TEMP = LOGBUF(LOGOFF)
                TRABUF(TWTROW1+TRAIND) = ZEXT( I1TEMP(1) )
                X = ZEXT( I1TEMP(2) )
                TRABUF(TWTPOL1+TRAIND) = IAND(X,15)
                TRABUF(TWTSTS1+TRAIND) = ISHFT(X,-4)
C
                TRAIND = TWTBLEN*(J+1)
                TRABUF(TWTROW1+TRAIND) = ZEXT( I1TEMP(3) )
                X = ZEXT( I1TEMP(4) )
                TRABUF(TWTPOL1+TRAIND) = IAND(X,15)
                TRABUF(TWTSTS1+TRAIND) = ISHFT(X,-4)
C
                LOGOFF = LOGOFF + 1
C
500          CONTINUE
C
             TRABUF(TWAMT) = LOGBUF(14)
C
             I4TEMP = LOGBUF(15)
             TRABUF(TWVSTS) = ZEXT(I1TEMP(1))
             TRABUF(TWSYSN) = ZEXT(I1TEMP(2))
             X = ZEXT( I1TEMP(3) )
             IF(IAND(X,'80'X).NE.0) TRABUF(TWQPF) = 1
             IF(IAND(X,'08'X).NE.0) TRABUF(TWFFLG) = 1
             TRABUF(TWSYST) = ZEXT(I1TEMP(4))
C
C SET CALCULATED FIELDS FOR BOARD 1
C
             TRABUF(TWDUR) = 1
             TRABUF(TWEND) = TRABUF(TWBEG)
             TRABUF(TWTOT) = TRABUF(TWAMT) + TRABUF(TWTKC)
C
             IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
C TOTO SELECT CONTINUATION RECORD 1
C
             DO 1000 I = 1,TRABUF(TWNBET)-1               !FOR BOARDS 2 AND UP
                BETIND = (I*TWTBLEN*TWTRMAX) + (I*TWTHLEN)
                BRDOFF = I * 5
                TRABUF(TWTAMT1+BETIND) = LOGBUF(12+BRDOFF)
C
                I4TEMP = LOGBUF(16+BRDOFF)
                TRABUF(TWTSEL1+BETIND) = ZEXT( I1TEMP(1) )
                TRABUF(TWSYST) =         ZEXT( I1TEMP(2) )
                TRABUF(TWSYSN) =         ZEXT( I1TEMP(3) )
C
                LOGOFF = BRDOFF
                DO 1010 J = 0,TRABUF(TWTSEL1+BETIND)-1,2 !TWO SELECTIONS AT TIME
                   TRAIND = BETIND + TWTBLEN*J
                   I4TEMP = LOGBUF(13+LOGOFF)
                   TRABUF(TWTROW1+TRAIND) = ZEXT( I1TEMP(1) )
                   X = ZEXT(I1TEMP(2))
                   TRABUF(TWTPOL1+TRAIND) = IAND(X,15)
                   TRABUF(TWTSTS1+TRAIND) = ISHFT(X,-4)
C
                   TRAIND = BETIND + TWTBLEN*(J+1)
                   TRABUF(TWTROW1+TRAIND) = ZEXT(I1TEMP(3))
                   X = ZEXT(I1TEMP(4))
                   TRABUF(TWTPOL1+TRAIND) = IAND(X,15)
                   TRABUF(TWTSTS1+TRAIND) = ISHFT(X,-4)
C
                   LOGOFF = LOGOFF + 1
1010            CONTINUE
1000         CONTINUE
C
             TRABUF(TWBNKNM) = LOGBUF(31)
C
             I4TEMP = LOGBUF(32) 
             I1TEMP(4) = 0
             TRABUF(TWBNKID) = I4TEMP 
C
C SET CALCULATED FIELDS
C
             TRABUF(TWTOT) = TRABUF(TWAMT) + TRABUF(TWTKC)
C
             GOTO 9000
          ENDIF
C
C DECODE BINGO WAGER BODY
C
           IF(TRABUF(TGAMTYP).EQ.TBNG) THEN
C
              TRABUF(TWCSER)   = LOGBUF(8)
C
              TRABUF(TWAMT)    = LOGBUF(9)
C
              I4TEMP           = LOGBUF(10)
              TRABUF(TWBEG)    = ZEXT( I2TEMP(1) )
              TRABUF(TWCTER)   = ZEXT( I2TEMP(2) )
C
              I4TEMP           = LOGBUF(11)
              TRABUF(TWQPF)    = ZEXT( I2TEMP(1) )
              TRABUF(TWSIMP)   = ZEXT( I2TEMP(2) )
C
              I4TEMP           = LOGBUF(12)
              TRABUF(TWSYSN)   = ZEXT( I1TEMP(1) )
              TRABUF(TWVSTS)   = ZEXT( I1TEMP(2) )
              TRABUF(TWDUR)    = ZEXT( I1TEMP(3) )
              TRABUF(TWSYST)   = ZEXT( I1TEMP(4) )
C
              I4TEMP           = LOGBUF(13)
              TRABUF(TWNMRK)   = ZEXT( I1TEMP(1) )
              X                = ZEXT( I1TEMP(2) )
              IF(IAND(X,'80'X).NE.0) TRABUF(TWMFLG) = 1
              IF(IAND(X,'20'X).NE.0) TRABUF(TWKFLG) = 1
              IF(IAND(X,'10'X).NE.0) TRABUF(TWKFLG2) = 1
              IF(IAND(X,'08'X).NE.0) TRABUF(TWFFLG) = 1
              TRABUF(TWCDCOFF) = ZEXT( I1TEMP(3) )
              TRABUF(TWMFRAC)  = ZEXT( I1TEMP(4) )
C
              TRABUF(TWBNKID)  = LOGBUF(14)
C
              TRABUF(TWBNKNM)  = LOGBUF(15)
C
              I4TEMP = LOGBUF(16)
              TRABUF(TWSRW)    = ZEXT( I1TEMP(1) )
              TRABUF(TWNBET)   = ZEXT( I1TEMP(2) )
              TRABUF(TWTKC)    = ZEXT( I1TEMP(3) )
C
C
C BEGIN CONTINUATION RECORD 1
C
              DO I=1,3
                 DO J=1,5
                    X=(I-1)*5+(J-1)
                    IF(MOD(X,2).EQ.0) THEN
                       I4TEMP = LOGBUF(17+X/2)                  ! 17...24
                       TRABUF(TWBBFH1+X)   = ZEXT( I2TEMP(1) )
                       TRABUF(TWBBFH1+X+1) = ZEXT( I2TEMP(2) )
                    ENDIF
                 ENDDO
              ENDDO

              TRABUF(TWBSED) = LOGBUF(25)
              TRABUF(TWBLUK) = LOGBUF(26)
C
              I4TEMP = LOGBUF(27)
              TRABUF(TWBBAS) = ZEXT( I2TEMP(1) )
C
C             *** NOW SET CALCULATED FIELDS ***
C
              TRABUF(TWEND) = TRABUF(TWBEG)+TRABUF(TWDUR)-1
              TRABUF(TWTOT) = (TRABUF(TWDUR) * TRABUF(TWAMT))+
     *                        TRABUF(TWTKC)
C
              GOTO 9000
          ENDIF
C
C TODAY'S TRIO WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TTRP) THEN
C
             TRABUF(TWCSER)  = LOGBUF(8)
C
             I4TEMP = LOGBUF(9)
             TRABUF(TWDAMT)  = ZEXT( I2TEMP(1))
             TRABUF(TWBEG )  = ZEXT( I2TEMP(2))
C
             TRABUF(TWTTAMT) = IAND(LOGBUF(10),'00FFFFFF'X)
             TRABUF(TWSYST)  = IAND(ISHFT(LOGBUF(10),-24),'000000FF'X)

             I4TEMP = LOGBUF(11)
             TRABUF(TWTTMA)  = ZEXT(I1TEMP(1))
             TRABUF(TWTTMB)  = ZEXT(I1TEMP(2))
             TRABUF(TWTTMC)  = ZEXT(I1TEMP(3))
             TRABUF(TWTTBET) = ZEXT(I1TEMP(4)) 
                     
             I4TEMP = LOGBUF(12)
             TRABUF(TWTTBET+1) = ZEXT(I1TEMP(1))
             TRABUF(TWTTBET+2) = ZEXT(I1TEMP(2))
             TRABUF(TWCTER)    = ZEXT(I2TEMP(2))

             TRABUF(TWBNKID)   = IAND(LOGBUF(13),'00FFFFFF'X)
             TRABUF(TWVSTS)    = IAND(ISHFT(LOGBUF(13),-24),'000000FF'X)

             I4TEMP = LOGBUF(14)
             TRABUF(TWBNKNM) = I4TEMP

             I4TEMP = LOGBUF(15)
             TRABUF(TWSYSN)  = ZEXT( I2TEMP(1))

             I4TEMP = LOGBUF(16)
             X              = ZEXT( I1TEMP(1) )
             IF(IAND(X,'80'X).NE.0) TRABUF(TWQPF)  = 1
             IF(IAND(X,'08'X).NE.0) TRABUF(TWFFLG) = 1
             TRABUF(TWTKC)  = ZEXT( I1TEMP(2) )
             TRABUF(TWTTCOUPID) = ZEXT( I1TEMP(3) )
C
C SET CALCULATED FIELDS
C
             TRABUF(TWAMT) = TRABUF(TWTTAMT) 
             IF(TRABUF(TWSYSN).GT.0) 
     *          TRABUF(TWAMT) = TRABUF(TWTTAMT)*TRABUF(TWSYSN)
             TRABUF(TWEND) = TRABUF(TWBEG)
             TRABUF(TWDUR) = 1
             TRABUF(TWTOT) = (TRABUF(TWAMT) * TRABUF(TWDUR)) + 
     *                        TRABUF(TWTKC)
C
             TRABUF(TWNBET) = 1  
             IF(TRABUF(TWSYST).EQ.NOSYS) GOTO 9000
C
             I4TEMP = LOGBUF(17)
             TRABUF(TWTTBET+3)  = ZEXT(I1TEMP(1))
             TRABUF(TWTTBET+4)  = ZEXT(I1TEMP(2))
             TRABUF(TWTTBET+5)  = ZEXT(I1TEMP(3))
             TRABUF(TWTTBET+6) = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(18)
             TRABUF(TWTTBET+7)  = ZEXT(I1TEMP(1))
             TRABUF(TWTTBET+8)  = ZEXT(I1TEMP(2))
             TRABUF(TWTTBET+9)  = ZEXT(I1TEMP(3))
             TRABUF(TWTTBET+10) = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(19)
             TRABUF(TWTTBET+11) = ZEXT(I1TEMP(1))
             TRABUF(TWTTBET+12) = ZEXT(I1TEMP(2))
             TRABUF(TWTTBET+13) = ZEXT(I1TEMP(3))
             TRABUF(TWTTBET+14) = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(20)
             TRABUF(TWTTBET+15) = ZEXT(I1TEMP(1))
             TRABUF(TWTTBET+16) = ZEXT(I1TEMP(2))
             TRABUF(TWTTBET+17) = ZEXT(I1TEMP(3))
             TRABUF(TWTTBET+18) = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(21)
             TRABUF(TWTTBET+19) = ZEXT(I1TEMP(1))
             TRABUF(TWTTBET+20) = ZEXT(I1TEMP(2))
             TRABUF(TWTTBET+21) = ZEXT(I1TEMP(3))
             TRABUF(TWTTBET+22) = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(22)
             TRABUF(TWTTBET+23) = ZEXT(I1TEMP(1))
             TRABUF(TWTTBET+24) = ZEXT(I1TEMP(2))
             TRABUF(TWTTBET+25) = ZEXT(I1TEMP(3))
             TRABUF(TWTTBET+26) = ZEXT(I1TEMP(4))

C
             GOTO 9000
          ENDIF
C
C SUPERSCORE WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TSSC) THEN
C
             TRABUF(TWCSER)  = LOGBUF(8)
C
             I4TEMP = LOGBUF(9)
             TRABUF(TWDAMT)  = ZEXT( I2TEMP(1))
             TRABUF(TWBEG )  = ZEXT( I2TEMP(2))
C
             TRABUF(TWSSAMT) = IAND(LOGBUF(10),'00FFFFFF'X)
             TRABUF(TWSYST)  = IAND(ISHFT(LOGBUF(10),-24),'000000FF'X)

             I4TEMP = LOGBUF(11)
             X               = ZEXT(I1TEMP(1))
             TRABUF(TWSSHM1) = ISHFT( X, -4 ) 
             TRABUF(TWSSAW1) = IAND ( X,'0F'X )
             X               = ZEXT(I1TEMP(2))
             TRABUF(TWSSHM2) = ISHFT( X, -4 )
             TRABUF(TWSSAW2) = IAND ( X,'0F'X )
             X               = ZEXT(I1TEMP(3))
             TRABUF(TWSSHM3) = ISHFT( X, -4 )
             TRABUF(TWSSAW3) = IAND ( X,'0F'X )
             TRABUF(TWSSBET) = ZEXT(I1TEMP(4)) 
                     
             I4TEMP = LOGBUF(12)
             TRABUF(TWSSBET+1) = ZEXT(I1TEMP(1))
             TRABUF(TWSSBET+2) = ZEXT(I1TEMP(2))
             TRABUF(TWSSBET+3) = ZEXT(I1TEMP(3))
             TRABUF(TWSSBET+4) = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(13)
             TRABUF(TWSSBET+5) = ZEXT(I1TEMP(1))
             TRABUF(TWVSTS)    = ZEXT(I1TEMP(2))
             TRABUF(TWCTER)    = ZEXT(I2TEMP(2))

             I4TEMP = LOGBUF(14)
             I1TEMP(4) = 0
             TRABUF(TWBNKID) = I4TEMP

             I4TEMP = LOGBUF(15)
             TRABUF(TWBNKNM) = I4TEMP

             I4TEMP=LOGBUF(16)
             X              = ZEXT( I1TEMP(1) )
             IF(IAND(X,'80'X).NE.0) TRABUF(TWQPF)  = 1
             IF(IAND(X,'08'X).NE.0) TRABUF(TWFFLG) = 1
             TRABUF(TWTKC)  = ZEXT( I1TEMP(2) )
             TRABUF(TWSSCOUPID) = ZEXT( I1TEMP(3) )
C
C SET CALCULATED FIELDS
C
             TRABUF(TWAMT) = TRABUF(TWSSAMT)  !if system then see below also 
             TRABUF(TWEND) = TRABUF(TWBEG)
             TRABUF(TWDUR) = 1
             TRABUF(TWTOT) = (TRABUF(TWAMT) * TRABUF(TWDUR)) + 
     *                        TRABUF(TWTKC)
C
             TRABUF(TWNBET) = 1  
             TRABUF(TWSYSN) = 1  
             IF(TRABUF(TWSYST).EQ.NOSYS) GOTO 9000
C
             I4TEMP = LOGBUF(17)
             TRABUF(TWSYSN) = I4TEMP
             TRABUF(TWAMT) = TRABUF(TWSSAMT)*TRABUF(TWSYSN)
             TRABUF(TWTOT) = (TRABUF(TWAMT) * TRABUF(TWDUR)) + 
     *                        TRABUF(TWTKC)

             I4TEMP = LOGBUF(18)
             TRABUF(TWSSBET+6) = ZEXT(I1TEMP(1))
             TRABUF(TWSSBET+7) = ZEXT(I1TEMP(2))
             TRABUF(TWSSBET+8) = ZEXT(I1TEMP(3))
             TRABUF(TWSSBET+9) = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(19)
             TRABUF(TWSSBET+10) = ZEXT(I1TEMP(1))
             TRABUF(TWSSBET+11) = ZEXT(I1TEMP(2))
             TRABUF(TWSSBET+12) = ZEXT(I1TEMP(3))
             TRABUF(TWSSBET+13) = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(20)
             TRABUF(TWSSBET+14) = ZEXT(I1TEMP(1))
             TRABUF(TWSSBET+15) = ZEXT(I1TEMP(2))
             TRABUF(TWSSBET+16) = ZEXT(I1TEMP(3))
             TRABUF(TWSSBET+17) = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(21)
             TRABUF(TWSSBET+18) = ZEXT(I1TEMP(1))
             TRABUF(TWSSBET+19) = ZEXT(I1TEMP(2))
             TRABUF(TWSSBET+20) = ZEXT(I1TEMP(3))
             TRABUF(TWSSBET+21) = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(22)
             TRABUF(TWSSBET+22) = ZEXT(I1TEMP(1))
             TRABUF(TWSSBET+23) = ZEXT(I1TEMP(2))
             TRABUF(TWSSBET+24) = ZEXT(I1TEMP(3))
             TRABUF(TWSSBET+25) = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(23)
             TRABUF(TWSSBET+26) = ZEXT(I1TEMP(1))
             TRABUF(TWSSBET+27) = ZEXT(I1TEMP(2))
             TRABUF(TWSSBET+28) = ZEXT(I1TEMP(3))
             TRABUF(TWSSBET+29) = ZEXT(I1TEMP(4))
C
             GOTO 9000
          ENDIF
C
C SUPER TRIPLE WAGER BODY
C
          IF(TRABUF(TGAMTYP).EQ.TSTR) THEN
C
             TRABUF(TWCSER)  = LOGBUF(8)
C
             I4TEMP = LOGBUF(9)
             TRABUF(TWDAMT)  = ZEXT( I2TEMP(1))
             TRABUF(TWBEG )  = ZEXT( I2TEMP(2))
C
             TRABUF(TWSTAMT) = IAND(LOGBUF(10),'00FFFFFF'X)
             TRABUF(TWSYST)  = IAND(ISHFT(LOGBUF(10),-24),'000000FF'X)

             I4TEMP = LOGBUF(11)
             TRABUF(TWSTM1)  = ZEXT(I1TEMP(1))
             TRABUF(TWSTM2)  = ZEXT(I1TEMP(2))
             TRABUF(TWSTM3)  = ZEXT(I1TEMP(3))
             TRABUF(TWSTBET) = ZEXT(I1TEMP(4)) 
                     
             I4TEMP = LOGBUF(12)
             TRABUF(TWSTBET+1) = ZEXT(I1TEMP(1))
             TRABUF(TWSTBET+2) = ZEXT(I1TEMP(2))
             TRABUF(TWCTER)    = ZEXT(I2TEMP(2))

             TRABUF(TWBNKID)   = IAND(LOGBUF(13),'00FFFFFF'X)
             TRABUF(TWVSTS)    = IAND(ISHFT(LOGBUF(13),-24),'000000FF'X)

             I4TEMP = LOGBUF(14)
             TRABUF(TWBNKNM) = I4TEMP

             I4TEMP = LOGBUF(15)
             TRABUF(TWSYSN)  = ZEXT( I2TEMP(1))

             I4TEMP = LOGBUF(16)
             X              = ZEXT( I1TEMP(1) )
             IF(IAND(X,'80'X).NE.0) TRABUF(TWQPF)  = 1
             IF(IAND(X,'08'X).NE.0) TRABUF(TWFFLG) = 1
             TRABUF(TWTKC)  = ZEXT( I1TEMP(2) )
             TRABUF(TWSTCOUPID) = ZEXT( I1TEMP(3) )
C
C SET CALCULATED FIELDS
C
             TRABUF(TWAMT) = TRABUF(TWSTAMT) 
             IF(TRABUF(TWSYSN).GT.0) 
     *          TRABUF(TWAMT) = TRABUF(TWSTAMT)*TRABUF(TWSYSN)
             TRABUF(TWEND) = TRABUF(TWBEG)
             TRABUF(TWDUR) = 1
             TRABUF(TWTOT) = (TRABUF(TWAMT) * TRABUF(TWDUR)) + 
     *                        TRABUF(TWTKC)
C
             TRABUF(TWNBET) = 1  
             IF(TRABUF(TWSYST).EQ.NOSYS) GOTO 9000
C
             I4TEMP = LOGBUF(17)
             TRABUF(TWSTBET+3)  = ZEXT(I1TEMP(1))
             TRABUF(TWSTBET+4)  = ZEXT(I1TEMP(2))
             TRABUF(TWSTBET+5)  = ZEXT(I1TEMP(3))
             TRABUF(TWSTBET+6)  = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(18)
             TRABUF(TWSTBET+7)  = ZEXT(I1TEMP(1))
             TRABUF(TWSTBET+8)  = ZEXT(I1TEMP(2))
             TRABUF(TWSTBET+9)  = ZEXT(I1TEMP(3))
             TRABUF(TWSTBET+10) = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(19)
             TRABUF(TWSTBET+11) = ZEXT(I1TEMP(1))
             TRABUF(TWSTBET+12) = ZEXT(I1TEMP(2))
             TRABUF(TWSTBET+13) = ZEXT(I1TEMP(3))
             TRABUF(TWSTBET+14) = ZEXT(I1TEMP(4))

             I4TEMP = LOGBUF(20)
             TRABUF(TWSTBET+15) = ZEXT(I1TEMP(1))
             TRABUF(TWSTBET+16) = ZEXT(I1TEMP(2))
             TRABUF(TWSTBET+17) = ZEXT(I1TEMP(3))
C
             GOTO 9000
          ENDIF
C    
C ANY OTHER TYPE OF WAGER, JUST RETURN
C
          GOTO 9000
C
        ENDIF
C
C DECODE VALIDATION BODY
C 
        IF( (TRABUF(TTYP).EQ.TVAL.OR.TRABUF(TTYP).EQ.TREF) .AND.
     *       TRABUF(TGAMTYP).NE.TPAS .OR.
     *       TRABUF(TTYP).EQ.TVAL.AND.
     *       TRABUF(TGAMTYP).EQ.TPAS.AND.TRABUF(TVEPVAL).NE.0 ) THEN     
 
           TRABUF(TVSER)  = LOGBUF(8)
           TRABUF(TVEXC)  = LOGBUF(9)
           TRABUF(TVPAY)  = LOGBUF(10)
           TRABUF(TVKPAY) = LOGBUF(11)
	   I4TEMP         = LOGBUF(12)
           TRABUF(TVREF)  = IAND(I4TEMP,'00FFFFFF'X)
           ! here in I1TEMP(4) is TRABUF(TVEPVAL)
C
           I4TEMP         = LOGBUF(13)
           TRABUF(TVCDC)  = ZEXT( I2TEMP(1) )
           TRABUF(TVWCDC) = ZEXT( I2TEMP(2) )
C
           I4TEMP         = LOGBUF(14)
           TRABUF(TVWKCDC)= ZEXT( I2TEMP(1) )
C
           I4TEMP        = LOGBUF(15)
           TRABUF(TVSTER) = ZEXT( I2TEMP(1) )
           TRABUF(TVCWT)  = ZEXT( I1TEMP(3) )
           TRABUF(TVTYPE) = ZEXT( I1TEMP(4) )
C 
           I4TEMP         = LOGBUF(16)
           TRABUF(TVCODE) = ZEXT( I1TEMP(1) )
           TRABUF(TVKGME) = ZEXT( I1TEMP(2) )
           TRABUF(TVSTS)  = ZEXT( I1TEMP(3) )
C
           IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
C VALIDATION CONTINUATION RECORD 1
C
           TRABUF(TVBNKID) = LOGBUF(17)
C
           TRABUF(TVBNKNUM) = LOGBUF(18)
C
	   TRABUF(TVOPPAY)  = LOGBUF(19)   !V39
	   TRABUF(TVKOPPAY) = LOGBUF(20)   !V39

C----+------------------------------------------------------------------
C V57| Adding new validation messages fields
C----+------------------------------------------------------------------
           IF(TRABUF(TGAMTYP).NE.TPAS) THEN
                TRABUF(TVPLCARD) = LOGBUF(21)  
        
                I4TEMP          =  LOGBUF(22)
                TRABUF(TVNIBBB) = ZEXT( I2TEMP(1) )
                TRABUF(TVNIBBO) = ZEXT( I2TEMP(2) )
        
                TRABUF(TVNIBBA1) =  LOGBUF(23)           
        
                I4TEMP           =  LOGBUF(24)
                TRABUF(TVNIBBA2) = ZEXT( I1TEMP(1) )  
                TRABUF(TVNIBCD)  = ZEXT( I1TEMP(2) )
                TRABUF(TVPLIDTYP) = ZEXT( I1TEMP(3) ) !V55
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------                   
                TRABUF(TVOLMSERL_TLTO) = LOGBUF(25)
                TRABUF(TVOLMSERM_TLTO) = LOGBUF(26)
                TRABUF(TVOLMMIDL_TLTO) = LOGBUF(28)
                I4TEMP = LOGBUF(27)
                TRABUF(TVOLMSERH_TLTO) = I1TEMP(1)
                TRABUF(TVOLMMIDH_TLTO) = I1TEMP(2)
                TRABUF(TVOLMCOMF_TLTO) = I1TEMP(3)                
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------                 
                GOTO 9000
           ENDIF  
C----+------------------------------------------------------------------
C V57| Adding new validation messages fields
C----+------------------------------------------------------------------

	   I4TEMP         =  LOGBUF(21)
	   TRABUF(TPTCK)  =  ISHFT (I1TEMP(4), -4)
           TRABUF(TPNUM1) =  IAND(I4TEMP,'00FFFFFF'X)

	   TRABUF(TPKEY1) =  LOGBUF(22)
	   TRABUF(TPPAY1) =  LOGBUF(23)

           I4TEMP         =  LOGBUF(24)
	   TRABUF(TPEMIS1)=  ZEXT( I2TEMP(1) )	  
	   TRABUF(TPSER1) =  ZEXT( I1TEMP(3) )	  
	   TRABUF(TPTEN1) =  ZEXT( I1TEMP(4) )	  
 
           I4TEMP         =  LOGBUF(25)
           TRABUF(TPSTS1) =  ZEXT( I1TEMP(1) )
           TRABUF(TVEPTYP)  = ZEXT( I1TEMP(2) )
           TRABUF(TPOFFTER) = ZEXT( I2TEMP(2) )

           I4TEMP         =  LOGBUF(26)
           TRABUF(TVEPWK) =  ZEXT( I1TEMP(1) ) 
           TRABUF(TVEPYR) =  ZEXT( I1TEMP(2) ) 
           TRABUF(TVPLIDTYP) = ZEXT( I1TEMP(3) ) !V55

           TRABUF(TVPLCARD) = LOGBUF(27)  

           I4TEMP          =  LOGBUF(28)
           TRABUF(TVNIBBB) = ZEXT( I2TEMP(1) )
           TRABUF(TVNIBBO) = ZEXT( I2TEMP(2) )

           TRABUF(TVNIBBA1) =  LOGBUF(29)           

           I4TEMP           =  LOGBUF(30)
           TRABUF(TVNIBBA2) = ZEXT( I1TEMP(1) )  
           TRABUF(TVNIBCD)  = ZEXT( I1TEMP(2) )

           GOTO 9000
        ENDIF
C
C PASSIVE LOTTERY "BUNCH" VALIDATION
C
	IF(TRABUF(TTYP).EQ.TVAL .AND. TRABUF(TGAMTYP).EQ.TPAS.AND.
     *    TRABUF(TVEPVAL).EQ.0) THEN

          TRABUF(TVEPTYP) = 0  

	  I4TEMP         =  LOGBUF(8)
	  TRABUF(TPTCK)  =  ISHFT (I1TEMP(4), -4)
          TRABUF(TPNUM1) =  IAND(I4TEMP,'00FFFFFF'X)

	  TRABUF(TPKEY1) =  LOGBUF(9)
	  TRABUF(TPPAY1) =  LOGBUF(10)

          I4TEMP         =  LOGBUF(11)
	  TRABUF(TPEMIS1)=  ZEXT( I2TEMP(1) )	  
	  TRABUF(TPSER1) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN1) =  ZEXT( I1TEMP(4) )	  

	  I4TEMP         =  LOGBUF(12)
          TRABUF(TPNUM2) =  IAND(I4TEMP,'00FFFFFF'X)
          ! here in I1TEMP(4) is TRABUF(TVEPVAL)

	  TRABUF(TPKEY2) =  LOGBUF(13)
	  TRABUF(TPPAY2) =  LOGBUF(14)

          I4TEMP         =  LOGBUF(15)
	  TRABUF(TPEMIS2)=  ZEXT( I2TEMP(1) )	  
	  TRABUF(TPSER2) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN2) =  ZEXT( I1TEMP(4) )	  

	  I4TEMP         =  LOGBUF(16)
	  TRABUF(TPOFFTER)= ZEXT( I2TEMP(1) )
	  X              =  ZEXT( I1TEMP(3) )
	  TRABUF(TPSTS1) =  IAND (X , '0F'X)
	  TRABUF(TPSTS2) =  ISHFT(X, -4)

          IF(TRABUF(TSIZE).EQ.1) GOTO 9000
C
C  *** PASSIVE CONTINUATION RECORD 1 ***
C
	  TRABUF(TPKEY3) =  LOGBUF(17)	  
	  TRABUF(TPPAY3) =  LOGBUF(18)

	  I4TEMP         =  LOGBUF(19)
	  TRABUF(TPEMIS3)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER3) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN3) =  ZEXT( I1TEMP(4) )	  

	  TRABUF(TPKEY4) =  LOGBUF(20)	  
	  TRABUF(TPPAY4) =  LOGBUF(21)	  

	  I4TEMP         =  LOGBUF(22)
	  TRABUF(TPEMIS4)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER4) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN4) =  ZEXT( I1TEMP(4) )	  

	  TRABUF(TPKEY5) =  LOGBUF(23)	  
	  TRABUF(TPPAY5) =  LOGBUF(24)	  

	  I4TEMP         =  LOGBUF(25)
	  TRABUF(TPEMIS5)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER5) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN5) =  ZEXT( I1TEMP(4) )	  

	  TRABUF(TPKEY6) =  LOGBUF(26)	  
	  TRABUF(TPPAY6) =  LOGBUF(27)	  

	  I4TEMP         =  LOGBUF(28)
	  TRABUF(TPEMIS6)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER6) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN6) =  ZEXT( I1TEMP(4) )	  

	  I4TEMP         =  LOGBUF(29)
          TRABUF(TPNUM3) =  IAND(I4TEMP,'00FFFFFF'X)
	  X              =  ZEXT( I1TEMP(4) )
	  TRABUF(TPSTS3) =  IAND (X , '0F'X)
	  TRABUF(TPSTS4) =  ISHFT(X, -4)

	  I4TEMP         =  LOGBUF(30)
          TRABUF(TPNUM4) =  IAND(I4TEMP,'00FFFFFF'X)
	  X              =  ZEXT( I1TEMP(4) )
	  TRABUF(TPSTS5) =  IAND (X , '0F'X)
	  TRABUF(TPSTS6) =  ISHFT(X, -4)

	  I4TEMP         =  LOGBUF(31)
          TRABUF(TPNUM5) =  IAND(I4TEMP,'00FFFFFF'X)

	  I4TEMP         =  LOGBUF(32)
          TRABUF(TPNUM6) =  IAND(I4TEMP,'00FFFFFF'X)

          IF(TRABUF(TSIZE).EQ.2) GOTO 9000
C
C  *** PASSIVE CONTINUATION RECORD 2 ***
C
	  TRABUF(TPKEY7) =  LOGBUF(33)	  
	  TRABUF(TPPAY7) =  LOGBUF(34)

	  I4TEMP         =  LOGBUF(35)
	  TRABUF(TPEMIS7)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER7) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN7) =  ZEXT( I1TEMP(4) )	  

	  TRABUF(TPKEY8) =  LOGBUF(36)	  
	  TRABUF(TPPAY8) =  LOGBUF(37)	  

	  I4TEMP         =  LOGBUF(38)
	  TRABUF(TPEMIS8)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER8) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN8) =  ZEXT( I1TEMP(4) )	  

	  TRABUF(TPKEY9) =  LOGBUF(39)	  
	  TRABUF(TPPAY9) =  LOGBUF(40)	  

	  I4TEMP         =  LOGBUF(41)
	  TRABUF(TPEMIS9)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER9) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN9) =  ZEXT( I1TEMP(4) )	  

	  TRABUF(TPKEY10) =  LOGBUF(42)	  
	  TRABUF(TPPAY10) =  LOGBUF(43)

	  I4TEMP          =  LOGBUF(44)
	  TRABUF(TPEMIS10)=  ZEXT( I2TEMP(1) )
	  TRABUF(TPSER10) =  ZEXT( I1TEMP(3) )	  
	  TRABUF(TPTEN10) =  ZEXT( I1TEMP(4) )	  

	  I4TEMP          =  LOGBUF(45)
          TRABUF(TPNUM7)  =  IAND(I4TEMP,'00FFFFFF'X)
	  X               =  ZEXT( I1TEMP(4) )
	  TRABUF(TPSTS7)  =  IAND (X , '0F'X)
	  TRABUF(TPSTS8)  =  ISHFT(X, -4)

	  I4TEMP          =  LOGBUF(46)
          TRABUF(TPNUM8)  =  IAND(I4TEMP,'00FFFFFF'X)
	  X               =  ZEXT( I1TEMP(4) )
	  TRABUF(TPSTS9)  =  IAND (X , '0F'X)
	  TRABUF(TPSTS10) =  ISHFT(X, -4)

	  I4TEMP          =  LOGBUF(47)
          TRABUF(TPNUM9)  =  IAND(I4TEMP,'00FFFFFF'X)

	  I4TEMP          =  LOGBUF(48)
          TRABUF(TPNUM10) =  IAND(I4TEMP,'00FFFFFF'X)

	  GOTO 9000
	ENDIF
C
C DECODE INSTANTS
C
        IF(TRABUF(TTYP).EQ.TCRS) THEN
          I4TEMP         = LOGBUF(16)
          TRABUF(TITYP)  = ZEXT( I1TEMP(1) )
          TRABUF(TIERR)  = ZEXT( I1TEMP(2) )
C
          TRABUF(TIXRF)  = LOGBUF(7)
C
          IF(TRABUF(TITYP).EQ.IVAL) THEN
            I4TEMP            = LOGBUF(16)
            X                 = ZEXT( I1TEMP(3) )
            TRABUF(TIIND)     = ISHFT( X, -4 )
            TRABUF(TIBCH)     = IAND ( X,'0F'X )
C
            I4TEMP            = LOGBUF(8)
!-------->>V56 -------------------------------------------------------------------
            TRABUF(TIVMT)    = ZEXT( I1TEMP(1) )
            TRABUF(TIVALM)    = ZEXT( I1TEMP(2) )
!            TRABUF(TIVALM)    = ZEXT( I2TEMP(1) )
!-------- V56<<-------------------------------------------------------------------
            TRABUF(TIVALT)    = ZEXT( I2TEMP(2) )
C

C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------            
C New Terminal Project TIVENV only uses 2 digits = 1 byte (99<256)
C           TRABUF(TIVENV)    = LOGBUF(9)
            I4TEMP =  LOGBUF(9)
            IF(ISHFT(I1TEMP(1), -7) .EQ. 1) THEN
                TRABUF(TVOLMSERL_IL) =  ISHFT(I4TEMP, -8)
                TRABUF(TVOLMCOMF_IL) =  ISHFT(ZEXT(I1TEMP(1)), -7) 
                TRABUF(TIVENV) = IAND(I1TEMP(1),'7F'X)
            ELSE
                TRABUF(TIVENV) = IAND(I1TEMP(1),'7F'X) 
            ENDIF
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------    
C
            I4TEMP = LOGBUF(10)
            TRABUF(TIVTYP) = ZEXT(I1TEMP(4))
            I1TEMP(4) = 0
            TRABUF(TIVAGT)    = I4TEMP

            IF(TRABUF(TIVMT) .EQ. IRVMT) THEN !OLD LAYOUT
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------     
              IF(TRABUF(TVOLMCOMF_IL) .EQ. 1) THEN      
                IF(TRABUF(TIBCH) .GE. 4) THEN           
                   I4TEMP = LOGBUF(32)
                   TRABUF(TVOLMSERL_IL) = IOR( TRABUF(TVOLMSERL_IL), ISHFT( IAND( I4TEMP,'FF'X), 24) )
                   TRABUF(TVOLMSERM_IL) = ISHFT( IAND(I4TEMP,'FFFFFF'X), -8)
                   I4TEMP = LOGBUF(48)
                   TRABUF(TVOLMSERM_IL) = IOR( ISHFT(IAND( I4TEMP,'FFFF'X), 16), TRABUF(TVOLMSERM_IL) )
                   TRABUF(TVOLMSERH_IL) = ZEXT( I1TEMP(3) )
                ELSEIF(TRABUF(TIBCH) .GT. 1) THEN                            
                   I4TEMP = LOGBUF(32)
                   TRABUF(TVOLMSERL_IL) = IOR( TRABUF(TVOLMSERL_IL), ISHFT( IAND( I4TEMP,'FF'X), 24) )
                   TRABUF(TVOLMSERM_IL) = ISHFT( IAND(I4TEMP,'FFFFFF'X), -8)
                   I4TEMP = LOGBUF(31)
                   TRABUF(TVOLMSERM_IL) = IOR( ISHFT(IAND( I4TEMP,'FFFF'X), 16), TRABUF(TVOLMSERM_IL) )
                   TRABUF(TVOLMSERH_IL) = ZEXT( I1TEMP(3) )                                 
                ELSE                          
                   I4TEMP = LOGBUF(17)  
                   TRABUF(TVOLMSERL_IL) = IOR( TRABUF(TVOLMSERL_IL), ISHFT( IAND( I4TEMP,'FF'X), 24) )
                   TRABUF(TVOLMSERM_IL) = ISHFT(I4TEMP, -8)
                   I4TEMP = LOGBUF(18)
                   TRABUF(TVOLMSERM_IL) = IOR( ISHFT(IAND( I4TEMP,'FF'X), 24), TRABUF(TVOLMSERM_IL) )
                   TRABUF(TVOLMSERH_IL) = ZEXT( I1TEMP(2) )                                  
                ENDIF     
              ENDIF
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------                   
C
              BUFIDX = 11
C
              DO 10001 I = 0,TRABUF(TIBCH)-1
                I4TEMP = LOGBUF(BUFIDX)
                TRABUF(TIPCK1+I)  = IAND(I4TEMP,'00FFFFFF'X)
                TRABUF(TISTS1+I)  = ZEXT(I1TEMP(4))
                BUFIDX = BUFIDX + 1
C
                TRABUF(TIVRN1+I)  = LOGBUF(BUFIDX)
                BUFIDX = BUFIDX + 1
C
                I4TEMP=LOGBUF(BUFIDX)
                TRABUF(TILTX1+I)  = ZEXT( I2TEMP(1) )
                X = ZEXT( I2TEMP(2) )
                TRABUF(TIPCKSTS1+I) = IAND(ISHFT(X,-12),'0F'X)
                TRABUF(TIGAM1+I)    = IAND(X,'0FFF'X)
                BUFIDX = BUFIDX + 1
C
                I4TEMP=LOGBUF(BUFIDX)
                TRABUF(TITIM1+I)  = ZEXT( I2TEMP(1) )*2
                TRABUF(TICDC1+I)  = ZEXT( I2TEMP(2) )
                BUFIDX = BUFIDX + 1
C
                TRABUF(TIPRZ1+I)  = LOGBUF(BUFIDX)
                BUFIDX = BUFIDX + 1
                IF (MOD(BUFIDX,16).EQ.0) BUFIDX = BUFIDX + 1
C
10001         CONTINUE
C
C IF TRABUF(TIVTYP) IT'S IVTP_NCP TRABUF(TIBCH) SHOULD BE ALWAYS ONE
C
              IF(TRABUF(TIVTYP) .EQ. IVTP_NCP) THEN
                 CALL FASTMOV(LOGBUF(17), TRABUF(TIVDESCR), 5)
              ENDIF
              
            ELSEIF(TRABUF(TIVMT) .EQ. IBVMT) THEN ! NEW BANK VALIDATION MODE LAYOUT
!-------->>V56 -------------------------------------------------------------------
                ! TRABUF(TIBCH) IS ALWAYS ONE
                I4TEMP = LOGBUF(11)
                TRABUF(TIPCK1)  = IAND(I4TEMP,'00FFFFFF'X)
                TRABUF(TISTS1)  = ZEXT(I1TEMP(4))
C
                TRABUF(TIVRN1)  = LOGBUF(12)
C
                I4TEMP=LOGBUF(13)
                TRABUF(TILTX1)  = ZEXT( I2TEMP(1) )
                X = ZEXT( I2TEMP(2) )
                TRABUF(TIPCKSTS1) = IAND(ISHFT(X,-12),'0F'X)
                TRABUF(TIGAM1)    = IAND(X,'0FFF'X)
C
                I4TEMP=LOGBUF(14)
                TRABUF(TITIM1)  = ZEXT( I2TEMP(1) )*2
                TRABUF(TICDC1)  = ZEXT( I2TEMP(2) )
C
                TRABUF(TIPRZ1)  = LOGBUF(15)
C
C               LOGBUF(16) ALREADY CONVERTED!
C
                I4TEMP = LOGBUF(17)
                TRABUF(TIPLIDTYP) = ZEXT( I1TEMP(1) ) !PLAYER ID TYPE
                TRABUF(TINIBBA2)  = ZEXT( I1TEMP(2) ) !NIB ACCOUNT NUMBER (PART 2)
                TRABUF(TINIBCD)   = ZEXT( I1TEMP(3) ) !NIB CHECK DIGITS
C
                TRABUF(TIPLCARD) = LOGBUF(18)         !PLAYER ID
C
                I4TEMP = LOGBUF(19)
                TRABUF(TINIBBB)  =  ZEXT( I2TEMP(1) ) !NIB BRANCH
                TRABUF(TINIBBO)  =  ZEXT( I2TEMP(2) ) !NIB OFFICE
C
                TRABUF(TINIBBA1) = LOGBUF(20)         !NIB ACCOUNT NUMBER (PART 1)
C
                TRABUF(TINETPRZ) = LOGBUF(21)         !NET PRIZE AMOUNT
C
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
                IF(TRABUF(TVOLMCOMF_IL) .EQ. 1) THEN
                  I4TEMP = LOGBUF(27)
                  TRABUF(TVOLMSERL_IL) = IOR( TRABUF(TVOLMSERL_IL), ISHFT( IAND( I4TEMP,'FF'X), 24) )
                  TRABUF(TVOLMSERM_IL) = ISHFT(I4TEMP, -8)
                  I4TEMP = LOGBUF(28)
                  TRABUF(TVOLMSERM_IL) = IOR( ISHFT(IAND( I4TEMP,'FF'X), 24), TRABUF(TVOLMSERM_IL) )
                  TRABUF(TVOLMSERH_IL) = ZEXT(I1TEMP(2))
                  I4TEMP = LOGBUF(29)
                  TRABUF(TVOLMMIDL_IL) = I4TEMP
                  I4TEMP = LOGBUF(30)
                  TRABUF(TVOLMMIDH_IL) = ZEXT(I1TEMP(1))
                ENDIF
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------

                IF(TRABUF(TIVTYP) .EQ. IVBM_NOPRZ) THEN
                  CALL FASTMOV(LOGBUF(22), TRABUF(TIVDESCR), 5)
                ENDIF
!-------- V56<<-------------------------------------------------------------------
              ENDIF
C
          ELSE IF(TRABUF(TITYP).EQ.IISS) THEN     ! V13
C
            TRABUF(TIREP)   = LOGBUF(8)
C
            I4TEMP          = LOGBUF(10)
            TRABUF(TINUM)   = ZEXT( I1TEMP(1) )
C
            BUFIDX = 11
            DO I = 0, TRABUF(TINUM)-1

                I4TEMP = LOGBUF(BUFIDX)
                TRABUF(TIGAM+I) = I2TEMP(1)

                IF (MOD(BUFIDX+1,16).EQ.0) THEN
                    BUFIDX = BUFIDX + 2
                ELSE
                    BUFIDX = BUFIDX + 1
                ENDIF

                I4TEMP = LOGBUF(BUFIDX)
                TRABUF(TIRES+I) = I1TEMP(4)
                I1TEMP(4) = 0
                TRABUF(TIPCK+I) = I4TEMP
                BUFIDX = BUFIDX+1

            END DO
C
          ELSE IF(TRABUF(TITYP).EQ.ILOT) THEN
C
            TRABUF(TLREP)   = LOGBUF(8)
C
            I4TEMP          = LOGBUF(9)
            TRABUF(TLCLS)   = ZEXT( I1TEMP(1) )
	    TRABUF(TLGAM)   = ZEXT( I2TEMP(2) )
C
            I4TEMP          = LOGBUF(10)
            TRABUF(TLSTR)   = ZEXT( I2TEMP(1) )
            TRABUF(TLEND)   = ZEXT( I2TEMP(2) )
C
            I4TEMP          = LOGBUF(11)
            I1TEMP(4)       = 0
            TRABUF(TLPCK)   = I4TEMP
C
            TRABUF(TLAMT)   = LOGBUF(12)
C
            TRABUF(TLCOM)   = LOGBUF(13)
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
            I4TEMP  = LOGBUF(20)  
            TRABUF(TGOLMCOMF_IL) = I4TEMP
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
              I4TEMP = LOGBUF(14)
              TRABUF(TGOLMSERL_IL) = I4TEMP
              I4TEMP = LOGBUF(15)
              TRABUF(TGOLMSERM_IL) = I4TEMP 
              I4TEMP = LOGBUF(17) 
              TRABUF(TGOLMSERH_IL) = I4TEMP
              I4TEMP  = LOGBUF(18)
              TRABUF(TGOLMMIDL_IL) = I4TEMP
              I4TEMP = LOGBUF(19)
              TRABUF(TGOLMMIDH_IL) = I4TEMP
            ENDIF 
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------            
C
          ELSE IF(TRABUF(TITYP).EQ.ICAR) THEN
C
            TRABUF(TCREP)   = LOGBUF(8)
C
            I4TEMP          = LOGBUF(9)
            TRABUF(TCCLS)   = ZEXT( I1TEMP(1) )
	    TRABUF(TCGAM)   = ZEXT( I2TEMP(2) )
C
            TRABUF(TCCAR)   = LOGBUF(10)
C
            TRABUF(TCSTA)   = LOGBUF(11)
C
            TRABUF(TCEND)   = LOGBUF(12)
C
            TRABUF(TCCNT)   = LOGBUF(13)
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
            I4TEMP  = LOGBUF(20)  
            TRABUF(TGOLMCOMF_IL) = I4TEMP
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
              I4TEMP = LOGBUF(14)
              TRABUF(TGOLMSERL_IL) = I4TEMP
              I4TEMP = LOGBUF(15)
              TRABUF(TGOLMSERM_IL) = I4TEMP 
              I4TEMP = LOGBUF(17) 
              TRABUF(TGOLMSERH_IL) = I4TEMP
              I4TEMP  = LOGBUF(18)
              TRABUF(TGOLMMIDL_IL) = I4TEMP
              I4TEMP = LOGBUF(19)
              TRABUF(TGOLMMIDH_IL) = I4TEMP 
            ENDIF
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------            
C
          ELSE IF(TRABUF(TITYP).EQ.IQTA.OR.
     *            TRABUF(TITYP).EQ.IINV.OR.
     *            TRABUF(TiTYP).EQ.ISET) THEN
C
            I4TEMP          = LOGBUF(8)
	    X               = ZEXT( I2TEMP(1) )
	    TRABUF(TRGAM)   = ISHFT( X, -4 )
            TRABUF(TRCLS)   = IAND( X, '0F'X )
            TRABUF(TRNXT1)  = ZEXT( I2TEMP(2) )
C
            TRABUF(TRNXT2)  = LOGBUF(9)
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
            I4TEMP = LOGBUF(15)
            TRABUF(TGOLMCOMF_IL) = I4TEMP
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1 .AND. TRABUF(TITYP).EQ.IQTA) THEN
                I4TEMP = LOGBUF(10)
                TRABUF(TGOLMSERL_IL) = I4TEMP
                I4TEMP = LOGBUF(11)
                TRABUF(TGOLMSERM_IL) = I4TEMP
                I4TEMP = LOGBUF(12)  
                TRABUF(TGOLMSERH_IL) = I4TEMP
                I4TEMP = LOGBUF(13) 
                TRABUF(TGOLMMIDL_IL) = I4TEMP
                I4TEMP = LOGBUF(14)  
                TRABUF(TGOLMMIDH_IL) = I4TEMP
            ENDIF
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------            
C
          ELSE IF(TRABUF(TITYP).EQ.IFIN) THEN
C
            I4TEMP          = LOGBUF(8)
            TRABUF(TRTYP)   = ZEXT( I1TEMP(1) )
            TRABUF(TRSUB)   = ZEXT( I1TEMP(2) )
C
            TRABUF(TRCHN)   = LOGBUF(9)
C
            TRABUF(TRCON)   = LOGBUF(10)
C
            TRABUF(TRCON1)  = LOGBUF(11)
C
            TRABUF(TRCON2)  = LOGBUF(12)
C
            TRABUF(TRCON3)  = LOGBUF(13)
C
          ELSE IF(TRABUF(TITYP).EQ.IORD) THEN
C
            I4TEMP          = LOGBUF(8)
            TRABUF(TGPGAM)  = ZEXT( I2TEMP(1) )
            TRABUF(TGPNXT)  = ZEXT( I2TEMP(2) )
C
            TRABUF(TGPAGT)  = LOGBUF(9)
C
            TRABUF(TGPRCL)  = LOGBUF(10)
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------ 
            I4TEMP = LOGBUF(17)
            TRABUF(TGOLMCOMF_IL) = I4TEMP
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
              I4TEMP = LOGBUF(11)
              TRABUF(TGOLMSERL_IL) = I4TEMP
              I4TEMP = LOGBUF(12)
              TRABUF(TGOLMSERM_IL) = I4TEMP
              I4TEMP = LOGBUF(13)  
              TRABUF(TGOLMSERH_IL) = I4TEMP
              I4TEMP = LOGBUF(14) 
              TRABUF(TGOLMMIDL_IL) = I4TEMP
              I4TEMP = LOGBUF(15)  
              TRABUF(TGOLMMIDH_IL) = I4TEMP
            ENDIF
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------             
C
          ELSE IF(TRABUF(TITYP).EQ.IMNU) THEN
C
            I4TEMP            = LOGBUF(16)
            TRABUF(TIBCH)     = ZEXT( I1TEMP(3) )
C
            TRABUF(TSORD)     = LOGBUF(8)
C
            TRABUF(TSINF)     = LOGBUF(9)
C
            CALL MOVBYT(LOGBUF(10),1,BUFF(1),1,24) 
C
            IF(TRABUF(TIBCH).GE.9)
     *        CALL MOVBYT(LOGBUF(17),1,BUFF(25),1,60) 
            IF(TRABUF(TIBCH).GE.29)
     *        CALL MOVBYT(LOGBUF(33),1,BUFF(85),1,36) 
C
            IND=1
            DO X = 0, TRABUF(TIBCH)-1     
C
                I4TEMP = 0
                CALL MOVBYT(BUFF,IND,I4TEMP,1,3)
                IND=IND+3
C
                IF(ISHFT(I4TEMP,-23) .EQ. 1) THEN
                  CALL BSET(TRABUF(TSSTK1),X)
                  I4TEMP = IAND(I4TEMP,'007FFFFF'X)
                ENDIF
C
                GTYP = ISHFT(I4TEMP,-20)
                GNUM = IAND(I4TEMP,'000FFFFF'X)
C
                TRABUF(TSQTY+X) = MOD(GNUM,1000)
                GNUM = (GNUM - TRABUF(TSQTY+X))/1000
                TRABUF(TSGAM+X) = ISHFT(GTYP,12) + IAND(GNUM,'0FFF'X)
C
            END DO
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------  
            IF(TRABUF(TIBCH).GE.29) THEN
              I4TEMP = LOGBUF(48)
              TRABUF(TGOLMCOMF_IL) = I1TEMP(1)              
              IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
                I4TEMP = LOGBUF(43)
                TRABUF(TGOLMSERL_IL) = I4TEMP
                I4TEMP = LOGBUF(44)
                TRABUF(TGOLMSERM_IL) = I4TEMP
                I4TEMP = LOGBUF(45)  
                TRABUF(TGOLMSERH_IL) = I4TEMP
                I4TEMP = LOGBUF(46) 
                TRABUF(TGOLMMIDL_IL) = I4TEMP
                I4TEMP = LOGBUF(47)  
                TRABUF(TGOLMMIDH_IL) = I4TEMP
              ENDIF
            ELSE IF(TRABUF(TIBCH).LE.21) THEN   
              I4TEMP = LOGBUF(32)
              TRABUF(TGOLMCOMF_IL) = I1TEMP(1)
              IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
                I4TEMP = LOGBUF(27)
                TRABUF(TGOLMSERL_IL) = I4TEMP
                I4TEMP = LOGBUF(28)
                TRABUF(TGOLMSERM_IL) = I4TEMP
                I4TEMP = LOGBUF(29)  
                TRABUF(TGOLMSERH_IL) = I4TEMP
                I4TEMP = LOGBUF(30) 
                TRABUF(TGOLMMIDL_IL) = I4TEMP
                I4TEMP = LOGBUF(31)  
                TRABUF(TGOLMMIDH_IL) = I4TEMP
              ENDIF
            ENDIF
            
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------              
C
          ELSE IF(TRABUF(TITYP).EQ.ICNF) THEN
C
            I4TEMP         = LOGBUF(8)
            TRABUF(TIINV1) = I2TEMP(1)
            TRABUF(TIINV2) = I2TEMP(2)
C
            TRABUF(TIINV3) = LOGBUF(9)
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------ 
            I4TEMP = LOGBUF(15)
            TRABUF(TGOLMCOMF_IL) = I4TEMP
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
              I4TEMP = LOGBUF(10)
              TRABUF(TGOLMSERL_IL) = I4TEMP
              I4TEMP = LOGBUF(11)
              TRABUF(TGOLMSERM_IL) = I4TEMP
              I4TEMP = LOGBUF(12)  
              TRABUF(TGOLMSERH_IL) = I4TEMP
              I4TEMP = LOGBUF(13) 
              TRABUF(TGOLMMIDL_IL) = I4TEMP
              I4TEMP = LOGBUF(14)  
              TRABUF(TGOLMMIDH_IL) = I4TEMP
            ENDIF
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------              
C
          ELSE IF(TRABUF(TITYP).EQ.IOACT) THEN
C
            I4TEMP         = LOGBUF(8)
            TRABUF(TOINV1) = I2TEMP(1)
            TRABUF(TOINV2) = I2TEMP(2)
C
            TRABUF(TOINV3) = LOGBUF(9)
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------ 
            I4TEMP = LOGBUF(15)
            TRABUF(TGOLMCOMF_IL) = I4TEMP
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
              I4TEMP = LOGBUF(10)
              TRABUF(TGOLMSERL_IL) = I4TEMP
              I4TEMP = LOGBUF(11)
              TRABUF(TGOLMSERM_IL) = I4TEMP
              I4TEMP = LOGBUF(12)  
              TRABUF(TGOLMSERH_IL) = I4TEMP
              I4TEMP = LOGBUF(13) 
              TRABUF(TGOLMMIDL_IL) = I4TEMP
              I4TEMP = LOGBUF(14)  
              TRABUF(TGOLMMIDH_IL) = I4TEMP
            ENDIF
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------            
C
          ELSE IF(TRABUF(TITYP).EQ.ISON.OR.
     *            TRABUF(TITYP).EQ.ISOF) THEN
C
            TRABUF(TIOLD) = LOGBUF(8)
            TRABUF(TINEW) = LOGBUF(9)
            TRABUF(TISGN) = LOGBUF(10)
            TRABUF(TIGVT1) = LOGBUF(11)
            TRABUF(TIGVT2) = LOGBUF(12)
            TRABUF(TIGVT3) = LOGBUF(13)
C
            I4TEMP = LOGBUF(14)
            TRABUF(TICLS) = ZEXT(I1TEMP(1))
            TRABUF(TIACT) = ZEXT(I1TEMP(2))
            TRABUF(TISTS) = ZEXT(I1TEMP(3))
C
            TRABUF(TISTN) = LOGBUF(15)
C
          ELSE IF(TRABUF(TITYP).EQ.IEST) THEN
C
            TRABUF(TISFT) = LOGBUF(8)
            TRABUF(TIPHONE1_1) = LOGBUF(10)
            TRABUF(TIPHONE1_2) = LOGBUF(11)
            TRABUF(TIPHONE1_3) = LOGBUF(12)
            TRABUF(TIPHONE2_1) = LOGBUF(13)
            TRABUF(TIPHONE2_2) = LOGBUF(14)
            TRABUF(TIPHONE2_3) = LOGBUF(15)
C
            I4TEMP = LOGBUF(9)
            TRABUF(TIRSTFLG) = I1TEMP(4)
            I1TEMP(4) = 0
            TRABUF(TIRSTTIM) = I4TEMP
C
            I4TEMP = LOGBUF(16)
            TRABUF(TICHKSUM) = ZEXT(I1TEMP(3))
C
            I4TEMP = LOGBUF(17)
            TRABUF(TIMINCB)  = I2TEMP(1)
C
          ELSE IF(TRABUF(TITYP).EQ.IGTB) THEN
C
            TRABUF(TIGMC)   = LOGBUF(8)
            TRABUF(TIGMN+0) = LOGBUF(9)
            TRABUF(TIGMN+1) = LOGBUF(10)
            TRABUF(TIGMN+2) = LOGBUF(11)
            TRABUF(TIGMN+3) = LOGBUF(12)
            TRABUF(TIGMN+4) = LOGBUF(13)
C
          ELSE IF(TRABUF(TITYP).EQ.IFSESON) THEN !V09
C
            I4TEMP                = LOGBUF(8)
            TRABUF(TIFSETYP)      = ZEXT(I1TEMP(1))
            TRABUF(TIFSERSLT)     = ZEXT(I1TEMP(2))
            TRABUF(TIFSEOFF)      = ZEXT(I2TEMP(2))
C
            TRABUF(TIFSEREP)      = LOGBUF(9)

            I4TEMP                = LOGBUF(10) 
            TRABUF(TIFSECLS)      = I2TEMP(1)
C
          ENDIF
          GOTO 9000
        ENDIF
C
C
C DECODE SPECIAL SERVICE BODY
C
        IF(TRABUF(TTYP).EQ.TSPE) THEN
           I4TEMP         = LOGBUF(16)
           TRABUF(TSFUN)  = ZEXT( I2TEMP(1) )
C
C CHECK IF GTECH DISTRIBUTED NETWORK RECORD.
C
           IF(TRABUF(TSFUN).EQ.TSX2X) THEN
              TRABUF(TXIDX)  = LOGBUF(8)
C
              I4TEMP         = LOGBUF(9)
              TRABUF(TXPTL)  = ZEXT( I1TEMP(1) )
              TRABUF(TXLAY)  = ZEXT( I1TEMP(2) )
              TRABUF(TXSTN)  = ZEXT( I2TEMP(2) )            !2 BYTES
C
              I4TEMP         = LOGBUF(10)
              TRABUF(TXSAP)  = ZEXT( I1TEMP(1) )
C
C X2X NETWORK GLOBAL MESSAGE (NONE DEFINED AT THIS TIME).
C
              IF(TRABUF(TXLAY).EQ.X2X_TRATYP_GLO) THEN
C
C X2X TRANSPORT LAYER MESSAGE.
C
              ELSE IF(TRABUF(TXLAY).EQ.X2X_TRATYP_XPORT) THEN
                 I4TEMP          = LOGBUF(10)
                 TRABUF(TXTFEID) = ZEXT( I1TEMP(2) )
                 TRABUF(TXTDSAP) = ZEXT( I1TEMP(3) )
                 TRABUF(TXTBTYP) = ZEXT( I1TEMP(4) )
C
C X2X FRONT END MESSAGE.
C
              ELSE IF(TRABUF(TXLAY).EQ.X2X_TRATYP_FE) THEN
                 I4TEMP          = LOGBUF(10)
                 TRABUF(TXFPID)  = ZEXT( I1TEMP(2) )
                 TRABUF(TXFSSAP) = ZEXT( I1TEMP(3) )
                 TRABUF(TXFMDUT) = ZEXT( I1TEMP(4) )
C
                 TRABUF(TXFDAD1) = LOGBUF(11)
                 TRABUF(TXFDAD2) = LOGBUF(12)
C
                 IF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_CMD) THEN
                    I4TEMP          = LOGBUF(13)
                    TRABUF(TXFCFID) = ZEXT ( I1TEMP(1) )
                    X               = ISHFT( I4TEMP, -8)
                    TRABUF(TXFCC)   = IAND ( X, '0000FFFF'X)
C
                 ELSE IF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_ACK) THEN
C
                 ELSE IF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_ERR) THEN
                    I4TEMP          = LOGBUF(13)
                    TRABUF(TXFDEC)  = ZEXT( I1TEMP(1) )
C
                 ELSE IF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_ALARM) THEN
                    I4TEMP          = LOGBUF(13)
                    TRABUF(TXFLFID) = ZEXT ( I1TEMP(1) )
                    X               = ISHFT( I4TEMP, -8)
                    TRABUF(TXFLMC)  = IAND ( X, '0000FFFF'X)
C
                 ELSE IF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_UP) THEN
                    TRABUF(TXFALT1) = LOGBUF(13)
                    TRABUF(TXFALT2) = LOGBUF(14)
                    IF (TRABUF(TXPTL).EQ.X2ERR_GOODVS) THEN
                       I4TEMP = LOGBUF(15)
                       X      = ISHFT(I4TEMP,-8)
                       TRABUF(TXFVS) = IAND( X,'0000FFFF'X)
                    ENDIF
C
                 ENDIF
C
C X2X STATION MESSAGE.
C
              ELSE IF(TRABUF(TXLAY).EQ.X2X_TRATYP_STTN) THEN
                 I4TEMP          = LOGBUF(10)
                 TRABUF(TXSPID)  = ZEXT( I1TEMP(2) )
                 TRABUF(TXSSDTU) = ZEXT( I1TEMP(3) )
C
                 IF(TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_CMD_UP) THEN
                    I4TEMP          = LOGBUF(10)
                    TRABUF(TXSCC)   = ZEXT( I1TEMP(4) )
C
                    I4TEMP          = LOGBUF(11)
                    TRABUF(TXSSNUM) = ZEXT( I2TEMP(1) )
C
                 ELSE IF((TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_RESET).OR.
     *              (TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_RESET2)) THEN
                    I4TEMP          = LOGBUF(10)
                    TRABUF(TXSSTYP) = ZEXT( I1TEMP(4) )
C
                    I4TEMP          = LOGBUF(11)
                    TRABUF(TXSSPHID)= ZEXT( I2TEMP(1) )
C
                 ENDIF
C
              ENDIF
C
C ONLINE AGENT UPDATE
C
	  ELSE IF(TRABUF(TSFUN).EQ.TAGTINF) THEN
	      I4TEMP     = LOGBUF(8)
              TRABUF(TSNEW) = ZEXT(I1TEMP(1))     ! number of items

              CALL MOVBYT(LOGBUF(8),2,TRABUF(TSDT1),1,31)
              CALL MOVBYT(LOGBUF(17),1,TRABUF(TSDT1),32,LREC*4-1)
              CALL MOVBYT(LOGBUF(33),1,TRABUF(TSDT1),95,LREC*4-1)

	      GOTO 9000
C
C MX LOG TRANSACTIONS
C                                                             !MXSRV
           ELSE IF(TRABUF(TSFUN).EQ.TMXL) THEN                !MXSRV
             CALL FASTMOV(LOGBUF(8),TRABUF(TMXL_RPCTAG),8)    !MXSRV
             CALL FASTMOV(LOGBUF(17),TRABUF(TMXL_RPCTAG+8),8) !MXSRV
             CALL FASTMOV(LOGBUF(25),TRABUF(TMXL_DATA),7)     !MXSRV
             CALL FASTMOV(LOGBUF(33),TRABUF(TMXL_DATA+7),15)  !MXSRV
             I4TEMP             = LOGBUF(16)                  !MXSRV
             TRABUF(TMXL_ERCODE)= I1TEMP(3)                   !MXSRV
             I4TEMP             = LOGBUF(32)                  !MXSRV
             TRABUF(TMXL_DATLEN)= I2TEMP(1)                   !MXSRV      
C
C NORMAL SPECIAL SERVICE RECORD.
C
           ELSE
              TRABUF(TSOLD) = LOGBUF( 8)
              TRABUF(TSNEW) = LOGBUF( 9)
              TRABUF(TSDT1) = LOGBUF(10)
              TRABUF(TSDT2) = LOGBUF(11)
              TRABUF(TSDT3) = LOGBUF(12)
              TRABUF(TSDT4) = LOGBUF(13)
              TRABUF(TSDT5) = LOGBUF(14)
              TRABUF(TSDT6) = LOGBUF(15)
C
              I4TEMP        = LOGBUF(16)
              TRABUF(TSSGN) = ZEXT( I1TEMP(3) )
              IF(TRABUF(TSIZE).GT.1) 
     *           CALL FASTMOV(LOGBUF(17),TRABUF(TSDT7),15)

C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C              TRABUF(TSDT8)  = LOGBUF(18)
C              TRABUF(TSDT9)  = LOGBUF(19)
C              TRABUF(TSDT10) = LOGBUF(20)
C              TRABUF(TSDT11) = LOGBUF(21)
C              TRABUF(TSDT12) = LOGBUF(22)
C              TRABUF(TSDT13) = LOGBUF(23)
C----+------------------------------------------------------------------
C V59| New Terminals Project - Olimpo
C----+------------------------------------------------------------------      
C
           ENDIF
           GOTO 9000
        ENDIF
C
C DECODE COMMAND BODY
C
        IF(TRABUF(TTYP).EQ.TCMD) THEN
           TRABUF(TCMOLD) = LOGBUF( 8)
           TRABUF(TCMNEW) = LOGBUF( 9)
           TRABUF(TCMDT1) = LOGBUF(10)
           TRABUF(TCMDT2) = LOGBUF(11)
           TRABUF(TCMDT3) = LOGBUF(12)
           TRABUF(TCMDT4) = LOGBUF(13)
           TRABUF(TCMSRC) = LOGBUF(14)
C
           TRABUF(TCMTER) = LOGBUF(15)
C
           I4TEMP         = LOGBUF(16)
           TRABUF(TCMNUM) = ZEXT( I2TEMP(1) )
           TRABUF(TCMTYP) = ZEXT( I1TEMP(3) )
C
C CONTINUATION RECORD 2
C
           TRABUF(TCMDT5) = LOGBUF(17)
	   TRABUF(TCMLIN) = LOGBUF(18)
C
           GOTO 9000
C
        ENDIF
C
9000    CONTINUE
        RETURN
        END

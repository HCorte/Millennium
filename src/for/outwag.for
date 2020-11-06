C
C SUBROUTINE OUTWAG
C  
C V22 18-MAR-1999 RXK Game type/game index change. Hack for V5 removed.
C
C     Rev 1.1   17 Jun 1997 17:56:10   RXK
C  Changes for Bingo Fullhouse 
C  
C     Rev 1.0   17 Apr 1996 14:20:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.15   17 Jul 1995 22:41:40   HXK
C  Minor bug fix for Ravi batch
C  
C     Rev 1.14   12 Jul 1995 19:20:36   HXK
C  General debug fixes during QA
C  
C     Rev 1.13   26 Jun 1995 19:20:10   HXK
C  Changes for RAVI modification, mainly QPs, alternatives, screening
C  and prognosis
C  
C     Rev 1.12   24 Apr 1995 17:04:10   HXK
C  Merge of V5 development with March 10th 1995 bible
C  
C     Rev 1.12   22 Feb 1995 20:22:36   HXK
C  Hack for V5 for terminal (i.e. terminal doesn't have game indices)
C  (Real smart, huh?)
C  
C     Rev 1.11   07 Nov 1994 12:13:34   HXK
C  Use REVNIB function (reverse nibble) to output bet for Bingo
C  
C     Rev 1.10   15 Oct 1994 16:38:18   HXK
C  Adding /developing Bingo (15.Oct.94)
C  
C     Rev 1.9   03 Aug 1993  8:55:14   GXA
C  Added Kicker duration byte to output message.
C  This is needed by the terminal when the main and companion games durations
C  are different, like Viking Lotto.
C  
C     Rev 1.8   19 Jul 1993 12:13:50   GXA
C  Added check to see if additional kicker date offsets are needed in msg.
C  
C     Rev 1.7   16 Jul 1993 17:07:40   GXA
C  Added TSUBERR to error message.
C  
C     Rev 1.6   01 Jul 1993 18:59:56   GXA
C  Removed RAVI Alternates. Terminal will generate all of them.
C  
C     Rev 1.5   01 Jul 1993 11:37:48   SXH
C  Added RAVI (V65) alternatives
C  
C     Rev 1.4   30 Jun 1993 15:10:24   HXN
C  Fixed and changed  TIME = TRABUF(TTIM)/3600   to TIME = TRABUF(TTIM).
C  
C     Rev 1.3   18 Jun 1993 14:53:38   HXK
C  SEPERATED KOFFGET FROM OUTWAG
C  
C     Rev 1.2   10 Jun 1993 18:23:12   SXH
C  Declare IND in KOFFGET
C  
C     Rev 1.1   10 May 1993 12:02:34   SXH
C  Released for Finland VAX
C  
C     Rev 1.0   21 Jan 1993 17:14:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - outwag.for **
C
C OUTWAG.FOR
C
C V02 01-NOV-91 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO BUILD WAGER OUTPUT MESSAGES.
C
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
        SUBROUTINE OUTWAG(TRABUF,OUTTAB,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
        ! arguments
        INTEGER*2  OUTLEN        ! output message length

        BYTE       OUTTAB(*)     ! output message

        ! variables
        INTEGER*4  MYCHKSUM      !
        INTEGER*4  CHKLEN        !
        INTEGER*4  OFFBEG        ! offset to first CDC date
        INTEGER*4  OFFEND        ! offset to second CDC date
        INTEGER*4  KOFFBEG       ! first joker CDC offset
        INTEGER*4  KOFFEND       ! second joker CDC offset
        INTEGER*4  IND           !
        INTEGER*4  ERRTYP        !
        INTEGER*4  OPTION        !
        INTEGER*4  CHKDIG        !
        INTEGER*4  I4TEMP        !
        INTEGER*4  I,K
C
        INTEGER*2  I2TEMP(2)     !
C
        BYTE       I1TEMP(4)     !


        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
        DATA ERRTYP/Z90/
C
C CONTROL AND SEQUENCE NUMBER
C '20'X (CONTROL NIBBLE) -> 10 - Application Message
        OUTTAB(1) = '20'X + TRABUF(TTRN) !TTRN=TRANSACTION SEQUENCE NUMBER
C
C IF TRANSACTION STATUS IS NOT GOOD
C BUILD ERROR MESSAGE.
C TERR -> NOER (No Error) - SYNT (Synthesis Error) - RETY (Retry Error) - DESMOD (???) - SUPR (Supression Error) - BSTS - NOTON - SDRW - SDOR
C Values:		Description
C 1 =		Invalid
C 2 =		Syntax Error
C 3 =		Function Suppressed
C 4 =		Not Signed On
C 5 =		System Dormant
C 6 =		Drawing Break
C 7 =		Liability Limit Exceeded
C 8 =		Non-drawing Day
C 9 =		DES Encryption Error
C 10 =		Results Not In
C 11 =		Invalid Terminal Number
C 12 =		Security Violation (>10 sign-on)
C 13 =		Bad Checksum
C 14 =		Bad Game Status
C 15 =		Retry Transaction
C 18 =		Game Revision Error
C 19 =		Can Not Fraction Ticket
C 20 =		Already Fractioned
C 21 =		Unfractioned
C 22 =		Card Present (Pass # mismatch)
C 23 =		No Card Present at SON (Do not SON)
C 25 =		Sports, Wrong number of marks with an event cancelled
C 26 =		Sports, The full draw has been cancelled.
C 31 =		Combination Closed
C 32 =		Odds exceeded
C 36 =         	Invalid Agent Or Password In Sign On
C 37 =		Not available passive number found
C 38 =		Passive IO error
C 39 =		Blocked NIF

        IF(TRABUF(TSTAT).NE.GOOD) THEN !quer seja sucesso ou erro o 1º byte têm sempre Control e Sequence
            OUTTAB(2) = ERRTYP !segundo byte têm o Type = 9 e Subtype = 0
C           byte 3 e 4 é usado para guardar o checksum  (validado no fim pois tb é campo obrigatorio em caso de sucesso)          
            OUTTAB(5) = TRABUF(TERR) !Error Number ou seja mensagem de erro que ficou durante o processamento
            OUTTAB(6) = TRABUF(TSUBERR) !não está na documentação, é usado???
            OUTLEN=6
            GOTO 1000
        ENDIF
C
C TYPE and Subtype are both 0 the byte needs to go with value 0
C
        OUTTAB(2) = 0                    ! #2
C Byte 3 and 4 are at the and for CHECKSUM
C GAME TYPE AND GAME INDEX
C
        OUTTAB(5) = TRABUF(TGAMTYP)    ! #5
        OUTTAB(6) = TRABUF(TGAMIND)    ! #6
        IND = 7
C
C SERIAL NUMBER AND CHECK DIGITS
C
        CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),I4TEMP,CHKDIG)
        OUTTAB(IND+0) = I1TEMP(3)        ! #7 -> Wager Serial Number
        OUTTAB(IND+1) = I1TEMP(2)        ! #8 -> Wager Serial Number
        OUTTAB(IND+2) = I1TEMP(1)        ! #9 -> Wager Serial Number
        OUTTAB(IND+3) = CHKDIG           ! #10 -> Wager Check Digits
        IND=IND+4
C
C OFFSET OF START AND END CDC DATES
C IND=11
        CALL GETOFF(TRABUF,OFFBEG,OFFEND)
        I4TEMP = OFFBEG
        OUTTAB(IND+0) = I1TEMP(2)        ! #11 -> Offset to First CDC Date (OFFBEG)
        OUTTAB(IND+1) = I1TEMP(1)        ! #12 -> Offset to First CDC Date (OFFBEG)
        I4TEMP = OFFEND
        OUTTAB(IND+2) = I1TEMP(2)        ! #13 -> Offset to Second CDC Date (OFFEND)
        OUTTAB(IND+3) = I1TEMP(1)        ! #14 -> Offset to Second CDC Date (OFFEND)
        IND=IND+4
C
C SET OFFSETS TO FIRST - SECOND WEEK / YEAR DATES
C IND = 15  -> until 19
        CALL PUT_WEEK_YEAR_DRAWS(TRABUF, OUTTAB, IND) !First Week Draw Date
C
C Fill in time
C IND = 19
        CALL PUTIME(TRABUF(TTIM), OUTTAB, IND) !Time (hh:mm:ss) 19,20,21
C
C GET OPTIONS AND KIKER OFFSETS
C CHECK IF OFFSETS ARE NEEDED BEFOR SETTING THE OPTIONS,
C IF NOT NEEDED CLEAR KICKER OFFSETS OPTION BIT.
C IND = 22
        CALL OGETOPT(TRABUF,OPTION)
        CALL KOFFGET(TRABUF,KOFFBEG,KOFFEND) !ignore its for joker
C ignore its for joker
        IF(OFFBEG.EQ.KOFFBEG.AND.OFFEND.EQ.KOFFEND)   !Additional Kicker offsets
     *     OPTION = IAND(OPTION,'7F'X)                !are not needed.
C
        OUTTAB(IND)=OPTION               ! #18?? IND = 22 <-> #22
        IND=IND+1                        ! IND=22+1=23
C 
C SET JOKER OFFSETS (option data data is dynamic in the length depending in the flags active from option flags)
C no longer in use
        IF(IAND(OPTION,'80'X).NE.0) THEN !option data if option flag haves the flag X80->(1000 0000)
            I4TEMP = KOFFBEG
            OUTTAB(IND) = I1TEMP(1)   
            I4TEMP = KOFFEND
            OUTTAB(IND+1) = I1TEMP(1) 
            OUTTAB(IND+2) = TRABUF(TWKDUR)
            IND=IND+3
        END IF
C
C SET JOKER 1 NUMBER
C no longer in use
        IF(IAND(OPTION,'40'X).NE.0) THEN !option data if option flag haves the flag X40->(0100 0000)
            I4TEMP = TRABUF(TWKICK)
            OUTTAB(IND+0) = I1TEMP(4)
            OUTTAB(IND+1) = I1TEMP(3)
            OUTTAB(IND+2) = I1TEMP(2)
            OUTTAB(IND+3) = I1TEMP(1)
            IND=IND+4     
        ENDIF
C
C SET JOKER 2 NUMBER
C no longer in use
        IF(IAND(OPTION,'20'X).NE.0) THEN !option data if option flag haves the flag X20->(0010 0000)
            I4TEMP = TRABUF(TWKICK2)
            OUTTAB(IND+0) = I1TEMP(4)
            OUTTAB(IND+1) = I1TEMP(3)
            OUTTAB(IND+2) = I1TEMP(2)
            OUTTAB(IND+3) = I1TEMP(1)
            IND=IND+4     
        ENDIF
C
C SET NUMBER OF FRACTIONS
C
        IF(IAND(OPTION,'10'X).NE.0) THEN !option data if option flag haves the flag X10->(0001 0000)
            OUTTAB(IND) = TRABUF(TFRAC)
            IND=IND+1     
        ENDIF
C
C SET FULL HOUSE BINGO BOARD
C not used by SCML
CC        IF(IAND(OPTION,'04'X).NE.0) THEN
        IF(TRABUF(TGAMTYP).EQ.TBNG) THEN   
           DO I=1,3
              DO K=1,5
                 I4TEMP = TRABUF(TWBBFH1+(I-1)*5+(K-1))
                 OUTTAB(IND+0) = I1TEMP(2)
                 OUTTAB(IND+1) = I1TEMP(1)
                 IND=IND+2
              ENDDO
           ENDDO
        ENDIF
C
C SET CANCELLATION EVENTS BITMAP -- TO PRINT "C" IN THE WAGER --
C
        IF(IAND(OPTION, '02'X) .NE. 0) THEN
           I4TEMP          = TRABUF(TWCEBM) ! SPORTS GAME CANCELATION EVENTS BITMAP
           OUTTAB(IND + 0) = I1TEMP(4)   ! Data is comming in The High I4 Byte Part
           OUTTAB(IND + 1) = I1TEMP(3)
           IND = IND + 2
        ENDIF
C
C SET LUCKY NUMBER AND BASE
C Not used
        IF(IAND(OPTION,'01'X).NE.0) THEN
            I4TEMP = TRABUF(TWBBAS)
            OUTTAB(IND+0) = I1TEMP(2)
            OUTTAB(IND+1) = I1TEMP(1)
            IND=IND+2
            I4TEMP = TRABUF(TWBLUK)
            OUTTAB(IND+0) = I1TEMP(4)
            OUTTAB(IND+1) = I1TEMP(3)
            OUTTAB(IND+2) = I1TEMP(2)
            OUTTAB(IND+3) = I1TEMP(1)
            IND=IND+4
        ENDIF
C
        OUTLEN=IND-1
C
C CALCULATE CHECKSUM
C
1000    CONTINUE
        I4CCITT   = TRABUF(TCHK)
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)
        CHKLEN=OUTLEN-1
        CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT   = MYCHKSUM
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)

        RETURN

        END


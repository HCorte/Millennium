CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C File      : OIGSTWAG.FOR
C Change Log:
C
C Ver Date       Author  Comment
C --- ---------- ------- ----------------------------------------------
C V01 2014.02.27  SCML   Created - PLACARD Project - Creates wager 
C                        output messages for IGS;
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      THIS ITEM IS THE PROPERTY OF SCML.
C
C      COPYRIGHT 2014 SCML. ALL RIGHTS RESERVED.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OIGSTWAG(WRKBUF, TRABUF, MESS, MES_LEN, BUFNUM, TER, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        
        ! Input parameters
        INTEGER*4 WRKBUF(TRALEN)
        INTEGER*4 BUFNUM, BUF
        INTEGER*4 MESS(EDLEN)
        INTEGER*4 MES_LEN
        INTEGER*4 ST
        INTEGER*4 TER
        
        
        INTEGER*4 RETRYIND, ERRORNUM, RETRY, MYCHKSUM
        INTEGER*4  I4TEMP,TEMP1,TEMP2
        INTEGER*2  I2TEMP(2)
        BYTE       I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)

        INTEGER*4 MTYPE, SUBTYP, IND, I, OFFSET
        
        INTEGER*4 XREFNUM
        
        BUF = BUFNUM
        ST = 0
        
        MES_LEN = ZEXT (HPRO(OUTLEN,BUF))

C----+------------------------------------------------------------------
C    | Setting XREFNUM field for IGS wager (bytes 5-8, of Millennium header)
C----+------------------------------------------------------------------
        OFFSET = WRKTAB*4-3
        I1TEMP(4) = ZEXT (BPRO(OFFSET+  5,BUF))
        I1TEMP(3) = ZEXT (BPRO(OFFSET+  6,BUF))
        I1TEMP(2) = ZEXT (BPRO(OFFSET+  7,BUF))
        I1TEMP(1) = ZEXT (BPRO(OFFSET+  8,BUF))
        TRABUF(TIGS_XREF) = I4TEMP
        
C----+------------------------------------------------------------------
C    | Setting transaction fields for IGS wager
C----+------------------------------------------------------------------
        OFFSET = WRKTAB*4-3 + 14
        MTYPE = ZEXT(BPRO(OFFSET+ 2,BUF))
        MTYPE = ISHFT(MTYPE,-4)
        SUBTYP = ZEXT(BPRO(OFFSET+ 2,BUF))
        SUBTYP = IAND(SUBTYP, 15)
        
C----+------------------------------------------------------------------
C    | Handling successful bet
C----+------------------------------------------------------------------
        IF(MTYPE .EQ. 14 .AND. SUBTYP .EQ. 0) THEN
C----+------------------------------------------------------------------
C    | (I/O):Decode ABP Game Id and set it up in TRABUF (1 byte)
C----+------------------------------------------------------------------
            TRABUF(TIGSW_XGID) = ZEXT (BPRO(OFFSET+ 29,BUF))
C----+------------------------------------------------------------------
C    | (I/O):Decode Sub-type Id and set it up in TRABUF (2 bytes)
C----+------------------------------------------------------------------
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 30,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 31,BUF))
            TRABUF(TIGSW_STID) = I4TEMP
C----+------------------------------------------------------------------
C    | (I/O):Decode Bet Unit Stake and set it up in TRABUF (4 bytes)
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 32,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 33,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 34,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 35,BUF))
            TRABUF(TIGSW_USTK) = I4TEMP
C----+------------------------------------------------------------------
C    | (I/O):Decode Total Bets and set it up in TRABUF (1 byte)
C----+------------------------------------------------------------------
            TRABUF(TIGSW_TBET) = ZEXT (BPRO(OFFSET+ 44,BUF))
        
C----+------------------------------------------------------------------
C    | (O):Decode BET REFERENCE DATE (YYMMDD) and set it up in TRABUF
C----+------------------------------------------------------------------
            TRABUF(TIGSW_WRDY) = ZEXT (BPRO(OFFSET+  7,BUF))
            TRABUF(TIGSW_WRDM) = ZEXT (BPRO(OFFSET+  8,BUF))
            TRABUF(TIGSW_WRDD) = ZEXT (BPRO(OFFSET+  9,BUF))
C----+------------------------------------------------------------------
C    | (O):Decode BET REFERENCE GAME and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = 0
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 10,BUF))
            TRABUF(TIGSW_WRGM) = I4TEMP
C----+------------------------------------------------------------------
C    | (O):Decode BET REFERENCE SERIAL NUMBER (HIGH ONE BYTE) and set it up 
C    | in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = 0
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 11,BUF))
            TRABUF(TIGSW_WRSH) = I4TEMP
C----+------------------------------------------------------------------
C    | (O):Decode BET REFERENCE SERIAL NUMBER (LOW FOUR BYTES) and set it up 
C    | in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 12,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 13,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 14,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 15,BUF))
            TRABUF(TIGSW_WRSL) = I4TEMP
C----+------------------------------------------------------------------
C    | (O):Decode BET REFERENCE CHECK DIGITS and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 16,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 17,BUF))
            TRABUF(TIGSW_WRCD) = I4TEMP
C----+------------------------------------------------------------------
C    | (O):Decode BET CREATION DATE (YYYYMMDD) and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 18,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 19,BUF))
            TRABUF(TIGSW_WCDY) = I4TEMP
            TRABUF(TIGSW_WCDM) = ZEXT (BPRO(OFFSET+  20,BUF))
            TRABUF(TIGSW_WCDD) = ZEXT (BPRO(OFFSET+  21,BUF))
C----+------------------------------------------------------------------
C    | (O):Decode BET CREATION TIME (HHMISS) and set it up in TRABUF
C----+------------------------------------------------------------------
            TRABUF(TIGSW_WCTH) = ZEXT (BPRO(OFFSET+ 22,BUF))
            TRABUF(TIGSW_WCTM) = ZEXT (BPRO(OFFSET+ 23,BUF))
            TRABUF(TIGSW_WCTS) = ZEXT (BPRO(OFFSET+ 24,BUF))
C----+------------------------------------------------------------------
C    | (O):Decode BET LAST EVENT DATE (YYYYMMDD) and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 25,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 26,BUF))
            TRABUF(TIGSW_LEDY) = I4TEMP
            TRABUF(TIGSW_LEDM) = ZEXT (BPRO(OFFSET+  27,BUF))
            TRABUF(TIGSW_LEDD) = ZEXT (BPRO(OFFSET+  28,BUF))
C----+------------------------------------------------------------------
C    | (O):Decode BET TOTAL STAKE and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 36,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 37,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 38,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 39,BUF))
            TRABUF(TIGSW_TSTK) = I4TEMP
C----+------------------------------------------------------------------
C    | (O):Decode BET MAXIMUM POSSIBLE RETURNS and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 40,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 41,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 42,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 43,BUF))
            TRABUF(TIGSW_MAXR) = I4TEMP
C----+------------------------------------------------------------------
C    | Handling error case: update transaction status if returned message is an error
C----+------------------------------------------------------------------
        ELSEIF (MTYPE .EQ. 14 .AND. SUBTYP .EQ. 15) THEN
            TRABUF(TSTAT) = REJT
            TRABUF(TERR) = INVL

            ! Recording system code where error occurred
            TRABUF(TIGS_SERR) = ZEXT (BPRO(OFFSET+  8,BUF))
            
            ! Receiving characters; we must put them in the receiving
            ! order rather than inverting it
            I1TEMP(1) = ZEXT (BPRO(OFFSET+  9,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 10,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 11,BUF))
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 12,BUF))
            TRABUF(TIGS_XERR + 0) = I4TEMP

            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 13,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 14,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 15,BUF))
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 16,BUF))
            TRABUF(TIGS_XERR + 1) = I4TEMP

            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 17,BUF))
            ! Initializing with spaces
            I1TEMP(2) = ZEXT (32)
            I1TEMP(3) = ZEXT (32)
            I1TEMP(4) = ZEXT (32)

            TRABUF(TIGS_XERR + 2) = I4TEMP
        ENDIF

C----+------------------------------------------------------------------
C    | Update Serial number in agent table
C----+------------------------------------------------------------------
        IF (MTYPE .EQ. 14 .AND. SUBTYP .EQ. 0) THEN
          IF (TRABUF(TSTAT) .EQ. GOOD) CALL UPDIGS(TRABUF)
        ENDIF

C----+------------------------------------------------------------------
C    | Create output message...
C----+------------------------------------------------------------------
100     CONTINUE
        IND = 0
        DO I=15, MES_LEN
           BPRO(BOUTTAB+I-15,BUF) = ZEXT (BPRO(WRKTAB*4-3+I,BUF))
           IND = IND + 1
        ENDDO
        HPRO(OUTLEN,BUF) = IND 
        
        RETURN
        END

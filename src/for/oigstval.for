CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C File      : OIGSTVAL.FOR
C Change Log:
C
C Ver Date       Author  Comment
C --- ---------- ------- ----------------------------------------------
C V01 2014.02.27  SCML   Created - PLACARD Project - Creates 
C                        validation output messages for IGS;
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      THIS ITEM IS THE PROPERTY OF SCML.
C
C      COPYRIGHT 2014 SCML. ALL RIGHTS RESERVED.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OIGSTVAL(WRKBUF, TRABUF, MESS, MES_LEN, BUFNUM, TER, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        

        INCLUDE 'INCLIB:IGSDEBUG.DEF'
        
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

        INTEGER*8 I8AUX_AMOUNT
        INTEGER*4 I4AUX_AMOUNT(2)
        EQUIVALENCE(I8AUX_AMOUNT,I4AUX_AMOUNT)

        INTEGER*4 MTYPE, SUBTYP, IND, I, OFFSET

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
C    | Handling successful validation
C----+------------------------------------------------------------------
        IF(MTYPE .EQ. 14 .AND. SUBTYP .EQ. 2) THEN
C----+------------------------------------------------------------------
C    | PLACARD NIF
C    | (O):Decode FISCAL ID CONFIRMATION NEEDED flag and set it up in TRABUF
C----+------------------------------------------------------------------
            TRABUF(TIGSV_FNIF) = ZEXT(BPRO(OFFSET+  7,BUF))
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPS('TRABUF(TIGSV_FNIF)',TRABUF(TIGSV_FNIF),TRABUF(TIGSV_FNIF))
            ENDIF
C----+------------------------------------------------------------------
C    | PLACARD NIF
C    | (O):Decode PLAYER FISCAL ID NUMBER (NIF) and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+  8,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+  9,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 10,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 11,BUF))
            TRABUF(TIGSV_PNIF) = I4TEMP
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPS('TRABUF(TIGSV_PNIF)',TRABUF(TIGSV_PNIF),TRABUF(TIGSV_PNIF))
            ENDIF
C----+------------------------------------------------------------------
C    | (O):Decode WAGER VALIDATION DATE (YYYYMMDD) and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 12,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 13,BUF))
            TRABUF(TIGSV_WVDY) = I4TEMP
            TRABUF(TIGSV_WVDM) = ZEXT (BPRO(OFFSET+ 14,BUF))
            TRABUF(TIGSV_WVDD) = ZEXT (BPRO(OFFSET+ 15,BUF))
C----+------------------------------------------------------------------
C    | (O):Decode WAGER VALIDATION TIME (HHMISS) and set it up in TRABUF
C----+------------------------------------------------------------------
            TRABUF(TIGSV_WVTH) = ZEXT (BPRO(OFFSET+ 16,BUF))
            TRABUF(TIGSV_WVTM) = ZEXT (BPRO(OFFSET+ 17,BUF))
            TRABUF(TIGSV_WVTS) = ZEXT (BPRO(OFFSET+ 18,BUF))
C----+------------------------------------------------------------------
C    | (O):Decode TOTAL PRIZE AMOUNT (VALIDATION UNITS) and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 19,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 20,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 21,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 22,BUF))
            TRABUF(TIGSV_TPRZ) = I4TEMP
C----+------------------------------------------------------------------
C    | (O):Decode TOTAL TAX AMOUNT (VALIDATION UNITS) and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 23,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 24,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 25,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 26,BUF))
            TRABUF(TIGSV_TTAX) = I4TEMP
C----+------------------------------------------------------------------
C    | (O):Decode NET PRIZE AMOUNT (VALIDATION UNITS) and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 27,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 28,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 29,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 30,BUF))
            TRABUF(TIGSV_NPRZ) = I4TEMP
C----+------------------------------------------------------------------
C    | (O):Decode Payment mode and set it up in TRABUF (1 byte)
C----+------------------------------------------------------------------
            TRABUF(TIGSV_PMOD) = ZEXT (BPRO(OFFSET+ 31,BUF))
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
        IF (MTYPE .EQ. 14 .AND. SUBTYP .EQ. 2) THEN
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

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C File      : OIGSTPAY.FOR
C Change Log:
C
C Ver Date       Author  Comment
C --- ---------- ------- ----------------------------------------------
C V01 2014.02.27  SCML   Created - PLACARD Project - Creates 
C                        payment output messages for IGS;
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      THIS ITEM IS THE PROPERTY OF SCML.
C
C      COPYRIGHT 2014 SCML. ALL RIGHTS RESERVED.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OIGSTPAY(WRKBUF, TRABUF, MESS, MES_LEN, BUFNUM, TER, ST)
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
C    | Handling successful payment
C----+------------------------------------------------------------------
        IF(MTYPE .EQ. 14 .AND. SUBTYP .EQ. 3) THEN
C----+------------------------------------------------------------------
C    | PLACARD NIF
C    | (O):Decode PLAYER FISCAL ID NUMBER (NIF) and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+  7,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+  8,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+  9,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 10,BUF))
            TRABUF(TIGSP_PNIF) = I4TEMP
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPS('TRABUF(TIGSP_PNIF)',TRABUF(TIGSP_PNIF),TRABUF(TIGSP_PNIF))
            ENDIF
C----+------------------------------------------------------------------
C    | (O):Decode PAYMENT REFERENCE DATE (YYMMDD) and set it up in TRABUF
C----+------------------------------------------------------------------
            TRABUF(TIGSP_PRDY) = ZEXT (BPRO(OFFSET+ 11,BUF))
            TRABUF(TIGSP_PRDM) = ZEXT (BPRO(OFFSET+ 12,BUF))
            TRABUF(TIGSP_PRDD) = ZEXT (BPRO(OFFSET+ 13,BUF))
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPS('TRABUF(TIGSP_PRDY)',TRABUF(TIGSP_PRDY),TRABUF(TIGSP_PRDY))
              CALL OPS('TRABUF(TIGSP_PRDM)',TRABUF(TIGSP_PRDM),TRABUF(TIGSP_PRDM))
              CALL OPS('TRABUF(TIGSP_PRDD)',TRABUF(TIGSP_PRDD),TRABUF(TIGSP_PRDD))
          ENDIF
C----+------------------------------------------------------------------
C    | (O):Decode PAYMENT REFERENCE GAME and set it up in TRABUF
C----+------------------------------------------------------------------
            TRABUF(TIGSP_PRGM) = ZEXT (BPRO(OFFSET+ 14,BUF))
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPS('TRABUF(TIGSP_PRGM)',TRABUF(TIGSP_PRGM),TRABUF(TIGSP_PRGM))
          ENDIF
C----+------------------------------------------------------------------
C    | (O):Decode PAYMENT REFERENCE SERIAL NUMBER (HIGH ONE BYTE) and set it up 
C    | in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = 0
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 15,BUF))
            TRABUF(TIGSP_PRSH) = I4TEMP
C----+------------------------------------------------------------------
C    | (O):Decode PAYMENT REFERENCE SERIAL NUMBER (LOW FOUR BYTES) and set it up 
C    | in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 16,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 17,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 18,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 19,BUF))
            TRABUF(TIGSP_PRSL) = I4TEMP
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPS('TRABUF(TIGSP_PRSH)',TRABUF(TIGSP_PRSH),TRABUF(TIGSP_PRSH))
              CALL OPS('TRABUF(TIGSP_PRSL)',TRABUF(TIGSP_PRSL),TRABUF(TIGSP_PRSL))
          ENDIF
C----+------------------------------------------------------------------
C    | (O):Decode PAYMENT REFERENCE CHECK DIGITS and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 20,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 21,BUF))
            TRABUF(TIGSP_PRCD) = I4TEMP
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPS('TRABUF(TIGSP_PRCD)',TRABUF(TIGSP_PRCD),TRABUF(TIGSP_PRCD))
          ENDIF
C----+------------------------------------------------------------------
C    | (O):Decode PRIZE PAYMENT DATE (YYYYMMDD) and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 22,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 23,BUF))
            TRABUF(TIGSP_PPDY) = I4TEMP
            TRABUF(TIGSP_PPDM) = ZEXT (BPRO(OFFSET+  24,BUF))
            TRABUF(TIGSP_PPDD) = ZEXT (BPRO(OFFSET+  25,BUF))
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPS('TRABUF(TIGSP_PPDY)',TRABUF(TIGSP_PPDY),TRABUF(TIGSP_PPDY))
              CALL OPS('TRABUF(TIGSP_PPDM)',TRABUF(TIGSP_PPDM),TRABUF(TIGSP_PPDM))
              CALL OPS('TRABUF(TIGSP_PPDD)',TRABUF(TIGSP_PPDY),TRABUF(TIGSP_PPDD))
          ENDIF
C----+------------------------------------------------------------------
C    | (O):Decode PRIZE PAYMENT TIME (HHMISS) and set it up in TRABUF
C----+------------------------------------------------------------------
            TRABUF(TIGSP_PPTH) = ZEXT (BPRO(OFFSET+ 26,BUF))
            TRABUF(TIGSP_PPTM) = ZEXT (BPRO(OFFSET+ 27,BUF))
            TRABUF(TIGSP_PPTS) = ZEXT (BPRO(OFFSET+ 28,BUF))
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPS('TRABUF(TIGSP_PPTH)',TRABUF(TIGSP_PPTH),TRABUF(TIGSP_PPTH))
              CALL OPS('TRABUF(TIGSP_PPTM)',TRABUF(TIGSP_PPTM),TRABUF(TIGSP_PPTM))
              CALL OPS('TRABUF(TIGSP_PPTS)',TRABUF(TIGSP_PPTS),TRABUF(TIGSP_PPTS))
          ENDIF
C----+------------------------------------------------------------------
C    | (O):Decode TOTAL PRIZE AMOUNT (PAYMENT UNITS) and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 29,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 30,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 31,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 32,BUF))
            TRABUF(TIGSP_TPRZ) = I4TEMP
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPS('TRABUF(TIGSP_TPRZ)',TRABUF(TIGSP_TPRZ),TRABUF(TIGSP_TPRZ))
          ENDIF
C----+------------------------------------------------------------------
C    | (O):Decode TOTAL TAX AMOUNT (PAYMENT UNITS) and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 33,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 34,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 35,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 36,BUF))
            TRABUF(TIGSP_TTAX) = I4TEMP
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPS('TRABUF(TIGSP_TTAX)',TRABUF(TIGSP_TTAX),TRABUF(TIGSP_TTAX))
          ENDIF
C----+------------------------------------------------------------------
C    | (O):Decode NET PRIZE AMOUNT (PAYMENT UNITS) and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 37,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 38,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 39,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 40,BUF))
            TRABUF(TIGSP_NPRZ) = I4TEMP
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPS('TRABUF(TIGSP_NPRZ)',TRABUF(TIGSP_NPRZ),TRABUF(TIGSP_NPRZ))
          ENDIF
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
        IF (MTYPE .EQ. 14 .AND. SUBTYP .EQ. 3) THEN
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

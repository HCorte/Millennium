CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C File      : OIGSTREP.FOR
C Change Log:
C
C Ver Date       Author  Comment
C --- ---------- ------- ----------------------------------------------
C V01 2014.03.19  SCML   Created - PLACARD Project - Creates 
C                        game programme report output messages for IGS;
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      THIS ITEM IS THE PROPERTY OF SCML.
C
C      COPYRIGHT 2014 SCML. ALL RIGHTS RESERVED.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OIGSTREP(WRKBUF, TRABUF, MESS, MES_LEN, BUFNUM, TER, ST)
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
        
        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSTREP:MTYPE  ',MTYPE,MTYPE)
            CALL OPS('OIGSTREP:SUBTYPE',SUBTYP,SUBTYP)
        ENDIF
C----+------------------------------------------------------------------
C    | Handling successful cancellation
C----+------------------------------------------------------------------
        IF(   MTYPE .EQ. 14 .AND. SUBTYP .EQ. 4) THEN
C----+------------------------------------------------------------------
C    | (O):Decode PROGRAMME REPORT DATE (YYYYMMDD) and set it up in TRABUF
C----+------------------------------------------------------------------
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = ZEXT (BPRO(OFFSET+  7,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+  8,BUF))
            TRABUF(TIGSR_PRDY) = I4TEMP
            TRABUF(TIGSR_PRDM) = ZEXT (BPRO(OFFSET+  9,BUF))
            TRABUF(TIGSR_PRDD) = ZEXT (BPRO(OFFSET+ 10,BUF))
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPS('OIGSTREP:TRABUF(TIGSR_PRDY)',TRABUF(TIGSR_PRDY),TRABUF(TIGSR_PRDY))
                CALL OPS('OIGSTREP:TRABUF(TIGSR_PRDM)',TRABUF(TIGSR_PRDM),TRABUF(TIGSR_PRDM))
                CALL OPS('OIGSTREP:TRABUF(TIGSR_PRDD)',TRABUF(TIGSR_PRDD),TRABUF(TIGSR_PRDD))
            ENDIF
C----+------------------------------------------------------------------
C    | (O):Decode PROGRAMME REPORT DATE TIME (HHMISS) and set it up in TRABUF
C----+------------------------------------------------------------------
            TRABUF(TIGSR_PRTH) = ZEXT (BPRO(OFFSET+ 11,BUF))
            TRABUF(TIGSR_PRTM) = ZEXT (BPRO(OFFSET+ 12,BUF))
            TRABUF(TIGSR_PRTS) = ZEXT (BPRO(OFFSET+ 13,BUF))
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPS('OIGSTREP:TRABUF(TIGSR_PRTH)',TRABUF(TIGSR_PRTH),TRABUF(TIGSR_PRTH))
                CALL OPS('OIGSTREP:TRABUF(TIGSR_PRTM)',TRABUF(TIGSR_PRTM),TRABUF(TIGSR_PRTM))
                CALL OPS('OIGSTREP:TRABUF(TIGSR_PRTS)',TRABUF(TIGSR_PRTS),TRABUF(TIGSR_PRTS))
            ENDIF
C----+------------------------------------------------------------------
C    | (O):Decode TOTAL SEGMENTS and set it up in TRABUF (1 byte)
C----+------------------------------------------------------------------
            TRABUF(TIGSR_TSEG) = ZEXT (BPRO(OFFSET+ 14,BUF))
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPS('OIGSTREP:TRABUF(TIGSR_TSEG)',TRABUF(TIGSR_TSEG),TRABUF(TIGSR_TSEG))
            ENDIF
C----+------------------------------------------------------------------
C    | (I/O):Decode SEGMENT NUMBER REQUESTED and set it up in TRABUF (1 byte)
C----+------------------------------------------------------------------
            TRABUF(TIGSR_SEGN) = ZEXT (BPRO(OFFSET+ 15,BUF))
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPS('OIGSTREP:TRABUF(TIGSR_SEGN)',TRABUF(TIGSR_SEGN),TRABUF(TIGSR_SEGN))
            ENDIF
C----+------------------------------------------------------------------
C    | (I/O):Decode MEDIA ID and set it up in TRABUF (8 bytes)
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 16,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 17,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 18,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 19,BUF))
            TRABUF(TIGSR_MVRH) = I4TEMP

            I1TEMP(4) = ZEXT (BPRO(OFFSET+ 20,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET+ 21,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET+ 22,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET+ 23,BUF))
            TRABUF(TIGSR_MVRL) = I4TEMP
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPS('OIGSTREP:TRABUF(TIGSR_MVRH)',TRABUF(TIGSR_MVRH),TRABUF(TIGSR_MVRH))
                CALL OPS('OIGSTREP:TRABUF(TIGSR_MVRL)',TRABUF(TIGSR_MVRL),TRABUF(TIGSR_MVRL))
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
         ! It is not necessary to keep last record of this kind of transaction
C        IF (MTYPE .EQ. 14 .AND. SUBTYP .EQ. 4) THEN
C           IF (TRABUF(TSTAT) .EQ. GOOD) AGTTAB(ALSTRA,TER) = TRABUF(TSER)
C       ENDIF

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

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C File      : OIGSTERR.FOR
C Change Log:
C
C Ver Date       Author  Comment
C --- ---------- ------- ----------------------------------------------
C V01 2014.03.19  SCML   Created - PLACARD Project - Creates 
C                        error output messages for IGS;
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      THIS ITEM IS THE PROPERTY OF SCML.
C
C      COPYRIGHT 2014 SCML. ALL RIGHTS RESERVED.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OIGSTERR(WRKBUF, TRABUF, MESS, MES_LEN, BUFNUM, TER, ST
     *                     , GTYP, GIND, RTYP, RSTYP, SYS_COD, MSG, MSG_TERM
     *                     , XREF)
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
        INTEGER*4 XREF
        
        INTEGER*4 GTYP, GIND, RTYP, RSTYP, SYS_COD
        BYTE SERR_C_DESC(9)
        BYTE B_MSG_TERM(234)
        
        CHARACTER*(*) MSG
        CHARACTER*(*) MSG_TERM
        
        CHARACTER*1 CH
        BYTE BT
        EQUIVALENCE(CH,BT)
        
        INTEGER*4 RETRYIND, ERRORNUM, RETRY, MYCHKSUM
        INTEGER*4  I4TEMP,TEMP1,TEMP2
        INTEGER*2  I2TEMP(2)
        BYTE       I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)


        INTEGER*4 MTYPE, SUBTYP, IND, I, OFFSET

        BUF = BUFNUM
        
        ST = 0

        DO I = 1,9
            CH = MSG(I:I)
            SERR_C_DESC(I) = BT
        ENDDO
        
        ! Initialize message with spaces
        DO I = 1,234
            B_MSG_TERM(I) = '20'X
        ENDDO
        I = 1
        BT = '20'X
        DO WHILE (I .LE. 234 .AND. BT .NE. 0)
            CH = MSG_TERM(I:I)
            IF(BT .NE. 0) THEN
                B_MSG_TERM(I) = BT
            ELSE
                B_MSG_TERM(I) = '20'X
            ENDIF
            I = I + 1
        ENDDO
        
        MES_LEN = ZEXT (HPRO(OUTLEN,BUF))
        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSTERR:BUF',BUF,BUF)
            CALL OPS('OIGSTERR:BUFNUM',BUFNUM,BUFNUM)
            CALL OPS('OIGSTERR:HPRO(OUTLEN,BUF)',ZEXT (HPRO(OUTLEN,BUF)),ZEXT (HPRO(OUTLEN,BUF)))
            CALL OPSTXT(MSG)
            CALL DUMP_MESSAGE(58,-1,SERR_C_DESC,9)
        ENDIF

C        ! Initializing with spaces
C        I1TEMP(1) = ZEXT (32)
C        I1TEMP(2) = ZEXT (32)
C        I1TEMP(3) = ZEXT (32)
C        I1TEMP(4) = ZEXT (32)
C        TRABUF(TIGS_XERR + 0) = I4TEMP
C        TRABUF(TIGS_XERR + 1) = I4TEMP
C        TRABUF(TIGS_XERR + 2) = I4TEMP
C        TRABUF(TIGS_SERR) = 0

C----+------------------------------------------------------------------
C    | Setting XREFNUM field for IGS wager (bytes 5-8, of Millennium header)
C----+------------------------------------------------------------------
        OFFSET = WRKTAB*4-3
        I1TEMP(4) = ZEXT (BPRO(OFFSET+  5,BUF))
        I1TEMP(3) = ZEXT (BPRO(OFFSET+  6,BUF))
        I1TEMP(2) = ZEXT (BPRO(OFFSET+  7,BUF))
        I1TEMP(1) = ZEXT (BPRO(OFFSET+  8,BUF))
        TRABUF(TIGS_XREF) = I4TEMP

        IF(XREF .GE. 0) THEN
            TRABUF(TIGS_XREF) = XREF
        ENDIF
        
        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSTERR:TRABUF(TIGS_XREF)',TRABUF(TIGS_XREF),TRABUF(TIGS_XREF))
            CALL OPS('OIGSTERR:OFFSET',OFFSET,OFFSET)
            CALL OPS('OIGSTERR:WRKTAB*4-3',WRKTAB*4-3,WRKTAB*4-3)
            CALL DUMP_MESSAGE(81,BUF,BPRO(OFFSET,BUF),MES_LEN)
        ENDIF
C----+------------------------------------------------------------------
C    | Setting transaction fields for IGS wager
C----+------------------------------------------------------------------
        OFFSET = WRKTAB*4-3 + 14
C----+------------------------------------------------------------------
C    | Handling error case: update transaction status if returned message is an error
C----+------------------------------------------------------------------
        IF (TRABUF(TSTAT) .EQ. GOOD) THEN
            TRABUF(TSTAT) = REJT
        ENDIF
        IF (TRABUF(TERR) .EQ. NOER) THEN
            TRABUF(TERR) = INVL
        ENDIF

        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSTERR:TRABUF(TSTAT)',TRABUF(TSTAT),TRABUF(TSTAT))
            CALL OPS('OIGSTERR:TRABUF(TERR)',TRABUF(TERR),TRABUF(TERR))
        ENDIF
        ! Recording system code where error occurred
        TRABUF(TIGS_SERR) = SYS_COD ! Millennium
        
        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSTERR:TRABUF(TIGS_SERR)',TRABUF(TIGS_SERR),TRABUF(TIGS_SERR))
        ENDIF
        I1TEMP(1) = ZEXT (SERR_C_DESC(1))
        I1TEMP(2) = ZEXT (SERR_C_DESC(2))
        I1TEMP(3) = ZEXT (SERR_C_DESC(3))
        I1TEMP(4) = ZEXT (SERR_C_DESC(4))
        ! Receiving characters; we must put them in the receiving
        ! order rather than inverting it
        TRABUF(TIGS_XERR + 0) = I4TEMP
        
        I1TEMP(1) = ZEXT (SERR_C_DESC(5))
        I1TEMP(2) = ZEXT (SERR_C_DESC(6))
        I1TEMP(3) = ZEXT (SERR_C_DESC(7))
        I1TEMP(4) = ZEXT (SERR_C_DESC(8))
        TRABUF(TIGS_XERR + 1) = I4TEMP
        
        I1TEMP(1) = ZEXT (SERR_C_DESC(9))
        I1TEMP(2) = ZEXT (32)
        I1TEMP(3) = ZEXT (32)
        I1TEMP(4) = ZEXT (32)
        TRABUF(TIGS_XERR + 2) = I4TEMP

        OFFSET = BOUTTAB 

        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSTERR:TRABUF(TCHK)',TRABUF(TCHK),TRABUF(TCHK))
            CALL OPS('OIGSTERR:TRABUF(TIGS_XERR + 0)',TRABUF(TIGS_XERR + 0),TRABUF(TIGS_XERR + 0))
            CALL OPS('OIGSTERR:TRABUF(TIGS_XERR + 1)',TRABUF(TIGS_XERR + 1),TRABUF(TIGS_XERR + 1))
            CALL OPS('OIGSTERR:TRABUF(TIGS_XERR + 2)',TRABUF(TIGS_XERR + 2),TRABUF(TIGS_XERR + 2))
            CALL OPS('OIGSTERR:OFFSET',OFFSET,OFFSET)
            CALL OPS('OIGSTERR:TRABUF(TSDT1)',TRABUF(TSDT1),TRABUF(TSDT1))
            CALL OPS('OIGSTERR:TRABUF(TSDT2)',TRABUF(TSDT2),TRABUF(TSDT2))
        ENDIF

        ! Building output message: Type/Sub-type
        BPRO(OFFSET+  1,BUF) = 'EF'X; ! Message type: 14, Sub-type: 15
        ! Building output message: Game type
        BPRO(OFFSET+  4,BUF) = MOD(GTYP,256) 
        ! Building output message: Game index
        BPRO(OFFSET+  5,BUF) = MOD(GIND,256) 
        ! Building output message: Request Type/Request Sub-type
        BPRO(OFFSET+  6,BUF) = MOD(RTYP,16) * 16 + MOD(RSTYP,16) 
        ! Building output message: System code
        BPRO(OFFSET+  7,BUF) = MOD(SYS_COD,256) 
        ! Building output message: System Error Code Description
        DO I = 0,8
            BPRO(OFFSET+  8+I,BUF) = SERR_C_DESC(1+I)
        ENDDO
        DO I = 17,251
            BPRO(OFFSET+  I,BUF) = B_MSG_TERM(I-16)
        ENDDO
        MES_LEN = 251 ! 14-byte header plus message length: total = 265 bytes

        I = 251
        BT = BPRO(OFFSET +  I,BUF)
        DO WHILE(I .GT. 17 .AND. (BT .EQ. '00'X .OR. BT .EQ. '20'X))
            I = I - 1
            BT = BPRO(OFFSET +  I,BUF)
        ENDDO
        MES_LEN = I + 1! 14-byte header plus message length: total = 265 bytes
        
C----+------------------------------------------------------------------
C    | Create output message...
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSTERR:BUF',BUF,BUF)
            CALL DUMP_MESSAGE(151,BUF,BPRO(OFFSET,BUF),MES_LEN)
            CALL OPS('OIGSTERR:OFFSET',OFFSET,OFFSET)
            CALL OPS('OIGSTERR:WRKTAB*4-3+14',WRKTAB*4-3+14,WRKTAB*4-3+14)
            CALL OPS('OIGSTERR:MES_LEN',MES_LEN,MES_LEN)
        ENDIF
        
C100     CONTINUE
C        IND = 0
C        DO I=15, MES_LEN
C           BPRO(BOUTTAB+I-15,BUF) = ZEXT (BPRO(WRKTAB*4-3+I,BUF))
C           IND = IND + 1
C           IF(IGSDEBUG(IA_OUTIGS)) THEN
C               CALL OPS('OIGSTERR:B OFF',BOUTTAB+I-15,BOUTTAB+I-15)
C               CALL OPS('OIGSTERR:W OFF',WRKTAB*4-3+I,WRKTAB*4-3+I)
C               CALL OPS('OIGSTERR:BPRO(B)',ZEXT(BPRO(BOUTTAB+I-15,BUF)),ZEXT(BPRO(BOUTTAB+I-15,BUF)))
C           ENDIF
C        ENDDO
        HPRO(OUTLEN,BUF) = MES_LEN 
        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSTERR:HPRO(OUTLEN,BUF)',ZEXT(HPRO(OUTLEN,BUF)),ZEXT(HPRO(OUTLEN,BUF)))
        ENDIF
        
        RETURN
        END

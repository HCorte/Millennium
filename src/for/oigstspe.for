CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C File      : OIGSTSPE.FOR
C Change Log:
C
C Ver Date       Author  Comment
C --- ---------- ------- ----------------------------------------------
C V03 2016.04.15  SCML   M16 PROJECT: set up TRABUF(TSDT7) of financial 
C                        reports with XGames/XSystems availability
C V02 2015.05.26  SCML   Bugfix if ABP returns error message
C V01 2014.02.27  SCML   Created - PLACARD Project - Creates 
C                        special functions' output messages for IGS;
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      THIS ITEM IS THE PROPERTY OF SCML.
C
C      COPYRIGHT 2014 SCML. ALL RIGHTS RESERVED.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OIGSTSPE(WRKBUF, TRABUF, MESS, MES_LEN, BUFNUM, TER, ST)
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
        
        INTEGER*4  MYCHKSUM
        INTEGER*4  I4TEMP,TEMP1,TEMP2
        INTEGER*2  I2TEMP(2)
        BYTE       I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)

        INTEGER*8 I8AUX_AMOUNT
        INTEGER*4 I4AUX_AMOUNT(2)
        EQUIVALENCE(I8AUX_AMOUNT,I4AUX_AMOUNT)

        INTEGER*4 MTYPE, IND, I, OPTIONS, LEN, MES_IND, SUBTYP, OFFSET
        INTEGER*4 ITYPE, ISUBTYPE, OTYPE, OSUBTYPE

        INTEGER*4 REPRINT_TYPE
        
        LOGICAL BYPASS_WRKTAB_COPY
C
        BYTE      GIND                                                          !V03
        INTEGER*4 CLASS                                                         !V03
C----+------------------------------------------------------------------
C V02| Bugfix if ABP returns error message
C----+------------------------------------------------------------------
        LOGICAL BYPASS_EXTRA_FIELD_REMOVAL
        
        BYPASS_EXTRA_FIELD_REMOVAL = .FALSE.
C----+------------------------------------------------------------------
C V02| Bugfix if ABP returns error message
C----+------------------------------------------------------------------
        BYPASS_WRKTAB_COPY = .FALSE.
                
        BUF = BUFNUM
        ST = 0
        
C----+------------------------------------------------------------------
C    | TODO: Check messages for IGS
C----+------------------------------------------------------------------
        MES_LEN = ZEXT (HPRO(OUTLEN,BUF))
        
        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSTSPE:TRABUF(TSFUN)',TRABUF(TSFUN),TRABUF(TSFUN))
            CALL OPS('OIGSTSPE:MES_LEN',MES_LEN,MES_LEN)
            CALL DUMP_MESSAGE(0,64,BPRO(WRKTAB*4-3 + 1,BUF),MES_LEN)
        ENDIF
C----+------------------------------------------------------------------
C    | Handling reprints
C----+------------------------------------------------------------------
        IF (TRABUF(TSFUN) .EQ. TREPR ) THEN
            
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPSTXT('OIGSTSPE:TRABUF(TSFUN) = TREPR')
            ENDIF
            
            !-----------------------------------------------------------
            ! Handling Non-IGS reprints: 
            !   Transaction and output message already built in SPESRV.
            !-----------------------------------------------------------
            IF (TRABUF(TSOLD) .NE. TIGS) RETURN
            
            !-----------------------------------------------------------
            ! Handling IGS reprints
            !-----------------------------------------------------------
            OFFSET = WRKTAB*4-3+14
            MTYPE = ZEXT(BPRO(OFFSET + 2,BUF))
            SUBTYP = MOD(MTYPE,16)
            MTYPE = ISHFT(MTYPE,-4)
            
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPS('OIGSTSPE: REPR: MTYPE ',MTYPE,MTYPE)
                CALL OPS('OIGSTSPE: REPR: SUBTYP',SUBTYP,SUBTYP)
            ENDIF
            !-----------------------------------------------------------
            ! Handling error messages
            !-----------------------------------------------------------
            IF (MTYPE .EQ. 14 .AND. SUBTYP .EQ. 15) THEN
                IF(IGSDEBUG(IA_OUTIGS)) THEN
                    CALL OPSTXT('OIGSTSPE: REPR: Handling error message')
                ENDIF
                TRABUF(TSTAT) = REJT
                TRABUF(TERR)  = INVL
                
                OFFSET = WRKTAB*4-3 ! Millennium header
                
                ! Setting message queue index number (4 bytes)
                I4TEMP = 0
                I1TEMP(4) = ZEXT(BPRO(OFFSET + 5,BUF))
                I1TEMP(3) = ZEXT(BPRO(OFFSET + 6,BUF))
                I1TEMP(2) = ZEXT(BPRO(OFFSET + 7,BUF))
                I1TEMP(1) = ZEXT(BPRO(OFFSET + 8,BUF))
                TRABUF(TSDT3) = I4TEMP
                
                IF(IGSDEBUG(IA_OUTIGS)) THEN
                    CALL OPS('OIGSTSPE: REPR: ERR: TRABUF(TSDT3) =',TRABUF(TSDT3),TRABUF(TSDT3))
                ENDIF
                
                OFFSET = WRKTAB*4-3+14 ! IGS error message header
                
                ! Setting system where error occurred (1 byte)
                TRABUF(TSDT4) = ZEXT(BPRO(OFFSET + 8,BUF))
                IF(IGSDEBUG(IA_OUTIGS)) THEN
                    CALL OPS('OIGSTSPE: REPR: ERR: TRABUF(TSDT4) =',TRABUF(TSDT4),TRABUF(TSDT4))
                ENDIF
                
                ! Setting IGS error code description (9 bytes)
                I4TEMP = 0 
                I1TEMP(1) = ZEXT(BPRO(OFFSET + 9,BUF))
                I1TEMP(2) = ZEXT(BPRO(OFFSET +10,BUF))
                I1TEMP(3) = ZEXT(BPRO(OFFSET +11,BUF))
                I1TEMP(4) = ZEXT(BPRO(OFFSET +12,BUF))
                TRABUF(TSDT5) = I4TEMP ! Bytes 1-4
                 
                I4TEMP = 0 
                I1TEMP(1) = ZEXT(BPRO(OFFSET +13,BUF))
                I1TEMP(2) = ZEXT(BPRO(OFFSET +14,BUF))
                I1TEMP(3) = ZEXT(BPRO(OFFSET +15,BUF))
                I1TEMP(4) = ZEXT(BPRO(OFFSET +16,BUF))
                TRABUF(TSDT6) = I4TEMP ! Bytes 5-8
                
                I4TEMP = 0 
                I1TEMP(1) = ZEXT(BPRO(OFFSET +17,BUF))
                I1TEMP(2) = ZEXT (32)
                I1TEMP(3) = ZEXT (32)
                I1TEMP(4) = ZEXT (32)
                TRABUF(TSDT7) = I4TEMP ! Byte 9
                IF(IGSDEBUG(IA_OUTIGS)) THEN
                    CALL OPS('OIGSTSPE: REPR: ERR: TRABUF(TSDT5) =',TRABUF(TSDT5),TRABUF(TSDT5))
                    CALL OPS('OIGSTSPE: REPR: ERR: TRABUF(TSDT6) =',TRABUF(TSDT6),TRABUF(TSDT6))
                    CALL OPS('OIGSTSPE: REPR: ERR: TRABUF(TSDT7) =',TRABUF(TSDT7),TRABUF(TSDT7))
                ENDIF
            ENDIF
            !-----------------------------------------------------------
            ! Handling reprints
            !-----------------------------------------------------------
            IF (MTYPE .EQ. 8) THEN
                OFFSET = WRKTAB*4-3 ! Millennium header
                
                ! Setting message queue index number (4 bytes)
                I4TEMP = 0
                I1TEMP(4) = ZEXT(BPRO(OFFSET + 5,BUF))
                I1TEMP(3) = ZEXT(BPRO(OFFSET + 6,BUF))
                I1TEMP(2) = ZEXT(BPRO(OFFSET + 7,BUF))
                I1TEMP(1) = ZEXT(BPRO(OFFSET + 8,BUF))
                TRABUF(TSDT3) = I4TEMP
                
                
                ! IGS error message header
                REPRINT_TYPE = ZEXT(BPRO(OFFSET +21, BUF))

                ! Setting reprint type (1 byte)
                TRABUF(TSDT8) = REPRINT_TYPE
                
                IF(IGSDEBUG(IA_OUTIGS)) THEN
                    CALL OPS('OIGSTSPE: REPR: TRABUF(TSDT3) =',TRABUF(TSDT3),TRABUF(TSDT3))
                    CALL OPS('OIGSTSPE: REPR: TRABUF(TSDT4) =',TRABUF(TSDT4),TRABUF(TSDT4))
                    CALL OPS('OIGSTSPE: REPR: TRABUF(TSDT5) =',TRABUF(TSDT5),TRABUF(TSDT5))
                    CALL OPS('OIGSTSPE: REPR: TRABUF(TSDT6) =',TRABUF(TSDT6),TRABUF(TSDT6))
                    CALL OPS('OIGSTSPE: REPR: TRABUF(TSDT7) =',TRABUF(TSDT7),TRABUF(TSDT7))
                    CALL OPS('OIGSTSPE: REPR: TRABUF(TSDT8) =',TRABUF(TSDT8),TRABUF(TSDT8))
                ENDIF
                ! Handling wager reprints
                IF(REPRINT_TYPE .EQ. 1) THEN
                    OFFSET = (WRKTAB*4-3) + 21 ! For wager reprints
                    ! Setting TRX REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY)
                    TRABUF(TSDT9)  = ZEXT(BPRO(OFFSET + 1,BUF))
                    ! Setting TRX REFERENCE DATE MONTH (MM)
                    TRABUF(TSDT10) = ZEXT(BPRO(OFFSET + 2,BUF))
                    ! Setting TRX REFERENCE DATE DAY (DD)
                    TRABUF(TSDT11) = ZEXT(BPRO(OFFSET + 3,BUF))
                    ! Setting TRX REFERENCE GAME
                    TRABUF(TSDT12) = ZEXT(BPRO(OFFSET + 4,BUF))
                    ! Setting TRX REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
                    I4TEMP = 0
                    I1TEMP(4) = ZEXT(BPRO(OFFSET + 6,BUF))
                    I1TEMP(3) = ZEXT(BPRO(OFFSET + 7,BUF))
                    I1TEMP(2) = ZEXT(BPRO(OFFSET + 8,BUF))
                    I1TEMP(1) = ZEXT(BPRO(OFFSET + 9,BUF))
                    TRABUF(TSDT13) = I4TEMP
                    ! Setting TRX REFERENCE SERIAL NUMBER (HIGH ONE BYTE)
                    TRABUF(TSDT14) = ZEXT(BPRO(OFFSET + 5,BUF))
                    ! Setting TRX REFERENCE CHECK DIGITS
                    I4TEMP = 0
                    I1TEMP(2) = ZEXT(BPRO(OFFSET +10,BUF))
                    I1TEMP(1) = ZEXT(BPRO(OFFSET +11,BUF))
                    TRABUF(TSDT15) = I4TEMP
                    IF(IGSDEBUG(IA_OUTIGS)) THEN
                        CALL OPS('OIGSTSPE: REPR: WAG: TRABUF(TSDT9 ) =',TRABUF(TSDT9),TRABUF(TSDT9))
                        CALL OPS('OIGSTSPE: REPR: WAG: TRABUF(TSDT10) =',TRABUF(TSDT10),TRABUF(TSDT10))
                        CALL OPS('OIGSTSPE: REPR: WAG: TRABUF(TSDT11) =',TRABUF(TSDT11),TRABUF(TSDT11))
                        CALL OPS('OIGSTSPE: REPR: WAG: TRABUF(TSDT12) =',TRABUF(TSDT12),TRABUF(TSDT12))
                        CALL OPS('OIGSTSPE: REPR: WAG: TRABUF(TSDT13) =',TRABUF(TSDT13),TRABUF(TSDT13))
                        CALL OPS('OIGSTSPE: REPR: WAG: TRABUF(TSDT14) =',TRABUF(TSDT14),TRABUF(TSDT14))
                        CALL OPS('OIGSTSPE: REPR: WAG: TRABUF(TSDT15) =',TRABUF(TSDT15),TRABUF(TSDT15))
                    ENDIF
                ! Handling payment reprints
                ELSEIF(REPRINT_TYPE .EQ. 2) THEN
C                    OFFSET = (WRKTAB*4-3) + 21 ! For payment reprints - without NIF
                    OFFSET = (WRKTAB*4-3) + 25 ! For payment reprints - with NIF
                    ! Setting TRX REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY)
                    TRABUF(TSDT9)  = ZEXT(BPRO(OFFSET + 1,BUF))
                    ! Setting TRX REFERENCE DATE MONTH (MM)
                    TRABUF(TSDT10) = ZEXT(BPRO(OFFSET + 2,BUF))
                    ! Setting TRX REFERENCE DATE DAY (DD)
                    TRABUF(TSDT11) = ZEXT(BPRO(OFFSET + 3,BUF))
                    ! Setting TRX REFERENCE GAME
                    TRABUF(TSDT12) = ZEXT(BPRO(OFFSET + 4,BUF))
                    ! Setting TRX REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
                    I4TEMP = 0
                    I1TEMP(4) = ZEXT(BPRO(OFFSET + 6,BUF))
                    I1TEMP(3) = ZEXT(BPRO(OFFSET + 7,BUF))
                    I1TEMP(2) = ZEXT(BPRO(OFFSET + 8,BUF))
                    I1TEMP(1) = ZEXT(BPRO(OFFSET + 9,BUF))
                    TRABUF(TSDT13) = I4TEMP
                    ! Setting TRX REFERENCE SERIAL NUMBER (HIGH ONE BYTE)
                    TRABUF(TSDT14) = ZEXT(BPRO(OFFSET + 5,BUF))
                    ! Setting TRX REFERENCE CHECK DIGITS
                    I4TEMP = 0
                    I1TEMP(2) = ZEXT(BPRO(OFFSET +10,BUF))
                    I1TEMP(1) = ZEXT(BPRO(OFFSET +11,BUF))
                    TRABUF(TSDT15) = I4TEMP
                    IF(IGSDEBUG(IA_OUTIGS)) THEN
                        CALL OPS('OIGSTSPE: REPR: PAY: TRABUF(TSDT9 ) =',TRABUF(TSDT9),TRABUF(TSDT9))
                        CALL OPS('OIGSTSPE: REPR: PAY: TRABUF(TSDT10) =',TRABUF(TSDT10),TRABUF(TSDT10))
                        CALL OPS('OIGSTSPE: REPR: PAY: TRABUF(TSDT11) =',TRABUF(TSDT11),TRABUF(TSDT11))
                        CALL OPS('OIGSTSPE: REPR: PAY: TRABUF(TSDT12) =',TRABUF(TSDT12),TRABUF(TSDT12))
                        CALL OPS('OIGSTSPE: REPR: PAY: TRABUF(TSDT13) =',TRABUF(TSDT13),TRABUF(TSDT13))
                        CALL OPS('OIGSTSPE: REPR: PAY: TRABUF(TSDT14) =',TRABUF(TSDT14),TRABUF(TSDT14))
                        CALL OPS('OIGSTSPE: REPR: PAY: TRABUF(TSDT15) =',TRABUF(TSDT15),TRABUF(TSDT15))
                    ENDIF
                ! Handling cancel reprints
                ELSEIF(REPRINT_TYPE .EQ. 3) THEN
                    OFFSET = (WRKTAB*4-3) + 21 ! For cancellation reprints
                    ! Setting TRX REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY)
                    TRABUF(TSDT9)  = ZEXT(BPRO(OFFSET + 1,BUF))
                    ! Setting TRX REFERENCE DATE MONTH (MM)
                    TRABUF(TSDT10) = ZEXT(BPRO(OFFSET + 2,BUF))
                    ! Setting TRX REFERENCE DATE DAY (DD)
                    TRABUF(TSDT11) = ZEXT(BPRO(OFFSET + 3,BUF))
                    ! Setting TRX REFERENCE GAME
                    TRABUF(TSDT12) = ZEXT(BPRO(OFFSET + 4,BUF))
                    ! Setting TRX REFERENCE SERIAL NUMBER (LOW FOUR BYTES)
                    I4TEMP = 0
                    I1TEMP(4) = ZEXT(BPRO(OFFSET + 6,BUF))
                    I1TEMP(3) = ZEXT(BPRO(OFFSET + 7,BUF))
                    I1TEMP(2) = ZEXT(BPRO(OFFSET + 8,BUF))
                    I1TEMP(1) = ZEXT(BPRO(OFFSET + 9,BUF))
                    TRABUF(TSDT13) = I4TEMP
                    ! Setting TRX REFERENCE SERIAL NUMBER (HIGH ONE BYTE)
                    TRABUF(TSDT14) = ZEXT(BPRO(OFFSET + 5,BUF))
                    ! Setting TRX REFERENCE CHECK DIGITS
                    I4TEMP = 0
                    I1TEMP(2) = ZEXT(BPRO(OFFSET +10,BUF))
                    I1TEMP(1) = ZEXT(BPRO(OFFSET +11,BUF))
                    TRABUF(TSDT15) = I4TEMP
                    IF(IGSDEBUG(IA_OUTIGS)) THEN
                        CALL OPS('OIGSTSPE: REPR: CAN: TRABUF(TSDT9 ) =',TRABUF(TSDT9),TRABUF(TSDT9))
                        CALL OPS('OIGSTSPE: REPR: CAN: TRABUF(TSDT10) =',TRABUF(TSDT10),TRABUF(TSDT10))
                        CALL OPS('OIGSTSPE: REPR: CAN: TRABUF(TSDT11) =',TRABUF(TSDT11),TRABUF(TSDT11))
                        CALL OPS('OIGSTSPE: REPR: CAN: TRABUF(TSDT12) =',TRABUF(TSDT12),TRABUF(TSDT12))
                        CALL OPS('OIGSTSPE: REPR: CAN: TRABUF(TSDT13) =',TRABUF(TSDT13),TRABUF(TSDT13))
                        CALL OPS('OIGSTSPE: REPR: CAN: TRABUF(TSDT14) =',TRABUF(TSDT14),TRABUF(TSDT14))
                        CALL OPS('OIGSTSPE: REPR: CAN: TRABUF(TSDT15) =',TRABUF(TSDT15),TRABUF(TSDT15))
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
C----+------------------------------------------------------------------
C    | Handling financial reports
C----+------------------------------------------------------------------
        IF (TRABUF(TSFUN) .EQ. TSREP ) THEN
            
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPSTXT('OIGSTSPE:TRABUF(TSFUN) = TSREP')
            ENDIF
            OFFSET = WRKTAB*4-3+14
            MTYPE = ZEXT(BPRO(OFFSET + 2,BUF))
            
            SUBTYP = MOD(MTYPE,16)
            MTYPE = ISHFT(MTYPE,-4)
            
            ITYPE    =     ZEXT(BPRO(BOUTTAB + 1,BUF))/16
            ISUBTYPE = MOD(ZEXT(BPRO(BOUTTAB + 1,BUF)),16)
            OTYPE    = MTYPE
            OSUBTYPE = SUBTYP
            
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPS('OIGSTSPE: FR: ITYPE   ',ITYPE,ITYPE)
                CALL OPS('OIGSTSPE: FR: ISUBTYPE',ISUBTYPE,ISUBTYPE)
                CALL OPS('OIGSTSPE: FR: OTYPE   ',OTYPE,OTYPE)
                CALL OPS('OIGSTSPE: FR: OSUBTYPE',OSUBTYPE,OSUBTYPE)
                CALL OPS('OIGSTSPE: FR: HPRO(OUTLEN,BUF) A',ZEXT(HPRO(OUTLEN,BUF)),ZEXT(HPRO(OUTLEN,BUF)))
            ENDIF
            
            IF(ITYPE .EQ. 6     .AND. ISUBTYPE .EQ. 3) THEN

                BYPASS_WRKTAB_COPY = .TRUE.

                IF(IGSDEBUG(IA_OUTIGS)) THEN
                    CALL OPS('OIGSTSPE: FR: BYPASS_WRKTAB_COPY',ZEXT(BYPASS_WRKTAB_COPY),ZEXT(BYPASS_WRKTAB_COPY))
                ENDIF
                ! Normal case
                IF( OTYPE .EQ. ITYPE .AND. OSUBTYPE .EQ. ISUBTYPE ) THEN
                    ! Copy from WRKTAB first
                    IND = 0
                    DO I = 15, MES_LEN
                        BPRO(BOUTTAB+I-15,BUF) = ZEXT (BPRO(WRKTAB*4-3+I,BUF))
                        IND = IND + 1
                    ENDDO
C----+------------------------------------------------------------------
C V02| Bugfix if ABP returns error message
C----+------------------------------------------------------------------
                ! ABP error message case
                ELSEIF( OTYPE .EQ. 14 .AND. OSUBTYPE .EQ. 15 ) THEN
                    IND = MES_LEN - 14
                    BYPASS_EXTRA_FIELD_REMOVAL = .TRUE.
                ELSE
C----+------------------------------------------------------------------
C V02| Bugfix if ABP returns error message
C----+------------------------------------------------------------------
                    IND = HPRO(INPLEN,BUF)
                ENDIF
                IF(IGSDEBUG(IA_OUTIGS)) THEN
                    CALL OPS('OIGSTSPE: FR: IND (1)',ZEXT(IND),ZEXT(IND))
                ENDIF
                
                
C----+------------------------------------------------------------------
C V02| Bugfix if ABP returns error message
C----+------------------------------------------------------------------
                IF( .NOT. BYPASS_EXTRA_FIELD_REMOVAL ) THEN 
                    ! Remove extra fields
                    DO I = 15, IND
                        BPRO(BOUTTAB + I - 10,BUF) = ZEXT(BPRO(BOUTTAB + I,BUF))
                    ENDDO
                ENDIF
C----+------------------------------------------------------------------
C V02| Bugfix if ABP returns error message
C----+------------------------------------------------------------------
                ! Set new message length
                HPRO(OUTLEN,BUF) = IND - 10
C----+---+-------------+------------------------------------------------
C V03|BEG| M16 PROJECT | SET TRABUF(TSDT7) WITH GAME/SYSTEM AVAILABILITY
C----+---+-------------+------------------------------------------------
                CLASS = MOD(ZEXT(BPRO(BOUTTAB + 7,BUF)),16)
                IF(CLASS.EQ.1) THEN
                  TRABUF(TSDT7) = ISHFT(ZEXT(BPRO(BOUTTAB + 7,BUF)),-4)
                ELSEIF(CLASS.EQ.2 .OR. CLASS.EQ.4 .OR. CLASS.EQ.8) THEN
                  GIND = 0
                  TRABUF(TSDT7) = 0
                  GIND = BPRO(BOUTTAB + 19,BUF)                                 !EM GAME INDEX
                  IF(GIND.EQ.'0F'X) TRABUF(TSDT7) = TRABUF(TSDT7) + 1
                  GIND = ZEXT(BPRO(BOUTTAB + 21,BUF))                           !SM GAME INDEX
                  IF(GIND.EQ.'0F'X) TRABUF(TSDT7) = TRABUF(TSDT7) + 2
                  GIND = ZEXT(BPRO(BOUTTAB + 23,BUF))                           !PLACARD GAME INDEX
                  IF(GIND.EQ.'0F'X) TRABUF(TSDT7) = TRABUF(TSDT7) + 4
                ENDIF
C----+---+-------------+------------------------------------------------
C V03|END| M16 PROJECT | SET TRABUF(TSDT7) WITH GAME/SYSTEM AVAILABILITY
C----+---+-------------+------------------------------------------------
            ENDIF
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPS('OIGSTSPE: FR: HPRO(OUTLEN,BUF) B',ZEXT(HPRO(OUTLEN,BUF)),ZEXT(HPRO(OUTLEN,BUF)))
            ENDIF
        ENDIF
C----+------------------------------------------------------------------
C    | Handling system down and time-out situations:
C    | when changing this, please also check the following files:
C    |    a) OIGSDOWN.FOR
C    |    b) OIGSTOUT.FOR
C----+------------------------------------------------------------------
        IF (  (ZEXT(BPRO(WRKTAB*4-3+16,BUF)) .EQ. 99) 
     *  .AND.     ((HPRO(REMSTS,BUF) .EQ. RMDOWN) 
     *        .OR. (HPRO(REMSTS,BUF) .EQ. RMTMOT))
     *  ) THEN
            IF (ZEXT(BPRO(WRKTAB*4-3+22,BUF)) .EQ. 1) THEN
                BPRO(WRKTAB*4-3+22,BUF) = 17
            ENDIF
        ENDIF
C----+------------------------------------------------------------------
C    | Create output message...
C----+------------------------------------------------------------------
        IND = 0

        IF(IGSDEBUG(IA_OUTIGS)) THEN
           CALL OPSTXT('OIGSTSPE: ANTE')
           CALL DUMP_MESSAGE(354,BUF,BPRO(BINPTAB,BUF),HPRO(INPLEN,BUF))
        ENDIF

        IF(BYPASS_WRKTAB_COPY .EQ. .FALSE.) THEN
            DO I=15, MES_LEN
                BPRO(BOUTTAB+I-15,BUF) = ZEXT (BPRO(WRKTAB*4-3+I,BUF))
                IND = IND + 1
            ENDDO
        ENDIF
        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSTSPE: FR: IND (2)',ZEXT(IND),ZEXT(IND))
        ENDIF

        IF(IGSDEBUG(IA_OUTIGS)) THEN
           CALL OPSTXT('OIGSTSPE: POST')
           CALL DUMP_MESSAGE(354,BUF,BPRO(BINPTAB,BUF),HPRO(INPLEN,BUF))
        ENDIF

        I = 5
        IF (  TRABUF(TSTAT) .NE. REJT 
     *  .AND. TRABUF(TSFUN) .EQ. TSREP 
     *  .AND. ITYPE .EQ. 6
     *  ) THEN
            CALL PUTIME(TRABUF(TTIM), BPRO(BOUTTAB,BUF), I)
        ENDIF
        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSTSPE: FR: HPRO(OUTLEN,BUF) C',ZEXT(HPRO(OUTLEN,BUF)),ZEXT(HPRO(OUTLEN,BUF)))
        ENDIF
        IF(BYPASS_WRKTAB_COPY .EQ. .FALSE.) THEN
            HPRO(OUTLEN,BUF) = IND
        ENDIF
        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSTSPE: FR: HPRO(OUTLEN,BUF) D',ZEXT(HPRO(OUTLEN,BUF)),ZEXT(HPRO(OUTLEN,BUF)))
        ENDIF
        I4CCITT   = TRABUF(TCHK)                         
        BPRO(BOUTTAB+2,BUF) = I1CCITT(2)                 
        BPRO(BOUTTAB+3,BUF) = I1CCITT(1)                 
        CALL GETCCITT(BPRO(BOUTTAB,BUF),1,HPRO(OUTLEN,BUF),MYCHKSUM)
        I4CCITT   = MYCHKSUM                             
        BPRO(BOUTTAB+2,BUF) = I1CCITT(2)                 
        BPRO(BOUTTAB+3,BUF) = I1CCITT(1)                 
        
        RETURN
        END

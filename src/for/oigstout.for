CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C File      : OIGSTOUT.FOR
C Change Log:
C
C Ver Date       Author  Comment
C --- ---------- ------- ----------------------------------------------
C V02 2015.04.30  SCML   Bugfix for timeout error
C V01 2014.02.27  SCML   Created - PLACARD Project - Creates timeout
C                        output messages for IGS;
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      THIS ITEM IS THE PROPERTY OF SCML.
C
C      COPYRIGHT 2014 SCML. ALL RIGHTS RESERVED.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OIGSTOUT(WRKBUF, TRABUF, MESS, MES_LEN, BUFNUM, TER, ST)
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
        
        INTEGER*4 I
        
        INTEGER*4  I4TEMP,TEMP1,TEMP2
        INTEGER*2  I2TEMP(2)
        BYTE       I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
        

        INTEGER*4 MTYPE,SUBTYP, OFFSET, STATUS
        INTEGER*4 ITYPE, ISUBTYPE, OTYPE, OSUBTYPE
        
        LOGICAL BYPASS_ERROR_MSG_GENERATION
        
        
        INTEGER*4 IND
        
        BUF = BUFNUM
        ST = 0


        OFFSET = WRKTAB*4-3+14
        MTYPE = ZEXT(BPRO(OFFSET + 2,BUF))
        SUBTYP = MOD(MTYPE,16)
        MTYPE = ISHFT(MTYPE,-4)
        
        ITYPE    =     ZEXT(BPRO(BOUTTAB + 1,BUF))/16
        ISUBTYPE = MOD(ZEXT(BPRO(BOUTTAB + 1,BUF)),16)
        OTYPE    = MTYPE
        OSUBTYPE = SUBTYP

        
C----+------------------------------------------------------------------
C    | If transaction is special function and not a game results and
C    | also not an accountability report then send a response to Altura
C    | with what we have
C----+------------------------------------------------------------------

        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSTOUT: ITYPE   ',ITYPE,ITYPE)
            CALL OPS('OIGSTOUT: ISUBTYPE',ISUBTYPE,ISUBTYPE)
            CALL OPS('OIGSTOUT: OTYPE   ',OTYPE,OTYPE)
            CALL OPS('OIGSTOUT: OSUBTYPE',OSUBTYPE,OSUBTYPE)
        ENDIF

        IF(ITYPE .EQ. 6     .AND. ISUBTYPE .EQ. 3) THEN
            OFFSET = WRKTAB*4-3+14
            BYPASS_ERROR_MSG_GENERATION = .TRUE.

            IND = HPRO(OUTLEN,BUF)
            
            IF(OTYPE .EQ. 6 .AND. OSUBTYPE .EQ. 3) THEN
                DO I = 1, IND
                    BPRO(BOUTTAB + I - 1,BUF) = ZEXT(BPRO(OFFSET + I,BUF))
                ENDDO
                
                ! Remove extra fields
                DO I = 15, IND
                    BPRO(BOUTTAB + I - 10,BUF) = ZEXT(BPRO(BOUTTAB + I,BUF))
                ENDDO
            ENDIF

            I = 5
            IF (  TRABUF(TSTAT) .NE. REJT 
     *      .AND. TRABUF(TSFUN) .EQ. TSREP 
     *      .AND. ITYPE .EQ. 6
     *      ) THEN
                CALL PUTIME(TRABUF(TTIM), BPRO(BOUTTAB,BUF), I)
            ENDIF
            
            ! Set new message length
C----+------------------------------------------------------------------
C V02| Bugfix for timeout error
C----+------------------------------------------------------------------
C           HPRO(OUTLEN,BUF) = IND - 10 - 14 - 14
            HPRO(OUTLEN,BUF) = IND - 10 - 14
C----+------------------------------------------------------------------
C V02| Bugfix for timeout error
C----+------------------------------------------------------------------
        ELSE
            OFFSET = WRKTAB*4-3
            !!! Create message error
            I1TEMP(4) = ZEXT (BPRO(OFFSET +  5,BUF))
            I1TEMP(3) = ZEXT (BPRO(OFFSET +  6,BUF))
            I1TEMP(2) = ZEXT (BPRO(OFFSET +  7,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET +  8,BUF))
            TRABUF(TIGS_XREF) = I4TEMP     
            
            
C           MTYPE = ZEXT(BPRO(WRKTAB*4-3+16,BUF))
C           SUBTYP = IAND(MTYPE,15)
C           MTYPE = ISHFT(MTYPE,-4)
            MTYPE  =     ZEXT(BPRO(BINPTAB+1,BUF))/16
            SUBTYP = MOD(ZEXT(BPRO(BINPTAB+1,BUF)),16)
            
            IND = 0 
    
            TRABUF(TSTAT) = REJT
            TRABUF(TERR) = BCRS
            
            MESS(2) = TEIGS
            MESS(3) = 3
            MESS(4) = BUF
            ! Inserting terminal number
            I4TEMP = 0
            I1TEMP(2) = ZEXT (BPRO(OFFSET + 13,BUF))
            I1TEMP(1) = ZEXT (BPRO(OFFSET + 14,BUF))
            MESS(5) = I4TEMP
            ! Inserting msg type/sub-type
            I4TEMP = 0
            I1TEMP(1) = ZEXT (BPRO(OFFSET + 16,BUF))
            MESS(6) = I4TEMP
            ! Inserting transaction serial #
            MESS(7) = TRABUF(TSER)
            CALL QUEMES(MESS)
        ENDIF

        IF(BYPASS_ERROR_MSG_GENERATION .EQ. .FALSE.) THEN
            CALL OIGSTERR(WRKBUF, TRABUF, PRO(OUTTAB,BUF), MES_LEN
     *                    , BUF, TER, STATUS
     *                    , TRABUF(TGAMTYP)
     *                    , TRABUF(TGAMIND)
     *                    , MTYPE
     *                    , SUBTYP
     *                    , IGS_MILSYS
     *                    , 'MIL-00005'    ! System dormant 
C     *                    , 'Sistema central n„o disponÌvel.'
     *                    , 'Sistema central n√£o dispon√≠vel.' ! UTF-8 Encoded
     *                    , 0) ! Don't use XREF from WRKTAB
            CALL OPSTXT('TIME OUT MESSAGE') 
        ENDIF

C        AGTTAB(ALSTRA,TER) = TRABUF(TSER)
        
        RETURN
        END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C File      : OIGSNOTON.FOR
C Change Log:
C
C Ver Date       Author  Comment
C --- ---------- ------- ----------------------------------------------
C V01 2014.05.20  SCML   Created - PLACARD Project - Creates not sign-on error 
C                        output messages for IGS;
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      THIS ITEM IS THE PROPERTY OF SCML.
C
C      COPYRIGHT 2014 SCML. ALL RIGHTS RESERVED.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OIGSNOTON(WRKBUF, TRABUF, MESS, MES_LEN, BUFNUM, TER, ST)
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
        INTEGER*4 TER, I
        
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
        
C----+------------------------------------------------------------------
C    | If transaction is special function and not a game results and
C    | also not an accountability report then send a response to Altura
C    | with what we have
C----+------------------------------------------------------------------
        
        OFFSET = WRKTAB*4-3
        !!! Create message error
        I1TEMP(4) = ZEXT (BPRO(OFFSET +  5,BUF))
        I1TEMP(3) = ZEXT (BPRO(OFFSET +  6,BUF))
        I1TEMP(2) = ZEXT (BPRO(OFFSET +  7,BUF))
        I1TEMP(1) = ZEXT (BPRO(OFFSET +  8,BUF))
        TRABUF(TIGS_XREF) = I4TEMP     
        
        OFFSET = WRKTAB*4-3+14
        MTYPE = ZEXT(BPRO(OFFSET + 2,BUF))
        SUBTYP = MOD(MTYPE,16)
        MTYPE = ISHFT(MTYPE,-4)
        
        ITYPE    =     ZEXT(BPRO(BOUTTAB + 1,BUF))/16
        ISUBTYPE = MOD(ZEXT(BPRO(BOUTTAB + 1,BUF)),16)
        OTYPE    = MTYPE
        OSUBTYPE = SUBTYP

        MTYPE  =     ZEXT(BPRO(BINPTAB+1,BUF))/16
        SUBTYP = MOD(ZEXT(BPRO(BINPTAB+1,BUF)),16)
        
        IND = 0 

        TRABUF(TSTAT) = REJT
        TRABUF(TERR) = NOTON
        
        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPS('OIGSNOTON: ITYPE   ',ITYPE,ITYPE)
            CALL OPS('OIGSNOTON: ISUBTYPE',ISUBTYPE,ISUBTYPE)
            CALL OPS('OIGSNOTON: OTYPE   ',OTYPE,OTYPE)
            CALL OPS('OIGSNOTON: OSUBTYPE',OSUBTYPE,OSUBTYPE)
        ENDIF
        
        IF(ITYPE .EQ. 6     .AND. ISUBTYPE .EQ. 3) THEN
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
            ! Set new message length
            HPRO(OUTLEN,BUF) = IND - 10 - 14
        ENDIF
        
        IF(BYPASS_ERROR_MSG_GENERATION .EQ. .FALSE.) THEN
            CALL OIGSTERR(WRKBUF, TRABUF, PRO(OUTTAB,BUF), MES_LEN
     *                    , BUF, TER, STATUS
     *                    , TRABUF(TGAMTYP)
     *                    , TRABUF(TGAMIND)
     *                    , MTYPE
     *                    , SUBTYP
     *                    , IGS_MILSYS
     *                    , 'MIL-00004' ! Not sign-on error
C     *                    , 'Mediador n„o est· sign-on.')
     *                    , 'Mediador n√£o est√° sign-on.' ! UTF-8 Encoded
     *                    , 0 ) ! Don't use XREF from WRKTAB
     
            CALL OPSTXT('NOT SIGN ON ERROR')
        ENDIF

C        AGTTAB(ALSTRA,TER) = TRABUF(TSER)
        
        RETURN
        END

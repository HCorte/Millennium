C
C V02 03-SEP-2010 MAC RFSS0145 - ASFIV FILE ADDED
C V01 19-APR-2010 RXK INITIAL RELEASE
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
C Copyright 2010 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE SALINV_PAS (TRABUF,MESTAB,OUTLEN)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SPECOM.DEF'

        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'

        ! arguments
        BYTE       MESTAB(*)           
        INTEGER*2  OUTLEN              

        ! variables
        INTEGER*4  RTER,AGT,ST,IND,CHKLEN,MYCHKSUM
        INTEGER*4  MESS(EDLEN)
        INTEGER*4  ERRTYP /Z90/
        LOGICAL    PRIV

        INTEGER*4  I4TEMP                    
        INTEGER*2  I2TEMP(2)                 
        BYTE       I1TEMP(4)                 
        EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)


        RTER=0
        TRABUF(TSDT1) = ZEXT( MESTAB(5) )
        TRABUF(TSDT2) = ZEXT( MESTAB(6) )
C
C CHECK IF INVOICE REPORTS ARE SUPRESSED
C
        IF(TSBIT(P(SUPRPT),INVREP)) THEN
           TRABUF(TERR)=SUPR
           GOTO 8000
        ENDIF
C
C GET REPORT TYPE AND PASS NUMBER
C
        !REPTYP = ZEXT( MESTAB(7) )   !NOT USED

        !I4TEMP=0
        !I1TEMP(2)=MESTAB(8)
        !I1TEMP(1)=MESTAB(9)
        !REPPNUM=I4TEMP               !NOT USED
C
C AGENT NUMBER TO REPORT ON
C
        PRIV = .FALSE.
        IF(TSBIT(AGTTAB(AGTTYP,TRABUF(TTER)),AGTPRV)) PRIV=.TRUE.

        IF(PRIV) THEN
           CALL TERM_TO_HOST(MESTAB(10), AGT, 4)
           IF(AGT.LE.0) THEN
              RTER = TRABUF(TTER)
           ELSE
              DO RTER = 1, NUMAGT
                 IF(AGT.EQ.AGTTAB(AGTNUM,RTER)) GOTO 1000
              ENDDO
              TRABUF(TERR) = INVL
              GOTO 8000  
           ENDIF
        ELSE
           RTER = TRABUF(TTER)
        ENDIF 

1000    CONTINUE   
        TRABUF(TSDT4)=RTER
C
C SEGMENT NUMBER
C
        !SEGNO = ZEXT(MESTAB(14))   !NOT USED
C
C READ ASF FOR PASSIVE ACCOUNTING REPORT
C
        IF(P(SUPFIL).EQ.1) THEN
           TRABUF(TERR)=SUPR
           GOTO 8000
        ENDIF

        CALL READW(ASFFDB,RTER,ASFREC,ST)
        IF(ST.NE.0) THEN
           MESS(1)=SPE
           MESS(2)=TEGEN
           MESS(3)=4
           CALL FASTMOV(SFNAMES(1,ASF),MESS(4),5)
           MESS(9)=RTER
           CALL QUEMES(MESS)
           GOTO 8000
        ENDIF
C
        CALL READW(ASFIVFDB,RTER,ASFIVREC,ST)                     !V02...
        IF(ST.NE.0) THEN
           MESS(1)=SPE
           MESS(2)=TEGEN
           MESS(3)=4
           CALL FASTMOV(SFNAMES(1,ASFIV),MESS(4),5)
           MESS(9)=RTER
           CALL QUEMES(MESS)
           GOTO 8000
        ENDIF                                                     !...V02
C
C PUT TIME (hours, mins, secs)
C
        IND = 5
        CALL PUTIME(TRABUF(TTIM), MESTAB, IND)
C
C PUT WEEK AND YEAR                                               !V02...
C
        MESTAB(IND) = ASFIVWEEK
        IND=IND+1
        MESTAB(IND) = 0
        MESTAB(IND+1) = MOD(ASFIVYEAR,100)                        !...V02
        IND=IND+2
C
C PUT INVOICE CDC
C
        I4TEMP = ASFINV(ASFEND,1)                                 !V02
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
C PUT AGENT NUMBER
C 
        I4TEMP = AGTTAB(AGTNUM,RTER)
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
C FIRM RESULT TYPE                                                !V02...
C
        I4TEMP = ASFIV_F_SGN
        MESTAB(IND+0) = I1TEMP(1)
        IND=IND+1
C
        IF (CHR_F_SGN(1) .EQ. 'Z') GOTO 2000
C
C PUT FINANCIAL DATA
C
        I4TEMP = ASFIV_F_INVAMT 
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP = ASFIV_F_TRAMT
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP = ASFIV_F_CNT_ON_SAL
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
        I4TEMP = ASFIV_F_AMT_ON_SAL
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP =  ASFIV_F_CNT_OF_SAL
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
        I4TEMP =  ASFIV_F_AMT_OF_SAL
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP =  ASFIV_F_CNT_RET_D
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
        I4TEMP = ASFIV_F_AMT_RET
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP = ASFIV_F_ON_COM
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP =  ASFIV_F_OF_COM
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP =  ASFIV_F_LST_BAL
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP =  ASFIV_F_PAID
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP = ASFIV_F_ACC_PAID
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
        I4TEMP =  ASFIV_F_AMT_PAID
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP =  ASFIV_F_AMT_DIF
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP = ASFIV_F_CNT_RET_A
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
        I4TEMP =  ASFIV_F_CODE
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
2000    CONTINUE
C
C SHOP RESULT TYPE
C
        I4TEMP = ASFIV_S_SGN
        MESTAB(IND+0) = I1TEMP(1)
        IND=IND+1
C
        IF (CH_S_SGN(1) .EQ. 'Z') GOTO 3000
C
C PUT FINANCIAL DATA
C
        I4TEMP = ASFIV_S_INVAMT 
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP = ASFIV_S_CNT_ON_SAL
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
        I4TEMP = ASFIV_S_AMT_ON_SAL
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP =  ASFIV_S_CNT_OF_SAL
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
        I4TEMP =  ASFIV_S_AMT_OF_SAL
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP =  ASFIV_S_CNT_RET_D
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
        I4TEMP = ASFIV_S_AMT_RET
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP = ASFIV_S_ON_COM
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP =  ASFIV_S_OF_COM
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP =  ASFIV_S_LST_BAL
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP =  ASFIV_S_PAID
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP = ASFIV_S_ACC_PAID
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
        I4TEMP =  ASFIV_S_AMT_PAID
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP =  ASFIV_S_AMT_DIF
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
        I4TEMP = ASFIV_S_CNT_RET_A
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
        I4TEMP = ASFIV_S_CODE
        MESTAB(IND+0) = I1TEMP(2)
        MESTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
3000    CONTINUE                                                    !...V02
C
        OUTLEN = IND-1
        IF(TRABUF(TERR).EQ.NOER) GOTO 9000
C
C ERROR IN REPORT REQUEST FROM TERMINAL
C
8000    CONTINUE
        TRABUF(TSTAT)=REJT
        MESTAB(2) = ERRTYP
        MESTAB(5) = TRABUF(TERR)
        MESTAB(6) = 0
        OUTLEN=6
C
C CALCULATE CHECKSUM FOR MESSAGE BACK TO TERMINAL
C
9000    CONTINUE
        I4CCITT = TRABUF(TCHK)
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
        CHKLEN=OUTLEN-1
        CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
C
        RETURN
C
        END

C
C V03 13-OCT-2010 MAC OLD VERSION PROCEDURE
C V02 11-SEP-2010 RXK SEQUENCE OF SHOP AND FIRM FIXED.
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
        SUBROUTINE SALINV_PAS_OLD(TRABUF,MESTAB,OUTLEN)         !V03
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
        INTEGER*4  GTYP,GIND,GNUM,NETSAL,SCOM(2)
        INTEGER*4  MESS(EDLEN)
        INTEGER*4  ERRTYP /Z90/
        LOGICAL    PRIV

        INTEGER*4  I4TEMP                    
        INTEGER*2  I2TEMP(2)                 
        BYTE       I1TEMP(4)                 
        EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
C
        INTEGER*4  INV_CDC, WEKNO, YEARIN                             !V03
C
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
C PUT TIME (hours, mins, secs)
C
        IND = 5
        CALL PUTIME(TRABUF(TTIM), MESTAB, IND)
C
        INV_CDC=ASFINV(ASFEND,1)                                  !V03...
        CALL FIGWEK(INV_CDC,WEKNO,YEARIN)
        MESTAB(IND) = WEKNO
        IND=IND+1
        MESTAB(IND) = 0
        MESTAB(IND+1) = MOD(YEARIN,100)                           !...V03
        IND=IND+2
C
C PUT INVOICE CDC
C
        I4TEMP = ASFINV(ASFEND,1)                                 !V03
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
C PUT FINANCIAL DATA 
C
        GTYP = TPAS 
        DO GIND = 1,NUMPAS
           GNUM = GTNTAB(TPAS,GIND)         
C
C CANCEL AMOUNT
C
           I4TEMP = ASFBIL(GCAMT,GNUM,1)       
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND=IND+4
C
C NET SALES AMOUNT
C
           NETSAL = ASFBIL(GSAMT,GNUM,1) - ASFBIL(GCAMT,GNUM,1)            
           I4TEMP = NETSAL
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND=IND+4
C
C CASH AMOUNT
C
           I4TEMP = ASFBIL(GVAMT,GNUM,1)       
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND=IND+4
C
C COMMISSION AMOUNT
C
           CALL GETCOM(NETSAL,TWAG,GNUM,I4TEMP,SCOM,GTYP,GIND,RTER)
           MESTAB(IND+0) = I1TEMP(4)
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND=IND+4
        ENDDO   
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

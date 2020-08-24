C
C V01 13-APR-2010 RXK Initial Release
C
C BUILD BET IMAGE FOR PASSIVE TRANSACTIONS
C ==============================================================
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINTRA_PABET(TRABUF,CBETS,LINES)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
C
        ! arguments
        INTEGER*4    LINES             !
        CHARACTER*80 CBETS(14)         !
C
        INTEGER*4 I,K,J,QP        
        INTEGER*4 QPTXT(2) /'    ','QP  '/
C
        QP = 1
        IF(TRABUF(TWQPF).NE.0) QP = 2
        I = 1
C
        IF(TRABUF(TWEPOP).EQ.EPASSAL) THEN
           WRITE(CBETS(I),901) QPTXT(QP),
     *           TRABUF(TWEPSN),TRABUF(TWEPSS),TRABUF(TWEPSF)
           I=I+1
        ELSEIF(TRABUF(TWEPOP).EQ.EPASRES) THEN
           DO K = 1,TRABUF(TWEPNR)
              WRITE(CBETS(I),902) TRABUF(TWEPRES1+(K-1)),
     *           (TRABUF(TWEPSER1_1+(K-1)*20+J),
     *           TRABUF(TWEPFRC1_1+(K-1)*20+J) ,J=0,TRABUF(TWEPNFR1+(K-1))-1)
              I=I+1
           ENDDO 
        ELSEIF(TRABUF(TWEPOP).EQ.EPASREL) THEN
           WRITE(CBETS(I),903) (TRABUF(TWEPRES1+(K-1)),K=1,TRABUF(TWEPNR))
           I=I+1 
        ENDIF 
C
        LINES = I
C
        RETURN
C
901     FORMAT(3X,'Sold',2X,A4,1X,I5.5,1X,I2,'+',I1)
902     FORMAT(3X,'Reserved',2X,I5.5,2X,<TRABUF(TWEPNFR1+K-1)>(I2,'+',I1,1X))
903     FORMAT(3X,'Released',2X,<TRABUF(TWEPNR)>(2X,I5.5))
        END

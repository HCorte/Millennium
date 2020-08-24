C
C V01 13-APR-2010 RXK Initial Release
C
C FORMAT PASSIVE BET DATA
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
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PABET(TRABUF,BIMAGE)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

        ! argument
        CHARACTER*56 BIMAGE(12)
        INTEGER*4 I,J,K,M

        I = 2
        IF(TRABUF(TWEPOP).EQ.EPASSAL) THEN
           WRITE(BIMAGE(I),901) 'Sold'
           WRITE(BIMAGE(I+1),902),TRABUF(TWEPSN),TRABUF(TWEPSS),TRABUF(TWEPSF)
        ELSEIF(TRABUF(TWEPOP).EQ.EPASRES) THEN
           IF(TRABUF(TWEPNR).GT.0) THEN
              WRITE(BIMAGE(I),901) 'Reserved'
              I = I + 1 
              DO K = 1,TRABUF(TWEPNR)
                 M = MIN(9,TRABUF(TWEPNFR1+(K-1)))
                 WRITE(BIMAGE(I),903),TRABUF(TWEPRES1+(K-1)),
     *              (TRABUF(TWEPSER1_1+(K-1)*20+J),     
     *               TRABUF(TWEPFRC1_1+(K-1)*20+J) ,J=0,M-1)
                 I = I + 1
                 IF(M.LT.TRABUF(TWEPNFR1+(K-1))) THEN
                    WRITE(BIMAGE(I),9031),
     *                 (TRABUF(TWEPSER1_1+(K-1)*20+J),
     *                 TRABUF(TWEPFRC1_1+(K-1)*20+J),
     *                 J=M,TRABUF(TWEPNFR1+(K-1))-1)
                    I = I + 1
                 ENDIF
              ENDDO
           ENDIF
           I = I + 1 
           WRITE(BIMAGE(I),904),TRABUF(TWEPSD)
           I = I + 1
           IF(TRABUF(TWEPNE).EQ.0) THEN
              WRITE(BIMAGE(I),9051),TRABUF(TWEPNF)   
           ELSE
              WRITE(BIMAGE(I),905),TRABUF(TWEPRM),TRABUF(TWEPNF)   
           ENDIF
        ELSEIF(TRABUF(TWEPOP).EQ.EPASREL) THEN
           WRITE(BIMAGE(I),901) 'Released'
           I = I + 1
           DO K = 1,TRABUF(TWEPNR)
              WRITE(BIMAGE(I),906),TRABUF(TWEPRES1+(K-1))
              I = I + 1
           ENDDO
        ENDIF
        RETURN

901     FORMAT(1X,A)
902     FORMAT(1X,I5.5,1X,I2,'+',I1)
903     FORMAT(1X,I5.5,2X,<M>(I2,'+',I1,1X))
9031    FORMAT(8X,<TRABUF(TWEPNFR1+(K-1))-M>(I2,'+',I1,1X))
904     FORMAT(1X,'Seed:',I8)
905     FORMAT(1X,'Request: Mask ',I5.<TRABUF(TWEPNE)>,',','  # of Fracs ',I2)
9051    FORMAT(1X,'Request: Mask not given,  # of Fracs ',I2)
906     FORMAT(1X,I5.5)
        END 


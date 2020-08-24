C
C V02 31-MAY-2011 FJG Correct OOB
C V01 15-JUL-2010 RXK INITIAL VERSION
C
C SUBROUTINE TO FIND MAXIMUM NUMBER OF AVAILABLE FRACTIONS
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 2010 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK = NOOVERFLOW /EXT
        SUBROUTINE GETMAXAVAIL
        IMPLICIT NONE

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'

        INTEGER*4 MAXAVAILABLE(PMAXSAL,NUMPAS)
        COMMON /MAXAVAIL/ MAXAVAILABLE

        INTEGER*4 DMAX
C       PARAMETER(DMAX=PMAXSERCLA*PMAXFRACLA)  !=50 
        PARAMETER(DMAX=PMAXSERCLA*PMAXFRAPOP)  !=60         

        INTEGER*4 MAXNUM(NUMPAS)
        INTEGER*4 AV(DMAX,PMAXSAL,NUMPAS)
        INTEGER*4 TOT(PMAXSAL,NUMPAS)
        INTEGER*4 OVER(PMAXSAL,NUMPAS)
        INTEGER*4 GIND,EMIS,DOFF,XNUM,X

        DO GIND=1,NUMPAS
           MAXNUM(PSBCLA) = PMAXNUMCLA
           MAXNUM(PSBPOP) = PMAXNUMPOP
        ENDDO 
        CALL FASTSET(0,MAXAVAILABLE,PMAXSAL*NUMPAS)

        DO GIND=1,NUMPAS
           DO EMIS = 1,PAGEMI       
              IF(PASSUBSTS(EMIS,GIND).EQ.PDRWWAG) THEN
                 DOFF = PASSALTAB(EMIS,GIND)  
                 IF(DOFF.GE.1.AND.DOFF.LE.PMAXSAL) THEN
                    CALL FASTSET(0,AV,DMAX*PMAXSAL*NUMPAS) 
                    CALL FASTSET(0,OVER,PMAXSAL*NUMPAS)
                    CALL FASTSET(0,TOT,PMAXSAL*NUMPAS)
                    DO XNUM = 1,MAXNUM(GIND)
                       IF(GIND.EQ.PSBCLA) THEN
                          X = PASNUMCLA(XNUM,DOFF).FORSAL
                       ELSE
                          X = PASNUMPOP(XNUM,DOFF).FORSAL
                       ENDIF   
                       IF(X.GT.0.AND.X.LE.DMAX) THEN
                          AV(X,DOFF,GIND) = AV(X,DOFF,GIND) + 1
                          TOT(DOFF,GIND) = TOT(DOFF,GIND) + 1
                          IF(X.GE.PMAXTIC) OVER(DOFF,GIND) = OVER(DOFF,GIND) +1
                       ENDIF
                    ENDDO
                    IF(OVER(DOFF,GIND).GT.0) THEN
                       MAXAVAILABLE(DOFF,GIND) = PMAXTIC
                    ELSEIF(TOT(DOFF,GIND).GT.0) THEN
                       DO X=PMAXTIC-1,1,-1
                          IF(AV(X,DOFF,GIND).GT.0) THEN
                             MAXAVAILABLE(DOFF,GIND) = X
                             GOTO 100
                          ENDIF
                       ENDDO
                    ENDIF 
                 ENDIF
              ENDIF
100           CONTINUE
           ENDDO
        ENDDO 

        RETURN 
        END  

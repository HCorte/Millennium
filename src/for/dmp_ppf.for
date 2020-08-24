C DMP_PPF.FOR
C
C V01 22-AUG-2005 FRP
C 
C SUBROUTINE TO DUMP PASSIVE PLAN FILE
C
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


C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE DMP_PPF(RLU,FILE,PLAN,MONEY_UNIT)
        IMPLICIT NONE


        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DPPREC.DEF'

        ! arguments
        INTEGER*4  RLU
        INTEGER*4  FILE(5)                      !
        INTEGER*4  PLAN                         !
        INTEGER*4  MONEY_UNIT                   !
        

        INTEGER*4 ST
        INTEGER*4 FDB(7)
        INTEGER*4 LUN
        INTEGER*4 I

        INTEGER*4 PAS_ROUND_VALUE

        CHARACTER DESCR(15)*30

        DATA DESCR/
     *            'SHARE VALUE                   ',
     *            'EXTRA SHARE VALUE             ',
     *            'EMISSION PLAN                 ',
     *            'NUMBER OF TICKETS             ',
     *            'BASE PRICE                    ',
     *            'NUMBER OF DIVISIONS           ',
     *            'PLAN TYP (CLAS/POP/EXTRA/SPEC)',
     *            'NUMBER OF WIN. NO.            ',
     *            'SHARES                        ',
     *            'PRIZE TYPE                    ',
     *            '# OF DIGITS                   ',
     *            'CROSS REFERENCE               ',
     *            'EXTRA SHARES                  ',
     *            'LAST CHANGE DATE              ',
     *            'NUMBER OF FRACTIONS           '/


C
C READ PPF FILE
C
        LUN = 9     ! ?
        CALL OPENW(LUN,FILE,4,0,0,ST)
        CALL IOINIT(FDB,LUN,DPPSEC*256)
        IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
        CALL READW(FDB,PLAN,DPPREC,ST)
        IF(ST.NE.0) CALL FILERR(FILE,2,ST,PLAN)
        CALL CLOSEFIL(FDB)
C
C DUMP RECORD
C
        DO 100 I=1,PAGDIV
          WRITE(RLU,906) DPPSHVOFF+I-1, CSMONY(PAS_ROUND_VALUE(DPPSHV(I)),
     *                   12,MONEY_UNIT),'DPPSHV', I, DESCR(1)
100     CONTINUE
        DO 110 I=1,PAGEDV
          WRITE(RLU,906) DPPEXSHVOFF+I-1, CSMONY(PAS_ROUND_VALUE(DPPEXSHV(I)),
     *                   12,MONEY_UNIT),'DPPEXSHV', I, DESCR(2)
110     CONTINUE
        WRITE(RLU,900) DPPPLANOFF, DPPPLAN, 'DPPPLAN', DESCR(3)
        WRITE(RLU,900) DPPNUMTCKOFF, DPPNUMTCK, 'DPPNUMTCK', DESCR(4)
        WRITE(RLU,911) DPPPRCOFF, CSMONY(DPPPRC,12,MONEY_UNIT),
     *              'DPPPRC', DESCR(5)
        WRITE(RLU,900) DPPDIVOFF, DPPDIV, 'DPPDIV', DESCR(6)
        WRITE(RLU,900) DPPPLTOFF, DPPPLT, 'DPPPLT', DESCR(7)
        DO 120 I=1,PAGDIV
          WRITE(RLU,901) DPPWNUMOFF+I-1, DPPWNUM(I), 'DPPWNUM', I, DESCR(8)
120     CONTINUE
        DO 130 I=1,PAGDIV
          WRITE(RLU,901) DPPSHROFF+I-1, DPPSHR(I), 'DPPSHR', I, DESCR(9)
130     CONTINUE
        DO 140 I=1,PAGDIV
          WRITE(RLU,901) DPPTYPOFF+I-1, DPPTYP(I), 'DPPTYP', I, DESCR(10)
140     CONTINUE
        DO 150 I=1,PAGDIV
          WRITE(RLU,901) DPPDIGOFF+I-1, DPPDIG(I), 'DPPDIG', I, DESCR(11)
150     CONTINUE
        DO 160 I=1,PAGDIV
          WRITE(RLU,901) DPPIDNUMOFF+I-1, DPPIDNUM(I), 'DPPIDNUM', I, DESCR(12)
160     CONTINUE
        DO 170 I=1,PAGEDV
          WRITE(RLU,901) DPPEXSHROFF+I-1, DPPEXSHR(I), 'DPPEXSHR', I, DESCR(13)
170     CONTINUE
        WRITE(RLU,900) DPPCHGCDCOFF, DPPCHGCDC, 'DPPCHGCDC', DESCR(14)
        WRITE(RLU,900) DPPNOFFRAOFF, DPPNOFFRA, 'DPPNOFFRA', DESCR(15)
C
        RETURN  
C
C
900     FORMAT(1X,I5,1X,I12,1X,15X,A6,9X,A30)
901     FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,')',5X,A30)
906     FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,')',5X,A30)
911     FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)

C
        END

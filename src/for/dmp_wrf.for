C DMP_WRF.FOR
C
C V01 28-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP ROLL POOL / WIN RESERVE FUND FILE
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
	SUBROUTINE DMP_WRF(RLU,FILE,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECRDF.DEF'

        ! arguments
	INTEGER*4  RLU
	INTEGER*4  FILE(5)			!
	INTEGER*4  MONEY_UNIT			!
	

	INTEGER*4 LUN
	INTEGER*4 ST
	INTEGER*4 FDB(7)
	INTEGER*4 I,J

	CHARACTER DESCR(11)*30

	DATA DESCR/
     *            'TOTAL ROLL POOL (UNITS/CENTS) ',
     *            'LAST AMOUNT ADDED(UNITS/CENTS)',
     *            'LAST AMOUNT USED (UNITS/CENTS)',
     *            'LAST DRAW USED                ',
     *            'LAST DRAW ADDED               ',
     *            'LAST AMOUNT ADDED TO A DRAW   ',
     *            'LAST AMOUNT USED FOR A DRAW   ',
     *            'LAST DRAW ADDED TO            ',
     *            'WINNER RESERVE FUND TABLE     ',
     *            'WINNER RESERVE DRAW           ',
     *            'SPT ROLL POOLS                '/



C
C READ RDF FILE
C
	LUN = 9     ! ?
        CALL OPENW(LUN,FILE,4,0,0,ST)
        CALL IOINIT(FDB,LUN,RDFSEC*256)
        IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
        CALL READW(FDB,1,RDFREC,ST)
        IF(ST.NE.0) CALL FILERR(FILE,2,ST,1)
        CALL CLOSEFIL(FDB)

C
C DUMP RECORD
C
	
	DO 100 I=1,MAXGAM
	  WRITE(RLU,906) RDFPOL_OFF+I-1, CSMONY(RDFPOL(I),12,MONEY_UNIT),
     *                  'RDFPOL', I, DESCR(1)
100	CONTINUE
	DO 110 I=1,MAXGAM
	  WRITE(RLU,906) RDFADD_OFF+I-1, CSMONY(RDFADD(I),12,MONEY_UNIT),
     *                  'RDFADD', I, DESCR(2)
110	CONTINUE
	DO 120 I=1,MAXGAM
	  WRITE(RLU,906) RDFUSE_OFF+I-1, CSMONY(RDFUSE(I),12,MONEY_UNIT),
     *                  'RDFUSE', I, DESCR(3)
120	CONTINUE
	DO 130 I=1,MAXGAM
	  WRITE(RLU,901) RDFUDW_OFF+I-1, RDFUDW(I),
     *                  'RDFUDW', I, DESCR(4)
130	CONTINUE
	DO 140 I=1,MAXGAM
	  WRITE(RLU,901) RDFADW_OFF+I-1, RDFADW(I),
     *                  'RDFADW', I, DESCR(5)
140	CONTINUE
	DO 150 I=1,MAXGAM
	  WRITE(RLU,906) RDFADR_OFF+I-1, CSMONY(RDFADR(I),12,MONEY_UNIT),
     *                  'RDFADR', I, DESCR(6)
150	CONTINUE
	DO 160 I=1,MAXGAM
	  WRITE(RLU,906) RDFUSD_OFF+I-1, CSMONY(RDFUSD(I),12,MONEY_UNIT),
     *                  'RDFUSD', I, DESCR(7)
160	CONTINUE
	DO 170 I=1,MAXGAM
	  WRITE(RLU,901) RDFDRW_OFF+I-1, RDFDRW(I),
     *                  'RDFDRW', I, DESCR(8)
170	CONTINUE
	DO 180 I=1,MAXGAM
	  DO J=1,10
	    WRITE(RLU,916) RDF_WRFTAB_OFF+(I-1)*10+J-1,
     *                     CSMONY(RDF_WRFTAB(J,I),12,MONEY_UNIT),
     *                    'RDF_WRFTAB', J, I, DESCR(9)
	  ENDDO
180	CONTINUE
	DO 190 I=1,MAXGAM
	  WRITE(RLU,917) RDF_WRFCUD_OFF+I-1, RDF_WRFCUD(I),
     *                  'RDF_WRFCUD', I, DESCR(10)
190	CONTINUE
	DO 200 I=1,NUMSPT
	  DO J=1,SPGDIV
	    WRITE(RLU,918) RDF_SPTPOLDIV_OFF+(I-1)*SPGDIV+J-1,
     *                     CSMONY(RDF_SPTPOLDIV(I,J),12,MONEY_UNIT),
     *                    'RDF_SPTPOLDIV', I, J, DESCR(11)
	  ENDDO
200	CONTINUE



	RETURN  
C
C
900	FORMAT(1X,I5,1X,I12,1X,15X,A6,9X,A30)
901	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,')',5X,A30)
902	FORMAT(1X,I5,1X,I12,1X,I12,3X,A6,'(',I2,',*)',3X,A30)
903	FORMAT(1X,I5,1X,I12,1X,12X,3X,A6,'(',I2,',',I2,')',2X,A30)
904	FORMAT(1X,I5,1X,I12,1X,12X,3X,A6,'(',I2,',',I2,',',I2,')',1X,A28)
905	FORMAT(1X,I5,1X,I12,1X,12X,3X,A11,4X,A30)
906	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,')',5X,A30)
907	FORMAT(1X,I5,1X,A12,1X,A12,3X,A6,'(',I2,',*)',3X,A30)
908	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,9X,A30)
909	FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)
910	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,'(',I2,')',5X,A30)
911	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,9X,A30)
912     FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I1,',',I2,')',1X,A29)
913	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,',',I2,')',2X,A30)
914	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I2,')',3X,A30)
915	FORMAT(1X,I5,1X,A12,1X,15X,A7,8X,A30)
916	FORMAT(1X,I5,1X,A12,1X,15X,A10,'(',I2,',',I2,')',1X,A27)
917	FORMAT(1X,I5,1X,I12,1X,15X,A10,'(',I2,')',1X,A30)
918	FORMAT(1X,I5,1X,A12,1X,15X,A13,'(',I2,',',I2,')',1X,A24)



C
	END

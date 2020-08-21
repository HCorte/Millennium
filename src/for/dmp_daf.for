C DMP_DAF.FOR
C
C V01 27-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP DAF FILE
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
	SUBROUTINE DMP_DAF(RLU,FILE,DMPCDC,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'

        ! arguments
	INTEGER*4  RLU
	INTEGER*4  FILE(5)			!
	INTEGER*4  DMPCDC
	INTEGER*4  MONEY_UNIT			!
	

	INTEGER*4 LUN
	INTEGER*4 ST
	INTEGER*4 FDB(7)
	INTEGER*4 I,J

	CHARACTER DESCR(14)*30

	DATA DESCR/
     *            'DAY STATUS                    ',
     *            'CURRENT CDC DATE              ',
     *            'CURRENT YEAR                  ',
     *            'CURRENT JULIAN DATE           ',
     *            'CURRENT WEEK NUMBER           ',
     *            'DAILY ACTIVITY BY GAME        ',
     *            'GAME DRAWING NUMBERS          ',
     *            'HIGH DRAW NUMBERS             ',
     *            'CASH EXPIRE DRAW BY GAME      ',
     *            'SALES BY DRAW/GAME            ',
     *            'DISCOUNTS BY GAME             ',
     *            'CROSS SYSTEM COUNTERS         ',
     *            'INSTANT VALIDATION AMT        ',
     *            'INSTANT CLAIM AMT             '/



C
C READ DAF FILE
C
	LUN = 9     ! ?
        CALL OPENW(LUN,FILE,4,0,0,ST)
        CALL IOINIT(FDB,LUN,DAFSEC*256)
        IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
        CALL READW(FDB,DMPCDC,DAFREC,ST)
        IF(ST.NE.0) CALL FILERR(FILE,2,ST,DMPCDC)
        CALL CLOSEFIL(FDB)

C
C DUMP RECORD
C
	WRITE(RLU,900) DAFSTS_OFF, DAFSTS, 'DAFSTS', DESCR(1)
	WRITE(RLU,900) DAFCDC_OFF, DAFCDC, 'DAFCDC', DESCR(2)
	WRITE(RLU,900) DAFYER_OFF, DAFYER, 'DAFYER', DESCR(3)
	WRITE(RLU,900) DAFJUL_OFF, DAFJUL, 'DAFJUL', DESCR(4)
	WRITE(RLU,900) DAFWEK_OFF, DAFWEK, 'DAFWEK', DESCR(5)
	DO 100 I=1,MAXGAM
	  DO J=1,NUMFIN
	    WRITE(RLU,912) DAFTYP_OFF+(I-1)*NUMFIN*2+(J-1)*2,
     *                     DAFTYP(TRACNT,J,I), 
     *                     CSMONY(DAFTYP(DOLAMT,J,I),12,MONEY_UNIT),
     *                    'DAFWIN', J, I, DESCR(6)
	  ENDDO
100	CONTINUE
	DO 110 I=1,MAXGAM
	  WRITE(RLU,901) DAFDRW_OFF+I-1, DAFDRW(I), 'DAFDRW', I, DESCR(7)
110	CONTINUE
	DO 120 I=1,MAXGAM
	  WRITE(RLU,901) DAFHDR_OFF+I-1, DAFHDR(I), 'DAFHDR', I, DESCR(8)
120	CONTINUE
	DO 130 I=1,MAXGAM
	  WRITE(RLU,901) DAFVAL_OFF+I-1, DAFVAL(I), 'DAFVAL', I, DESCR(9)
130	CONTINUE
	DO 140 I=1,MAXGAM
	  DO J=1,MAXDRW
	    WRITE(RLU,913) DAFSAL_OFF+(I-1)*MAXDRW+J-1,
     *                     CSMONY(DAFSAL(J,I),12,MONEY_UNIT),
     *                    'DAFSAL', J, I, DESCR(10)
	  ENDDO
140	CONTINUE
	DO 150 I=1,MAXGAM
	  WRITE(RLU,914) DAFDIS_OFF+I-1, DAFDIS(TRACNT,I),
     *                   CSMONY(DAFDIS(DOLAMT,I),12,MONEY_UNIT),
     *                  'DAFDIS', I, DESCR(11)
150	CONTINUE
	DO 160 I=1,NUMCRS
	  WRITE(RLU,901) DAFCRS_OFF+I-1, DAFCRS(I), 'DAFCRS', I, DESCR(12)
160	CONTINUE
	WRITE(RLU,915) DAFIVAL_OFF, CSMONY(DAFIVAL,12,MONEY_UNIT),
     *                'DAFIVAL', DESCR(13)
	WRITE(RLU,915) DAFICLM_OFF, CSMONY(DAFICLM,12,MONEY_UNIT),
     *                'DAFICLM', DESCR(14)


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



C
	END

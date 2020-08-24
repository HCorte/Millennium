C DMP_TEB.FOR
C
C V01 28-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP TEB FILE
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
	SUBROUTINE DMP_TEB(RLU,FILE,DMPCDC,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:RECTEB.DEF'

        ! arguments
	INTEGER*4  RLU
	INTEGER*4  FILE(5)			!
	INTEGER*4  DMPCDC
	INTEGER*4  MONEY_UNIT			!
	

	INTEGER*4 LUN
	INTEGER*4 ST
	INTEGER*4 FDB(7)
	INTEGER*4 I

	CHARACTER DESCR(2)*30
        CHARACTER DESCR_2(15)*30

	DATA DESCR/
     *            'CURRENT CDC DATE              ',
     *            'DAILY ACTIVITY BY GAME        '/

        DATA DESCR_2/
     *            'SALES COUNT                   ',
     *            'SALES AMOUNT                  ',
     *            'CANCEL COUNT                  ',
     *            'CANCEL AMOUNT                 ',
     *            'VALID COUNT                   ',
     *            'VALID AMOUNT                  ',
     *            'CLAIM COUNT                   ',
     *            'CLAIM AMOUNT                  ',
     *            'REFUND COUNT                  ',
     *            'REFUND AMOUNT                 ',
     *            'DISCOUNT COUNT                ',
     *            'DISCOUNT AMOUNT               ',
     *            'TICKET CHARGE                 ',
     *            'GAME FLAGS  (SAME AS AGTTYP)  ',
     *            '                              '/



C
C READ TEB FILE
C
	LUN = 9     ! ?
        CALL OPENW(LUN,FILE,4,0,0,ST)
        CALL IOINIT(FDB,LUN,TEBSEC*256)
        IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
        CALL READW(FDB,DMPCDC,TEBREC,ST)
        IF(ST.NE.0) CALL FILERR(FILE,2,ST,DMPCDC)
        CALL CLOSEFIL(FDB)

C
C DUMP RECORD
C
	WRITE(RLU,900) TEBCDC_OFF, TEBCDC, 'TEBCDC', DESCR(1)
	WRITE(RLU,916) DESCR(2)	
        DO I=1,MAXGAM
              WRITE(RLU,903) TEBTYP_OFF+(I-1)*MAXGAM,
     *                       TEBTYP(GSCNT,I),
     *                      'TEBTYP', GSCNT, I, DESCR_2(1)
              WRITE(RLU,913) TEBTYP_OFF+(I-1)*MAXGAM*+1,
     *                       CSMONY(TEBTYP(GSAMT,I),12,MONEY_UNIT),
     *                      'TEBTYP', GSAMT, I, DESCR_2(2)
              WRITE(RLU,903) TEBTYP_OFF+(I-1)*MAXGAM+2,
     *                       TEBTYP(GCCNT,I),
     *                      'TEBTYP', GCCNT, I, DESCR_2(3)
              WRITE(RLU,913) TEBTYP_OFF+(I-1)*MAXGAM+3,
     *                       CSMONY(TEBTYP(GCAMT,I),12,MONEY_UNIT),
     *                      'TEBTYP', GCAMT, I, DESCR_2(4)
              WRITE(RLU,903) TEBTYP_OFF+(I-1)*MAXGAM+4,
     *                       TEBTYP(GVCNT,I),
     *                      'TEBTYP', GVCNT, I, DESCR_2(5)
              WRITE(RLU,913) TEBTYP_OFF+(I-1)*MAXGAM+5,
     *                       CSMONY(TEBTYP(GVAMT,I),12,MONEY_UNIT),
     *                      'TEBTYP', GVAMT, I, DESCR_2(6)
              WRITE(RLU,903) TEBTYP_OFF+(I-1)*MAXGAM+6,
     *                       TEBTYP(GCLCNT,I),
     *                      'TEBTYP', GCLCNT, I, DESCR_2(7)
              WRITE(RLU,913) TEBTYP_OFF+(I-1)*MAXGAM+7,
     *                       CSMONY(TEBTYP(GCLAMT,I),12,MONEY_UNIT),
     *                      'TEBTYP', GCLAMT, I, DESCR_2(8)
              WRITE(RLU,903) TEBTYP_OFF+(I-1)*MAXGAM+8,
     *                       TEBTYP(GRCNT,I),
     *                      'TEBTYP', GRCNT, I, DESCR_2(9)
              WRITE(RLU,913) TEBTYP_OFF+(I-1)*MAXGAM+9,
     *                       CSMONY(TEBTYP(GRAMT,I),12,MONEY_UNIT),
     *                      'TEBTYP', GRAMT, I, DESCR_2(10)
              WRITE(RLU,903) TEBTYP_OFF+(I-1)*MAXGAM+10,
     *                       TEBTYP(GDCNT,I),
     *                      'TEBTYP', GDCNT, I, DESCR_2(11)
              WRITE(RLU,913) TEBTYP_OFF+(I-1)*MAXGAM+11,
     *                       CSMONY(TEBTYP(GDAMT,I),12,MONEY_UNIT),
     *                      'TEBTYP', GDAMT, I, DESCR_2(12)
              WRITE(RLU,913) TEBTYP_OFF+(I-1)*MAXGAM+12,
     *                       CSMONY(TEBTYP(GTKCHG,I),12,MONEY_UNIT),
     *                      'TEBTYP', GTKCHG, I, DESCR_2(13)
              WRITE(RLU,903) TEBTYP_OFF+(I-1)*MAXGAM+13,
     *                       TEBTYP(GFLAGS,I),
     *                      'TEBTYP', GFLAGS, I, DESCR_2(14)
        ENDDO


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
916     FORMAT(1X,A30)


C
	END

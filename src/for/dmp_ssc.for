C DMP_LTO.FOR
C
C V01 16-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP SUPERSCORE GAME FILE
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
	SUBROUTINE DMP_SSC(RLU,FILE,DRAW,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DSSREC.DEF'

        ! arguments
	INTEGER*4  RLU				!
	INTEGER*4  FILE(5)			!
        INTEGER*4  DRAW                         !
	INTEGER*4  MONEY_UNIT			!

	INTEGER*4 LUN
	INTEGER*4 I,K
        CHARACTER DESCR(48)*30

        DATA DESCR/
     *            'GAME STATUS                   ',
     *            'WEEK NUMBER                   ',
     *            'DRAW DATE                     ',
     *            'DRAW NUMBER                   ',
     *            'BEGINNING SALES DATE          ',
     *            'ENDING SALES DATE             ',
     *            'LAST PURGE UPDATE  (FILE ONLY)',
     *            'LAST FILE UPDATE   (FILE ONLY)',
     *            'ACTUAL TIME WHEN CLOSED       ',
     *            'TIME WHEN GAME SHOULD CLOSE   ',
     *            'LAST SERIAL NUMBER            ',
     *            'TOTAL SALES                   ',
     *            'PRIZES PAID + REFUNDS         ',
     *            'PRIZES PURGED                 ',
     *            'PAID REFUNDS                  ',
     *            'TOTAL REFUND AMOUNT           ',
     *            'PRIZES WON                    ',
     *            'ROLL POOL                     ',
     *            'TOTAL PRIZE POOL              ',
     *            'TOTAL BREAKAGE                ',
     *            'WINNING ROUNDING POT          ',
     *            'AMOUNT BET ON WINNING SCORE   ',
     *            'WINNING ODDS                  ',
     *            'WINNING RESULTS               ',
     *            'WINNING RESULTS HOLD          ',
     *            'WINNING TAXES                 ',
     *            'OVER TAX LIMIT                ',
     *            '>REDMIN <REDSSX               ',
     *            'UNDER TAX LIMIT               ',
     *            'HIGHEST WINNER FOR THIS GAME  ',
     *            'OVER REDSSX                   ',
     *            'MASTER EVENT NAME             ',
     *            'EVENT DESCRIPTION             ',
     *            'EVENT NAMES                   ',
     *            'EVENT STATUS                  ',
     *            'REV #                         ',
     *            'GAME TYPE 1=SOC. 2=HOCK.      ',
     *            'POOL FILE NAME                ',
     *            'POOL OVERFLOW FILE NAME       ',
     *            'MINIMUM STAKE                 ',
     *            'POOL PERCENTAGE               ',
     *            'WINNING:CNT,AMT REFUNDS ONLY  ',
     *            'WINNING:CNT,AMT REFUNDS ALTOGE',
     *            'WINNING:CNT,AMT PRIZES ONLY   ',
     *            'WINNING:CNT,AMT PRIZES ALTOGET',
     *            'WINNING:CNT,AMT PRIZES+REFUNDS',
     *            'EVENT CLOSE DATE              ',
     *            'EVENT CLOSE TIME              '/




C
C READ GAME FILE
C
	LUN = 9     ! ?
        CALL READGFL(LUN,FILE,DSSSEC,DRAW,DSSREC)
C
C DUMP RECORD
C
	WRITE(RLU,900) DSSSTS_OFF, DSSSTS, 'DSSSTS', DESCR(1)
	WRITE(RLU,900) DSSWEK_OFF, DSSWEK, 'DSSWEK', DESCR(2)
	WRITE(RLU,900) DSSDAT_OFF, DSSDAT, 'DSSDAT', DESCR(3)
	WRITE(RLU,900) DSSDRW_OFF, DSSDRW, 'DSSDRW', DESCR(4)
	WRITE(RLU,900) DSSBSD_OFF, DSSBSD, 'DSSBSD', DESCR(5)
	WRITE(RLU,900) DSSESD_OFF, DSSESD, 'DSSESD', DESCR(6)
	WRITE(RLU,900) DSSPUP_OFF, DSSPUP, 'DSSPUP', DESCR(7)
	WRITE(RLU,900) DSSUPD_OFF, DSSUPD, 'DSSUPD', DESCR(8)
	WRITE(RLU,913) DSSCTM_OFF, DISTIM(DSSCTM), 'DSSCTM', DESCR(9)
	WRITE(RLU,913) DSSTIM_OFF, DISTIM(DSSTIM), 'DSSTIM', DESCR(10)
	WRITE(RLU,900) DSSSER_OFF, DSSSER, 'DSSSER', DESCR(11)
	WRITE(RLU,914) DSSSAL_OFF, CSMONY(DSSSAL,12,MONEY_UNIT),
     *              'DSSSAL', DESCR(12)
	WRITE(RLU,914) DSSPAD_OFF, CSMONY(DSSPAD,12,MONEY_UNIT),
     *              'DSSPAD', DESCR(13)
	WRITE(RLU,914) DSSPRG_OFF, CSMONY(DSSPRG,12,MONEY_UNIT),
     *              'DSSPRG', DESCR(14)
	WRITE(RLU,914) DSSPRF_OFF, CSMONY(DSSPRF,12,MONEY_UNIT),
     *              'DSSPRF', DESCR(15)
	WRITE(RLU,914) DSSREF_OFF, CSMONY(DSSREF,12,MONEY_UNIT),
     *              'DSSREF', DESCR(16)
	WRITE(RLU,914) DSSWON_OFF, CSMONY(DSSWON,12,MONEY_UNIT),
     *              'DSSWON', DESCR(17)
	WRITE(RLU,915) DSSPOL_OFF,   CSMONY(DSSPOL(1),12,MONEY_UNIT),
     *              'DSSPOL', 1, DESCR(18)
	WRITE(RLU,915) DSSPOL_OFF+1, CSMONY(DSSPOL(2),12,MONEY_UNIT),
     *              'DSSPOL', 2, DESCR(18)
	WRITE(RLU,914) DSSTPL_OFF, CSMONY(DSSTPL,12,MONEY_UNIT),
     *              'DSSTPL', DESCR(19)
	WRITE(RLU,914) DSSTBK_OFF, CSMONY(DSSTBK,12,MONEY_UNIT),
     *              'DSSTBK', DESCR(20)
	WRITE(RLU,915) DSSBRK_OFF,   CSMONY(DSSBRK(1),12,MONEY_UNIT), 
     *              'DSSBRK', 1, DESCR(21)
	WRITE(RLU,915) DSSBRK_OFF+1, CSMONY(DSSBRK(2),12,MONEY_UNIT),
     *              'DSSBRK', 2, DESCR(21)
	WRITE(RLU,914) DSSABW_OFF, CSMONY(DSSABW,12,MONEY_UNIT),
     *              'DSSABW', DESCR(22)
	WRITE(RLU,900) DSSODS_OFF, DSSODS, 'DSSODS', DESCR(23)
	DO 100 I=1,3
	  WRITE(RLU,911) DSSWIN_OFF,   DSSWIN(1,I), DSSWIN(2,I),
     *                'DSSWIN', I, DESCR(24)
100	CONTINUE
	DO 110 I=1,3
	  WRITE(RLU,911) DSSHLD_OFF,   DSSHLD(1,I), DSSHLD(2,I),
     *                'DSSHLD', I, DESCR(25)
110	CONTINUE
	WRITE(RLU,900) DSSTAX_OFF, DSSTAX, 'DSSTAX', DESCR(26)
	DO 120 I=1,NUMTOT
	  WRITE(RLU,901) DSSOTX_OFF+I-1, DSSOTX(I), 'DSSOTX', I, DESCR(27)
120	CONTINUE
	DO 130 I=1,NUMTOT
	  WRITE(RLU,901) DSSMID_OFF+I-1, DSSMID(I), 'DSSMID', I, DESCR(28)
130	CONTINUE
	DO 140 I=1,NUMTOT
	  WRITE(RLU,901) DSSUTX_OFF+I-1, DSSUTX(I), 'DSSUTX', I, DESCR(29)
140	CONTINUE
	WRITE(RLU,900) DSSHST_OFF, DSSHST, 'DSSHST', DESCR(30)
	DO 150 I=1,NUMTOT
	  WRITE(RLU,901) DSSORM_OFF+I-1, DSSORM(I), 'DSSORM', I, DESCR(31)
150	CONTINUE
        WRITE(RLU,908) DSSMNM_OFF, (DSSMNM(K),K=1,5), 'DSSMNM', DESCR(32)
        WRITE(RLU,908) DSSDES_OFF, (DSSDES(K),K=1,5), 'DSSDES', DESCR(33)
	DO 160 I=1,3
	  WRITE(RLU,912) DSSSNM_OFF,   (DSSSNM(K,I),K=1,4),
     *                'DSSSNM', I, DESCR(34)
160	CONTINUE
	DO 170 I=1,3
	  WRITE(RLU,901) DSSEST_OFF+I-1, DSSEST(I), 'DSSEST', I, DESCR(35)
170	CONTINUE
	WRITE(RLU,900) DSSREV_OFF, DSSREV, 'DSSREV', DESCR(36)
	WRITE(RLU,900) DSSTYP_OFF, DSSTYP, 'DSSTYP', DESCR(37)
	WRITE(RLU,908) DSSPFN_OFF, (DSSPFN(K),K=1,5), 'DSSPFN', DESCR(38)
	WRITE(RLU,908) DSSPOF_OFF, (DSSPOF(K),K=1,5), 'DSSPOF', DESCR(39)
	WRITE(RLU,914) DSSPRC_OFF, CSMONY(DSSPRC,12,MONEY_UNIT),
     *              'DSSPRC', DESCR(40)
	WRITE(RLU,916) DSSSPR_OFF, DISPER(DSSSPR), 'DSSSPR', DESCR(41)
        DO 180 I=1,2
          WRITE(RLU,917) DSSWRO_OFF+(I-1)*2, DSSWRO(1,I), 
     *                 CSMONY(DSSWRO(2,I),12,MONEY_UNIT),
     *                 'DSSWRO', I, DESCR(42)
180     CONTINUE
        DO 190 I=1,2
          WRITE(RLU,917) DSSWRA_OFF+(I-1)*2, DSSWRA(1,I), 
     *                 CSMONY(DSSWRA(2,I),12,MONEY_UNIT),
     *                 'DSSWRA', I, DESCR(43)
190     CONTINUE
        DO 200 I=1,2
          WRITE(RLU,917) DSSWPO_OFF+(I-1)*2, DSSWPO(1,I), 
     *                 CSMONY(DSSWPO(2,I),12,MONEY_UNIT),
     *                 'DSSWPO', I, DESCR(44)
200     CONTINUE
        DO 210 I=1,2
          WRITE(RLU,917) DSSWPA_OFF+(I-1)*2, DSSWPA(1,I), 
     *                 CSMONY(DSSWPA(2,I),12,MONEY_UNIT),
     *                 'DSSWPA', I, DESCR(45)
210     CONTINUE
        DO 220 I=1,2
          WRITE(RLU,917) DSSWPR_OFF+(I-1)*2, DSSWPR(1,I), 
     *                 CSMONY(DSSWPR(2,I),12,MONEY_UNIT),
     *                 'DSSWPR', I, DESCR(46)
220     CONTINUE
	DO 230 I=1,3
	  WRITE(RLU,901) DSSECD_OFF+I-1, DSSECD(I), 'DSSECD', I, DESCR(47)
230	CONTINUE
	DO 240 I=1,3
	  WRITE(RLU,918) DSSECT_OFF+I-1, DISTIM(DSSECT(I)), 'DSSECT', I, DESCR(48)
240	CONTINUE



	RETURN  
C
C
900	FORMAT(1X,I5,1X,I12,1X,15X,A6,9X,A30)
901	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,')',5X,A30)
902	FORMAT(1X,I5,1X,I12,1X,I12,3X,A6,'(',I2,',*)',3X,A30)
903	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,')',2X,A30)
904	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,',',I2,')',1X,A28)
905	FORMAT(1X,I5,1X,I12,1X,15X,A11,8X,A30)
906	FORMAT(1X,I5,1X,A13,1X,A13,1X,A6,'(',I2,')',5X,A30)
907	FORMAT(1X,I5,1X,A16,1X,10X,1X,A6,9X,A30)
908	FORMAT(1X,I5,1X,5A4,1X,7X,A6,9X,A30)
909	FORMAT(1X,I5,1X,A17,1X,A6,9X,A30)
910	FORMAT(1X,I5,1X,8X,A4,1X,15X,A6,9X,A30)
911	FORMAT(1X,I5,1X,I12,1X,I12,3X,A6,'(*,',I2,')',3X,A30)
912	FORMAT(1X,I5,1X,4A4,12X,A6,'(1,',I2,')'3X,A30)
913	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,9X,A30)
914	FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)
915	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,')',5X,A30)
916	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,9X,A30)
917	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I2,')',3X,A30)
918	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,'(',I2,')',5X,A30)


C
	END

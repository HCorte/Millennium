C DMP_LTO.FOR
C
C V01 15-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP SCORE GAME FILE
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
	SUBROUTINE DMP_SCR(RLU,FILE,DRAW,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DSCREC.DEF'

        ! arguments
	INTEGER*4  RLU				
	INTEGER*4  FILE(5)			!
        INTEGER*4  DRAW                         !
	INTEGER*4  MONEY_UNIT			!

	INTEGER*4 LUN
	INTEGER*4 I,K

        CHARACTER DESCR(47)*30

        DATA DESCR/
     *            'GAME STATUS                   ',
     *            'WEEK NUMBER                   ',
     *            'DRAW DATE                     ',
     *            'DRAW NUMBER                   ',
     *            'BEGINNING SALES DATE          ',
     *            'ENDING SALES DATE             ',
     *            'LAST PURGE UPDATE             ',
     *            'LAST FILE UPDATE              ',
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
     *            'FULL SYS SALES CNT AMT        ',
     *            'OVER TAX LIMIT                ',
     *            '>REDMIN <REDSCX               ',
     *            'UNDER TAX LIMIT               ',
     *            'HIGHEST WINNER FOR THIS GAME  ',
     *            'OVER REDSCX                   ',
     *            'TEAM 1 NAME                   ',
     *            'TEAM 2 NAME                   ',
     *            'REV #                         ',
     *            'GAME TYPE 1=SOC. 2=HOCK.      ',
     *            'POOL FILE NAME                ',
     *            'BASE PRICE                    ',
     *            'POOL PERCENTAGE               ',
     *            'GAME DESCRIPTION              ',
     *            'TV-CHANEL NAME                ',
     *            'MULTI-DRAW SELECTED TABLE     ',
     *            'WINNING:CNT,AMNT REFUNDS ONLY ',
     *            'WINNING:CNT,AMNT REFUNDS ALTOG',
     *            'WINNING:CNT,AMNT PRIZES ONLY  ',
     *            'WINNING:CNT,AMNT PRIZES ALTOGE',
     *            'WINNING:CNT,AMNT PRIZES+REFUND'/




C
C READ GAME FILE
C
	LUN = 9     ! ?
        CALL READGFL(LUN,FILE,DSCSEC,DRAW,DSCREC)
C
C DUMP RECORD
C
	WRITE(RLU,900) DSCSTS_OFF, DSCSTS, 'DSCSTS', DESCR(1)
	WRITE(RLU,900) DSCWEK_OFF, DSCWEK, 'DSCWEK', DESCR(2)
	WRITE(RLU,900) DSCDAT_OFF, DSCDAT, 'DSCDAT', DESCR(3)
	WRITE(RLU,900) DSCDRW_OFF, DSCDRW, 'DSCDRW', DESCR(4)
	WRITE(RLU,900) DSCBSD_OFF, DSCBSD, 'DSCBSD', DESCR(5)
	WRITE(RLU,900) DSCESD_OFF, DSCESD, 'DSCESD', DESCR(6)
	WRITE(RLU,900) DSCPUP_OFF, DSCPUP, 'DSCPUP', DESCR(7)
	WRITE(RLU,900) DSCUPD_OFF, DSCUPD, 'DSCUPD', DESCR(8)
	WRITE(RLU,911) DSCCTM_OFF, DISTIM(DSCCTM), 'DSCCTM', DESCR(9)
	WRITE(RLU,911) DSCTIM_OFF, DISTIM(DSCTIM), 'DSCTIM', DESCR(10)
	WRITE(RLU,900) DSCSER_OFF, DSCSER, 'DSCSER', DESCR(11)
	WRITE(RLU,912) DSCSAL_OFF, CSMONY(DSCSAL,12,MONEY_UNIT),
     *              'DSCSAL', DESCR(12)
	WRITE(RLU,912) DSCPAD_OFF, CSMONY(DSCPAD,12,MONEY_UNIT),
     *              'DSCPAD', DESCR(13)
	WRITE(RLU,912) DSCPRG_OFF, CSMONY(DSCPRG,12,MONEY_UNIT),
     *              'DSCPRG', DESCR(14)
	WRITE(RLU,912) DSCPRF_OFF, CSMONY(DSCPRF,12,MONEY_UNIT),
     *              'DSCPRF', DESCR(15)
	WRITE(RLU,912) DSCREF_OFF, CSMONY(DSCREF,12,MONEY_UNIT),
     *              'DSCREF', DESCR(16)
	WRITE(RLU,912) DSCWON_OFF, CSMONY(DSCWON,12,MONEY_UNIT),
     *              'DSCWON', DESCR(17)
	WRITE(RLU,913) DSCPOL_OFF,   CSMONY(DSCPOL(1),12,MONEY_UNIT),
     *              'DSCPOL', 1, DESCR(18)
	WRITE(RLU,913) DSCPOL_OFF+1, CSMONY(DSCPOL(2),12,MONEY_UNIT),
     *              'DSCPOL', 2, DESCR(18)
	WRITE(RLU,912) DSCTPL_OFF, CSMONY(DSCTPL,12,MONEY_UNIT),
     *              'DSCTPL', DESCR(19)
	WRITE(RLU,912) DSCTBK_OFF, CSMONY(DSCTBK,12,MONEY_UNIT),
     *              'DSCTBK', DESCR(20)
	WRITE(RLU,913) DSCBRK_OFF,   CSMONY(DSCBRK(1),12,MONEY_UNIT),
     *              'DSCBRK', 1, DESCR(21)
	WRITE(RLU,913) DSCBRK_OFF+1, CSMONY(DSCBRK(2),12,MONEY_UNIT),
     *              'DSCBRK', 2, DESCR(21)
	WRITE(RLU,912) DSCABW_OFF, CSMONY(DSCABW,12,MONEY_UNIT),
     *              'DSCABW', DESCR(22)
	WRITE(RLU,900) DSCODS_OFF, DSCODS, 'DSCODS', DESCR(23)
	WRITE(RLU,901) DSCWIN_OFF,   DSCWIN(1), 'DSCWIN', 1, DESCR(24)
	WRITE(RLU,901) DSCWIN_OFF+1, DSCWIN(2), 'DSCWIN', 2, DESCR(24)
	WRITE(RLU,901) DSCHLD_OFF,   DSCHLD(1), 'DSCHLD', 1, DESCR(25)
	WRITE(RLU,901) DSCHLD_OFF+1, DSCHLD(2), 'DSCHLD', 2, DESCR(25)
	WRITE(RLU,900) DSCTAX_OFF, DSCTAX, 'DSCTAX', DESCR(26)
        DO 100 I=1,MAXFUL
          WRITE(RLU,916) DSCFUL_OFF+(I-1)*2, DSCFUL(1,I),
     *                   CSMONY(DSCFUL(2,I),12,MONEY_UNIT),
     *                   'DSCFUL', I, DESCR(27)
100	CONTINUE
	DO 110 I=1,NUMTOT
	  WRITE(RLU,901) DSCOTX_OFF+I-1, DSCOTX(I), 'DSCOTX', I, DESCR(28)
110	CONTINUE
	DO 120 I=1,NUMTOT
	  WRITE(RLU,901) DSCMID_OFF+I-1, DSCMID(I), 'DSCMID', I, DESCR(29)
120	CONTINUE
	DO 130 I=1,NUMTOT
	  WRITE(RLU,901) DSCUTX_OFF+I-1, DSCUTX(I), 'DSCUTX', I, DESCR(30)
130	CONTINUE
	WRITE(RLU,900) DSCHST_OFF, DSCHST, 'DSCHST', DESCR(31)
	DO 140 I=1,NUMTOT
	  WRITE(RLU,901) DSCORM_OFF+I-1, DSCORM(I), 'DSCORM', I, DESCR(32)
140	CONTINUE
	WRITE(RLU,907) DSCNM1_OFF, (DSCNM1(K),K=1,4), 'DSCNM1', DESCR(33)
	WRITE(RLU,907) DSCNM2_OFF, (DSCNM2(K),K=1,4), 'DSCNM2', DESCR(34)
	WRITE(RLU,900) DSCREV_OFF, DSCREV, 'DSCREV', DESCR(35)
	WRITE(RLU,900) DSCTYP_OFF, DSCTYP, 'DSCTYP', DESCR(36)
	WRITE(RLU,908) DSCPFN_OFF, (DSCPFN(K),K=1,5), 'DSCPFN', DESCR(37)
	WRITE(RLU,912) DSCPRC_OFF, CSMONY(DSCPRC,12,MONEY_UNIT),
     *              'DSCPRC', DESCR(38)
	WRITE(RLU,915) DSCSPR_OFF, DISPER(DSCSPR), 'DSCSPR', DESCR(39)
	WRITE(RLU,908) DSCDES_OFF, (DSCDES(K),K=1,5), 'DSCDES', DESCR(40)
	WRITE(RLU,910) DSCTVC_OFF, DSCTVC(1), 'DSCTVC', DESCR(41)
	DO 150 I=1,MAXMLTD_AVL
	  WRITE(RLU,901) DSCMDS_OFF+I-1, DSCMDS(I), 'DSCMDS', I, DESCR(42)
150	CONTINUE
        DO 160 I=1,2
          WRITE(RLU,914) DSCWRO_OFF+(I-1)*2, DSCWRO(1,I),
     *                 CSMONY(DSCWRO(2,I),12,MONEY_UNIT),
     *                 'DSCWRO', I, DESCR(43)
160     CONTINUE
        DO 170 I=1,2
          WRITE(RLU,914) DSCWRA_OFF+(I-1)*2, DSCWRA(1,I),
     *                 CSMONY(DSCWRA(2,I),12,MONEY_UNIT),
     *                 'DSCWRA', I, DESCR(44)
170     CONTINUE
        DO 180 I=1,2
          WRITE(RLU,914) DSCWPO_OFF+(I-1)*2, DSCWPO(1,I), 
     *                 CSMONY(DSCWPO(2,I),12,MONEY_UNIT),
     *                 'DSCWPO', I, DESCR(45)
180     CONTINUE
        DO 190 I=1,2
          WRITE(RLU,914) DSCWPA_OFF+(I-1)*2, DSCWPA(1,I), 
     *                 CSMONY(DSCWPA(2,I),12,MONEY_UNIT),
     *                 'DSCWPA', I, DESCR(46)
190     CONTINUE
        DO 200 I=1,2
          WRITE(RLU,914) DSCWPR_OFF+(I-1)*2, DSCWPR(1,I), 
     *                 CSMONY(DSCWPR(2,I),12,MONEY_UNIT),
     *                 'DSCWPR', I, DESCR(47)
200     CONTINUE



	RETURN  
C
C
900	FORMAT(1X,I5,1X,I12,1X,15X,A6,9X,A30)
901	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,')',5X,A30)
902	FORMAT(1X,I5,1X,I12,1X,I12,3X,A6,'(',I2,',*)',3X,A30)
903	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I3,')',1X,A30)
904	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,',',I2,')',1X,A28)
905	FORMAT(1X,I5,1X,I12,1X,10X,A11,8X,A30)
906	FORMAT(1X,I5,1X,A13,1X,A13,1X,A6,'(',I2,')',5X,A30)
907	FORMAT(1X,I5,1X,4A4,1X,10X,1X,A6,9X,A30)
908	FORMAT(1X,I5,1X,5A4,1X,7X,A6,9X,A30)
909	FORMAT(1X,I5,1X,A17,11X,A6,9X,A30)
910	FORMAT(1X,I5,1X,8X,A4,1X,12X,3X,A6,9X,A30)
911	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,9X,A30)
912	FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)
913	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,')',5X,A30)
914	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I2,')',3X,A30)
915	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,9X,A30)
916	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I3,')',2X,A30)



C
	END

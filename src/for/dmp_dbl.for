C DMP_DBL.FOR
C
C V01 16-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP DOUBLE GAME FILE
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
	SUBROUTINE DMP_DBL(RLU,FILE,DRAW,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DDBREC.DEF'

        ! arguments
	INTEGER*4  RLU				!
	INTEGER*4  FILE(5)			!
        INTEGER*4  DRAW                         !
	INTEGER*4  MONEY_UNIT			!

	INTEGER*4 LUN
	INTEGER*4 I,K

        CHARACTER DESCR(56)*30

        DATA DESCR/
     *            'GAME STATUS                   ',
     *            'WEEK NUMBER                   ',
     *            'DRAW DATE                     ',
     *            'DRAW NUMBER                   ',
     *            'BEGINNING SALES DATE          ',
     *            'ENDING SALES DATE             ',
     *            'LAST PRUGE UPDATE  (FILE ONLY)',
     *            'LAST FILE UPDATE   (FILE ONLY)',
     *            'ACTUAL TIME WHEN CLOSED       ',
     *            'TIME WHEN GAME SHOULD CLOSE   ',
     *            'LAST SERIAL NUMBER            ',
     *            'TOTAL SALES (COUNT , AMOUNT)  ',
     *            'PRIZES PAID + REFUNDS         ',
     *            'PRIZES PURGED                 ',
     *            'PAID REFUNDS                  ',
     *            'TOTAL REFUND AMOUNT           ',
     *            'EARLY REFUNDS PAID            ',
     *            'TOTAL EARLY REFUNDS (LIAB)    ',
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
     *            '>REDMIN <REDMAX               ',
     *            'UNDER TAX LIMIT               ',
     *            'HIGHEST WINNER PER WINING COMB',
     *            'OVER REDMAX                   ',
     *            'EVENT NAME                    ',
     *            'GAME DESCRIPTION              ',
     *            'ROW NAMES                     ',
     *            'ROW STATUS 1 THROUGH MAXTRW   ',
     *            'SALES BY ROW TABLE            ',
     *            'REV #                         ',
     *            'POOL FILE NAME                ',
     *            'ROW CLOSE TIMES               ',
     *            'BASE PRICE                    ',
     *            'POOL PERCENTAGE               ',
     *            'TV-CHANEL NAME                ',
     *            'MULTI DRAW SELECTED TABLE     ',
     *            'ACTUAL # OF ROWS USED.        ',
     *            '# WINNING COUPONS PALYED.     ',
     *            'CNT, AMT PLAYED.              ',
     *            'WINNING:CNT,AMT REFUNDS ONLY  ',
     *            'WINNING:CNT,AMT REFUNDS ALTOGE',
     *            'WINNING:CNT,AMT PRIZES ONLY   ',
     *            'WINNING:CNT,AMT PRIZES ALTOGET',
     *            'WINNING:CNT,AMT PRIZES+REFUNDS',
     *            'AMOUNTS ON COMBNS. FOR ODDS   ',
     *            'Partially closed combination  ',
     *            'NUMBER OF WINNING COMBINATIONS'/




C
C READ GAME FILE
C
	LUN = 9     ! ?
        CALL READGFL(LUN,FILE,DDBSEC,DRAW,DDBREC)
C
C DUMP RECORD
C
	WRITE(RLU,900) DDBSTS_OFF, DDBSTS, 'DDBSTS', DESCR(1)
	WRITE(RLU,900) DDBWEK_OFF, DDBWEK, 'DDBWEK', DESCR(2)
	WRITE(RLU,900) DDBDAT_OFF, DDBDAT, 'DDBDAT', DESCR(3)
	WRITE(RLU,900) DDBDRW_OFF, DDBDRW, 'DDBDRW', DESCR(4)
	WRITE(RLU,900) DDBBSD_OFF, DDBBSD, 'DDBBSD', DESCR(5)
	WRITE(RLU,900) DDBESD_OFF, DDBESD, 'DDBESD', DESCR(6)
	WRITE(RLU,900) DDBPUP_OFF, DDBPUP, 'DDBPUP', DESCR(7)
	WRITE(RLU,900) DDBUPD_OFF, DDBUPD, 'DDBUPD', DESCR(8)
	WRITE(RLU,913) DDBCTM_OFF, DISTIM(DDBCTM), 'DDBCTM', DESCR(9)
	WRITE(RLU,913) DDBTIM_OFF, DISTIM(DDBTIM), 'DDBTIM', DESCR(10)
	WRITE(RLU,900) DDBSER_OFF, DDBSER, 'DDBSER', DESCR(11)
	WRITE(RLU,920) DDBSAL_OFF, DDBSAL(1),
     *                 CSMONY(DDBSAL(2),12,MONEY_UNIT),
     *                 'DDBSAL', DESCR(12)
	WRITE(RLU,915) DDBPAD_OFF, CSMONY(DDBPAD,12,MONEY_UNIT),
     *               'DDBPAD', DESCR(13)
	WRITE(RLU,915) DDBPRG_OFF, CSMONY(DDBPRG,12,MONEY_UNIT), 
     *               'DDBPRG', DESCR(14)
	WRITE(RLU,915) DDBPRF_OFF, CSMONY(DDBPRF,12,MONEY_UNIT),
     *               'DDBPRF', DESCR(15)
	WRITE(RLU,915) DDBREF_OFF, CSMONY(DDBREF,12,MONEY_UNIT),
     *               'DDBREF', DESCR(16)
	WRITE(RLU,915) DDBERF_OFF, CSMONY(DDBERF,12,MONEY_UNIT), 
     *               'DDBERF', DESCR(17)
	WRITE(RLU,915) DDBTER_OFF, CSMONY(DDBTER,12,MONEY_UNIT),
     *               'DDBTER', DESCR(18)
	WRITE(RLU,915) DDBWON_OFF, CSMONY(DDBWON,12,MONEY_UNIT), 
     *               'DDBWON', DESCR(19)
	WRITE(RLU,914) DDBPOL_OFF,   CSMONY(DDBPOL(1),12,MONEY_UNIT),
     *               'DDBPOL', 1, DESCR(20)
	WRITE(RLU,914) DDBPOL_OFF+1, CSMONY(DDBPOL(2),12,MONEY_UNIT),
     *               'DDBPOL', 2, DESCR(20)
	WRITE(RLU,915) DDBTPL_OFF,   CSMONY(DDBTPL,12,MONEY_UNIT),
     *              'DDBTPL',    DESCR(21)
	WRITE(RLU,915) DDBTBK_OFF,   CSMONY(DDBTBK,12,MONEY_UNIT),
     *               'DDBTBK',    DESCR(22)
	WRITE(RLU,914) DDBBRK_OFF,   CSMONY(DDBBRK(1),12,MONEY_UNIT),
     *               'DDBBRK', 1, DESCR(23)
	WRITE(RLU,914) DDBBRK_OFF+1, CSMONY(DDBBRK(2),12,MONEY_UNIT),
     *               'DDBBRK', 2, DESCR(23)
	WRITE(RLU,915) DDBABW_OFF,   CSMONY(DDBABW,12,MONEY_UNIT),
     *               'DDBABW',    DESCR(24)
	DO 110 I=1,MAXDBLTI
	  WRITE(RLU,901) DDBODS_OFF+I-1, DDBODS(I), 'DDBODS', I, DESCR(25)
110	CONTINUE
        DO 120 I=1,MAXDBLTI
          WRITE(RLU,902) DDBWIN_OFF+(I-1)*2, DDBWIN(1,I), DDBWIN(2,I),
     *                 'DDBWIN', I, DESCR(26)
120     CONTINUE
        DO 130 I=1,MAXDBLTI
          WRITE(RLU,902) DDBHLD_OFF+(I-1)*2, DDBHLD(1,I), DDBHLD(2,I),
     *                 'DDBHLD', I, DESCR(27)
130     CONTINUE
	WRITE(RLU,900) DDBTAX_OFF, DDBTAX, 'DDBTAX', DESCR(28)
	DO 140 I=1,NUMTOT
	  WRITE(RLU,901) DDBOTX_OFF+I-1, DDBOTX(I), 'DDBOTX', I, DESCR(29)
140	CONTINUE
	DO 150 I=1,NUMTOT
	  WRITE(RLU,901) DDBMID_OFF+I-1, DDBMID(I), 'DDBMID', I, DESCR(30)
150	CONTINUE
	DO 160 I=1,NUMTOT
	  WRITE(RLU,901) DDBUTX_OFF+I-1, DDBUTX(I), 'DDBUTX', I, DESCR(31)
160	CONTINUE
	DO 170 I=1,MAXDBLTI
	  WRITE(RLU,901) DDBHST_OFF+I-1, DDBHST(I), 'DDBHST', I, DESCR(32)
170	CONTINUE
	DO 180 I=1,NUMTOT
	  WRITE(RLU,901) DDBORM_OFF+I-1, DDBORM(I), 'DDBORM', I, DESCR(33)
180	CONTINUE
	WRITE(RLU,909) DDBENM_OFF, (DDBENM(K),K=1,4), 'DDBENM', DESCR(34)
	WRITE(RLU,909) DDBDES_OFF, (DDBDES(K),K=1,4), 'DDBDES', DESCR(35)
	DO 190 I=1,MAXDBLRW
	  WRITE(RLU,911) DDBNMS_OFF+(I-1)*DBLNMS_LEN/4, (DDBNMS(K,I),K=1,4),
     *                  'DDBNMS', I, DESCR(36)
190	CONTINUE
	DO 200 I=1,MAXDBLRW
	  WRITE(RLU,901) DDBSTA_OFF+I-1, DDBSTA(I), 'DDBSTA', I, DESCR(37)
200	CONTINUE
	DO 210 I=1,MAXDBLRW
	  WRITE(RLU,914) DDBSBR_OFF+I-1, CSMONY(DDBSBR(I),12,MONEY_UNIT),
     *                 'DDBSBR', I, DESCR(38)
210	CONTINUE
	WRITE(RLU,900) DDBREV_OFF, DDBREV, 'DDBREV', DESCR(39)
	WRITE(RLU,908) DDBPFN_OFF, (DDBPFN(K),K=1,5), 'DDBPFN', DESCR(40)
	DO 220 I=1,MAXDBLRW
	  WRITE(RLU,916) DDBRTM_OFF+I-1, DISTIM(DDBRTM(I)), 'DDBRTM', I, DESCR(41)
220	CONTINUE
	WRITE(RLU,915) DDBPRC_OFF, CSMONY(DDBPRC,12,MONEY_UNIT),
     *               'DDBPRC', DESCR(42)
	WRITE(RLU,917) DDBSPR_OFF, DISPER(DDBSPR), 'DDBSPR', DESCR(43)
	WRITE(RLU,910) DDBTVC_OFF, DDBTVC(1), 'DDBTVC', DESCR(44)
	DO 230 I=1,MAXMLTD_AVL
	  WRITE(RLU,901) DDBMDS_OFF+I-1, DDBMDS(I), 'DDBMDS', I, DESCR(45)
230	CONTINUE
	WRITE(RLU,900) DDBRWS_OFF, DDBRWS, 'DDBRWS', DESCR(46)
	DO 240 I=1,MAXDBLTI
	  WRITE(RLU,901) DDBWCP_OFF+I-1, DDBWCP(I), 'DDBWCP', I, DESCR(47)
240	CONTINUE
	DO 250 I=1,MAXDBLTI
	  WRITE(RLU,912) DDBWBT_OFF+I-1, DDBWBT(1,I), 
     *                 CSMONY(DDBWBT(2,I),12,MONEY_UNIT),
     *                'DDBWBT', I, DESCR(48)
250	CONTINUE
        DO 260 I=1,2
          WRITE(RLU,918) DDBWRO_OFF+(I-1)*2, DDBWRO(1,I), 
     *                 CSMONY(DDBWRO(2,I),12,MONEY_UNIT),
     *                 'DDBWRO', I, DESCR(49)
260     CONTINUE
        DO 270 I=1,2
          WRITE(RLU,918) DDBWRA_OFF+(I-1)*2, DDBWRA(1,I), 
     *                 CSMONY(DDBWRA(2,I),12,MONEY_UNIT),
     *                 'DDBWRA', I, DESCR(50)
270     CONTINUE
        DO 280 I=1,2
          WRITE(RLU,918) DDBWPO_OFF+(I-1)*2, DDBWPO(1,I),
     *                 CSMONY(DDBWPO(2,I),12,MONEY_UNIT),
     *                 'DDBWPO', I, DESCR(51)
280     CONTINUE
        DO 290 I=1,2
          WRITE(RLU,918) DDBWPA_OFF+(I-1)*2, DDBWPA(1,I),
     *                 CSMONY(DDBWPA(2,I),12,MONEY_UNIT),
     *                 'DDBWPA', I, DESCR(52)
290     CONTINUE
        DO 300 I=1,2
          WRITE(RLU,918) DDBWPR_OFF+(I-1)*2, DDBWPR(1,I),
     *                 CSMONY(DDBWPR(2,I),12,MONEY_UNIT),
     *                 'DDBWPR', I, DESCR(53)
300     CONTINUE
	DO 310 I=1,MAXDBLRW*MAXDBLRW
	  WRITE(RLU,919) DDBODT_OFF+I-1, CSMONY(DDBODT(I),12,MONEY_UNIT),
     *                'DDBODT', I, DESCR(54)
310	CONTINUE
	WRITE(RLU,900) DDBPCC_OFF, DDBPCC, 'DDBPCC', DESCR(55)
	WRITE(RLU,900) DDBCMB_OFF, DDBCMB, 'DDBCMB', DESCR(56)



	RETURN  
C
C
900	FORMAT(1X,I5,1X,I12,1X,15X,A6,9X,A30)
901	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,')',5X,A30)
902	FORMAT(1X,I5,1X,I12,1X,I12,3X,A6,'(',I2,',*)',3X,A30)
903	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,')',2X,A30)
904	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,',',I2,')',1X,A28)
905	FORMAT(1X,I5,1X,I12,1X,10X,A11,8X,A30)
906	FORMAT(1X,I5,1X,A13,1X,A13,1X,A6,'(',I2,')',5X,A30)
907	FORMAT(1X,I5,1X,A16,1X,10X,1X,A6,9X,A30)
908	FORMAT(1X,I5,1X,5A4,1X,7X,A6,9X,A30)
909	FORMAT(1X,I5,1X,4A4,12X,A6,9X,A30)
910	FORMAT(1X,I5,1X,8X,A4,1X,12X,3X,A6,9X,A30)
911	FORMAT(1X,I5,1X,4A4,1X,10X,1X,A6,'(1,',I2,')',3X,A30)
912	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I2,')',3X,A30)
913	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,9X,A30)
914	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,')',5X,A30)
915	FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)
916	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,'(',I2,')',5X,A30)
917	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,9X,A30)
918	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I2,')',3X,A30)
919	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I3,')',4X,A30)
920	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*)',6X,A30)



C
	END

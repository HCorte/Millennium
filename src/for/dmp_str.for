C DMP_STR.FOR
C
C V01 16-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP SUPER TRIPLE GAME FILE
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
	SUBROUTINE DMP_STR(RLU,FILE,DRAW,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DSTREC.DEF'

        ! arguments
	INTEGER*4  RLU				!
	INTEGER*4  FILE(5)			!
        INTEGER*4  DRAW                         !
	INTEGER*4  MONEY_UNIT			!

	INTEGER*4 LUN
	INTEGER*4 I,J,K

        CHARACTER DESCR(55)*30

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
     *            'TV-CHANNEL NAME               ',
     *            'MULTI DRAW SELECTED TABLE     ',
     *            'ACTUAL # OF ROWS USED.        ',
     *            '# WINNING COUPONS PLAYED.     ',
     *            'CNT, AMT PLAYED.              ',
     *            'WINNING:CNT,AMT REFUNDS ONLY  ',
     *            'WINNING:CNT,AMT REFUNDS ALTOGE',
     *            'WINNING:CNT,AMT PRIZES ONLY   ',
     *            'WINNING:CNT,AMT PRIZES ALTOGET',
     *            'WINNING:CNT,AMT PRIZES+REFUNDS',
     *            'Partially closed combinations.',
     *            'NUMBER OF WINNING COMBINATIONS'/




C
C READ GAME FILE
C
	LUN = 9     ! ?
        CALL READGFL(LUN,FILE,DSTSEC,DRAW,DSTREC)
C
C DUMP RECORD
C
	WRITE(RLU,900) DSTSTS_OFF, DSTSTS, 'DSTSTS', DESCR(1)
	WRITE(RLU,900) DSTWEK_OFF, DSTWEK, 'DSTWEK', DESCR(2)
	WRITE(RLU,900) DSTDAT_OFF, DSTDAT, 'DSTDAT', DESCR(3)
	WRITE(RLU,900) DSTDRW_OFF, DSTDRW, 'DSTDRW', DESCR(4)
	WRITE(RLU,900) DSTBSD_OFF, DSTBSD, 'DSTBSD', DESCR(5)
	WRITE(RLU,900) DSTESD_OFF, DSTESD, 'DSTESD', DESCR(6)
	WRITE(RLU,900) DSTPUP_OFF, DSTPUP, 'DSTPUP', DESCR(7)
	WRITE(RLU,900) DSTUPD_OFF, DSTUPD, 'DSTUPD', DESCR(8)
	WRITE(RLU,914) DSTCTM_OFF, DISTIM(DSTCTM), 'DSTCTM', DESCR(9)
	WRITE(RLU,914) DSTTIM_OFF, DISTIM(DSTTIM), 'DSTTIM', DESCR(10)
	WRITE(RLU,900) DSTSER_OFF, DSTSER, 'DSTSER', DESCR(11)
 	WRITE(RLU,920) DSTSAL_OFF, DSTSAL(1), CSMONY(DSTSAL(2),12,MONEY_UNIT),
     *                'DSTSAL', DESCR(12)
	WRITE(RLU,916) DSTPAD_OFF, CSMONY(DSTPAD,12,MONEY_UNIT),
     *              'DSTPAD', DESCR(13)
	WRITE(RLU,916) DSTPRG_OFF, CSMONY(DSTPRG,12,MONEY_UNIT),
     *              'DSTPRG', DESCR(14)
	WRITE(RLU,916) DSTPRF_OFF, CSMONY(DSTPRF,12,MONEY_UNIT),
     *              'DSTPRF', DESCR(15)
	WRITE(RLU,916) DSTREF_OFF, CSMONY(DSTREF,12,MONEY_UNIT),
     *              'DSTREF', DESCR(16)
	WRITE(RLU,916) DSTERF_OFF, CSMONY(DSTERF,12,MONEY_UNIT),
     *              'DSTERF', DESCR(17)
	WRITE(RLU,916) DSTTER_OFF, CSMONY(DSTTER,12,MONEY_UNIT),
     *              'DSTTER', DESCR(18)
	WRITE(RLU,916) DSTWON_OFF, CSMONY(DSTWON,12,MONEY_UNIT),
     *              'DSTWON', DESCR(19)
	WRITE(RLU,915) DSTPOL_OFF,   CSMONY(DSTPOL(1),12,MONEY_UNIT),
     *              'DSTPOL', 1, DESCR(20)
	WRITE(RLU,915) DSTPOL_OFF+1, CSMONY(DSTPOL(2),12,MONEY_UNIT),
     *              'DSTPOL', 2, DESCR(20)
	WRITE(RLU,916) DSTTPL_OFF,   CSMONY(DSTTPL,12,MONEY_UNIT),
     *              'DSTTPL',    DESCR(21)
	WRITE(RLU,916) DSTTBK_OFF,   CSMONY(DSTTBK,12,MONEY_UNIT),
     *              'DSTTBK',    DESCR(22)
	WRITE(RLU,915) DSTBRK_OFF,   CSMONY(DSTBRK(1),12,MONEY_UNIT),
     *              'DSTBRK', 1, DESCR(23)
	WRITE(RLU,915) DSTBRK_OFF+1, CSMONY(DSTBRK(2),12,MONEY_UNIT),
     *              'DSTBRK', 2, DESCR(23)
	WRITE(RLU,916) DSTABW_OFF,   CSMONY(DSTABW,12,MONEY_UNIT),
     *              'DSTABW',    DESCR(24)
	DO 110 I=1,MAXSTRTI
 	  WRITE(RLU,901) DSTODS_OFF+I-1, DSTODS(I), 'DSTODS', I, DESCR(25)
110	CONTINUE
        DO 120 I=1,3
	  DO J=1,MAXSTRTI
            WRITE(RLU,903) DSTWIN_OFF+(I-1)*MAXSTRTI+J-1, DSTWIN(I,J), 
     *                  'DSTWIN', I, J, DESCR(26)
	ENDDO
120     CONTINUE
        DO 130 I=1,3
	  DO J=1,MAXSTRTI
            WRITE(RLU,903) DSTHLD_OFF+(I-1)*MAXSTRTI+J-1, DSTHLD(I,J), 
     *                  'DSTHLD', I, J, DESCR(27)
	ENDDO
130     CONTINUE
	WRITE(RLU,900) DSTTAX_OFF, DSTTAX, 'DSTTAX', DESCR(28)
	DO 140 I=1,NUMTOT
	  WRITE(RLU,901) DSTOTX_OFF+I-1, DSTOTX(I), 'DSTOTX', I, DESCR(29)
140	CONTINUE
	DO 150 I=1,NUMTOT
	  WRITE(RLU,901) DSTMID_OFF+I-1, DSTMID(I), 'DSTMID', I, DESCR(30)
150	CONTINUE
	DO 160 I=1,NUMTOT
	  WRITE(RLU,901) DSTUTX_OFF+I-1, DSTUTX(I), 'DSTUTX', I, DESCR(31)
160	CONTINUE
	DO 170 I=1,MAXSTRTI
	  WRITE(RLU,901) DSTHST_OFF+I-1, DSTHST(I), 'DSTHST', I, DESCR(32)
170	CONTINUE
	DO 180 I=1,NUMTOT
	  WRITE(RLU,901) DSTORM_OFF+I-1, DSTORM(I), 'DSTORM', I, DESCR(33)
180	CONTINUE
	WRITE(RLU,908) DSTENM_OFF, (DSTENM(K),K=1,5), 'DSTENM', DESCR(34)
	WRITE(RLU,908) DSTDES_OFF, (DSTDES(K),K=1,5), 'DSTDES', DESCR(35)
        DO 190 I=1,MAXSTRRW
          WRITE(RLU,911) DSTNMS_OFF+(I-1)*STRNMS_LEN/4, (DSTNMS(K,I),K=1,4),
     *                  'DSTNMS', I, DESCR(36)
190     CONTINUE
        DO 200 I=1,MAXSTRRW
          WRITE(RLU,901) DSTSTA_OFF+I-1, DSTSTA(I), 'DSTSTA', I, DESCR(37)
200     CONTINUE
        DO 210 I=1,MAXSTRRW
          WRITE(RLU,901) DSTSBR_OFF+I-1, DSTSBR(I), 'DSTSBR', I, DESCR(38)
210     CONTINUE
	WRITE(RLU,900) DSTREV_OFF, DSTREV, 'DSTREV', DESCR(39)
	WRITE(RLU,908) DSTPFN_OFF, (DSTPFN(K),K=1,5), 'DSTPFN', DESCR(40)
        DO 220 I=1,MAXSTRRW
          WRITE(RLU,917) DSTRTM_OFF+I-1, DISTIM(DSTRTM(I)), 'DSTRTM', I, DESCR(41)
220     CONTINUE
	WRITE(RLU,916) DSTPRC_OFF, CSMONY(DSTPRC,12,MONEY_UNIT),
     *              'DSTPRC', DESCR(42)
	WRITE(RLU,918) DSTSPR_OFF, DISPER(DSTSPR), 'DSTSPR', DESCR(43)
        WRITE(RLU,910) DSTTVC_OFF, DSTTVC(1), 'DSTTVC', DESCR(44)
        DO 230 I=1,MAXMLTD_AVL
          WRITE(RLU,901) DSTMDS_OFF+I-1, DSTMDS(I), 'DSTMDS', I, DESCR(45)
230     CONTINUE
	WRITE(RLU,900) DSTRWS_OFF, DSTRWS, 'DSTRWS', DESCR(46)
	DO 240 I=1,MAXSTRTI
	  WRITE(RLU,901) DSTWCP_OFF+I-1, DSTWCP(I), 'DSTWCP', I, DESCR(47)
240	CONTINUE
        DO 250 I=1,MAXSTRTI
          WRITE(RLU,919) DSTWBT_OFF+I-1, DSTWBT(1,I), 
     *                 CSMONY(DSTWBT(2,I),12,MONEY_UNIT),
     *                'DSTWBT', I, DESCR(48)
250     CONTINUE
        DO 260 I=1,2
          WRITE(RLU,919) DSTWRO_OFF+(I-1)*2, DSTWRO(1,I), 
     *                 CSMONY(DSTWRO(2,I),12,MONEY_UNIT),
     *                 'DSTWRO', I, DESCR(49)
260     CONTINUE
        DO 270 I=1,2
          WRITE(RLU,919) DSTWRA_OFF+(I-1)*2, DSTWRA(1,I),
     *                 CSMONY(DSTWRA(2,I),12,MONEY_UNIT),
     *                 'DSTWRA', I, DESCR(50)
270     CONTINUE
        DO 280 I=1,2
          WRITE(RLU,919) DSTWPO_OFF+(I-1)*2, DSTWPO(1,I),
     *                 CSMONY(DSTWPO(2,I),12,MONEY_UNIT),
     *                 'DSTWPO', I, DESCR(51)
280     CONTINUE
        DO 290 I=1,2
          WRITE(RLU,919) DSTWPA_OFF+(I-1)*2, DSTWPA(1,I),
     *                 CSMONY(DSTWPA(2,I),12,MONEY_UNIT),
     *                 'DSTWPA', I, DESCR(52)
290     CONTINUE
        DO 300 I=1,2
          WRITE(RLU,919) DSTWPR_OFF+(I-1)*2, DSTWPR(1,I),
     *                 CSMONY(DSTWPR(2,I),12,MONEY_UNIT),
     *                 'DSTWPR', I, DESCR(53)
300     CONTINUE
	WRITE(RLU,900) DSTPCC_OFF, DSTPCC, 'DSTPCC', DESCR(54)
	WRITE(RLU,900) DSTCMB_OFF, DSTCMB, 'DSTCMB', DESCR(55)



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
909	FORMAT(1X,I5,1X,A17,1X,A6,9X,A30)
910	FORMAT(1X,I5,1X,8X,A4,1X,15X,A6,9X,A30)
911	FORMAT(1X,I5,1X,4A4,1X,10X,1X,A6,'(1,',I2,')',3X,A30)
913	FORMAT(1X,I5,1X,A16,1X,10X,1X,A6,'(1,',I2,',',I2,')',1X,A28)
914	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,9X,A30)
915	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,')',5X,A30)
916	FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)
917	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,'(',I2,')',5X,A30)
918	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,9X,A30)
919	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I2,')',3X,A30)
920	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*)',6X,A30)


C
	END

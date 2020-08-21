C DMP_TGL.FOR
C
C V01 29-NOV-2000 UXN Initial release.
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
	SUBROUTINE DMP_TGL(RLU,FILE,DRAW,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DTGREC.DEF'

        ! arguments
	INTEGER*4  RLU
	INTEGER*4  FILE(5)			!
        INTEGER*4  DRAW                         !
	INTEGER*4  MONEY_UNIT			!

	INTEGER*4 LUN
	INTEGER*4 I,K

        CHARACTER DESCR(54)*30

        DATA DESCR/
     *            'STATUS                        ',
     *            'ACTUAL TIME WHEN CLOSED       ',
     *            'TIME WHEN GAME SHOULD CLOSE   ',
     *            'DRAW NUMBER                   ',
     *            'SPORT BEGGINING DRAW DATE     ',
     *            'SPORT ENDING DRAW DATE        ',
     *            'LAST PURGE UPDATE             ',
     *            'LAST FILE UPDATE              ',
     *            'DRAW DATES                    ',
     *            'ADVANCE DRAW DATES            ',
     *            'SALES DATA                    ',
     *            'SHARE VALUES                  ',
     *            'SHARES                        ',
     *            'POOL CARRIED OVER             ',
     *            'AMOUNT PAID                   ',
     *            'AMOUNT PURGED                 ',
     *            'ANNUITY PRIZES                ',
     *            'BREAKAGE AMOUNT BY POOL       ',
     *            'OLD SHARE VALUES              ',
     *            'DIVISION PAYOUT FROZEN FLAGS  ',
     *            'WINNING RESULTS               ',
     *            'WINNING RESULTS HOLD          ',
     *            '% OF THE PROMISED AMOUNT      ',
     *            'MAX NUM. OF WINNERS FOR PR.AMT',
     *            'LOTTERY TAX                   ',
     *            'RESERVE POOL                  ',
     *            'ADDITIONAL POOL               ',
     *            'MINIMUM POOL                  ',
     *            'LAST SERIAL NUMBER            ',
     *            'OVER-RIDE POOL AMOUNT         ',
     *            'PRICE/SINGLE COMBINATION      ',
     *            'MAXIMUM NUMBER OF EVENTS      ',
     *            'MULTI-DRAW ENABLE FLAG        ',
     *            'MULTI-DRAW SELECTED TABLE     ',
     *            '# OF DIVISIONS                ',
     *            'DIVISION MATCH TABLE          ',
     *            'DIVISION PERCENTAGES          ',
     *            'SALES PERCENTAGE              ',
     *            'TOTAL SHARE COUNT             ',
     *            'SPORT REVISION NUMBER         ',
     *            'TEAM NAMES                    ',
     *            'CLUB VOTING COUNT             ',
     *            'ADDITIONAL POOL BY DIV.       ',
     *            'FLAG FOR FREE GAME (PRICE = 0)',
     *            'WIN RESERVE FUND              ',
     *            'BEST MATCH FLAG               ',
     *            'BALANCE SALES INFO. BY DRAW   ',
     *            'EVENT NAME                    ',
     *            'ROLLOVER RULE TO APPLY        ',
     *            'ROLLOVER DIVISION             ',
     *            'FIXED PRIZE (0=NOT FIXED)     ',
     *            'EXTRAORDINARY DRAW OPTION     ',
     *            'WHERE ROLLOVER GO TO          ',
     *            'WHERE ROLLOVER CAME FROM      '/




C
C READ GAME FILE
C
	LUN = 9     ! ?
        CALL READGFL(LUN,FILE,DTGSEC,DRAW,DTGREC)
C
C DUMP RECORD
C
	WRITE(RLU,900) DTGSTS_OFF, DTGSTS, 'DTGSTS', DESCR(1)
	WRITE(RLU,908) DTGCTM_OFF, DISTIM(DTGCTM), 'DTGCTM', DESCR(2)
	WRITE(RLU,908) DTGTIM_OFF, DISTIM(DTGTIM), 'DTGTIM', DESCR(3)
	WRITE(RLU,900) DTGDRW_OFF, DTGDRW, 'DTGDRW', DESCR(4)
	WRITE(RLU,900) DTGBSD_OFF, DTGBSD, 'DTGBSD', DESCR(5)
	WRITE(RLU,900) DTGESD_OFF, DTGESD, 'DTGESD', DESCR(6)
	WRITE(RLU,900) DTGPUP_OFF, DTGPUP, 'DTGPUP', DESCR(7)
	WRITE(RLU,900) DTGUPD_OFF, DTGUPD, 'DTGUPD', DESCR(8)
	DO 100 I=1,DATLEN
	  WRITE(RLU,901) DTGDAT_OFF+I-1, DTGDAT(I), 'DTGDAT', I, DESCR(9)
100	CONTINUE
	DO 110 I=1,NUMADV
	  WRITE(RLU,901) DTGADV_OFF+I-1, DTGADV(I), 'DTGADV', I, DESCR(10)
110	CONTINUE
	DO 120 I=1,TGGENT
	  WRITE(RLU,909) DTGSAL_OFF+I-1, CSMONY(DTGSAL(I),12,MONEY_UNIT),
     *                'DTGSAL', I, DESCR(11)
120	CONTINUE
	DO 130 I=1,TGGDIV
	  WRITE(RLU,909) DTGSHV_OFF+I-1, CSMONY(DTGSHV(I),12,MONEY_UNIT),
     *                'DTGSHV', I, DESCR(12)
130	CONTINUE
	DO 140 I=1,TGGDIV
	  WRITE(RLU,901) DTGSHR_OFF+I-1, DTGSHR(I), 'DTGSHR', I, DESCR(13)
140	CONTINUE
	DO 145 I=1,TGGDIV
	  WRITE(RLU,909) DTGPOL_OFF+I-1, CSMONY(DTGPOL(I),12,MONEY_UNIT),
     *                'DTGPOL', I, DESCR(14)
145	CONTINUE
	DO 150 I=1,TGGDIV
	  WRITE(RLU,909) DTGPAD_OFF+I-1, CSMONY(DTGPAD(I),12,MONEY_UNIT),
     *                'DTGPAD', I, DESCR(15)
150	CONTINUE
	DO 160 I=1,TGGDIV
	  WRITE(RLU,909) DTGPRG_OFF+I-1, CSMONY(DTGPRG(I),12,MONEY_UNIT),
     *                'DTGPRG', I, DESCR(16)
160	CONTINUE
	DO 170 I=1,TGGDIV
	  WRITE(RLU,909) DTGAFD_OFF+I-1, CSMONY(DTGAFD(I),12,MONEY_UNIT),
     *                'DTGAFD', I, DESCR(17)
170	CONTINUE
	DO 180 I=1,TGGDIV
	  WRITE(RLU,909) DTGCSP_OFF+I-1, CSMONY(DTGCSP(I),12,MONEY_UNIT),
     *                'DTGCSP', I, DESCR(18)
180	CONTINUE
	DO 190 I=1,TGGDIV
	  WRITE(RLU,909) DTGOSV_OFF+I-1, CSMONY(DTGOSV(I),12,MONEY_UNIT),
     *                'DTGOSV', I, DESCR(19)
190	CONTINUE
	DO 200 I=1,TGGDIV
	  WRITE(RLU,901) DTGOSH_OFF+I-1, DTGOSH(I), 'DTGOSH', I, DESCR(20)
200	CONTINUE
	DO 210 I=1,TGGNBR
	  WRITE(RLU,901) DTGWIN_OFF+2*I-2, DTGWIN(1,I), '1.DTGWIN', I, DESCR(21)
	  WRITE(RLU,901) DTGWIN_OFF+2*I-1, DTGWIN(2,I), '2.DTGWIN', I, DESCR(21)
210	CONTINUE
	DO 220 I=1,TGGNBR
	  WRITE(RLU,901) DTGHLD_OFF+2*I-2, DTGHLD(1,I), '1.DTGHLD', I, DESCR(22)
	  WRITE(RLU,901) DTGHLD_OFF+2*I-1, DTGHLD(2,I), '2.DTGHLD', I, DESCR(22)
220	CONTINUE
	WRITE(RLU,910) DTGPRP_OFF, DISPER(DTGPRP), 'DTGPRP', DESCR(23)
	WRITE(RLU,900) DTGPRN_OFF, DTGPRN, 'DTGPRN', DESCR(24)
	WRITE(RLU,900) DTGTAX_OFF, DTGTAX, 'DTGTAX', DESCR(25)
	WRITE(RLU,909) DTGRES_OFF,   CSMONY(DTGRES(1),12,MONEY_UNIT),
     *              'DTGRES', 1, DESCR(26)
	WRITE(RLU,909) DTGRES_OFF+1, CSMONY(DTGRES(2),12,MONEY_UNIT),
     *              'DTGRES', 2, DESCR(26)
	WRITE(RLU,911) DTGAPL_OFF, CSMONY(DTGAPL,12,MONEY_UNIT),
     *              'DTGAPL', DESCR(27)
	WRITE(RLU,911) DTGMIN_OFF, CSMONY(DTGMIN,12,MONEY_UNIT),
     *              'DTGMIN', DESCR(28)
	WRITE(RLU,900) DTGSER_OFF, DTGSER, 'DTGSER', DESCR(29)
	WRITE(RLU,911) DTGOPA_OFF, CSMONY(DTGOPA,12,MONEY_UNIT),
     *              'DTGOPA', DESCR(30)
	WRITE(RLU,911) DTGPRC_OFF, CSMONY(DTGPRC,12,MONEY_UNIT),
     *              'DTGPRC', DESCR(31)
	WRITE(RLU,900) DTGMAX_OFF, DTGMAX, 'DTGMAX', DESCR(32)
	WRITE(RLU,900) DTGMLT_OFF, DTGMLT, 'DTGMLT', DESCR(33)
	DO 230 I=1,MAXMLTD_AVL
	  WRITE(RLU,901) DTGMDS_OFF+I-1, DTGMDS(I), 'DTGMDS', I, DESCR(34)
230	CONTINUE
	WRITE(RLU,900) DTGDIV_OFF, DTGDIV, 'DTGDIV', DESCR(35)
	DO 240 I=1,TGGDIV
	  WRITE(RLU,901) DTGMAT_OFF+I-1, DTGMAT(I), 'DTGMAT', I, DESCR(36)
240	CONTINUE
	DO 250 I=1,TGGDIV
	  WRITE(RLU,912) DTGPER_OFF+I-1, DISPER(DTGPER(I)), 'DTGPER', I, DESCR(37)
250	CONTINUE
	WRITE(RLU,910) DTGSPR_OFF, DISPER(DTGSPR), 'DTGSPR', DESCR(38)
	DO 260 I=1,TGGDIV
	  WRITE(RLU,901) DTGTSR_OFF+I-1, DTGTSR(I), 'DTGTSR', I, DESCR(39)
260	CONTINUE
	WRITE(RLU,900) DTGREV_OFF, DTGREV, 'DTGREV', DESCR(40)
	DO 270 I=1,TGGNBR
	  WRITE(RLU,906) DTGNMS_OFF+(I-1)*2*SPNMS_LEN,
     *                (DTGNMS(K,1,I),K=1,3), (DTGNMS(K,2,I),K=1,3),
     *                'DTGNMS', I, DESCR(41)
270	CONTINUE
	DO 280 I=1,25
	  WRITE(RLU,901) DTGCLB_OFF+I-1, DTGCLB(I), 'DTGCLB', I, DESCR(42)
280	CONTINUE
	DO 290 I=1,TGGDIV
	  WRITE(RLU,909) DTGASH_OFF+I-1, CSMONY(DTGASH(I),12,MONEY_UNIT),
     *                'DTGASH', I, DESCR(43)
290	CONTINUE
	WRITE(RLU,900) DTGFRG_OFF, DTGFRG, 'DTGFRG', DESCR(44)
	DO 300 I=1,POSTED
	  WRITE(RLU,909) DTGWRF_OFF+I-1, CSMONY(DTGWRF(I),12,MONEY_UNIT),
     *                  'DTGWRF', I, DESCR(45)
300	CONTINUE
	WRITE(RLU,900) DTGBST_OFF, DTGBST, 'DTGBST', DESCR(46)
	DO 310 I=1,MAXDRW
	  WRITE(RLU,909) DTGBAL_OFF+I-1, CSMONY(DTGBAL(I),12,MONEY_UNIT),
     *                  'DTGBAL', I, DESCR(47)
310	CONTINUE
	WRITE(RLU,907) DTGEVN_OFF, (DTGEVN(K),K=1,4), 'DTGEVN', DESCR(48)
C
	WRITE(RLU,900) DTGEXT_OFF, DTGEXT, 'DTGEXT', DESCR(52)        
	WRITE(RLU,901) DTGRGT_OFF,   DTGRGT(1), 'DTGRGT', 1, DESCR(53)
	WRITE(RLU,901) DTGRGT_OFF+1, DTGRGT(2), 'DTGRGT', 2, DESCR(53)
	WRITE(RLU,901) DTGROD_OFF,   DTGROD(1), 'DTGROD', 1, DESCR(54)
	WRITE(RLU,901) DTGROD_OFF+1, DTGROD(2), 'DTGROD', 2, DESCR(54)



	RETURN  
C
C
900	FORMAT(1X,I5,1X,I12,1X,15X,A6,9X,A30)
901	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,')',5X,A30)
902	FORMAT(1X,I5,1X,I12,1X,I12,3X,A6,'(',I2,',*)',3X,A30)
903	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,')',2X,A30)
904	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,',',I2,')',1X,A28)
905	FORMAT(1X,I5,1X,I12,1X,15X,A11,8X,A30)
906	FORMAT(1X,I5,1X,3A4,1X,3A4,3X,A6,'(',I2,')',5X,A30)
907	FORMAT(1X,I5,1X,4A4,1X,10X,1X,A6,9X,A30)
908	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,9X,A30)
909	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,')',5X,A30)
910	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,9X,A30)
911	FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)
912	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,'(',I2,')',5X,A30)

C
	END

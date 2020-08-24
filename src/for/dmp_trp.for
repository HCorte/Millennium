C DMP_TRP.FOR
C
C V01 16-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP TRP GAME FILE
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
	SUBROUTINE DMP_TRP(RLU,FILE,DRAW,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DTRREC.DEF'

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
     *            'MASTER EVENT NAME             ',
     *            'EVENT NAME                    ',
     *            'EVENT DESCRIPTION             ',
     *            'ROW NAMES                     ',
     *            'ROW STATUS                    ',
     *            'EVENT STATUS                  ',
     *            'EVENT BEGIN DATE              ',
     *            'EVENT BEGIN TIME              ',
     *            'SALES BY ROW TABLE            ',
     *            'REV #                         ',
     *            'POOL FILE NAME                ',
     *            'BASE PRICE                    ',
     *            'POOL PERCENTAGE               ',
     *            'ACTUAL # OF ROWS USED.        ',
     *            '# WINNING COUPONS PLAYED      ',
     *            'CNT, AMT WINNERS PLAYED       ',
     *            'WINNING:CNT,AMT REFUNDS ONLY  ',
     *            'WINNING:CNT,AMT REFUNDS ALTOGE',
     *            'WINNING:CNT,AMT PRIZES ONLY   ',
     *            'WINNING:CNT,AMT PRIZES ALTOGET',
     *            'WINNING:CNT,AMT PRIZES+REFUNDS',
     *            'NUMBER OF WINNING COMBINATIONS'/




C
C READ GAME FILE
C
	LUN = 9     ! ?
        CALL READGFL(LUN,FILE,DTRSEC,DRAW,DTRREC)
C
C DUMP RECORD
C
	WRITE(RLU,900) DTRSTS_OFF, DTRSTS, 'DTRSTS', DESCR(1)
	WRITE(RLU,900) DTRWEK_OFF, DTRWEK, 'DTRWEK', DESCR(2)
	WRITE(RLU,900) DTRDAT_OFF, DTRDAT, 'DTRDAT', DESCR(3)
	WRITE(RLU,900) DTRDRW_OFF, DTRDRW, 'DTRDRW', DESCR(4)
	WRITE(RLU,900) DTRBSD_OFF, DTRBSD, 'DTRBSD', DESCR(5)
	WRITE(RLU,900) DTRESD_OFF, DTRESD, 'DTRESD', DESCR(6)
	WRITE(RLU,900) DTRPUP_OFF, DTRPUP, 'DTRPUP', DESCR(7)
	WRITE(RLU,900) DTRUPD_OFF, DTRUPD, 'DTRUPD', DESCR(8)
	WRITE(RLU,914) DTRCTM_OFF, DISTIM(DTRCTM), 'DTRCTM', DESCR(9)
	WRITE(RLU,914) DTRTIM_OFF, DISTIM(DTRTIM), 'DTRTIM', DESCR(10)
	WRITE(RLU,900) DTRSER_OFF, DTRSER, 'DTRSER', DESCR(11)
 	WRITE(RLU,920) DTRSAL_OFF, DTRSAL(1), CSMONY(DTRSAL(2),12,MONEY_UNIT),
     *                'DTRSAL', DESCR(12)
	WRITE(RLU,916) DTRPAD_OFF, CSMONY(DTRPAD,12,MONEY_UNIT),
     *              'DTRPAD', DESCR(13)
	WRITE(RLU,916) DTRPRG_OFF, CSMONY(DTRPRG,12,MONEY_UNIT),
     *              'DTRPRG', DESCR(14)
	WRITE(RLU,916) DTRPRF_OFF, CSMONY(DTRPRF,12,MONEY_UNIT),
     *              'DTRPRF', DESCR(15)
	WRITE(RLU,916) DTRREF_OFF, CSMONY(DTRREF,12,MONEY_UNIT),
     *              'DTRREF', DESCR(16)
	WRITE(RLU,916) DTRERF_OFF, CSMONY(DTRERF,12,MONEY_UNIT),
     *              'DTRERF', DESCR(17)
	WRITE(RLU,916) DTRTER_OFF, CSMONY(DTRTER,12,MONEY_UNIT),
     *              'DTRTER', DESCR(18)
	WRITE(RLU,916) DTRWON_OFF, CSMONY(DTRWON,12,MONEY_UNIT),
     *              'DTRWON', DESCR(19)
	WRITE(RLU,915) DTRPOL_OFF,   CSMONY(DTRPOL(1),12,MONEY_UNIT),
     *              'DTRPOL', 1, DESCR(20)
	WRITE(RLU,915) DTRPOL_OFF+1, CSMONY(DTRPOL(2),12,MONEY_UNIT),
     *              'DTRPOL', 2, DESCR(20)
	WRITE(RLU,916) DTRTPL_OFF,   CSMONY(DTRTPL,12,MONEY_UNIT),
     *              'DTRTPL',    DESCR(21)
	WRITE(RLU,916) DTRTBK_OFF,   CSMONY(DTRTBK,12,MONEY_UNIT),
     *              'DTRTBK',    DESCR(22)
	WRITE(RLU,915) DTRBRK_OFF,   CSMONY(DTRBRK(1),12,MONEY_UNIT),
     *              'DTRBRK', 1, DESCR(23)
	WRITE(RLU,915) DTRBRK_OFF+1, CSMONY(DTRBRK(2),12,MONEY_UNIT),
     *              'DTRBRK', 2, DESCR(23)
	WRITE(RLU,916) DTRABW_OFF,   CSMONY(DTRABW,12,MONEY_UNIT),
     *              'DTRABW',    DESCR(24)
	DO 110 I=1,MAXTRPTI
 	  WRITE(RLU,901) DTRODS_OFF+I-1, DTRODS(I), 'DTRODS', I, DESCR(25)
110	CONTINUE
        DO 120 I=1,3
	  DO J=1,MAXTRPTI
            WRITE(RLU,903) DTRWIN_OFF+(I-1)*MAXTRPTI+J-1, DTRWIN(I,J), 
     *                  'DTRWIN', I, J, DESCR(26)
	ENDDO
120     CONTINUE
        DO 130 I=1,3
	  DO J=1,MAXTRPTI
            WRITE(RLU,903) DTRHLD_OFF+(I-1)*MAXTRPTI+J-1, DTRHLD(I,J), 
     *                  'DTRHLD', I, J, DESCR(27)
	ENDDO
130     CONTINUE
	WRITE(RLU,900) DTRTAX_OFF, DTRTAX, 'DTRTAX', DESCR(28)
	DO 140 I=1,NUMTOT
	  WRITE(RLU,901) DTROTX_OFF+I-1, DTROTX(I), 'DTROTX', I, DESCR(29)
140	CONTINUE
	DO 150 I=1,NUMTOT
	  WRITE(RLU,901) DTRMID_OFF+I-1, DTRMID(I), 'DTRMID', I, DESCR(30)
150	CONTINUE
	DO 160 I=1,NUMTOT
	  WRITE(RLU,901) DTRUTX_OFF+I-1, DTRUTX(I), 'DTRUTX', I, DESCR(31)
160	CONTINUE
	DO 170 I=1,MAXTRPTI
	  WRITE(RLU,915) DTRHST_OFF+I-1, CSMONY(DTRHST(I),12,MONEY_UNIT),
     *                  'DTRHST', I, DESCR(32)
170	CONTINUE
	DO 180 I=1,NUMTOT
	  WRITE(RLU,901) DTRORM_OFF+I-1, DTRORM(I), 'DTRORM', I, DESCR(33)
180	CONTINUE
	WRITE(RLU,908) DTRMNM_OFF, (DTRMNM(K),K=1,5), 'DTRMNM', DESCR(34)
	DO 190 I=1,3
	  WRITE(RLU,911) DTRENM_OFF+(I-1)*TRPENM_LEN/4, (DTRENM(K,I),K=1,4), 
     *                'DTRENM', I, DESCR(35)
190	CONTINUE
	DO 200 I=1,3
	  WRITE(RLU,911) DTRDES_OFF+(I-1)*TRPDES_LEN/4, (DTRDES(K,I),K=1,4), 
     *                'DTRDES', I, DESCR(36)
200	CONTINUE
	DO 210 I=1,MAXTRPRW
	  DO J=1,3
	  WRITE(RLU,913) DTRNMS_OFF+(I-1)*3*TRPNMS_LEN/4+(J-1)*TRPNMS_LEN/4,
     *                 (DTRNMS(K,I,J),K=1,4), 'DTRNMS', I, J, DESCR(37)
	  ENDDO
210	CONTINUE
	DO 220 I=1,MAXTRPRW
	  DO J=1,3
	    WRITE(RLU,903) DTRSTA_OFF+(I-1)*3+J-1, DTRSTA(I,J), 'DTRSTA', 
     *                   I, J, DESCR(38)
	  ENDDO
220	CONTINUE
	DO 230 I=1,3
	  WRITE(RLU,901) DTREST_OFF+I-1, DTREST(I), 'DTREST', I, DESCR(39)
230	CONTINUE
	DO 240 I=1,3
	  WRITE(RLU,901) DTREVD_OFF+I-1, DTREVD(I), 'DTREVD', I, DESCR(40)
240	CONTINUE
	DO 250 I=1,3
	  WRITE(RLU,901) DTREVT_OFF+I-1, DTREVT(I), 'DTREVT', I, DESCR(41)
250	CONTINUE
	DO 260 I=1,MAXTRPRW
	  DO J=1,3
	    WRITE(RLU,917) DTRSBR_OFF+(I-1)*3+J-1, 
     *                   CSMONY(DTRSBR(I,J),12,MONEY_UNIT), 'DTRSBR', 
     *                   I, J, DESCR(42)
	  ENDDO
260	CONTINUE
	WRITE(RLU,900) DTRREV_OFF, DTRREV, 'DTRREV', DESCR(43)
	WRITE(RLU,908) DTRPFN_OFF, (DTRPFN(K),K=1,5), 'DTRPFN', DESCR(44)
	WRITE(RLU,916) DTRPRC_OFF, CSMONY(DTRPRC,12,MONEY_UNIT),
     *              'DTRPRC', DESCR(45)
	WRITE(RLU,918) DTRSPR_OFF, DISPER(DTRSPR), 'DTRSPR', DESCR(46)
	DO 270 I=1,3
	  WRITE(RLU,901) DTRRWS_OFF+I-1, DTRRWS(I), 'DTRRWS', I, DESCR(47)
270	CONTINUE
	DO 280 I=1,MAXTRPTI
	  WRITE(RLU,901) DTRWCP_OFF+I-1, DTRWCP(I), 'DTRWCP', I, DESCR(48)
280	CONTINUE
        DO 290 I=1,MAXTRPTI
          WRITE(RLU,919) DTRWBT_OFF+I-1, DTRWBT(1,I), 
     *                 CSMONY(DTRWBT(2,I),12,MONEY_UNIT),
     *                'DTRWBT', I, DESCR(49)
290     CONTINUE
        DO 300 I=1,2
          WRITE(RLU,919) DTRWRO_OFF+(I-1)*2, DTRWRO(1,I), 
     *                 CSMONY(DTRWRO(2,I),12,MONEY_UNIT),
     *                 'DTRWRO', I, DESCR(50)
300     CONTINUE
        DO 310 I=1,2
          WRITE(RLU,919) DTRWRA_OFF+(I-1)*2, DTRWRA(1,I),
     *                 CSMONY(DTRWRA(2,I),12,MONEY_UNIT),
     *                 'DTRWRA', I, DESCR(51)
310     CONTINUE
        DO 320 I=1,2
          WRITE(RLU,919) DTRWPO_OFF+(I-1)*2, DTRWPO(1,I), 
     *                 CSMONY(DTRWPO(2,I),12,MONEY_UNIT),
     *                 'DTRWPO', I, DESCR(52)
320     CONTINUE
        DO 330 I=1,2
          WRITE(RLU,919) DTRWPA_OFF+(I-1)*2, DTRWPA(1,I), 
     *                 CSMONY(DTRWPA(2,I),12,MONEY_UNIT),
     *                 'DTRWPA', I, DESCR(53)
330     CONTINUE
        DO 340 I=1,2
          WRITE(RLU,919) DTRWPR_OFF+(I-1)*2, DTRWPR(1,I), 
     *                 CSMONY(DTRWPR(2,I),12,MONEY_UNIT),
     *                 'DTRWPR', I, DESCR(54)
340     CONTINUE
	WRITE(RLU,900) DTRCMB_OFF, DTRCMB, 'DTRCMB', DESCR(55)



	RETURN  
C
C
900	FORMAT(1X,I5,1X,I12,1X,15X,A6,9X,A30)
901	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,')',5X,A30)
902	FORMAT(1X,I5,1X,I12,1X,I12,3X,A6,'(',I2,',*)',3X,A30)
903	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,')',2X,A30)
904	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,',',I2,')',1X,A28)
905	FORMAT(1X,I5,1X,I12,1X,10X,A11,9X,A30)
906	FORMAT(1X,I5,1X,A13,1X,A13,1X,A6,'(',I2,')',5X,A30)
907	FORMAT(1X,I5,1X,A16,1X,10X,1X,A6,9X,A30)
908	FORMAT(1X,I5,1X,5A4,1X,7X,A6,9X,A30)
909	FORMAT(1X,I5,1X,A17,1X,A6,9X,A30)
910	FORMAT(1X,I5,1X,8X,A4,1X,15X,A6,9X,A30)
911	FORMAT(1X,I5,1X,4A4,1X,10X,1X,A6,'(1,',I2,')',3X,A30)
913	FORMAT(1X,I5,1X,4A4,1X,10X,1X,A6,'(1,',I2,',',I2,')',1X,A28)
914	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,9X,A30)
915	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,')',5X,A30)
916	FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)
917	FORMAT(1X,I5,1X,A12,1X,14X,1X,A6,'(',I2,',',I2,')',2X,A30)
918	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,9X,A30)
919	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I2,')',3X,A30)
920	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*)',6X,A30)

C
	END

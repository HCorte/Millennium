C DMP_LTO.FOR
C
C V02 30-MAR-2015 MTK Modified Super 14 game
C V01 15-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP VAKIO GAME FILE
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
      SUBROUTINE DMP_SPT(RLU,FILE,DRAW,MONEY_UNIT)
      IMPLICIT NONE


      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'

      ! arguments
      INTEGER*4  RLU
      INTEGER*4  FILE(5)
      INTEGER*4  DRAW
      INTEGER*4  MONEY_UNIT

      INTEGER*4 LUN
      INTEGER*4 I,K

      CHARACTER DESCR(58)*30

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
     *            'NUM. WEEKS WITHOUT DIV1 WINS  ',
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
     *            'SUPER 14 BONUS ROW TYPE       ',
     *            'WIN RESERVE FUND              ',
     *            'BEST MATCH FLAG               ',
     *            'BALANCE SALES INFO. BY DRAW   ',
     *            'EVENT NAME                    ',
     *            'ROLLOVER RULE TO APPLY        ',
     *            'ROLLOVER DIVISION             ',
     *            'FIXED PRIZE (0=NOT FIXED)     ',
     *            'EXTRAORDINARY DRAW OPTION     ',
     *            'WHERE ROLLOVER GO TO          ',
     *            'WHERE ROLLOVER CAME FROM      ',
     *            'EVENT CANCEL DATE AND TIME    ',
     *            '# EVENTS CANC. TO DRAW CANCEL ',
     *            'DRAW CANCEL DATE AND TIME     ',
     *            'REFUND WINNING DIVISON        ' /
C
C READ GAME FILE
C
      LUN = 9     ! ?
      CALL READGFL(LUN,FILE,DSPSEC,DRAW,DSPREC)
C
C DUMP RECORD
C
      WRITE(RLU,900) DSPSTS_OFF, DSPSTS, 'DSPSTS', DESCR(1)
      WRITE(RLU,908) DSPCTM_OFF, DISTIM(DSPCTM), 'DSPCTM', DESCR(2)
      WRITE(RLU,908) DSPTIM_OFF, DISTIM(DSPTIM), 'DSPTIM', DESCR(3)
      WRITE(RLU,900) DSPDRW_OFF, DSPDRW, 'DSPDRW', DESCR(4)
      WRITE(RLU,900) DSPBSD_OFF, DSPBSD, 'DSPBSD', DESCR(5)
      WRITE(RLU,900) DSPESD_OFF, DSPESD, 'DSPESD', DESCR(6)
      WRITE(RLU,900) DSPPUP_OFF, DSPPUP, 'DSPPUP', DESCR(7)
      WRITE(RLU,900) DSPUPD_OFF, DSPUPD, 'DSPUPD', DESCR(8)
      DO 100 I=1,DATLEN
        WRITE(RLU,901) DSPDAT_OFF+I-1, DSPDAT(I), 'DSPDAT', I, DESCR(9)
100   CONTINUE
      DO 110 I=1,NUMADV
        WRITE(RLU,901) DSPADV_OFF+I-1, DSPADV(I), 'DSPADV', I, DESCR(10)
110   CONTINUE
      DO 120 I=1,SPGENT
        WRITE(RLU,909) DSPSAL_OFF+I-1, CSMONY(DSPSAL(I),12,MONEY_UNIT),
     *                'DSPSAL', I, DESCR(11)
120   CONTINUE
      DO 130 I=1,SPGDIV
        WRITE(RLU,909) DSPSHV_OFF+I-1, CSMONY(DSPSHV(I),12,MONEY_UNIT),
     *                'DSPSHV', I, DESCR(12)
130   CONTINUE
      DO 140 I=1,SPGDIV
        WRITE(RLU,901) DSPSHR_OFF+I-1, DSPSHR(I), 'DSPSHR', I, DESCR(13)
140   CONTINUE
      DO 145 I=1,SPGDIV
        WRITE(RLU,909) DSPPOL_OFF+I-1, CSMONY(DSPPOL(I),12,MONEY_UNIT),
     *                'DSPPOL', I, DESCR(14)
145   CONTINUE
      DO 150 I=1,SPGDIV
        WRITE(RLU,909) DSPPAD_OFF+I-1, CSMONY(DSPPAD(I),12,MONEY_UNIT),
     *                'DSPPAD', I, DESCR(15)
150   CONTINUE
      DO 160 I=1,SPGDIV
        WRITE(RLU,909) DSPPRG_OFF+I-1, CSMONY(DSPPRG(I),12,MONEY_UNIT),
     *                'DSPPRG', I, DESCR(16)
160   CONTINUE
      DO 170 I=1,SPGDIV
        WRITE(RLU,909) DSPAFD_OFF+I-1, CSMONY(DSPAFD(I),12,MONEY_UNIT),
     *                'DSPAFD', I, DESCR(17)
170   CONTINUE
      DO 180 I=1,SPGDIV
        WRITE(RLU,909) DSPCSP_OFF+I-1, CSMONY(DSPCSP(I),12,MONEY_UNIT),
     *                'DSPCSP', I, DESCR(18)
180   CONTINUE
      DO 190 I=1,SPGDIV
        WRITE(RLU,909) DSPOSV_OFF+I-1, CSMONY(DSPOSV(I),12,MONEY_UNIT),
     *                'DSPOSV', I, DESCR(19)
190   CONTINUE
      DO 200 I=1,SPGDIV
        WRITE(RLU,901) DSPOSH_OFF+I-1, DSPOSH(I), 'DSPOSH', I, DESCR(20)
200   CONTINUE
      DO 210 I=1,SPGNBR
        WRITE(RLU,901) DSPWIN_OFF+I-1, DSPWIN(I), 'DSPWIN', I, DESCR(21)
210   CONTINUE
      DO 220 I=1,SPGNBR
        WRITE(RLU,901) DSPHLD_OFF+I-1, DSPHLD(I), 'DSPHLD', I, DESCR(22)
220   CONTINUE
      WRITE(RLU,910) DSPPRP_OFF, DISPER(DSPPRP), 'DSPPRP', DESCR(23)
      WRITE(RLU,900) DSPPRN_OFF, DSPPRN, 'DSPPRN', DESCR(24)
      WRITE(RLU,900) DSPTAX_OFF, DSPTAX, 'DSPTAX', DESCR(25)
      WRITE(RLU,909) DSPRES_OFF,   CSMONY(DSPRES(1),12,MONEY_UNIT),
     *              'DSPRES', 1, DESCR(26)
      WRITE(RLU,909) DSPRES_OFF+1, CSMONY(DSPRES(2),12,MONEY_UNIT),
     *              'DSPRES', 2, DESCR(26)
      WRITE(RLU,911) DSPAPL_OFF, CSMONY(DSPAPL,12,MONEY_UNIT),
     *              'DSPAPL', DESCR(27)
      WRITE(RLU,911) DSPMIN_OFF, CSMONY(DSPMIN,12,MONEY_UNIT),
     *              'DSPMIN', DESCR(28)
      WRITE(RLU,900) DSPSER_OFF, DSPSER, 'DSPSER', DESCR(29)
      WRITE(RLU,911) DSPOPA_OFF, CSMONY(DSPOPA,12,MONEY_UNIT),
     *              'DSPOPA', DESCR(30)
      WRITE(RLU,920) DSPPRC_OFF, FLOAT(DSPPRC)/(P(PRFACTOR)*DOLL_BASE),
     *              'DSPPRC', DESCR(31)
      WRITE(RLU,900) DSPMAX_OFF, DSPMAX, 'DSPMAX', DESCR(32)
      WRITE(RLU,900) DSPMLT_OFF, DSPMLT, 'DSPMLT', DESCR(33)
      DO 230 I=1,MAXMLTD_AVL
        WRITE(RLU,901) DSPMDS_OFF+I-1, DSPMDS(I), 'DSPMDS', I, DESCR(34)
230   CONTINUE
      WRITE(RLU,900) DSPDIV_OFF, DSPDIV, 'DSPDIV', DESCR(35)
      DO 240 I=1,SPGDIV
        WRITE(RLU,901) DSPMAT_OFF+I-1, DSPMAT(I), 'DSPMAT', I, DESCR(36)
240   CONTINUE
      DO 250 I=1,SPGDIV
        WRITE(RLU,912) DSPPER_OFF+I-1, DISPER(DSPPER(I)), 'DSPPER', I, DESCR(37)
250   CONTINUE
      WRITE(RLU,910) DSPSPR_OFF, DISPER(DSPSPR), 'DSPSPR', DESCR(38)
      DO 260 I=1,SPGDIV
        WRITE(RLU,901) DSPTSR_OFF+I-1, DSPTSR(I), 'DSPTSR', I, DESCR(39)
260   CONTINUE
      WRITE(RLU,900) DSPREV_OFF, DSPREV, 'DSPREV', DESCR(40)
      DO 270 I=1,SPGNBR
        WRITE(RLU,906) DSPNMS_OFF+(I-1)*2*SPNMS_LEN,
     *                (DSPNMS(K,1,I),K=1,3), (DSPNMS(K,2,I),K=1,3),
     *                'DSPNMS', I, DESCR(41)
270   CONTINUE
      DO 280 I=1,25
        WRITE(RLU,901) DSPCLB_OFF+I-1, DSPCLB(I), 'DSPCLB', I, DESCR(42)
280   CONTINUE
      DO 290 I=1,SPGDIV
        WRITE(RLU,909) DSPASH_OFF+I-1, CSMONY(DSPASH(I),12,MONEY_UNIT),
     *                'DSPASH', I, DESCR(43)
290   CONTINUE
      WRITE(RLU,900) DSPFRG_OFF, DSPFRG, 'DSPFRG', DESCR(44)
      DO 300 I=1,POSTED
        WRITE(RLU,909) DSPWRF_OFF+I-1, CSMONY(DSPWRF(I),12,MONEY_UNIT),
     *                  'DSPWRF', I, DESCR(45)
300   CONTINUE
      WRITE(RLU,900) DSPBST_OFF, DSPBST, 'DSPBST', DESCR(46)
      DO 310 I=1,MAXDRW
        WRITE(RLU,909) DSPBAL_OFF+I-1, CSMONY(DSPBAL(I),12,MONEY_UNIT),
     *                  'DSPBAL', I, DESCR(47)
310   CONTINUE
      WRITE(RLU,907) DSPEVN_OFF, (DSPEVN(K),K=1,4), 'DSPEVN', DESCR(48)
C
      WRITE(RLU,900) DSPEXT_OFF, DSPEXT, 'DSPEXT', DESCR(52)
      WRITE(RLU,901) DSPRGT_OFF,   DSPRGT(1), 'DSPRGT', 1, DESCR(53)
      WRITE(RLU,901) DSPRGT_OFF+1, DSPRGT(2), 'DSPRGT', 2, DESCR(53)
      WRITE(RLU,901) DSPROD_OFF,   DSPROD(1), 'DSPROD', 1, DESCR(54)
      WRITE(RLU,901) DSPROD_OFF+1, DSPROD(2), 'DSPROD', 2, DESCR(54)
C
      DO I = 1, SPGNBR
        WRITE(RLU, 901) DSPECD_OFF + I - 1, DSPECD(I), 'DSPECD', I, DESCR(55)
      ENDDO
C
      WRITE(RLU,900) DSPMCE_OFF, DSPMCE, 'DSPMCE', DESCR(56)
      WRITE(RLU,900) DSPDCD_OFF, DSPDCD, 'DSPDCD', DESCR(57)
      WRITE(RLU,900) DSPRWD_OFF, DSPRWD, 'DSPRWD', DESCR(58)
C
C RECORD LENGTH / FREE SPACE
C
      WRITE(RLU,900)  DSPFRE_OFF, DSPLEN - DSPFRE_OFF + 1, 'DSPFRE', 'FREE SPACE   '
      WRITE(RLU,900)           0, DSPLEN,                  'DSPLEN', 'RECORD LENGTH'
      RETURN  
C
C
900   FORMAT(1X,I5,1X,I12,1X,15X,A6,9X,A30)
901   FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,')',5X,A30)
902   FORMAT(1X,I5,1X,I12,1X,I12,3X,A6,'(',I2,',*)',3X,A30)
903   FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,')',2X,A30)
904   FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,',',I2,')',1X,A28)
905   FORMAT(1X,I5,1X,I12,1X,15X,A11,8X,A30)
906   FORMAT(1X,I5,1X,3A4,1X,3A4,3X,A6,'(',I2,')',5X,A30)
907   FORMAT(1X,I5,1X,4A4,1X,10X,1X,A6,9X,A30)
908   FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,9X,A30)
909   FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,')',5X,A30)
910   FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,9X,A30)
911   FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)
912   FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,'(',I2,')',5X,A30)
920   FORMAT(1X,I5,1X,F12.4,1X,15X,A6,9X,A30)

C
      END

C DMP_DBL.FOR
C
C V01 16-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP COUPLE GAME FILE
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
	SUBROUTINE DMP_CPL(RLU,FILE,DRAW,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DCPREC.DEF'

        ! arguments
	INTEGER*4  RLU				!
	INTEGER*4  FILE(5)			!
        INTEGER*4  DRAW                         !
	INTEGER*4  MONEY_UNIT			!

	INTEGER*4 LUN
	INTEGER*4 I,K

        CHARACTER DESCR(58)*30

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
     *            'EVENT NAME 1 EVENT NAME 2     ',
     *            'GAME DESCRIPTION              ',
     *            'ROW NAMES                     ',
     *            'ROW STATUS 1 THROUGH MAXTRW   ',
     *            'EVENT STATUS                  ',
     *            'EVENT BEGIN DATE              ',
     *            'EVENT BEGIN TIME              ',
     *            'SALES BY ROW TABLE            ',
     *            'REV #                         ',
     *            'POOL FILE NAME                ',
     *            'ROW CLOSE TIMES               ',
     *            'BASE PRICE                    ',
     *            'POOL PERCENTAGE               ',
     *            'TV-CHANEL NAME                ',
     *            'MULTIDRAW SELECTED TABLE      ',
     *            'ACTUAL # OF ROWS USED.        ',
     *            '# WINNING COUPONS PLAYED      ',
     *            'CNT, AMT WINNERS PLAYED       ',
     *            'WINNING: CNT,AMT REFUNDS ONLY ',
     *            'WINNING: CNT,AMT REFUNDS ALTOG',
     *            'WINNING: CNT,AMT PRIZES ONLY  ',
     *            'WINNING: CNT,AMT PRIZES ALTOGE',
     *            'WINNING:CNT,AMT PRIZES+REFUNDS',
     *            'TABLE AMTS ON COMBNS FOR ODDS ',
     *            'NUMBER OF WINNING COMBINATIONS'/



C
C READ GAME FILE
C
	LUN = 9     ! ?
        CALL READGFL(LUN,FILE,DCPSEC,DRAW,DCPREC)
C
C DUMP RECORD
C
	WRITE(RLU,900) DCPSTS_OFF, DCPSTS, 'DCPSTS', DESCR(1)
	WRITE(RLU,900) DCPWEK_OFF, DCPWEK, 'DCPWEK', DESCR(2)
	WRITE(RLU,900) DCPDAT_OFF, DCPDAT, 'DCPDAT', DESCR(3)
	WRITE(RLU,900) DCPDRW_OFF, DCPDRW, 'DCPDRW', DESCR(4)
	WRITE(RLU,900) DCPBSD_OFF, DCPBSD, 'DCPBSD', DESCR(5)
	WRITE(RLU,900) DCPESD_OFF, DCPESD, 'DCPESD', DESCR(6)
	WRITE(RLU,900) DCPPUP_OFF, DCPPUP, 'DCPPUP', DESCR(7)
	WRITE(RLU,900) DCPUPD_OFF, DCPUPD, 'DCPUPD', DESCR(8)
	WRITE(RLU,914) DCPCTM_OFF, DISTIM(DCPCTM), 'DCPCTM', DESCR(9)
	WRITE(RLU,914) DCPTIM_OFF, DISTIM(DCPTIM), 'DCPTIM', DESCR(10)
	WRITE(RLU,900) DCPSER_OFF, DCPSER, 'DCPSER', DESCR(11)
        WRITE(RLU,921) DCPSAL_OFF+I-1, DCPSAL(1),
     *                 CSMONY(DCPSAL(2),12,MONEY_UNIT),
     *                'DCPSAL', DESCR(12)
	WRITE(RLU,916) DCPPAD_OFF, CSMONY(DCPPAD,12,MONEY_UNIT),
     *              'DCPPAD', DESCR(13)
	WRITE(RLU,916) DCPPRG_OFF, CSMONY(DCPPRG,12,MONEY_UNIT),
     *              'DCPPRG', DESCR(14)
	WRITE(RLU,916) DCPPRF_OFF, CSMONY(DCPPRF,12,MONEY_UNIT),
     *              'DCPPRF', DESCR(15)
	WRITE(RLU,916) DCPREF_OFF, CSMONY(DCPREF,12,MONEY_UNIT),
     *              'DCPREF', DESCR(16)
	WRITE(RLU,916) DCPERF_OFF, CSMONY(DCPERF,12,MONEY_UNIT),
     *              'DCPERF', DESCR(17)
	WRITE(RLU,900) DCPTER_OFF, DCPTER, 'DCPTER', DESCR(18)
	WRITE(RLU,900) DCPWON_OFF, DCPWON, 'DCPWON', DESCR(19)
	WRITE(RLU,915) DCPPOL_OFF,   CSMONY(DCPPOL(1),12,MONEY_UNIT),
     *              'DCPPOL', 1, DESCR(20)
	WRITE(RLU,915) DCPPOL_OFF+1, CSMONY(DCPPOL(2),12,MONEY_UNIT),
     *              'DCPPOL', 2, DESCR(20)
	WRITE(RLU,916) DCPTPL_OFF,   CSMONY(DCPTPL,12,MONEY_UNIT),
     *              'DCPTPL', DESCR(21)
	WRITE(RLU,916) DCPTBK_OFF,   CSMONY(DCPTBK,12,MONEY_UNIT),
     *              'DCPTBK', DESCR(22)
	WRITE(RLU,915) DCPBRK_OFF,   CSMONY(DCPBRK(1),12,MONEY_UNIT),
     *              'DCPBRK', 1, DESCR(23)
	WRITE(RLU,915) DCPBRK_OFF+1, CSMONY(DCPBRK(2),12,MONEY_UNIT),
     *              'DCPBRK', 2, DESCR(23)
	WRITE(RLU,916) DCPABW_OFF,   CSMONY(DCPABW,12,MONEY_UNIT),
     *              'DCPABW',    DESCR(24)
	DO 110 I=1,MAXCPLTI
	  WRITE(RLU,901) DCPODS_OFF+I-1, DCPODS(I), 'DCPODS', I, DESCR(25)
110	CONTINUE
        DO 120 I=1,MAXCPLTI
          WRITE(RLU,902) DCPWIN_OFF+(I-1)*2, DCPWIN(1,I), DCPWIN(2,I),
     *                 'DCPWIN', I, DESCR(26)
120     CONTINUE
        DO 130 I=1,MAXCPLTI
          WRITE(RLU,902) DCPHLD_OFF+(I-1)*2, DCPHLD(1,I), DCPHLD(2,I),
     *                 'DCPHLD', I, DESCR(27)
130     CONTINUE
	WRITE(RLU,900) DCPTAX_OFF, DCPTAX, 'DCPTAX', DESCR(28)
	DO 140 I=1,NUMTOT
	  WRITE(RLU,901) DCPOTX_OFF+I-1, DCPOTX(I), 'DCPOTX', I, DESCR(29)
140	CONTINUE
	DO 150 I=1,NUMTOT
	  WRITE(RLU,901) DCPMID_OFF+I-1, DCPMID(I), 'DCPMID', I, DESCR(30)
150	CONTINUE
	DO 160 I=1,NUMTOT
	  WRITE(RLU,901) DCPUTX_OFF+I-1, DCPUTX(I), 'DCPUTX', I, DESCR(31)
160	CONTINUE
	DO 170 I=1,MAXCPLTI
	  WRITE(RLU,901) DCPHST_OFF+I-1, DCPHST(I), 'DCPHST', I, DESCR(32)
170	CONTINUE
	DO 180 I=1,NUMTOT
	  WRITE(RLU,901) DCPORM_OFF+I-1, DCPORM(I), 'DCPORM', I, DESCR(33)
180	CONTINUE
	WRITE(RLU,913) DCPENM_OFF, (DCPENM(K,1),K=1,4), (DCPENM(K,2),K=1,4),
     *                'DCPENM', DESCR(34)
	WRITE(RLU,913) DCPDES_OFF, (DCPDES(K,1),K=1,4), (DCPDES(K,2),K=1,4),
     *                'DCPDES', DESCR(35)
	DO 190 I=1,MAXCPLRW
	  WRITE(RLU,911) DCPNMS_OFF+(I-1)*CPLNMS_LEN/4,
     *                  (DCPNMS(K,I),K=1,4), 'DCPNMS', I, DESCR(36)
190	CONTINUE
	DO 200 I=1,MAXCPLRW
	  WRITE(RLU,901) DCPSTA_OFF+I-1, DCPSTA(I), 'DCPSTA', I, DESCR(37)
200	CONTINUE
	WRITE(RLU,901) DCPEST_OFF,   DCPEST(1), 'DCPEST', 1, DESCR(38)
	WRITE(RLU,901) DCPEST_OFF+1, DCPEST(2), 'DCPEST', 2, DESCR(38)
	WRITE(RLU,901) DCPEVD_OFF,   DCPEVD(1), 'DCPEVD', 1, DESCR(39)
	WRITE(RLU,901) DCPEVD_OFF+1, DCPEVD(2), 'DCPEVD', 2, DESCR(39)
	WRITE(RLU,917) DCPEVT_OFF,   DISTIM(DCPEVT(1)), 'DCPEVT', 1, DESCR(40)
	WRITE(RLU,917) DCPEVT_OFF+1, DISTIM(DCPEVT(2)), 'DCPEVT', 2, DESCR(40)
	DO 210 I=1,MAXCPLRW
	  WRITE(RLU,915) DCPSBR_OFF+I-1, CSMONY(DCPSBR(I),12,MONEY_UNIT),
     *                'DCPSBR', I, DESCR(41)
210	CONTINUE
	WRITE(RLU,900) DCPREV_OFF, DCPREV, 'DCPREV', DESCR(42)
	WRITE(RLU,908) DCPPFN_OFF, (DCPPFN(K),K=1,5), 'DCPPFN', DESCR(43)
	DO 220 I=1,MAXCPLRW
	  WRITE(RLU,917) DCPRTM_OFF+I-1, DISTIM(DCPRTM(I)), 'DCPRTM', I, DESCR(44)
220	CONTINUE
	WRITE(RLU,916) DCPPRC_OFF, CSMONY(DCPPRC,12,MONEY_UNIT), 
     *              'DCPPRC', DESCR(45)
	WRITE(RLU,918) DCPSPR_OFF, DISPER(DCPSPR), 'DCPSPR', DESCR(46)
	WRITE(RLU,910) DCPTVC_OFF, DCPTVC(1), 'DCPTVC', DESCR(47)
	DO 230 I=1,MAXMLTD_AVL
	  WRITE(RLU,901) DCPMDS_OFF+I-1, DCPMDS(I), 'DCPMDS', I, DESCR(48)
230	CONTINUE
	WRITE(RLU,900) DCPRWS_OFF, DCPRWS, 'DCPRWS', DESCR(49)
	DO 240 I=1,MAXCPLTI
	  WRITE(RLU,901) DCPWCP_OFF+I-1, DCPWCP(I), 'DCPWCP', I, DESCR(50)
240	CONTINUE
	DO 250 I=1,MAXCPLTI
	  WRITE(RLU,912) DCPWBT_OFF+I-1, DCPWBT(1,I), 
     *                 CSMONY(DCPWBT(2,I),12,MONEY_UNIT),
     *                'DCPWBT', I, DESCR(51)
250	CONTINUE
        DO 260 I=1,2
          WRITE(RLU,919) DCPWRO_OFF+(I-1)*2, DCPWRO(1,I), 
     *                 CSMONY(DCPWRO(2,I),12,MONEY_UNIT),
     *                 'DCPWRO', I, DESCR(52)
260     CONTINUE
        DO 270 I=1,2
          WRITE(RLU,919) DCPWRA_OFF+(I-1)*2, DCPWRA(1,I),
     *                 CSMONY(DCPWRA(2,I),12,MONEY_UNIT),
     *                 'DCPWRA', I, DESCR(53)
270     CONTINUE
        DO 280 I=1,2
          WRITE(RLU,919) DCPWPO_OFF+(I-1)*2, DCPWPO(1,I),
     *                 CSMONY(DCPWPO(2,I),12,MONEY_UNIT),
     *                 'DCPWPO', I, DESCR(54)
280     CONTINUE
        DO 290 I=1,2
          WRITE(RLU,919) DCPWPA_OFF+(I-1)*2, DCPWPA(1,I),
     *                 CSMONY(DCPWPA(2,I),12,MONEY_UNIT),
     *                 'DCPWPA', I, DESCR(55)
290     CONTINUE
        DO 300 I=1,2
          WRITE(RLU,919) DCPWPR_OFF+(I-1)*2, DCPWPR(1,I),
     *                 CSMONY(DCPWPR(2,I),12,MONEY_UNIT),
     *                 'DCPWPR', I, DESCR(56)
300     CONTINUE
	DO 310 I=1,MAXCPLRW/2*MAXCPLRW/2
	  WRITE(RLU,920) DCPODT_OFF+I-1, CSMONY(DCPODT(I),12,MONEY_UNIT),
     *                 'DCPODT', I, DESCR(57)
310	CONTINUE
	WRITE(RLU,900) DCPCMB_OFF, DCPCMB, 'DCPCMB', DESCR(58)



	RETURN  
C
C
900	FORMAT(1X,I5,1X,I12,1X,15X,A6,9X,A30)
901	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,')',5X,A30)
902	FORMAT(1X,I5,1X,I12,1X,I12,3X,A6,'(',I2,',*)',3X,A30)
903	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,')',2X,A30)
904	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,',',I2,')',1X,A28)
905	FORMAT(1X,I5,1X,I10,1X,12X,A11,9X,A30)
906	FORMAT(1X,I5,1X,A13,1X,A13,1X,A6,'(',I2,')',5X,A30)
907	FORMAT(1X,I5,1X,A16,1X,10X,1X,A6,8X,A30)
908	FORMAT(1X,I5,1X,5A4,1X,7X,A6,9X,A30)
909	FORMAT(1X,I5,1X,A17,1X,A6,9X,A30)
910	FORMAT(1X,I5,1X,8X,A4,1X,15X,A6,9X,A30)
911	FORMAT(1X,I5,1X,4A4,1X,10X,1X,A6,'(1,',I2,')',3X,A30)
912	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I2,')',3X,A30)
913	FORMAT(1X,I5,1X,4A4,1X,4A4,1X,A6,3X,A30)
914	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,9X,A30)
915	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,')',5X,A30)
916	FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)
917	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,'(',I2,')',5X,A30)
918	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,9X,A30)
919	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I2,')',3X,A30)
920	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I3,')',4X,A30)
921	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*)',6X,A30)



C
	END

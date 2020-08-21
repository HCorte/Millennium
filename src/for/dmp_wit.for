C DMP_LTO.FOR
C
C V01 15-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP VOITTAJA GAME FILE
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
	SUBROUTINE DMP_WIT(RLU,FILE,DRAW,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DWIREC.DEF'

        ! arguments
	INTEGER*4  RLU				!
	INTEGER*4  FILE(5)			!
        INTEGER*4  DRAW                         !
	INTEGER*4  MONEY_UNIT			!

	INTEGER*4 LUN
	INTEGER*4 I,K

        CHARACTER DESCR(50)*30

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
     *            'HIGHEST WINNER FOR THIS GAME  ',
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
     *            'DRAW SELECTED TABLE           ',
     *            'WINNING:CNT,AMT REFUNDS ONLY  ',
     *            'WINNING:CNT,AMT REFUNDS ALTOGE',
     *            'WINNING:CNT,AMT PRIZES ONLY   ',
     *            'WINNING:CNT,AMT PRIZES ALTOGET',
     *            'WINNING:CNT,AMT PRIZES+REFUNDS'/




C
C READ GAME FILE
C
	LUN = 9     ! ?
        CALL READGFL(LUN,FILE,DWISEC,DRAW,DWIREC)
C
C DUMP RECORD
C
	WRITE(RLU,900) DWISTS_OFF, DWISTS, 'DWISTS', DESCR(1)
	WRITE(RLU,900) DWIWEK_OFF, DWIWEK, 'DWIWEK', DESCR(2)
	WRITE(RLU,900) DWIDAT_OFF, DWIDAT, 'DWIDAT', DESCR(3)
	WRITE(RLU,900) DWIDRW_OFF, DWIDRW, 'DWIDRW', DESCR(4)
	WRITE(RLU,900) DWIBSD_OFF, DWIBSD, 'DWIBSD', DESCR(5)
	WRITE(RLU,900) DWIESD_OFF, DWIESD, 'DWIESD', DESCR(6)
	WRITE(RLU,900) DWIPUP_OFF, DWIPUP, 'DWIPUP', DESCR(7)
	WRITE(RLU,900) DWIUPD_OFF, DWIUPD, 'DWIUPD', DESCR(8)
	WRITE(RLU,912) DWICTM_OFF, DISTIM(DWICTM), 'DWICTM', DESCR(9)
	WRITE(RLU,912) DWITIM_OFF, DISTIM(DWITIM), 'DWITIM', DESCR(10)
	WRITE(RLU,900) DWISER_OFF, DWISER, 'DWISER', DESCR(11)
	WRITE(RLU,913) DWISAL_OFF, CSMONY(DWISAL,12,MONEY_UNIT),
     *              'DWISAL', DESCR(12)
	WRITE(RLU,913) DWIPAD_OFF, CSMONY(DWIPAD,12,MONEY_UNIT), 
     *              'DWIPAD', DESCR(13)
	WRITE(RLU,913) DWIPRG_OFF, CSMONY(DWIPRG,12,MONEY_UNIT),
     *              'DWIPRG', DESCR(14)
	WRITE(RLU,913) DWIPRF_OFF, CSMONY(DWIPRF,12,MONEY_UNIT),
     *              'DWIPRF', DESCR(15)
	WRITE(RLU,913) DWIREF_OFF, CSMONY(DWIREF,12,MONEY_UNIT),
     *              'DWIREF', DESCR(16)
	WRITE(RLU,913) DWIERF_OFF, CSMONY(DWIERF,12,MONEY_UNIT),
     *              'DWIERF', DESCR(17)
	WRITE(RLU,913) DWITER_OFF, CSMONY(DWITER,12,MONEY_UNIT),
     *              'DWITER', DESCR(18)
	WRITE(RLU,913) DWIWON_OFF, CSMONY(DWIWON,12,MONEY_UNIT),
     *              'DWIWON', DESCR(19)
	WRITE(RLU,914) DWIPOL_OFF,   CSMONY(DWIPOL(1),12,MONEY_UNIT),
     *              'DWIPOL', 1, DESCR(20)
	WRITE(RLU,914) DWIPOL_OFF+1, CSMONY(DWIPOL(2),12,MONEY_UNIT),
     *              'DWIPOL', 2, DESCR(20)
	WRITE(RLU,913) DWITPL_OFF,   CSMONY(DWITPL,12,MONEY_UNIT),
     *              'DWITPL',    DESCR(21)
	WRITE(RLU,913) DWITBK_OFF,   CSMONY(DWITBK,12,MONEY_UNIT),
     *              'DWITBK',    DESCR(22)
	WRITE(RLU,914) DWIBRK_OFF,   CSMONY(DWIBRK(1),12,MONEY_UNIT),
     *              'DWIBRK', 1, DESCR(23)
	WRITE(RLU,914) DWIBRK_OFF+1, CSMONY(DWIBRK(2),12,MONEY_UNIT),
     *              'DWIBRK', 2, DESCR(23)
	WRITE(RLU,913) DWIABW_OFF,   CSMONY(DWIABW,12,MONEY_UNIT),
     *              'DWIABW',    DESCR(24)
	WRITE(RLU,901) DWIODS_OFF,   DWIODS(1), 'DWIODS', 1, DESCR(25)
	WRITE(RLU,901) DWIODS_OFF+1, DWIODS(2), 'DWIODS', 2, DESCR(25)
	WRITE(RLU,901) DWIODS_OFF+2, DWIODS(3), 'DWIODS', 3, DESCR(25)
	WRITE(RLU,901) DWIODS_OFF+3, DWIODS(4), 'DWIODS', 4, DESCR(25)
	WRITE(RLU,901) DWIWIN_OFF,   DWIWIN(1), 'DWIWIN', 1, DESCR(26)
	WRITE(RLU,901) DWIWIN_OFF+1, DWIWIN(2), 'DWIWIN', 2, DESCR(26)
	WRITE(RLU,901) DWIWIN_OFF+2, DWIWIN(3), 'DWIWIN', 3, DESCR(26)
	WRITE(RLU,901) DWIWIN_OFF+3, DWIWIN(4), 'DWIWIN', 4, DESCR(26)
	WRITE(RLU,901) DWIHLD_OFF,   DWIHLD(1), 'DWIHLD', 1, DESCR(27)
	WRITE(RLU,901) DWIHLD_OFF+1, DWIHLD(2), 'DWIHLD', 2, DESCR(27)
	WRITE(RLU,901) DWIHLD_OFF+2, DWIHLD(3), 'DWIHLD', 3, DESCR(27)
	WRITE(RLU,901) DWIHLD_OFF+3, DWIHLD(4), 'DWIHLD', 4, DESCR(27)
	WRITE(RLU,900) DWITAX_OFF, DWITAX, 'DWITAX', DESCR(28)
	DO 100 I=1,NUMTOT
	  WRITE(RLU,901) DWIOTX_OFF+I-1, DWIOTX(I), 'DWIOTX', I, DESCR(29)
100	CONTINUE
	DO 110 I=1,NUMTOT
	  WRITE(RLU,901) DWIMID_OFF+I-1, DWIMID(I), 'DWIMID', I, DESCR(30)
110	CONTINUE
	DO 120 I=1,NUMTOT
	  WRITE(RLU,901) DWIUTX_OFF+I-1, DWIUTX(I), 'DWIUTX', I, DESCR(31)
120	CONTINUE
	WRITE(RLU,900) DWIHST_OFF, DWIHST, 'DWIHST', DESCR(32)
	DO 130 I=1,NUMTOT
	  WRITE(RLU,901) DWIORM_OFF+I-1, DWIORM(I), 'DWIORM', I, DESCR(33)
130	CONTINUE
	WRITE(RLU,908) DWIENM_OFF, (DWIENM(K),K=1,5), 'DWIENM', DESCR(34)
	WRITE(RLU,908) DWIDES_OFF, (DWIDES(K),K=1,5), 'DWIDES', DESCR(35)
	DO 140 I=1,MAXWRW
	  WRITE(RLU,911) DWINMS_OFF+(I-1)*(WNMS_LEN/4), (DWINMS(K,I),K=1,4),
     *                  'DWINMS', I, DESCR(36)
140	CONTINUE
	DO 150 I=1,MAXWRW
	  WRITE(RLU,901) DWISTA_OFF+I-1, DWISTA(I), 'DWISTA', I, DESCR(37)
150	CONTINUE
	DO 160 I=1,MAXWRW
	  WRITE(RLU,914) DWISBR_OFF+I-1, CSMONY(DWISBR(I),12,MONEY_UNIT),
     *                'DWISBR', I, DESCR(38)
160	CONTINUE
	WRITE(RLU,900) DWIREV_OFF, DWIREV, 'DWIREV', DESCR(39)
	WRITE(RLU,908) DWIPFN_OFF, (DWIPFN(K),K=1,5), 'DWIPFN', DESCR(40)
	DO 170 I=1,MAXWRW
	  WRITE(RLU,917) DWIRTM_OFF+I-1, DISTIM(DWIRTM(I)), 'DWIRTM', I, DESCR(41)
170	CONTINUE
	WRITE(RLU,913) DWIPRC_OFF, CSMONY(DWIPRC,12,MONEY_UNIT),
     *              'DWIPRC', DESCR(42)
	WRITE(RLU,915) DWISPR_OFF, DISPER(DWISPR), 'DWISPR', DESCR(43)
	WRITE(RLU,910) DWITVC_OFF, DWITVC(1), 'DWITVC', DESCR(44)
	DO 180 I=1,MAXMLTD_AVL
	  WRITE(RLU,901) DWIMDS_OFF+I-1, DWIMDS(I), 'DWIMDS', I, DESCR(45)
180	CONTINUE
        DO 190 I=1,2
          WRITE(RLU,916) DWIWRO_OFF+(I-1)*2, DWIWRO(1,I),
     *                 CSMONY(DWIWRO(2,I),12,MONEY_UNIT),
     *                 'DWIWRO', I, DESCR(46)
190     CONTINUE
        DO 200 I=1,2
          WRITE(RLU,916) DWIWRA_OFF+(I-1)*2, DWIWRA(1,I), 
     *                 CSMONY(DWIWRA(2,I),12,MONEY_UNIT),
     *                 'DWIWRA', I, DESCR(47)
200     CONTINUE
        DO 210 I=1,2
          WRITE(RLU,916) DWIWPO_OFF+(I-1)*2, DWIWPO(1,I), 
     *                 CSMONY(DWIWPO(2,I),12,MONEY_UNIT),
     *                 'DWIWPO', I, DESCR(48)
210     CONTINUE
        DO 220 I=1,2
          WRITE(RLU,916) DWIWPA_OFF+(I-1)*2, DWIWPA(1,I), 
     *                 CSMONY(DWIWPA(2,I),12,MONEY_UNIT),
     *                 'DWIWPA', I, DESCR(49)
220     CONTINUE
        DO 230 I=1,2
          WRITE(RLU,916) DWIWPR_OFF+(I-1)*2, DWIWPR(1,I), 
     *                 CSMONY(DWIWPR(2,I),12,MONEY_UNIT),
     *                 'DWIWPR', I, DESCR(50)
230     CONTINUE



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
909	FORMAT(1X,I5,1X,A17,11X,A6,9X,A30)
910	FORMAT(1X,I5,1X,8X,A4,1X,15X,A6,9X,A30)
911     FORMAT(1X,I5,1X,4A4,1X,10X,1X,A6,'(1,',I2,')',3X,A30)
912	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,9X,A30)
913	FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)
914	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,')',5X,A30)
915	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,9X,A30)
916	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I2,')',3X,A30)
917	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,'(',I2,')',5X,A30)
918	FORMAT(1X,I5,1X,4A4,1X,11X,A6,'(',I2,')',5X,A30)

C
	END

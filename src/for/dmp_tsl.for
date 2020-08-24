C DMP_LTO.FOR
C
C V01 15-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP TOTO SELECT GAME FILE
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
	SUBROUTINE DMP_TSL(RLU,FILE,DRAW,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DTSREC.DEF'

        ! arguments
	INTEGER*4  RLU				!
	INTEGER*4  FILE(5)			!
        INTEGER*4  DRAW                         !
	INTEGER*4  MONEY_UNIT			!

	INTEGER*4 LUN
	INTEGER*4 I,J,K

        CHARACTER DESCR(44)*30

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
     *            'BREAKAGE AMOUNT               ',
     *            'TOTAL SALES                   ',
     *            'PRIZES PAID + REFUNDS         ',
     *            'PRIZES PURGED                 ',
     *            'PAID REFUNDS                  ',
     *            'TOTAL REFUND AMOUNT           ',
     *            'PRIZES WON                    ',
     *            'WON BY DIVISION COUNT,AMOUNT  ',
     *            'REFUNDS BY DIVISION COUNT,AMT ',
     *            'PAID BY DIVISION COUNT, AMOUNT',
     *            'WINNING TAX BY DIVISION       ',
     *            'SALES BY DIVISION CNT,AMT     ',
     *            'TOTOSELECT ODDS BY ROW (1,2,3)',
     *            'WINNING RESULTS               ',
     *            'RESULTS HOLD                  ',
     *            'WINNING TAXES                 ',
     *            'OVER TAX LIMIT                ',
     *            '>REDMIN <REDMAX               ',
     *            'UNDER TAX LIMIT               ',
     *            'HIGHEST WINNER FOR THIS GAME  ',
     *            'OVER REDMAX                   ',
     *            'TEAM NAMES (HOME,AWAY)        ',
     *            'ROW STATUS                    ',
     *            'REV #                         ',
     *            'POOL FILE NAME                ',
     *            'TOP 30 COMBINATIONS           ',
     *            'ACTUAL NUMBER OF ROWS USED    ',
     *            'NUMBER OF PLAYED COMB. TODAY  ',
     *            'BASE PRICE                    ',
     *            'EVENT DRAW DATE               ',
     *            'TV-CHANNEL NAME               ',
     *            'MULTI DRAW SELECTED TABLE     ',
     *            'TOP 100 LIABILITIES           ',
     *            '1-SINGLE,2-DOUBLE,3-NORMAL    '/



C
C READ GAME FILE
C
	LUN = 9     ! ?
        CALL READGFL(LUN,FILE,DTSSEC,DRAW,DTSREC)
C
C DUMP RECORD
C
	WRITE(RLU,900) DTSSTS_OFF, DTSSTS, 'DTSSTS', DESCR(1)
	WRITE(RLU,900) DTSWEK_OFF, DTSWEK, 'DTSWEK', DESCR(2)
	DO 100 I=1,MAXSRW
	  WRITE(RLU,901) DTSDAT_OFF+I-1, DTSDAT(I), 'DTSDAT', I, DESCR(3)
100	CONTINUE
	WRITE(RLU,900) DTSDRW_OFF, DTSDRW, 'DTSDRW', DESCR(4)
	WRITE(RLU,900) DTSBSD_OFF, DTSBSD, 'DTSBSD', DESCR(5)
	WRITE(RLU,900) DTSESD_OFF, DTSESD, 'DTSESD', DESCR(6)
	WRITE(RLU,900) DTSPUP_OFF, DTSPUP, 'DTSPUP', DESCR(7)
	WRITE(RLU,900) DTSUPD_OFF, DTSUPD, 'DTSUPD', DESCR(8)
	DO 110 I=1,MAXSRW
	  WRITE(RLU,914) DTSCTM_OFF+I-1, DISTIM(DTSCTM(I)), 'DTSCTM', I, DESCR(9)
110	CONTINUE
	DO 120 I=1,MAXSRW
	  WRITE(RLU,914) DTSTIM_OFF+I-1, DISTIM(DTSTIM(I)), 'DTSTIM', I, DESCR(10)
120	CONTINUE
	WRITE(RLU,915) DTSBRK_OFF, CSMONY(DTSBRK,12,MONEY_UNIT),
     *              'DTSBRK', DESCR(11)
	WRITE(RLU,915) DTSSAL_OFF, CSMONY(DTSSAL,12,MONEY_UNIT),
     *              'DTSSAL', DESCR(12)
	WRITE(RLU,915) DTSPAD_OFF, CSMONY(DTSPAD,12,MONEY_UNIT),
     *              'DTSPAD', DESCR(13)
	WRITE(RLU,915) DTSPRG_OFF, CSMONY(DTSPRG,12,MONEY_UNIT),
     *              'DTSPRG', DESCR(14)
	WRITE(RLU,915) DTSPRF_OFF, CSMONY(DTSPRF,12,MONEY_UNIT),
     *              'DTSPRF', DESCR(15)
	WRITE(RLU,915) DTSREF_OFF, CSMONY(DTSREF,12,MONEY_UNIT),
     *              'DTSREF', DESCR(16)
	WRITE(RLU,915) DTSWON_OFF, CSMONY(DTSWON,12,MONEY_UNIT),
     *              'DTSWON', DESCR(17)
        DO 130 I=1,TSLDIV
          WRITE(RLU,916) DTSWBD_OFF+(I-1)*2, DTSWBD(I,1), 
     *                 CSMONY(DTSWBD(I,2),12,MONEY_UNIT),
     *                 'DTSWBD', I, DESCR(18)
130     CONTINUE
        DO 140 I=1,TSLDIV
          WRITE(RLU,916) DTSRBD_OFF+(I-1)*2, DTSRBD(I,1), 
     *                 CSMONY(DTSRBD(I,2),12,MONEY_UNIT),
     *                 'DTSRBD', I, DESCR(19)
140     CONTINUE
        DO 150 I=1,TSLDIV
          WRITE(RLU,916) DTSPBD_OFF+(I-1)*2, DTSPBD(I,1), 
     *                 CSMONY(DTSPBD(I,2),12,MONEY_UNIT),
     *                 'DTSPBD', I, DESCR(20)
150     CONTINUE
	DO 160 I=1,TSLDIV
	  WRITE(RLU,901) DTSTBD_OFF+I-1, DTSTBD(I), 'DTSTBD', I, DESCR(21)
160	CONTINUE
        DO 170 I=1,TSLDIV
          WRITE(RLU,916) DTSSBD_OFF+(I-1)*2, DTSSBD(I,1), 
     *                 CSMONY(DTSSBD(I,2),12,MONEY_UNIT),
     *                 'DTSSBD', I, DESCR(22)
170     CONTINUE
        DO 180 I=1,3
          DO J=1,MAXSRW
            WRITE(RLU,903) DTSODS_OFF+(I-1)*MAXSRW+J-1, DTSODS(I,J),
     *                   'DTSODS', I, J, DESCR(23)
          ENDDO
180     CONTINUE
	DO 190 I=1,MAXSRW
	  WRITE(RLU,901) DTSWIN_OFF+I-1, DTSWIN(I), 'DTSWIN', I, DESCR(24)
190	CONTINUE
	DO 200 I=1,MAXSRW
	  WRITE(RLU,901) DTSHLD_OFF+I-1, DTSHLD(I), 'DTSHLD', I, DESCR(25)
200	CONTINUE
	WRITE(RLU,900) DTSTAX_OFF, DTSTAX, 'DTSTAX', DESCR(26)
	DO 210 I=1,NUMTOT
	  WRITE(RLU,901) DTSOTX_OFF+I-1, DTSOTX(I), 'DTSOTX', I, DESCR(27)
210	CONTINUE
	DO 220 I=1,NUMTOT
	  WRITE(RLU,901) DTSMID_OFF+I-1, DTSMID(I), 'DTSMID', I, DESCR(28)
220	CONTINUE
	DO 230 I=1,NUMTOT
	  WRITE(RLU,901) DTSUTX_OFF+I-1, DTSUTX(I), 'DTSUTX', I, DESCR(29)
230	CONTINUE
	WRITE(RLU,900) DTSHST_OFF, DTSHST, 'DTSHST', DESCR(30)
	DO 240 I=1,NUMTOT
	  WRITE(RLU,901) DTSORM_OFF+I-1, DTSORM(I), 'DTSORM', I, DESCR(31)
240	CONTINUE
        DO 250 I=1,MAXSRW
          WRITE(RLU,906) DTSNMS_OFF+(I-1)*2*TNMS_LEN,
     *                  (DTSNMS(K,1,I),K=1,3), (DTSNMS(K,2,I),K=1,3),
     *                  'DTSNMS', I, DESCR(32)
250     CONTINUE
	DO 260 I=1,MAXSRW
	  WRITE(RLU,901) DTSSTA_OFF+I-1, DTSSTA(I), 'DTSSTA', I, DESCR(33)
260	CONTINUE
	WRITE(RLU,900) DTSREV_OFF, DTSREV, 'DTSREV', DESCR(34)
	WRITE(RLU,908) DTSPFN_OFF, (DTSPFN(K),K=1,5), 'DTSPFN', DESCR(35)
        DO 270 I=1,30
          WRITE(RLU,902) DTSTOP_OFF+(I-1)*2, DTSTOP(1,I), DTSTOP(2,I),
     *                 'DTSTOP', I, DESCR(36)
270     CONTINUE
	WRITE(RLU,900) DTSRWS_OFF, DTSRWS, 'DTSRWS', DESCR(37)
	WRITE(RLU,900) DTSCMB_OFF, DTSCMB, 'DTSCMB', DESCR(38)
	WRITE(RLU,915) DTSPRC_OFF, CSMONY(DTSPRC,12,MONEY_UNIT),
     *              'DTSPRC', DESCR(39)
	WRITE(RLU,900) DTSDTE_OFF, DTSDTE, 'DTSDTE', DESCR(40)
	DO 280 I=1,MAXSRW
	  WRITE(RLU,911) DTSTVC_OFF+(I-1)*TTVC_LEN/4, DTSTVC(1,I),
     *                'DTSTVC', I, DESCR(41)
280	CONTINUE
	DO 290 I=1,MAXMLTD_AVL
	  WRITE(RLU,901) DTSMDS_OFF+I-1, DTSMDS(I), 'DTSMDS', I, DESCR(42)
290	CONTINUE
        DO 300 I=1,100
          WRITE(RLU,912) DTSWTOP_OFF+(I-1)*2, DTSWTOP(1,I), DTSWTOP(2,I),
     *                 'DTSWTOP', I, DESCR(43)
300     CONTINUE
	DO 310 I=1,MAXSRW
	  WRITE(RLU,913) DTSROWTYP_OFF+I-1, DTSROWTYP(I), 
     *                'DTSROWTYP', I, DESCR(44)
310	CONTINUE



	RETURN  
C
C
900	FORMAT(1X,I5,1X,I12,1X,15X,A6,9X,A30)
901	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,')',5X,A30)
902	FORMAT(1X,I5,1X,I12,1X,I12,3X,A6,'(',I2,',*)',3X,A30)
903	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,')',2X,A30)
904	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,',',I2,')',1X,A28)
905	FORMAT(1X,I5,1X,I12,1X,10X,A11,8X,A30)
906	FORMAT(1X,I5,1X,3A4,1X,3A4,3X,A6,'(',I2,')',5X,A30)
907	FORMAT(1X,I5,1X,A16,1X,10X,1X,A6,9X,A30)
908	FORMAT(1X,I5,1X,5A4,1X,7X,A6,9X,A30)
909	FORMAT(1X,I5,1X,A17,1X,A6,8X,A30)
910	FORMAT(1X,I5,1X,8X,A4,1X,15X,A6,9X,A30)
911	FORMAT(1X,I5,1X,8X,A4,1X,15X,A6,'(1,',I2,')',3X,A30)
912	FORMAT(1X,I5,1X,I12,1X,I12,3X,A7,'(',I3,',*)',1X,A30)
913	FORMAT(1X,I5,1X,I12,1X,15X,A9,'(',I2,')',2X,A30)
914	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,'(',I2,')',5X,A30)
915	FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)
916	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(',I2,',*)',3X,A30)

C
	END

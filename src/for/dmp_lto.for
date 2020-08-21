C DMP_LTO.FOR
C
C V01 14-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP LOTTO GAME FILE
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
	SUBROUTINE DMP_LTO(RLU,FILE,DRAW,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	

        ! arguments
	INTEGER*4  RLU
	INTEGER*4  FILE(5)			!
        INTEGER*4  DRAW                         !
	INTEGER*4  MONEY_UNIT			!
	

	INTEGER*4 LUN
	INTEGER*4 I,J,K

	CHARACTER DESCR(66)*30

	DATA DESCR/
     *		  'STATUS                        ',
     *		  'ACTUAL TIME WHEN CLOSED       ',
     *		  'TIME WHEN GAME SHOULD CLOSE   ',
     *		  'DRAW NUMBER                   ',
     *		  'LOTTO BEGINNING DRAW DATE     ',
     *		  'LOTTO ENDING DRAW DATE        ',
     *            'LAST PURGE UPDATE             ',
     *            'LAST FILE UPDATE              ',
     *            'DRAW DATES                    ',
     *            'ADVANCE DRAW DATES            ',
     *            'SALES DATA                    ',
     *            'SHARE VALUES   (REG,BONUS)    ',
     *            'SHARES         (REG,BONUS)    ',
     *            'AMOUNT PAID    (REG,BONUS)    ',
     *            'AMOUNT PURGED  (REG,BONUS)    ',
     *            'ANNUITY PRIZES (REG,BONUS)    ',
     *            'MAILSUB SHARES (REG,BONUS)    ',
     *            'POOL CARRIED OVER             ',
     *            'BREAKAGE AMOUNT BY POOL       ',
     *            'DIVISION PAYOUT FROZEN FLAGS  ',
     *            'WINNING NUMBERS               ',
     *            'WINNING NUMBERS HOLD          ',
     *            'WINNING BONUS NUMBER(S)       ',
     *            'BONUS NUMBER(S) HOLD          ',
     *            '% OF THE PROMISED AMOUNT      ',
     *            'MAX NUM. WINNERS FOR PR AMT   ',
     *            'LOTTERY TAX AMOUNT            ',
     *            'RESERVE POOL                  ',
     *            'ADDITIONAL POOL               ',
     *            'MINIMUM POOL                  ',
     *            'LAST SERIAL NUMBER            ',
     *            'OVER-RIDE POOL AMOUNT         ',
     *            'PRICE/BOARD                   ',
     *            'HIGHEST # BET                 ',
     *            '# OF NUMBERS DRAWN            ',
     *            'BONUS NUMBER ENABLE FLAG      ',
     *            'MULTI-DRAW ENABLE FLAG        ',
     *            '# OF DIVISIONS                ',
     *            'WINNING DIVISIONS TABLE       ',
     *            'DIVISION PERCENTAGES          ',
     *            'SALES PERCENTAGE              ',
     *            'TOTAL SHARE COUNT (REG,BONUS) ',
     *            'LOTTO REVISION NUMBER         ',
     *            '# OF BONUS DRAWS ENABLED      ',
     *            'LAST DRAWS GAME STATUS        ',
     *            'LAST DRAWS SHR VAL (REG,BONUS)',
     *            'ADVANCE WAGERING FLAG         ',
     *            'ANUITY VALUES (REG,BONUS)     ',
     *            'ACTIVE BET TYPES              ',
     *            'WEDNESDAY DRAW DATES          ',
     *            'PERCENTAGE APPLIED TO WINS    ',
     *            'INTERNATIONAL CONTRBN. (ECUs) ',
     *            'FINNISH CONTRBN. (MARKs)      ',
     *            'SHARE TAX VALUE(REG,BONUS)    ',
     *            'ADDITIONAL POOL BY DIV        ',
     *            'WIN RESERVE FUND TABLE        ',
     *            'PRIZE % AFTER DIV 1 PAID (VIK)',
     *            'ESTIMATED VIKING JACKPOT      ',
     *            'MULTI DRAW SELECTED TABLE     ',
     *            'BALANCE SALES INFO. BY DRAW   ',
     *            'FIRST POS.TO MATCH IN XTRA DRW',
     *            'LAST POS. TO MATCH IN XTRA DRW',
     *            'DIGITS(MIN) TO MATCH IN EXT.  ',
     *            'BITMAP FOR DIVISIONS IN EXT.  ',
     *            'TOTAL NUMBER OF EXTRA WINNERS.',
     *            'SPEC. POOL ADDED TO TOTAL POOL'/



C
C READ GAME FILE
C
	LUN = 9     ! ?
        CALL READGFL(LUN,FILE,DLTSEC,DRAW,DLTREC)
C
C DUMP RECORD
C
	WRITE(RLU,900) DLTSTSOFF, DLTSTS, 'DLTSTS', DESCR(1)
	WRITE(RLU,911) DLTCTMOFF, DISTIM(DLTCTM), 'DLTCTM', DESCR(2)
	WRITE(RLU,911) DLTTIMOFF, DISTIM(DLTTIM), 'DLTTIM', DESCR(3)
	WRITE(RLU,900) DLTDRWOFF, DLTDRW, 'DLTDRW', DESCR(4)
	WRITE(RLU,900) DLTBSDOFF, DLTBSD, 'DLTBSD', DESCR(5)
	WRITE(RLU,900) DLTESDOFF, DLTESD, 'DLTESD', DESCR(6)
	WRITE(RLU,900) DLTPUPOFF, DLTPUP, 'DLTPUP', DESCR(7)
	WRITE(RLU,900) DLTUPDOFF, DLTUPD, 'DLTUPD', DESCR(8)
	DO 100 I=1,DATLEN
	  WRITE(RLU,901) DLTDATOFF+I-1, DLTDAT(I), 'DLTDAT', I, DESCR(9)
100	CONTINUE
	DO 110 I=1,NUMADV
	  WRITE(RLU,901) DLTADVOFF+I-1, DLTADV(I), 'DLTADV', I, DESCR(10)
110	CONTINUE
	DO 120 I=1,LTGENT
	  WRITE(RLU,906) DLTSALOFF+I-1, CSMONY(DLTSAL(I),12,MONEY_UNIT),
     *                'DLTSAL', I, DESCR(11)
120	CONTINUE
	DO 130 I=1,LTGDIV
	  WRITE(RLU,907) DLTSHVOFF+(I-1)*2, CSMONY(DLTSHV(I,1),12,MONEY_UNIT),
     *                 CSMONY(DLTSHV(I,2),12,MONEY_UNIT),
     *                 'DLTSHV', I, DESCR(12)
130	CONTINUE
	DO 140 I=1,LTGDIV
	  WRITE(RLU,902) DLTSHROFF+(I-1)*2, DLTSHR(I,1), DLTSHR(I,2),
     *                 'DLTSHR', I, DESCR(13)
140	CONTINUE
	DO 150 I=1,LTGDIV
	  WRITE(RLU,907) DLTPADOFF+(I-1)*2, CSMONY(DLTPAD(I,1),12,MONEY_UNIT),
     *                 CSMONY(DLTPAD(I,2),12,MONEY_UNIT),
     *                 'DLTPAD', I, DESCR(14)
150	CONTINUE
	DO 160 I=1,LTGDIV
	  WRITE(RLU,907) DLTPRGOFF+(I-1)*2, CSMONY(DLTPRG(I,1),12,MONEY_UNIT),
     *                 CSMONY(DLTPRG(I,2),12,MONEY_UNIT),
     *                 'DLTPRG', I, DESCR(15)
160	CONTINUE
	DO 170 I=1,LTGDIV
	  WRITE(RLU,907) DLTANUOFF+(I-1)*2, CSMONY(DLTANU(I,1),12,MONEY_UNIT),
     *                 CSMONY(DLTANU(I,2),12,MONEY_UNIT),
     *                 'DLTANU', I, DESCR(16)
170	CONTINUE
	DO 180 I=1,LTGDIV
	  WRITE(RLU,902) DLTMSROFF+(I-1)*2, DLTMSR(I,1), DLTMSR(I,2),
     *                 'DLTMSR', I, DESCR(17)
180	CONTINUE
	DO 190 I=1,LTGDIV
	  WRITE(RLU,906) DLTPOLOFF+I-1, CSMONY(DLTPOL(I),12,MONEY_UNIT), 
     *                'DLTPOL', I, DESCR(18)
190	CONTINUE
	DO 200 I=1,LTGDIV
	  WRITE(RLU,906) DLTBRKOFF+I-1, CSMONY(DLTBRK(I),12,MONEY_UNIT), 
     *                'DLTBRK', I, DESCR(19)
200	CONTINUE
	DO 210 I=1,LTGDIV
	  WRITE(RLU,901) DLTFRZOFF+I-1, DLTFRZ(I), 'DLTFRZ', I, DESCR(20)
210	CONTINUE
	DO 220 I=1,LTGDIV
	  DO J=1,MAXBDR
	    WRITE(RLU,903) DLTWINOFF+(I-1)*MAXBDR+J-1, DLTWIN(I,J),
     *                   'DLTWIN', I, J, DESCR(21)
	  ENDDO
220	CONTINUE
	DO 230 I=1,LTGDIV
	  DO J=1,MAXBDR
	    WRITE(RLU,903) DLTHLDOFF+(I-1)*MAXBDR+J-1, DLTHLD(I,J),
     *                   'DLTHLD', I, J, DESCR(22)
	  ENDDO
230	CONTINUE
	DO 240 I=1,LTGBON
	  DO J=1,MAXBDR
	    WRITE(RLU,903) DLTBNMOFF+(I-1)*MAXBDR+J-1, DLTBNM(I,J),
     *                   'DLTBNM', I, J, DESCR(23)
	  ENDDO
240	CONTINUE
	DO 250 I=1,LTGBON
	  DO J=1,MAXBDR
	    WRITE(RLU,903) DLTBHLOFF+(I-1)*MAXBDR+J-1, DLTBHL(I,J),
     *                   'DLTBHL', I, J, DESCR(24)
	  ENDDO
250	CONTINUE
	WRITE(RLU,908) DLTPRPOFF, DISPER(DLTPRP), 'DLTPRP', DESCR(25)
	WRITE(RLU,900) DLTPRNOFF, DLTPRN, 'DLTPRN', DESCR(26)
	WRITE(RLU,909) DLTTAXOFF, CSMONY(DLTTAX,12,MONEY_UNIT), 'DLTTAX', 
     *               DESCR(27)
	WRITE(RLU,906) DLTRESOFF, CSMONY(DLTRES(1),12,MONEY_UNIT), 
     *              'DLTRES', 1, DESCR(28)
	WRITE(RLU,906) DLTRESOFF+1, CSMONY(DLTRES(2),12,MONEY_UNIT), 
     *              'DLTRES', 2, DESCR(28)
	WRITE(RLU,909) DLTAPLOFF, CSMONY(DLTAPL,12,MONEY_UNIT), 
     *              'DLTAPL', DESCR(29)
	WRITE(RLU,909) DLTMINOFF, CSMONY(DLTMIN,12,MONEY_UNIT), 
     *              'DLTMIN', DESCR(30)
	WRITE(RLU,900) DLTSEROFF, DLTSER, 'DLTSER', DESCR(31)
	WRITE(RLU,909) DLTOPAOFF, CSMONY(DLTOPA,12,MONEY_UNIT),
     *              'DLTOPA', DESCR(32)
	WRITE(RLU,912) DLTPRCOFF, ( DFLOAT(DLTPRC)/(P(PRFACTOR)*DOLL_BASE) ),
     *                'DLTPRC', DESCR(33)
	WRITE(RLU,900) DLTMAXOFF, DLTMAX, 'DLTMAX', DESCR(34)
	WRITE(RLU,900) DLTNUMOFF, DLTNUM, 'DLTNUM', DESCR(35)
	WRITE(RLU,900) DLTBFLOFF, DLTBFL, 'DLTBFL', DESCR(36)
	WRITE(RLU,900) DLTMLTOFF, DLTMLT, 'DLTMLT', DESCR(37)
	WRITE(RLU,900) DLTDIVOFF, DLTDIV, 'DLTDIV', DESCR(38)
	DO 260 I=1,LTGNBR
	  DO J=1,LTGBON+1
            DO K=1,LTGBET
	    WRITE(RLU,904) DLTWTBOFF+(I-1)*(LTGBON+1)*LTGBET+(J-1)*LTGBET+K-1, 
     *                   DLTWTB(I,J,K), 'DLTWTB', I, J, K, DESCR(39)
	    ENDDO
	  ENDDO
260	CONTINUE
	DO 270 I=1,LTGDIV
	  WRITE(RLU,910) DLTPEROFF+I-1, DISPER(DLTPER(I)), 'DLTPER', I, DESCR(40)
270	CONTINUE
	WRITE(RLU,908) DLTSPROFF, DISPER(DLTSPR), 'DLTSPR', DESCR(41)
	DO 280 I=1,LTGDIV
	  WRITE(RLU,902) DLTTSROFF+(I-1)*2, DLTTSR(I,1), DLTTSR(I,2),
     *                 'DLTTSR', I, DESCR(42)
280	CONTINUE
	WRITE(RLU,900) DLTREVOFF, DLTREV, 'DLTREV', DESCR(43)
	WRITE(RLU,900) DLTBDROFF, DLTBDR, 'DLTBDR', DESCR(44)
	WRITE(RLU,900) DLTLSTOFF, DLTLST, 'DLTLST', DESCR(45)
	DO 290 I=1,LTGDIV
	  WRITE(RLU,907) DLTLSVOFF+(I-1)*2, CSMONY(DLTLSV(I,1),12,MONEY_UNIT),
     *                 CSMONY(DLTLSV(I,2),12,MONEY_UNIT),
     *                 'DLTLSV', I, DESCR(46)
290	CONTINUE
	WRITE(RLU,900) DLTADWOFF, DLTADW, 'DLTADW', DESCR(47)
	DO 295 I=1,LTGDIV
	  WRITE(RLU,907) DLTLANOFF+(I-1)*2, CSMONY(DLTLAN(I,1),12,MONEY_UNIT),
     *                   CSMONY(DLTLAN(I,2),12,MONEY_UNIT),
     *                  'DLTLAN', I, DESCR(48)
295	CONTINUE
	DO 300 I=1,LTGBET
	  WRITE(RLU,901) DLTBETOFF+I-1, DLTBET(I), 'DLTBET', I, DESCR(49)
300	CONTINUE
	WRITE(RLU,901) DLTWEDOFF,   DLTWED(1), 'DLTWED', 1, DESCR(50)
	WRITE(RLU,901) DLTWEDOFF+1, DLTWED(2), 'DLTWED', 2, DESCR(50)
	WRITE(RLU,908) DLTPAWOFF, DISPER(DLTPAW), 'DLTPAW', DESCR(51)
	DO 310 I=1,LTGDIV
	  WRITE(RLU,907) DLTSTXOFF+(I-1)*2, CSMONY(DLTSTX(I,1),12,MONEY_UNIT),
     *                   CSMONY(DLTSTX(I,2),12,MONEY_UNIT),
     *                  'DLTSTX', I, DESCR(54)
310	CONTINUE
	DO 320 I=1,LTGDIV
	  WRITE(RLU,906) DLTASHOFF+I-1, CSMONY(DLTASH(I),12,MONEY_UNIT), 
     *                'DLTASH', I, DESCR(55)
320	CONTINUE
	DO 330 I=1,10
	  WRITE(RLU,906) DLTWRFOFF+I-1, CSMONY(DLTWRF(I),12,MONEY_UNIT),
     *                  'DLTWRF', I, DESCR(56)
330	CONTINUE
	WRITE(RLU,909) DLTESTOFF, CSMONY(DLTEST,12,MONEY_UNIT), 'DLTEST',
     *                 DESCR(58)
	DO 350 I=1,MAXMLTD_AVL
	  WRITE(RLU,901) DLTMDSOFF+I-1, DLTMDS(I), 'DLTMDS', I, DESCR(59)
350	CONTINUE
	DO 360 I=1,MAXDRW
	  WRITE(RLU,906) DLTBALOFF+I-1, CSMONY(DLTBAL(I),12,MONEY_UNIT),
     *                'DLTBAL', I, DESCR(60)
360	CONTINUE
	WRITE(RLU,909) DLTSPLOFF, CSMONY(DLTSPL,12,MONEY_UNIT), 
     *              'DLTSPL', DESCR(66)



	RETURN  
C
C
900	FORMAT(1X,I5,1X,I12,1X,15X,A6,9X,A30)
901	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,')',5X,A30)
902	FORMAT(1X,I5,1X,I12,1X,I12,3X,A6,'(',I2,',*)',3X,A30)
903	FORMAT(1X,I5,1X,I12,1X,12X,3X,A6,'(',I2,',',I2,')',2X,A30)
904	FORMAT(1X,I5,1X,I12,1X,12X,3X,A6,'(',I2,',',I2,',',I2,')',1X,A28)
905	FORMAT(1X,I5,1X,I12,1X,12X,3X,A11,4X,A30)
906	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,')',5X,A30)
907	FORMAT(1X,I5,1X,A12,1X,A12,3X,A6,'(',I2,',*)',3X,A30)
908	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,9X,A30)
909	FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)
910	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,'(',I2,')',5X,A30)
911	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,9X,A30)
912	FORMAT(1X,I5,1X,F12.4,1X,15X,A6,9X,A30)




C
	END

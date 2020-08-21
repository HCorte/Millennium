C DMP_LTO.FOR
C
C V01 15-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP JOKER GAME FILE
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
	SUBROUTINE DMP_KIK(RLU,FILE,DRAW,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DKKREC.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'

        ! arguments
	INTEGER*4  RLU				!
	INTEGER*4  FILE(5)			!
        INTEGER*4  DRAW                         !
	INTEGER*4  MONEY_UNIT			!

	INTEGER*4 LUN
	INTEGER*4 I,J

        CHARACTER DESCR(46)*30

        DATA DESCR/
     *            'STATUS                        ',
     *            'ACTUAL TIME CLOSED            ',
     *            'TIME WHEN GAME SHOULD CLOSE   ',
     *            'DRAW NUMBER                   ',
     *            'JOKER BEGGINING DRAW DATE     ',
     *            'JOKER ENDING DRAW DATE        ',
     *            'LAST PURGE UPDATE             ',
     *            'LAST FILE UPDATE              ',
     *            'DRAW DATES                    ',
     *            'ADVANCE DRAW DATES            ',
     *            'SALES DATA                    ',
     *            'SHARES                        ',
     *            'AMOUNT PAID                   ',
     *            'AMOUNT PURGED                 ',
     *            'SHARE VALUES                  ',
     *            'POOL                          ',
     *            'BREAKAGE                      ',
     *            'DIVISION PAYOUT FROZEN FLAGS  ',
     *            'LOTTERY TAX                   ',
     *            'RESERVE AMOUNT                ',
     *            'WINNING NUMBERS               ',
     *            'WINNING NUMBER HOLD           ',
     *            'LAST SERIAL NUMBER            ',
     *            'PRICE                         ',
     *            'MAXIMUM NUMBER BET            ',
     *            'NUMBER OF DIVISIONS           ',
     *            'SALES PERCENTAGE              ',
     *            'WINNERS MATCH TABLE           ',
     *            'DIVISION PERCENTAGE           ',
     *            'TOTAL SHARE COUNT             ',
     *            'JOKER REVISION NUMBER         ',
     *            'JOKER NUMBER SEED             ',
     *            '# OF OCTAL DIGITS (FOR UNIRAN)',
     *            'MULTI-DRAW FLAG               ',
     *            'WIN RESERVE FUND TABLE        ',
     *            'ADDITIONAL POOL BY DIV        ',
     *            'MINIMUM JACKPOT               ',
     *            'DOUBLE JOKERI FREE FLAG       ',
     *            'MULTI DRAW SELECTED TABLE     ',
     *            'BALANCE SALES INFO. BY DRAW   ',
     *            'LAST OLD JOKER NORMAL DRAW #  ',
     *            'OLD JOKER WINNERS MATCH TABLE ',
     *            'SHRES FOR OLD MLTIWEEK TICKETS',
     *            'SHARE VALUES FOR OLD JOKER    ',
     *            'AMOUNT PAID FOR OLD JOKER     ',
     *            'SPEC. POOL,ADDED TO TOTAL POOL'/





C
C READ GAME FILE
C
	LUN = 9     ! ?
        CALL READGFL(LUN,FILE,DKKSEC,DRAW,DKKREC)
C
C DUMP RECORD
C
	WRITE(RLU,900) DKKSTSOFF, DKKSTS, 'DKKSTS', DESCR(1)
	WRITE(RLU,911) DKKCTMOFF, DISTIM(DKKCTM), 'DKKCTM', DESCR(2)
	WRITE(RLU,911) DKKTIMOFF, DISTIM(DKKTIM), 'DKKTIM', DESCR(3)
	WRITE(RLU,900) DKKDRWOFF, DKKDRW, 'DKKDRW', DESCR(4)
	WRITE(RLU,900) DKKBSDOFF, DKKBSD, 'DKKBSD', DESCR(5)
	WRITE(RLU,900) DKKESDOFF, DKKESD, 'DKKESD', DESCR(6)
	WRITE(RLU,900) DKKPUPOFF, DKKPUP, 'DKKPUP', DESCR(7)
	WRITE(RLU,900) DKKUPDOFF, DKKUPD, 'DKKUPD', DESCR(8)
	DO 100 I=1,DATLEN
	  WRITE(RLU,901) DKKDATOFF+I-1, DKKDAT(I), 'DKKDAT', I, DESCR(9)
100	CONTINUE
	DO 110 I=1,NUMADV
	  WRITE(RLU,901) DKKADVOFF+I-1, DKKADV(I), 'DKKADV', I, DESCR(10)
110	CONTINUE
        DO 120 I=1,KIGENT
          DO J=1,MAXGAM
            WRITE(RLU,912) DKKSALOFF+(I-1)*MAXGAM+J-1, 
     *                   CSMONY(DKKSAL(I,J),12,MONEY_UNIT),
     *                   'DKKSAL', I, J, DESCR(11)
          ENDDO
120     CONTINUE
	DO 130 I=1,KIGDIV
	  WRITE(RLU,901) DKKSHROFF+I-1, DKKSHR(I), 'DKKSHR', I, DESCR(12)
130	CONTINUE
	DO 140 I=1,KIGDIV
	  WRITE(RLU,913) DKKPADOFF+I-1, CSMONY(DKKPAD(I),12,MONEY_UNIT),
     *                 'DKKPAD', I, DESCR(13)
140	CONTINUE
	DO 150 I=1,KIGDIV
	  WRITE(RLU,913) DKKPRGOFF+I-1, CSMONY(DKKPRG(I),12,MONEY_UNIT),
     *                 'DKKPRG', I, DESCR(14)
150	CONTINUE
	DO 160 I=1,KIGDIV
	  WRITE(RLU,913) DKKSHVOFF+I-1, CSMONY(DKKSHV(I),12,MONEY_UNIT),
     *                 'DKKSHV', I, DESCR(15)
160	CONTINUE
	DO 170 I=1,KIGDIV
	  WRITE(RLU,917) DKKPOLOFF+(I-1)*2, 
     *                   CSMONYI8(DKKPOL(1,I),12,MONEY_UNIT),
     *                  'DKKPOL', I, DESCR(16)
170	CONTINUE
	DO 180 I=1,KIGDIV
	  WRITE(RLU,913) DKKBRKOFF+I-1, CSMONY(DKKBRK(I),12,MONEY_UNIT),
     *                 'DKKBRK', I, DESCR(17)
180	CONTINUE
	DO 190 I=1,KIGDIV
	  WRITE(RLU,901) DKKFRZOFF+I-1, DKKFRZ(I), 'DKKFRZ', I, DESCR(18)
190	CONTINUE
	WRITE(RLU,900) DKKTAXOFF, DKKTAX, 'DKKTAX', DESCR(19)
	WRITE(RLU,913) DKKRESOFF,   CSMONY(DKKRES(1),12,MONEY_UNIT),
     *               'DKKRES', 1, DESCR(20)
	WRITE(RLU,913) DKKRESOFF+1, CSMONY(DKKRES(2),12,MONEY_UNIT),
     *               'DKKRES', 2, DESCR(20)
	WRITE(RLU,900) DKKWINOFF, DKKWIN, 'DKKWIN', DESCR(21)
	WRITE(RLU,900) DKKHLDOFF, DKKHLD, 'DKKHLD', DESCR(22)
	WRITE(RLU,900) DKKSEROFF, DKKSER, 'DKKSER', DESCR(23)
	WRITE(RLU,919) DKKPRCOFF, DFLOAT(DKKPRC) / (P(PRFACTOR)*DOLL_BASE),
     *              'DKKPRC', DESCR(24)
	WRITE(RLU,900) DKKMAXOFF, DKKMAX, 'DKKMAX', DESCR(25)
	WRITE(RLU,900) DKKDIVOFF, DKKDIV, 'DKKDIV', DESCR(26)
	WRITE(RLU,914) DKKSPROFF, DISPER(DKKSPR), 'DKKSPR', DESCR(27)
        DO 200 I=1,3
          DO J=1,KIGDIV
            WRITE(RLU,903) DKKMATOFF+(I-1)*KIGDIV+J-1, DKKMAT(I,J),
     *                   'DKKMAT', I, J, DESCR(28)
          ENDDO
200     CONTINUE
	DO 210 I=1,KIGDIV
	  WRITE(RLU,916) DKKPEROFF+I-1, DISPER(DKKPER(I)), 'DKKPER', I, DESCR(29)
210	CONTINUE
	DO 220 I=1,KIGDIV
	  WRITE(RLU,901) DKKTSROFF+I-1, DKKTSR(I), 'DKKTSR', I, DESCR(30)
220	CONTINUE
	WRITE(RLU,900) DKKREVOFF, DKKREV, 'DKKREV', DESCR(31)
	WRITE(RLU,901) DKKSEDOFF,   DKKSED(1), 'DKKSED', 1, DESCR(32)
	WRITE(RLU,901) DKKSEDOFF+1, DKKSED(2), 'DKKSED', 2, DESCR(32)
	WRITE(RLU,900) DKKOCTOFF, DKKOCT, 'DKKOCT', DESCR(33)
	WRITE(RLU,900) DKKMLTOFF, DKKMLT, 'DKKMLT', DESCR(34)
	DO 230 I=1,10
	  WRITE(RLU,901) DKKWRFOFF+I-1, DKKWRF(I), 'DKKWRF', I, DESCR(35)
230	CONTINUE
	DO 240 I=1,KIGDIV
	  WRITE(RLU,913) DKKASHOFF+I-1, CSMONY(DKKASH(I),12,MONEY_UNIT),
     *                'DKKASH', I, DESCR(36)
240	CONTINUE
	WRITE(RLU,915) DKKMINOFF, CSMONY(DKKMIN,12,MONEY_UNIT), 
     *              'DKKMIN', DESCR(37)
	WRITE(RLU,900) DKKDFFOFF, DKKDFF, 'DKKDFF', DESCR(38)
	DO 250 I=1,MAXMLTD_AVL
	  WRITE(RLU,901) DKKMDSOFF+I-1, DKKMDS(I), 'DKKMDS', I, DESCR(39)
250	CONTINUE
	DO 260 I=1,MAXDRW
	  WRITE(RLU,913) DKKBALOFF+I-1, CSMONY(DKKBAL(I),12,MONEY_UNIT),
     *                'DKKBAL', I, DESCR(40)
260	CONTINUE
	WRITE(RLU,915) DKKSPLOFF, CSMONY(DKKSPL,12,MONEY_UNIT), 'DKKSPL',
     *                 DESCR(46)

	WRITE(RLU,918) 'LIVRE EM INTEIROS DE 4 EM DISCO   =', DKKRECLEN-DKKFREOFF+1
	WRITE(RLU,918) 'LIVRE EM INTEIROS DE 4 EM MEMORIA =', KIKLEN-KIKFRE_OFF+1


	RETURN  
C
C
900	FORMAT(1X,I5,1X,I12,1X,15X,A6,9X,A30)
901	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,')',5X,A30)
902	FORMAT(1X,I5,1X,I12,1X,I12,3X,A6,'(',I2,',*)',3X,A30)
903	FORMAT(1X,I5,1X,I12,1X,12X,3X,A6,'(',I2,',',I2,')',2X,A30)
904	FORMAT(1X,I5,1X,I12,1X,15X,A6,'(',I2,',',I2,',',I2,')',1X,A28)
905	FORMAT(1X,I5,1X,I12,1X,10X,A11,8X,A30)
906	FORMAT(1X,I5,1X,A13,1X,A13,1X,A6,'(',I2,')',5X,A30)
907	FORMAT(1X,I5,1X,A16,1X,10X,1X,A6,8X,A30)
908	FORMAT(1X,I5,1X,I12,1X,I12,3X,A6,'(*,',I2,')',3X,A30)
909	FORMAT(1X,I5,1X,I12,1X,15X,A7,'(',I2,',',I2,')',1X,A30)
910	FORMAT(1X,I5,1X,I12,1X,15X,A7,'(',I2,')',4X,A30)
911	FORMAT(1X,I5,1X,4X,A8,1X,15X,A6,9X,A30)
912	FORMAT(1X,I5,1X,A12,1X,14X,1X,A6,'(',I2,',',I2,')',2X,A30)
913	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,')',5X,A30)
914	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,9X,A30)
915	FORMAT(1X,I5,1X,A12,1X,15X,A6,9X,A30)
916	FORMAT(1X,I5,1X,5X,F7.3,1X,15X,A6,'(',I2,')',5X,A30)
917	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(1,',I2,')',3X,A30)
918     FORMAT(1X,A35,I8)
919	FORMAT(1X,I5,1X,F12.4,1X,15X,A6,9X,A30)

C
	END

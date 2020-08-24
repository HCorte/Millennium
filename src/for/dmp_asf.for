C DMP_ASF.FOR
C
C V03 11-MAR-2010 RXK CLAIMS REPLACED WITH RETURNS
C V02 12-JUN-2001 EPH ADDED SOME DATA FROM ASFINF
C V01 27-JUN-2000 PXO
C 
C SUBROUTINE TO DUMP ASF FILE
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
	SUBROUTINE DMP_ASF(RLU,FILE,SAGT,EAGT,MONEY_UNIT)
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE	'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'

        ! arguments
	INTEGER*4  RLU
	INTEGER*4  FILE(5)			!
	INTEGER*4  SAGT
	INTEGER*4  EAGT
	INTEGER*4  MONEY_UNIT			!
	

	INTEGER*4 LUN
	INTEGER*4 ST
	INTEGER*4 FDB(7)
	INTEGER*4 I,J,K
	INTEGER*4 RAGT

	INTEGER*4 LINHA_DISTRIBUICAO, CENTRAL_RECEPCAO, STATUS_TRANSPORTE

	INTEGER*4 BEGSAL, ENDSAL, BEGSUP, ENDSUP
	INTEGER*4 SAPN, BKOP, BROP
	CHARACTER*21 PASNIB, WAGNIB

	CHARACTER DESCR(24)*30
	CHARACTER DESCR_2(15)*30
	CHARACTER DESCR_3(30)*30
	CHARACTER DESCR_4(6)*30
	CHARACTER DESCR_5(11)*30

	DATA DESCR/
     *            'RECORD LOCK FLAG              ',
     *            'AGENT ID INFORMATION          ',
     *            'SALES DATES                   ',
     *            'DAILY SALES                   ',
     *            'INVOICE SALES                 ',
     *            'INVOICE DATA                  ',
     *            'LEDGER TABLE                  ',
     *            'GAME FLAGS                    ',
     *            'WEEKLY UPDATE COUNT           ',
     *            'SECURITY NUMBER 1             ',
     *            'SECURITY NUMBER 2             ',
     *            'YTD SALES                     ',
     *            'YTD INVOICE DATA              ',
     *            'GUTS SWAP INFO                ',
     *            'HIGH TIER WINS CNT/AMT        ',
     *            'DAILY SALES SPECIAL           ',
     *            'MISCELLANEOUS SALES           ',
     *            'DELETED AGENT NUMBER          ',
     *            'INSTANT TICKET INVOICE        ',
     *            'INSTANT GAME SALES            ',
     *            'FIRST SIGNON CDC              ',
     *            'TIME FOR GVT TO CALL(DLL)     ',
     *            'NEXT CALLBACK CDC             ',
     *            'LAST CALLBACK CDC             '/



	DATA DESCR_2/
     *            'SALES COUNT                   ',
     *            'SALES AMOUNT                  ',
     *            'CANCEL COUNT                  ',
     *            'CANCEL AMOUNT                 ',
     *            'VALID COUNT                   ',
     *            'VALID AMOUNT                  ',
     *            'RETURN COUNT                  ',
     *            'RETURN AMOUNT                 ',
     *            'REFUND COUNT                  ',
     *            'REFUND AMOUNT                 ',
     *            'DISCOUNT COUNT                ',
     *            'DISCOUNT AMOUNT               ',
     *            'TICKET CHARGE                 ',
     *            'GAME FLAGS  (SAME AS AGTTYP)  ',
     *            '                              '/


	DATA DESCR_3/
     *            'DAYS ACTIVE                   ',
     *            'TICKET CHARGE                 ',
     *            'SERVICE CHARGE                ',
     *            'SALES COM (UNITS)             ',
     *            'SALES COM (PENNIES)           ',
     *            'VALIDATION COM  (UNITS)       ',
     *            'VALIDATION COM (PENNNIES)     ',
     *            'WINNERS COM (UNITS)           ',
     *            'WINNERS COM (PENNIES)         ',
     *            'PAYMENTS (UNITS)              ',
     *            'PAYMENTS (PENNIES)            ',
     *            'ADJUSTMENTS (UNITS)           ',
     *            'ADJUSTMENTS (PENNIES)         ',
     *            'START DATE                    ',
     *            'END DATE                      ',
     *            'BALANCE (UNITS)               ',
     *            'BALANCE (PENNIES)             ',
     *            'AMOUNT DUE (UNITS)            ',
     *            'AMOUNT DUE (PENNIES)          ',
     *            'WINNERS COUNT                 ',
     *            'WINNERS AMOUNT                ',
     *            'BTW TAX (UNITS)               ',
     *            'BTW TAX (PENNIES)             ',
     *            'OFFLINE VALIDATIONS           ',
     *            'TOTAL AMOUNT WON FOR BEING... ',
     *            'WEEK+YEAR                     ',
     *            'TYPE FOR FIRM                 ',
     *            'TOTAL FOR FIRM                ',
     *            'TYPE FOR SHOP                 ',
     *            'TOTAL FOR SHOP                '/

	DATA DESCR_4/
     *            'CODE                          ',
     *            'CDC DATE                      ',
     *            'AMOUNT (UNITS)                ',
     *            'AMOUNT (PENNIES)              ',
     *            'INFORMATIONAL                 ',
     *            '                              '/

	DATA DESCR_5/
     *            'PACKS SOLD COUNT              ',
     *            'PACKS SOLD AMOUNT             ',
     *            'VALIDATIONS COUNT             ',
     *            'VALIDATIONS AMOUNT            ',
     *            'BANK VALIDATIONS COUNT        ',
     *            'BANK VALIDATIONS AMOUNT       ',
     *            'RETURNS AMOUNT                ',
     *            'SALES COM                     ',
     *            'PRIZE COM                     ',
     *            'ADJUSTMENT AMOUNT             ',
     *            'TOTAL AMOUNT DUE              '/




C
C READ ASF FILE
C
	LUN = 9     ! ?
        CALL OPENW(LUN,FILE,4,0,0,ST)
        CALL IOINIT(FDB,LUN,ASFSEC*256)
        IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
C
C LOOP THROUGH SELECTED AGENTS
C 
	DO 2000 RAGT=SAGT,EAGT
		
        CALL READW(FDB,RAGT,ASFREC,ST)
        IF(ST.NE.0) CALL FILERR(FILE,2,ST,RAGT)
C
C	
	WRITE(RLU,926) RAGT
C
C DUMP RECORD
C
	WRITE(RLU,900) ASFLOK_OFF, ASFLOK, 'ASFLOK', DESCR(1)

C ASFINF !!

	CALL ASCBIN(ASFINF, SLIND, LLIND, LINHA_DISTRIBUICAO, ST)
	CALL ASCBIN(ASFINF, SCENR, LCENR, CENTRAL_RECEPCAO,   ST)
	CALL ASCBIN(ASFINF, SSTTP, LSTTP, STATUS_TRANSPORTE,  ST)

	WRITE(RLU,787) LINHA_DISTRIBUICAO, CENTRAL_RECEPCAO, STATUS_TRANSPORTE
787	FORMAT(1X,'LINHA DE DISTRIBUICAO : ', I2.2,/,
     *         1X,'CENTRAL DE RECEPCAO   : ', I7.7,/,
     *         1X,'STATUS DE TRANSPORTE  : ', I1,/)

	CALL ASCBIN(ASFINF, SWBSU, LWBSU, BEGSUP, ST)
	CALL ASCBIN(ASFINF, SWESU, LWESU, ENDSUP, ST)
	CALL ASCBIN(ASFINF, SWBSA, LWBSA, BEGSAL, ST)
	CALL ASCBIN(ASFINF, SWESA, LWESA, ENDSAL, ST)

	WRITE(RLU,788) BEGSAL, ENDSAL, BEGSUP, ENDSUP
788	FORMAT(1X,'MUTUAS: INICIO VENDAS     = ', I6.6,/,
     *         1X,'MUTUAS: FIM VENDAS        = ', I6.6,/,
     *         1X,'MUTUAS: INICIO SUSPENSAO  = ', I6.6,/,
     *         1X,'MUTUAS: FIM SUSPENSAO     = ', I6.6,/)

	CALL ASCBIN(ASFINF, SPBSU, LPBSU, BEGSUP, ST)
	CALL ASCBIN(ASFINF, SPESU, LPESU, ENDSUP, ST)
	CALL ASCBIN(ASFINF, SPBSA, LPBSA, BEGSAL, ST)
	CALL ASCBIN(ASFINF, SPESA, LPESA, ENDSAL, ST)

	WRITE(RLU,789) BEGSAL, ENDSAL, BEGSUP, ENDSUP
789	FORMAT(1X,'PASSIVA: INICIO VENDAS     = ', I6.6,/,
     *         1X,'PASSIVA: FIM VENDAS        = ', I6.6,/,
     *         1X,'PASSIVA: INICIO SUSPENSAO  = ', I6.6,/,
     *         1X,'PASSIVA: FIM SUSPENSAO     = ', I6.6,/)

	CALL ASCBIN(ASFINF, SSAPN, LSAPN, SAPN, ST)

	WRITE(WAGNIB, FMT='(21A1)') (ASFBYT(K), K=SWANB,EWANB)
	WRITE(PASNIB, FMT='(21A1)') (ASFBYT(K), K=SPANB,EPANB)

	CALL ASCBIN(ASFINF, SBKOP, LBKOP, BKOP, ST)
	CALL ASCBIN(ASFINF, SBROP, LBROP, BROP, ST)

	WRITE(RLU,790) SAPN, WAGNIB, PASNIB, BKOP, BROP
790	FORMAT(1X,'SAP NUMBER = ', I6.6,/,
     *         1X,'NIB MUTUAS   = ', A21,/,
     *         1X,'NIB PASSIVA  = ', A21,/,
     *         1X,'BANCO DE OPS   = ', I4.4,/,
     *         1X,'BALCAO DE OPS  = ', I4.4,/)

	DO 100 I=1,ANUMDAY
	  DO J=1,2
	    WRITE(RLU,903) ASFDAT_OFF+(I-1)*2+J-1,
     *                     ASFDAT(J,I), 
     *                    'ASFDAT', J, I, DESCR(3)
	  ENDDO
100	CONTINUE

	WRITE(RLU,917) DESCR(4)
	DO 110 K=1,ANUMDAY
	  DO J=1,MAXGAM
	      WRITE(RLU,904) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN,
     *                       ASFDAY(GSCNT,J,K), 
     *                      'ASFDAY', 1, J, K, DESCR_2(1)
	      WRITE(RLU,916) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+1,
     *                       CSMONY(ASFDAY(GSAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFDAY', 2, J, K, DESCR_2(2)
	      WRITE(RLU,904) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+2,
     *                       ASFDAY(GCCNT,J,K), 
     *                      'ASFDAY', 3, J, K, DESCR_2(3)
	      WRITE(RLU,916) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+3,
     *                       CSMONY(ASFDAY(GCAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFDAY', 4, J, K, DESCR_2(4)
	      WRITE(RLU,904) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+4,
     *                       ASFDAY(GVCNT,J,K), 
     *                      'ASFDAY', 5, J, K, DESCR_2(5)
	      WRITE(RLU,916) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+5,
     *                       CSMONY(ASFDAY(GVAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFDAY', 6, J, K, DESCR_2(6)
	      WRITE(RLU,904) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+6,
     *                       ASFDAY(GCLCNT,J,K), 
     *                      'ASFDAY', 7, J, K, DESCR_2(7)
	      WRITE(RLU,916) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+7,
     *                       CSMONY(ASFDAY(GCLAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFDAY', 8, J, K, DESCR_2(8)
	      WRITE(RLU,904) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+8,
     *                       ASFDAY(GRCNT,J,K), 
     *                      'ASFDAY', 9, J, K, DESCR_2(9)
	      WRITE(RLU,916) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+9,
     *                       CSMONY(ASFDAY(GRAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFDAY', 10, J, K, DESCR_2(10)
	      WRITE(RLU,904) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+10,
     *                       ASFDAY(GDCNT,J,K), 
     *                      'ASFDAY', 11, J, K, DESCR_2(11)
	      WRITE(RLU,916) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+11,
     *                       CSMONY(ASFDAY(GDAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFDAY', 12, J, K, DESCR_2(12)
	      WRITE(RLU,916) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+12,
     *                       CSMONY(ASFDAY(GTKCHG,J,K),12,MONEY_UNIT), 
     *                      'ASFDAY', 13, J, K, DESCR_2(13)
	      WRITE(RLU,904) ASFDAY_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+13,
     *                       ASFDAY(GFLAGS,J,K), 
     *                      'ASFDAY', 14, J, K, DESCR_2(14)
	  ENDDO
110	CONTINUE


	WRITE(RLU,917) DESCR(5)
	DO 120 K=1,2
	  DO J=1,MAXGAM
	      WRITE(RLU,904) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL,
     *                       ASFBIL(GSCNT,J,K), 
     *                      'ASFBIL', 1, J, K, DESCR_2(1)
	      WRITE(RLU,916) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL+1,
     *                       CSMONY(ASFBIL(GSAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFBIL', 2, J, K, DESCR_2(2)
	      WRITE(RLU,904) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL+2,
     *                       ASFBIL(GCCNT,J,K), 
     *                      'ASFBIL', 3, J, K, DESCR_2(3)
	      WRITE(RLU,916) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL+3,
     *                       CSMONY(ASFBIL(GCAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFBIL', 4, J, K, DESCR_2(4)
	      WRITE(RLU,904) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL+4,
     *                       ASFBIL(GVCNT,J,K), 
     *                      'ASFBIL', 5, J, K, DESCR_2(5)
	      WRITE(RLU,916) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL+5,
     *                       CSMONY(ASFBIL(GVAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFBIL', 6, J, K, DESCR_2(6)
	      WRITE(RLU,904) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL+6,
     *                       ASFBIL(GCLCNT,J,K), 
     *                      'ASFBIL', 7, J, K, DESCR_2(7)
	      WRITE(RLU,916) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL+7,
     *                       CSMONY(ASFBIL(GCLAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFBIL', 8, J, K, DESCR_2(8)
	      WRITE(RLU,904) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL+8,
     *                       ASFBIL(GRCNT,J,K), 
     *                      'ASFBIL', 9, J, K, DESCR_2(9)
	      WRITE(RLU,916) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL+9,
     *                       CSMONY(ASFBIL(GRAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFBIL', 10, J, K, DESCR_2(10)
	      WRITE(RLU,904) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL+10,
     *                       ASFBIL(GDCNT,J,K), 
     *                      'ASFBIL', 11, J, K, DESCR_2(11)
	      WRITE(RLU,916) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL+11,
     *                       CSMONY(ASFBIL(GDAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFBIL', 12, J, K, DESCR_2(12)
	      WRITE(RLU,916) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL+12,
     *                       CSMONY(ASFBIL(GTKCHG,J,K),12,MONEY_UNIT), 
     *                      'ASFBIL', 13, J, K, DESCR_2(13)
	      WRITE(RLU,904) 
     * ASFBIL_OFF+(K-1)*MAXGAM*AGAMLEN+MAXMLTD_SEL+(J-1)*AGAMLEN+MAXMLTD_SEL+13,
     *                       ASFBIL(GFLAGS,J,K), 
     *                      'ASFBIL', 14, J, K, DESCR_2(14)
	      DO I=1,MAXMLTD_SEL
	        WRITE(RLU,904)
     * ASFBIL_OFF+((K-1)*MAXGAM+(J-1))*AGAMLEN+MAXMLTD_SEL+MAXMLTD_SEL+13+I,
     *  ASFBIL(AGAMLEN+I,J,K), 'ASFBIL', 14+I, J, K, DESCR_2(15)
	      ENDDO
	  ENDDO
120	CONTINUE

	WRITE(RLU,917) DESCR(6)
	WRITE(RLU,903) ASFINV_OFF,
     *                       ASFINV(ASFACT,1), 
     *                      'ASFINV', 1, 1, DESCR_3(1)
	WRITE(RLU,913) ASFINV_OFF+2,
     *                       CSMONY(ASFINV(ASFTKC,1),12,MONEY_UNIT), 
     *                      'ASFINV', 2, 1, DESCR_3(2)
	WRITE(RLU,913) ASFINV_OFF+4,
     *                       CSMONY(ASFINV(ASFSRV,1),12,MONEY_UNIT), 
     *                      'ASFINV', 3, 1, DESCR_3(3)
	WRITE(RLU,913) ASFINV_OFF+6,
     *                       CSMONYI8(ASFINV(ASFSCMU,1),12,MONEY_UNIT), 
     *                      'ASFINV', 4, 1, DESCR_3(4)
	WRITE(RLU,903) ASFINV_OFF+8,
     *                       ASFINV(ASFSCMP,1), 
     *                      'ASFINV', 5, 1, DESCR_3(5)
	WRITE(RLU,913) ASFINV_OFF+10,
     *                       CSMONYI8(ASFINV(ASFVCMU,1),12,MONEY_UNIT), 
     *                      'ASFINV', 6, 1, DESCR_3(6)
	WRITE(RLU,903) ASFINV_OFF+12,
     *                       ASFINV(ASFVCMP,1), 
     *                      'ASFINV', 7, 1, DESCR_3(7)
	WRITE(RLU,913) ASFINV_OFF+14,
     *                       CSMONYI8(ASFINV(ASFWCMU,1),12,MONEY_UNIT), 
     *                      'ASFINV', 8, 1, DESCR_3(8)
	WRITE(RLU,903) ASFINV_OFF+16,
     *                       ASFINV(ASFWCMP,1), 
     *                      'ASFINV', 9, 1, DESCR_3(9)
	WRITE(RLU,913) ASFINV_OFF+18,
     *                       CSMONYI8(ASFINV(ASFPADU,1),12,MONEY_UNIT), 
     *                      'ASFINV', 10, 1, DESCR_3(10)
	WRITE(RLU,903) ASFINV_OFF+20,
     *                       ASFINV(ASFPADP,1), 
     *                      'ASFINV', 11, 1, DESCR_3(11)
	WRITE(RLU,913) ASFINV_OFF+22,
     *                       CSMONYI8(ASFINV(ASFADJU,1),12,MONEY_UNIT), 
     *                      'ASFINV', 12, 1, DESCR_3(12)
	WRITE(RLU,903) ASFINV_OFF+24,
     *                       ASFINV(ASFADJP,1), 
     *                      'ASFINV', 13, 1, DESCR_3(13)
	WRITE(RLU,903) ASFINV_OFF+26,
     *                       ASFINV(ASFSTR,1), 
     *                      'ASFINV', 14, 1, DESCR_3(14)
	WRITE(RLU,903) ASFINV_OFF+28,
     *                       ASFINV(ASFEND,1), 
     *                      'ASFINV', 15, 1, DESCR_3(15)
	WRITE(RLU,903) ASFINV_OFF+29,
     *                       ASFINV(ASFEND,2), 
     *                      'ASFINV', 15, 2, DESCR_3(15)
	WRITE(RLU,913) ASFINV_OFF+30,
     *                       CSMONY(ASFINV(ASFBALU,1),12,MONEY_UNIT), 
     *                      'ASFINV', 16, 1, DESCR_3(16)
	WRITE(RLU,903) ASFINV_OFF+32,
     *                       ASFINV(ASFBALP,1), 
     *                      'ASFINV', 17, 1, DESCR_3(17)
	WRITE(RLU,913) ASFINV_OFF+34,
     *                       CSMONYI8(ASFINV(ASFDUEU,1),12,MONEY_UNIT), 
     *                      'ASFINV', 18, 1, DESCR_3(18)
	WRITE(RLU,903) ASFINV_OFF+36,
     *                       ASFINV(ASFDUEP,1), 
     *                      'ASFINV', 19, 1, DESCR_3(19)
	WRITE(RLU,903) ASFINV_OFF+38,
     *                       ASFINV(ASFWCNT,1), 
     *                      'ASFINV', 20, 1, DESCR_3(20)
	WRITE(RLU,913) ASFINV_OFF+40,
     *                       CSMONY(ASFINV(ASFWAMT,1),12,MONEY_UNIT), 
     *                      'ASFINV', 21, 1, DESCR_3(21)
	WRITE(RLU,913) ASFINV_OFF+42,
     *                       CSMONYI8(ASFINV(ASFBTWU,1),12,MONEY_UNIT), 
     *                      'ASFINV', 22, 1, DESCR_3(22)
	WRITE(RLU,903) ASFINV_OFF+44,
     *                       ASFINV(ASFBTWC,1), 
     *                      'ASFINV', 23, 1, DESCR_3(23)
	WRITE(RLU,903) ASFINV_OFF+46,
     *                       ASFINV(ASFOFFPAY,1), 
     *                      'ASFINV', 24, 1, DESCR_3(24)
	WRITE(RLU,903) ASFINV_OFF+48,
     *                       ASFINV(ASFCRAMT,1), 
     *                      'ASFINV', 25, 1, DESCR_3(25)
	WRITE(RLU,903) ASFINV_OFF+50,
     *                       ASFINV(ASFORCWEK,1), 
     *                      'ASFINV', 26, 1, DESCR_3(26)
	WRITE(RLU,903) ASFINV_OFF+52,
     *                       ASFINV(ASFORCFRT,1), 
     *                      'ASFINV', 27, 1, DESCR_3(27)
	WRITE(RLU,903) ASFINV_OFF+54,
     *                       ASFINV(ASFORCFRD,1), 
     *                      'ASFINV', 28, 1, DESCR_3(28)
	WRITE(RLU,903) ASFINV_OFF+56,
     *                       ASFINV(ASFORCSHT,1), 
     *                      'ASFINV', 29, 1, DESCR_3(29)
	WRITE(RLU,903) ASFINV_OFF+58,
     *                       ASFINV(ASFORCSHD,1), 
     *                      'ASFINV', 30, 1, DESCR_3(30)


	WRITE(RLU,917) DESCR(7)
	DO 140 I=1,15
	      WRITE(RLU,903) ASFLGR_OFF+(I-1)*6,
     *                       ASFLGR(LGRCOD,I), 
     *                      'ASFLGR', LGRCOD, I, DESCR_4(1)
	      WRITE(RLU,903) ASFLGR_OFF+(I-1)*6+1,
     *                       ASFLGR(LGRCDC,I), 
     *                      'ASFLGR', LGRCDC, I, DESCR_4(2)
	      WRITE(RLU,913) ASFLGR_OFF+(I-1)*6+2,
     *                       CSMONY(ASFLGR(LGRAMTU,I),12,MONEY_UNIT), 
     *                      'ASFLGR', LGRAMTU, I, DESCR_4(3)
	      WRITE(RLU,903) ASFLGR_OFF+(I-1)*6+3,
     *                       ASFLGR(LGRAMTP,I), 
     *                      'ASFLGR', LGRAMTP, I, DESCR_4(4)
	      WRITE(RLU,903) ASFLGR_OFF+(I-1)*6+4,
     *                       ASFLGR(LGRINF,I), 
     *                      'ASFLGR', LGRINF, I, DESCR_4(5)
	      WRITE(RLU,903) ASFLGR_OFF+(I-1)*6+5,
     *                       ASFLGR(6,I), 
     *                      'ASFLGR', 6, I, DESCR_4(6)
	  
140	CONTINUE

	DO 150 I=1,MAXGAM
	      WRITE(RLU,901) ASFGFL_OFF+(I-1),
     *                       ASFGFL(I), 
     *                      'ASFGFL', I, DESCR(8)
	  
150	CONTINUE

	WRITE(RLU,900) ASFWCT_OFF, ASFWCT, 'ASFWCT', DESCR(9)
	WRITE(RLU,900) ASFSC1_OFF, ASFSC1, 'ASFSC1', DESCR(10)
	WRITE(RLU,900) ASFSC2_OFF, ASFSC2, 'ASFSC2', DESCR(11)
	WRITE(RLU,917) DESCR(12)
	DO 160 K=1,2
	  DO J=1,MAXGAM
	      WRITE(RLU,904) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN,
     *                       ASFYTD(GSCNT,J,K), 
     *                      'ASFYTD', GSCNT, J, K, DESCR_2(1)
	      WRITE(RLU,916) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+1,
     *                       CSMONY(ASFYTD(GSAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFYTD', GSAMT, J, K, DESCR_2(2)
	      WRITE(RLU,904) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+2,
     *                       ASFYTD(GCCNT,J,K), 
     *                      'ASFYTD', GCCNT, J, K, DESCR_2(3)
	      WRITE(RLU,916) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+3,
     *                       CSMONY(ASFYTD(GCAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFYTD', GCAMT, J, K, DESCR_2(4)
	      WRITE(RLU,904) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+4,
     *                       ASFYTD(GVCNT,J,K), 
     *                      'ASFYTD', GVCNT, J, K, DESCR_2(5)
	      WRITE(RLU,916) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+5,
     *                       CSMONY(ASFYTD(GVAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFYTD', GVAMT, J, K, DESCR_2(6)
	      WRITE(RLU,904) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+6,
     *                       ASFYTD(GCLCNT,J,K), 
     *                      'ASFYTD', GCLCNT, J, K, DESCR_2(7)
	      WRITE(RLU,916) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+7,
     *                       CSMONY(ASFYTD(GCLAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFYTD', GCLAMT, J, K, DESCR_2(8)
	      WRITE(RLU,904) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+8,
     *                       ASFYTD(GRCNT,J,K), 
     *                      'ASFYTD', GRCNT, J, K, DESCR_2(9)
	      WRITE(RLU,916) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+9,
     *                       CSMONY(ASFYTD(GRAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFYTD', GRAMT, J, K, DESCR_2(10)
	      WRITE(RLU,904) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+10,
     *                       ASFYTD(GDCNT,J,K), 
     *                      'ASFYTD', GDCNT, J, K, DESCR_2(11)
	      WRITE(RLU,916) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+11,
     *                       CSMONY(ASFYTD(GDAMT,J,K),12,MONEY_UNIT), 
     *                      'ASFYTD', GDAMT, J, K, DESCR_2(12)
	      WRITE(RLU,916) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+12,
     *                       CSMONY(ASFYTD(GTKCHG,J,K),12,MONEY_UNIT), 
     *                      'ASFYTD', GTKCHG, J, K, DESCR_2(13)
	      WRITE(RLU,904) ASFYTD_OFF+(K-1)*MAXGAM*AGAMLEN+(J-1)*AGAMLEN+13,
     *                       ASFYTD(GFLAGS,J,K), 
     *                      'ASFYTD', GFLAGS, J, K, DESCR_2(14)
	  ENDDO
160	CONTINUE

	WRITE(RLU,917) DESCR(13)
	WRITE(RLU,919) ASFYTDINV_OFF,
     *                       ASFYTDINV(ASFACT,1), 
     *                      'ASFYTDINV', 1, 1, DESCR_3(1)
	WRITE(RLU,920) ASFYTDINV_OFF+2,
     *                       CSMONY(ASFYTDINV(ASFTKC,1),12,MONEY_UNIT), 
     *                      'ASFYTDINV', 2, 1, DESCR_3(2)
	WRITE(RLU,920) ASFYTDINV_OFF+4,
     *                       CSMONY(ASFYTDINV(ASFSRV,1),12,MONEY_UNIT), 
     *                      'ASFYTDINV', 3, 1, DESCR_3(3)
	WRITE(RLU,920) ASFYTDINV_OFF+6,
     *                       CSMONYI8(ASFYTDINV(ASFSCMU,1),12,MONEY_UNIT), 
     *                      'ASFYTDINV', 4, 1, DESCR_3(4)
	WRITE(RLU,919) ASFYTDINV_OFF+8,
     *                       ASFYTDINV(ASFSCMP,1), 
     *                      'ASFYTDINV', 5, 1, DESCR_3(5)
	WRITE(RLU,920) ASFYTDINV_OFF+10,
     *                       CSMONYI8(ASFYTDINV(ASFVCMU,1),12,MONEY_UNIT), 
     *                      'ASFYTDINV', 6, 1, DESCR_3(6)
	WRITE(RLU,919) ASFYTDINV_OFF+12,
     *                       ASFYTDINV(ASFVCMP,1), 
     *                      'ASFYTDINV', 7, 1, DESCR_3(7)
	WRITE(RLU,920) ASFYTDINV_OFF+14,
     *                       CSMONYI8(ASFYTDINV(ASFWCMU,1),12,MONEY_UNIT), 
     *                      'ASFYTDINV', 8, 1, DESCR_3(8)
	WRITE(RLU,919) ASFYTDINV_OFF+16,
     *                       ASFYTDINV(ASFWCMP,1), 
     *                      'ASFYTDINV', 9, 1, DESCR_3(9)
	WRITE(RLU,920) ASFYTDINV_OFF+18,
     *                       CSMONYI8(ASFYTDINV(ASFPADU,1),12,MONEY_UNIT), 
     *                      'ASFYTDINV', 10, 1, DESCR_3(10)
	WRITE(RLU,919) ASFYTDINV_OFF+20,
     *                       ASFYTDINV(ASFPADP,1), 
     *                      'ASFYTDINV', 11, 1, DESCR_3(11)
	WRITE(RLU,920) ASFYTDINV_OFF+22,
     *                       CSMONYI8(ASFYTDINV(ASFADJU,1),12,MONEY_UNIT), 
     *                      'ASFYTDINV', 12, 1, DESCR_3(12)
	WRITE(RLU,919) ASFYTDINV_OFF+24,
     *                       ASFYTDINV(ASFADJP,1), 
     *                      'ASFYTDINV', 13, 1, DESCR_3(13)
	WRITE(RLU,919) ASFYTDINV_OFF+26,
     *                       ASFYTDINV(ASFSTR,1), 
     *                      'ASFYTDINV', 14, 1, DESCR_3(14)
	WRITE(RLU,919) ASFYTDINV_OFF+28,
     *                       ASFYTDINV(ASFEND,1), 
     *                      'ASFYTDINV', 15, 1, DESCR_3(15)
	WRITE(RLU,919) ASFYTDINV_OFF+29,
     *                       ASFYTDINV(ASFEND,2), 
     *                      'ASFYTDINV', 15, 2, DESCR_3(15)
	WRITE(RLU,920) ASFYTDINV_OFF+30,
     *                       CSMONY(ASFYTDINV(ASFBALU,1),12,MONEY_UNIT), 
     *                      'ASFYTDINV', 16, 1, DESCR_3(16)
	WRITE(RLU,919) ASFYTDINV_OFF+32,
     *                       ASFYTDINV(ASFBALP,1), 
     *                      'ASFYTDINV', 17, 1, DESCR_3(17)
	WRITE(RLU,920) ASFYTDINV_OFF+34,
     *                       CSMONYI8(ASFYTDINV(ASFDUEU,1),12,MONEY_UNIT), 
     *                      'ASFYTDINV', 18, 1, DESCR_3(18)
	WRITE(RLU,919) ASFYTDINV_OFF+36,
     *                       ASFYTDINV(ASFDUEP,1), 
     *                      'ASFYTDINV', 19, 1, DESCR_3(19)
	WRITE(RLU,919) ASFYTDINV_OFF+38,
     *                       ASFYTDINV(ASFWCNT,1), 
     *                      'ASFYTDINV', 20, 1, DESCR_3(20)
	WRITE(RLU,920) ASFYTDINV_OFF+40,
     *                       CSMONY(ASFYTDINV(ASFWAMT,1),12,MONEY_UNIT), 
     *                      'ASFYTDINV', 21, 1, DESCR_3(21)
	WRITE(RLU,920) ASFYTDINV_OFF+42,
     *                       CSMONYI8(ASFYTDINV(ASFBTWU,1),12,MONEY_UNIT), 
     *                      'ASFYTDINV', 22, 1, DESCR_3(22)
	WRITE(RLU,919) ASFYTDINV_OFF+44,
     *                       ASFYTDINV(ASFBTWC,1), 
     *                      'ASFYTDINV', 23, 1, DESCR_3(23)


	DO 180 I=1,GUTLEN
	      WRITE(RLU,901) ASFGUT_OFF+(I-1),
     *                       ASFGUT(I), 
     *                      'ASFGUT', I, DESCR(13)
180	CONTINUE
	      WRITE(RLU,901) ASFHWN_OFF, ASFHWN(1), 
     *                      'ASFHWN', 1, DESCR(14)
	      WRITE(RLU,906) ASFHWN_OFF+1,
     *                       CSMONY(ASFHWN(2),12,MONEY_UNIT), 
     *                      'ASFHWN', 2, DESCR(15)
	DO 190 I=1,ANUMDAY
	  DO J=1,MAXGAM
	    DO K=1,ASPELEN
	      WRITE(RLU,916) ASFSPE_OFF+(I-1)*MAXGAM*ASPELEN+(J-1)*ASPELEN+K-1,
     *                       CSMONY(ASFSPE(K,J,I),12,MONEY_UNIT), 
     *                      'ASFSPE', K, J, I, DESCR(16)
	    ENDDO
	  ENDDO
190	CONTINUE

	DO 200 I=1,ANUMDAY
	  DO J=1,AMISLEN
	      WRITE(RLU,918) ASFMIS_OFF+(I-1)*AMISLEN*2+(J-1)*2,
     *                       ASFMIS(J,1,I),
     *                       CSMONY(ASFMIS(J,2,I),12,MONEY_UNIT), 
     *                      'ASFMIS', J, I, DESCR(17)
	  ENDDO
200	CONTINUE
	WRITE(RLU,900) ASFDNM_OFF, ASFDNM, 'ASFDNM', DESCR(18)

	WRITE(RLU,917) DESCR(19)
	WRITE(RLU,921) ASFITINV_OFF, ASFITINV(ASFITSCNT), 'ASFITINV',
     *                 1, DESCR_5(1)
	WRITE(RLU,922) ASFITINV_OFF+1,
     *                 CSMONY(ASFITINV(ASFITSAMT),12,MONEY_UNIT),
     *                'ASFITINV', 2, DESCR_5(2)
	WRITE(RLU,921) ASFITINV_OFF+2, ASFITINV(ASFITVCNT), 'ASFITINV',
     *                 3, DESCR_5(3)
	WRITE(RLU,922) ASFITINV_OFF+3,
     *                 CSMONY(ASFITINV(ASFITVAMT),12,MONEY_UNIT),
     *                'ASFITINV', 4, DESCR_5(4)
	WRITE(RLU,921) ASFITINV_OFF+4, ASFITINV(ASFITBCNT), 'ASFITINV',
     *                 5, DESCR_5(5)
	WRITE(RLU,922) ASFITINV_OFF+5,
     *                 CSMONY(ASFITINV(ASFITBAMT),12,MONEY_UNIT),
     *                'ASFITINV', 6, DESCR_5(6)
	WRITE(RLU,922) ASFITINV_OFF+6,
     *                 CSMONY(ASFITINV(ASFITRAMT),12,MONEY_UNIT),
     *                'ASFITINV', 7, DESCR_5(7)
	WRITE(RLU,922) ASFITINV_OFF+7,
     *                 CSMONY(ASFITINV(ASFITSCM),12,MONEY_UNIT),
     *                'ASFITINV', 8, DESCR_5(8)
	WRITE(RLU,922) ASFITINV_OFF+8,
     *                 CSMONY(ASFITINV(ASFITPCM),12,MONEY_UNIT),
     *                'ASFITINV', 9, DESCR_5(9)
	WRITE(RLU,922) ASFITINV_OFF+9,
     *                 CSMONY(ASFITINV(ASFITADJ),12,MONEY_UNIT),
     *                'ASFITINV', 10, DESCR_5(10)
	WRITE(RLU,922) ASFITINV_OFF+10,
     *                 CSMONY(ASFITINV(ASFITDUE),12,MONEY_UNIT),
     *                'ASFITINV', 11, DESCR_5(11)
	
	DO 210 I=1,AITGAM
	  WRITE(RLU,923) ASFITGSAL_OFF+(I-1)*2,
     *                       ASFITGSAL(1,I),
     *                       CSMONY(ASFITGSAL(2,I),12,MONEY_UNIT), 
     *                      'ASFITGSAL', I, DESCR(20)
210	CONTINUE

	WRITE(RLU,900) ASFGVT_OFF, ASFGVT, 'ASFGVT', DESCR(21)
	WRITE(RLU,924) ASFGVTIM_OFF, DISTIM(ASFGVTIM), 'ASFGVTIM', DESCR(22)
	WRITE(RLU,925) ASFNCDC_OFF, ASFNCDC, 'ASFNCDC', DESCR(23)
	WRITE(RLU,925) ASFLCDC_OFF, ASFLCDC, 'ASFLCDC', DESCR(24)




2000	CONTINUE

        CALL CLOSEFIL(FDB)

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
912     FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I1,',',I2,')',1X,A29)
913	FORMAT(1X,I5,1X,A12,1X,15X,A6,'(',I2,',',I2,')',2X,A30)
914	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(*,',I2,')',3X,A30)
915	FORMAT(1X,I5,1X,A12,1X,15X,A7,8X,A30)
916	FORMAT(1X,I5,1X,A12,1X,12X,3X,A6,'(',I2,',',I2,',',I2,')',1X,A28)
917     FORMAT(1X,A30)
918	FORMAT(1X,I5,1X,I12,1X,A12,3X,A6,'(',I2,',*,',I2,')',1X,A29)
919	FORMAT(1X,I5,1X,I12,1X,12X,3X,A9,'(',I2,',',I2,')',1X,A28)
920	FORMAT(1X,I5,1X,A12,1X,15X,A9,'(',I2,',',I2,')',1X,A28)
921	FORMAT(1X,I5,1X,I12,1X,15X,A8,'(',I2,')',3X,A30)
922	FORMAT(1X,I5,1X,A12,1X,15X,A8,'(',I2,')',3X,A30)
923	FORMAT(1X,I5,1X,I12,1X,A12,3X,A9,'(*,',I2,')',1X,A29)
924	FORMAT(1X,I5,1X,4X,A8,1X,15X,A8,7X,A30)
925	FORMAT(1X,I5,1X,I12,1X,15X,A6,9X,A30)
926	FORMAT(1X,'Agent number ',I5,/)

C
	END

C PROGRAM POOLSEE
C  
C V12 14-FEB-2000 UXN Userinterface improved for VAKIO. BONBITMAP added for TLTO
C V11 02-DEC-1999 UXN Totals added for Viking.
C V10 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V09 04-DEC-1995 HXK Made changes for LTPOOL_GAMENR not having MAXTYP as
C                     array size!
C V08 21-MAR-1995 HXK Print share values to report,except Viking
C V07 11-NOV-1994 JXP Revised for lotto and vakio prognosis to screen display
C V06 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                     and Comm 1/93 update DEC Baseline
C V05 24-APR-1991 MP  CHANGED TO HANDLE 0 BONUS NUMBERS
C V04 01-AUG-1990 XXX RELEASED FOR VAX
C V03 01-NOV-1989 GCAN MODIFIED TO DO POOL STUFF IN A SUBROUTINE
C V02 15-JUL-1989 WS  RELEASED FOR SWEDEN
C V01 01-JUN-1988 XXX RELEASED FOR MICHIGAN
C
C ** Source - poolsee.for **
C
C POOLSEE.FOR
C
C DESCRIPTION  :
C
C    PROGRAM TO SEE HOW MANY SHARES WON FROM LOTTO POOLS FOR ANY
C    DIVISION.
C
C     REVISION HISTORY
C     2.0 WS 4/21
C
C SPECIAL NOTES:
C
C====================================================================
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM POOLSEE
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:POOLSEE.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
C
	INTEGER*4 OFF, BET, DRAW, PAGENM, ALLTOT, II, NEXT, BONUS
	INTEGER*4 TEMP, NUM, FLAG, I, ROW, NUMROWS, COPY, ST, GAM
	INTEGER*4 GAME_INDEX, EXT, GAME_TYPE
C
	CHARACTER*11 REPORTFILE/'POOLSEE.REP'/
	CHARACTER*1 ROW_RESULT
	INTEGER*4 I4_ROW_RESULT
	EQUIVALENCE (ROW_RESULT,I4_ROW_RESULT)
	INTEGER*4 CNV1X2(4) /'1111','XXXX','CCCC','2222'/
	LOGICAL SORT
	CHARACTER*20 WINBUF(10),BONBUF(5)
	DATA WINBUF/'Enter first   number',
     *	            'Enter second  number',
     *	            'Enter third   number',
     *	            'Enter forth   number',
     *	            'Enter fifth   number',
     *	            'Enter sixth   number',
     *	            'Enter seventh number',
     *	            'Enter eigth   number',
     *	            'Enter ninth   number',
     *	            'Enter tenth   number'/
C
	DATA BONBUF/'Enter first  bonus #',
     *	            'Enter second bonus #',
     *	            'Enter third  bonus #',
     *	            'Enter forth  bonus #',
     *	            'Enter fifth  bonus #'/

        CHARACTER*22  STRING(SPGNBR)  !
        DATA STRING/'Enter row  1:  ','Enter row  2:  ',
     *              'Enter row  3:  ','Enter row  4:  ',
     *              'Enter row  5:  ','Enter row  6:  ',
     *              'Enter row  7:  ','Enter row  8:  ',
     *              'Enter row  9:  ','Enter row 10:  ',
     *              'Enter row 11:  ','Enter row 12:  ',
     *              'Enter row 13:  ','Enter row 14:  ',
     *              'Enter row 15:  ','Enter row 16:  ',
     *              'Enter row 17:  '/
C
C
	INTEGER*4 BOARD(LTPOOL_MAXSHR) /LTPOOL_MAXSHR*0/  !NUMBERS WON
	INTEGER*4 BONNUM(5)
	INTEGER*4 BONUS_TAB(SEEBET) /SEEBET*0/ !BONUS NRS DRAWN
	INTEGER*4 BONUS_MASK(2) /2*0/
C
	INTEGER*4 DRWSHV(20)
	INTEGER*4 DIVISIONSHR(LTPOOL_MAXSHR)/LTPOOL_MAXSHR*0/ !SHARES
C                                          FOR ALL FILES
C                           FOR EVERY DIVISION AND HAS THE STRUCTURE
C                           THE SAME AS LTPOOLFLAG TABLE
	INTEGER*4 BONBITMAP(2)

	LOGICAL FSTPRG/.FALSE./

	COMMON SCFREC
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
	CALL COPYRITE
C
	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)

C
      TYPE *,IAM(),'--------------------- POOLSEE ---------------------'
	TYPE *,IAM(),'     This program will display pools statistics.'
	TYPE *,IAM(),' '
C
	CALL INPNUM('Enter game type ',GAME_TYPE,1,LTPOOL_MAXTYP,EXT)
	IF (EXT.LT.0) GOTO 9999
	CALL INPNUM('Enter game index ',GAME_INDEX,1,MAXIND,EXT)
	IF (EXT.LT.0) GOTO 9999
	GAM=LTPOOL_GAMENR(GAME_TYPE,GAME_INDEX)
	IF (GAM.LE.0) GOTO 9999
C
C
	IF (LTPOOLDRAW(GAM).LE.0) THEN
	   TYPE *,IAM(),'Invalid game ',GAM
	   GOTO 9999
	ENDIF
C
C
C OPEN REPORT FILE
C
	CALL ROPEN(REPORTFILE,7,ST)
	IF(ST.NE.0) THEN
	  TYPE *,IAM(),'POOLSEE.REP report file open error-',ST
	  CALL GPAUSE
	ENDIF
C
C GET NUMBER OF COPIES
C
	CALL INPNUM('Enter number of copies: ',COPY,0,20,EXT)
	IF(EXT.LT.0) GOTO 9999
C
C--------------------------------------------------------------------
C
	IF (GAME_TYPE.EQ.TSPT) THEN
	   TYPE *,IAM(),'Processing sports pools'
	   NUMROWS=LTPOOLBET(GAM)
10	   CONTINUE
C
	   DO 20, ROW=1,NUMROWS
	      CALL INPRES(STRING(ROW), BOARD(ROW), 0, .FALSE., EXT)
	      IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
20	   CONTINUE
	   WRITE (6,911) (CNV1X2(BOARD(I)),I=1,LTPOOLBET(GAM))
	   CALL INPYESNO('Are you sure these are correct results [Y/N]? ',FLAG)
	   IF(FLAG.NE.1) GOTO 10
	   GOTO 85
	ENDIF
C
C
C====================================================================
30	CONTINUE
	DO 35 I=1,LTPOOLBET(GAM)
	CALL INPNUM(WINBUF(I),NUM,1,LTPOOLNR(GAM),EXT)
	IF(EXT.LT.0) GOTO 30
	BOARD(I)=NUM
35	CONTINUE
C
C SORT NUMBERS
C
40	CONTINUE
	SORT=.FALSE.
	DO 45 I=1,LTPOOLBET(GAM)-1
	IF(BOARD(I).GT.BOARD(I+1)) THEN
	  TEMP=BOARD(I)
	  BOARD(I)=BOARD(I+1)
	  BOARD(I+1)=TEMP
	  SORT=.TRUE.
	ENDIF
45	CONTINUE
	IF(SORT) GOTO 40
C
C
50	CONTINUE
	BONUS_MASK(1)=0
	BONUS_MASK(2)=0
	IF (LTPOOL_BONUS(GAM).EQ.0) GOTO 81
	BONBITMAP(1) = 0
	BONBITMAP(2) = 0
C
C GET BONUS NUMBERS
C
	DO 55 I=1,LTPOOL_BONUS(GAM)
56	CONTINUE
	CALL INPNUM(BONBUF(I),BONUS,1,LTPOOLNR(GAM),EXT)
	IF(EXT.LT.0) GOTO 30
	BONNUM(I)=BONUS
	IF(TSBIT(BONBITMAP,BONUS)) THEN
	  TYPE*,'SORRY CANNOT HAVE SAME BONUS NUMBERS '
	  TYPE*,'PLEASE RE-ENTER        '
        ENDIF
	CALL BSET(BONBITMAP,BONUS)
55	CONTINUE
C
C SORT NUMBERS
C
60	CONTINUE
	SORT=.FALSE.
	DO 65 I=1,LTPOOL_BONUS(GAM)-1
	IF(BONNUM(I).GT.BONNUM(I+1)) THEN
	  TEMP=BONNUM(I)
	  BONNUM(I)=BONNUM(I+1)
	  BONNUM(I+1)=TEMP
	  SORT=.TRUE.
	ENDIF
65	CONTINUE
	IF(SORT) GOTO 60
C
	DO 80 NEXT=1,LTPOOL_BONUS(GAM)
C
	BONUS_TAB(NEXT)=BONNUM(NEXT)
	CALL BSET(BONUS_MASK(1),BONNUM(NEXT)-1)
	IF (EXT.LT.0) GOTO 9999
C
	DO 70 I=1,LTPOOLBET(GAM)
	  IF (BONNUM(NEXT).EQ.BOARD(I)) THEN
	    WRITE(5,930)
	    GOTO 30
	  ENDIF
70	CONTINUE
C
80	CONTINUE
81	CONTINUE
	WRITE(6,910) (BOARD(I),I=1,LTPOOLBET(GAM))
	IF (LTPOOL_BONUS(GAM).NE.0)
     *	   WRITE(6,920)    (BONUS_TAB(II),II=1,LTPOOL_BONUS(GAM))
	CALL INPYESNO('Are you sure these are correct results [Y/N]? ',FLAG)
	IF(FLAG.NE.1) GOTO 30
C
85	CONTINUE
        CALL POOLSEE2(GAME_TYPE,GAME_INDEX,BOARD,BONUS_MASK,
     *                DIVISIONSHR,ALLTOT,FSTPRG,BONUS_TAB,
     *                DRWSHV)

C
C
C  PRINT POOLSEE REPORT
C
	CALL TITLE('POOLSEE DRAWING RESULTS',' POOLSEE',1,7,PAGENM,
     !	              DAYCDC)
	DRAW=LTPOOLDRAW(GAM)
	WRITE(7,940) GAM,DRAW,GAME_TYPE,GAME_INDEX
	IF (GAME_TYPE.EQ.TLTO) THEN
	   WRITE(7,910) (BOARD(I),I=1,LTPOOLBET(GAM))
	ELSE
	   WRITE(7,911) (CNV1X2(BOARD(I)),I=1,LTPOOLBET(GAM))
	ENDIF
	IF (LTPOOL_BONUS(GAM).NE.0)
     *	       WRITE(7,920) (BONUS_TAB(II),II=1,LTPOOL_BONUS(GAM))
	WRITE (7,925)
     *	   'Total number of boards bet for draw ',DRAW,' =',ALLTOT
	BET=LTPOOLBET(GAM)
C
	DO 300, OFF=1,LTPOOLFLAGS(GAM)
	   IF (LTPOOLFLAG(OFF,GAM).NE.0) THEN
	      IF (MOD(OFF,2).NE.0) THEN
                    WRITE(7,950) BET-OFF/2,BET,DIVISIONSHR(OFF),
     *                           CSMONY(DRWSHV(OFF),11,VALUNIT)
              ELSE
   	            WRITE(7,951) BET-OFF/2,BET,DIVISIONSHR(OFF),
     *                           CSMONY(DRWSHV(OFF),11,VALUNIT)
	      ENDIF
	   ENDIF
300	CONTINUE
C
	CALL USRCLOS1(     7)
C
C SPOOL ALL COPIES
C
	CALL SPOOL(REPORTFILE,COPY,ST)
C
C COMMON EXIT
C
9999	CONTINUE
	CALL GSTOP(GEXIT_SUCCESS)
C
C FORMAT AREA
C
910	FORMAT(/,10X,'Winning lotto board: ',20(1X,I2.2))
911	FORMAT(/,10X,'Winning sport board: ',20(1X,A1))
920	FORMAT(10X,'Bonus number: ',20(1X,I2.2),/)
925	FORMAT(1H ,/,10X,A,I4,A,I8)
930	FORMAT('The bonus number cannot be the same as one of the',
     !	       ' draw numbers.  ')
940	FORMAT(/////////,10X,'Game no   =',I4,' draw no = ',I4,
     *	       ' Game type - ',I3,'     Game index - ',I3,///)
950	FORMAT(//,10X,'Match ',I2,'  of ',I2,' =',I7,8X,A11)
951	FORMAT(//,10X,'Match ',I2,'+ of ',I2,' =',I7,8X,A11)
	END

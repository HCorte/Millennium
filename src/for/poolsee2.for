C SUBROUTINE POOLSEE2
C
C V21 27-SEP-2000 OXK Search w/ partial Vakio results
C V20 31-JUL-2000 OXK Cleanup of Vakio GIND 1
C V19 30-MAY-2000 OXK Added ability to check additional results to TSPT
C V18 08-MAY-2000 UXN DIVSHR dimension fixed.
C V17 29-FEB-2000 OXK No question about postponed game asked for Sport
C v16 28-feb-2000 oxk vakio changes
C V15 26-JUL-1999 PXO Changed SWITCH so that 0 is prognosis, 1 is final
C V14 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V13 08-AUG-1995 HXK Fix for postponed draw for Vakio
C V12 07-APR-1995 HXK Fix for amount mapped to division
C V11 24-MAR-1995 HXK New rule - when jackpot amount promised:
C  i) if jackpot won at least the promised amount must be paid unless
C     the 95% rule applies.
C  2) if jackpot not won the difference between the jackpot as it would
C     have been if there had been a winner and the promised jackpot must
C     be rolled to the next round. 95% rule doe NOT apply in this case.
C V10 21-MAR-1995 HXK Print share values to report, except Viking
C V09 01-FEB-1995 HXK Fix for share value calculations
C V08 16-DEC-1994 JXP NO DISPLAY OF SHARE VALUES FOR VOKING
C V07 09-DEC-1994 JXP NO SHARE VALUES FOR VIKING 
C V06 11-NOV-1994 JXP Revised for lotto and vakio prognosis to screen display
C V05 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                     and Comm 1/93 update DEC Baseline
C V04 01-AUG-1990 XXX RELEASED FOR VAX
C V03 01-NOV-1989 GCAN MODIFED TO BE A SUBROUTINE
C V02 15-JUL-1989 WS   RELEASED FOR SWEDEN
C V01 01-JUN-1988 XXX  RELEASED FOR MICHIGAN
C
C    PROGRAM TO SEE HOW MANY SHARES WON FROM POOLS FOR ANY DIVISION.
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE POOLSEE2(GTYP,GIND,BOARD,BONUS_MASK,
     *	                    DIVISIONSHR,ALLTOT,FSTPRG,BONUS_TAB,
     *                      DIVISIONSHV)

	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:POOLSEE.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
C
C PARAMETERS
C
	INTEGER*4 GTYP, GIND
	INTEGER*4 BOARD(LTPOOL_MAXSHR)		!NUMBERS WON
	INTEGER*4 BONUS_MASK(2)
	INTEGER*4 DIVISIONSHR(LTPOOL_MAXSHR)	!SHARES
				    ! FOR ALL FILES
				    ! FOR EVERY DIVISION AND HAS THE STRUCTURE
				    ! THE SAME AS LTPOOLFLAG TABLE
	INTEGER*4 ALLTOT
	LOGICAL	  FSTPRG
	INTEGER*4 BONUS_TAB(SEEBET) ! BONUS NRS DRAWN
        INTEGER*4 DIVISIONSHV(20)   ! HARDCODED ALSO IN POOLSEE...
C
C LOCAL VARIABLES
C
	INTEGER*4 MX, MISS, OFF, DRAW, TOTAL, ST, FILE_INDEX

	INTEGER*4 GAM, CURRENT_FILE
C
	INTEGER*4 FDB(7)
C
	INTEGER*4 TOTALSHR(LTPOOL_MAXSHR)/LTPOOL_MAXSHR*0/ !SHARES
C                                        TOTAL (1 SET OF FILE ONLY)
	INTEGER*4 FILE_INDEX_TAB(4) /LPR,LP2,0,0/

        INTEGER*4 DRWSHV(8)	  ! MAX OF SPGDIV AND LTGDIV
        INTEGER*4 DIVSHR(8)       ! CHECK LTCALC, VAKDOCAL% IF YOU CHANGE THESE
        INTEGER*4 TMPPOL(SPGDIV)
cv20	INTEGER*4 TMPMOV
cv20	INTEGER*4 TMPASH(SPGDIV)
        REAL*8    TMPAPL
	REAL*8    TMPPOL2
 	INTEGER*4 SWITCH/0/, I, II, BET, GNUM
 	INTEGER*4 CNV1X2(4)  /'1111','XXXX','????','2222'/
	INTEGER*4 CDC, WEEK, YEAR

	INTEGER*4 YESNO, DUMMY
	LOGICAL*4 FOUND
	LOGICAL*4 FIRST/.TRUE./

        COMMON SCFREC
        COMMON /DSPCOM/ DSPREC
        COMMON /DLTCOM/ DLTREC
	COMMON /CURRFIL/ CURRENT_FILE
C
C	FOR PARTIALLY FILLED ROWS...
C
	INTEGER*4 BITCNT(0:7) /0,1,1,2,1,2,2,3/
	INTEGER*4 BITOR(3,0:7)
	DATA BITOR/0,0,0, 1,0,0, 2,0,0, 1,2,0, 4,0,0, 1,4,0 , 2,4,0, 1,2,4/
	INTEGER*4 P_BOARD(LTPOOL_MAXSHR)
	INTEGER*4 P_TOTAL, P_TOTAL2, P_TMP
	INTEGER*4 CURRENT_ROW, ROW
	INTEGER*4 CNT_INDEX(16)
	INTEGER*4 LOCAL_BET(16)
	LOGICAL*4 PARTIAL
C
C
C
C
C
	GAM=LTPOOL_GAMENR(GTYP,GIND)
	IF(GAM.LE.0.OR.LTPOOLDRAW(GAM).LE.0) THEN
	   TYPE *,IAM(),'Invalid game ',GAM
	   CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	CALL DEFLST(SORTQ,LSORT-QHEDSZ)      !INITIALIZE DATA STRUCTURE
	ALLTOT=0
	CURRENT_FILE=1               !PROCESSING 1 SET OF FILES
	FILE_INDEX=FILE_INDEX_TAB(CURRENT_FILE)
90	CONTINUE
	IF (CURRENT_FILE.EQ.1) THEN
	   TYPE *,IAM(), ' Processing pools currently in memory'
	ELSE
	   TYPE *,IAM(), 'Merging previous pool files'
	ENDIF
	TYPE *,IAM(),' '
	CALL OPENQW(2,SFNAMES(1,FILE_INDEX),4,0,0,ST)
	IF (ST.NE.0) THEN
	  CALL FILERR(SFNAMES(1,FILE_INDEX),1,ST,0)
	  CALL GPAUSE
	  GOTO 90
	ENDIF
	SEECURPAG=-1              !INVALIDATE MEMORY PAGE
C
	CALL IOQINIT(FDB,2,SEEPAGESIZE/64*256)
C
	TYPE *,IAM(),' '
	TYPE *,IAM(), ' **** PROCESSING POOLS FOR TOTAL BETS ****'
	CALL POOLTOT(FDB,TOTAL,CURRENT_FILE,GAM,SEEPAGE,SEEPAGESIZE)
	DRAW=LTPOOLDRAW(GAM)
	TYPE *,IAM(),
     1	  '------------------------------------------------------'
	TYPE *,IAM(),
     1	  'Total number of boards bet for draw ',DRAW,' =',TOTAL
	TYPE *,IAM(),
     1	  '------------------------------------------------------'
	ALLTOT=ALLTOT+TOTAL
C====================================================================
	TYPE *,IAM(),' '
	TYPE *,IAM(), ' **** Processing pools phase 1 ****'
C
	IF (GTYP.EQ.TSPT) THEN
	   PARTIAL = .FALSE.

	   DO I=1,LTPOOLBET(GAM)
	     P_BOARD(I) = BOARD(I)
	     IF (BOARD(I).EQ.3) THEN
		PARTIAL = .TRUE.
		P_BOARD(I) = 7
	     ENDIF
	   ENDDO

	   IF (PARTIAL) THEN
	     P_TOTAL  = 0
	     P_TOTAL2 = 0
	     TYPE *,IAM(), ' +++++++++++++++++++++++++++'
	     TYPE *,IAM(), ' ** Processing Vakio pools for partial results'
	     CALL FASTSET(1,CNT_INDEX,LTPOOLBET(GAM))
	     CURRENT_ROW=1
10	     CONTINUE
	     DO ROW=1,LTPOOLBET(GAM)     !GET NEXT SIMPLE BET
		LOCAL_BET(ROW)=BITOR(CNT_INDEX(ROW),P_BOARD(ROW))
	     ENDDO

	     DO 55, OFF=1,LTPOOLFLAGS(GAM)
	        IF (LTPOOLFLAG(OFF,GAM).EQ.0) GOTO 55
	        MISS=OFF/2
	        CALL SPTSHARE(FDB,MISS,LOCAL_BET,P_TMP,GAM)
		TOTALSHR(OFF) = TOTALSHR(OFF)+P_TMP
		GOTO 57     ! ONLY FOR 1ST DIV
55	     CONTINUE
57	     CONTINUE

C     FIND NEXT SIMPLE BET
	     CNT_INDEX(CURRENT_ROW)=CNT_INDEX(CURRENT_ROW)+1
	     IF (CNT_INDEX(CURRENT_ROW).GT.BITCNT(P_BOARD(CURRENT_ROW))) THEN
50	CONTINUE
		CNT_INDEX(CURRENT_ROW)=1
		CURRENT_ROW=CURRENT_ROW+1
		IF (CURRENT_ROW.GT.LTPOOLBET(GAM)) GOTO 60	!ALL DONE
		IF (CNT_INDEX(CURRENT_ROW)+1.GT.BITCNT(P_BOARD(CURRENT_ROW)))
     *		  GOTO 50		      !NO MORE COMBINATIONS IN THIS ROW
		CNT_INDEX(CURRENT_ROW)=CNT_INDEX(CURRENT_ROW)+1 !NEXT ROW
		CURRENT_ROW=1
	     ENDIF
           GOTO 10
60	CONTINUE
	     DO 65, OFF=1,LTPOOLFLAGS(GAM)
	       IF (LTPOOLFLAG(OFF,GAM).EQ.0) GOTO 65
	       MISS=OFF/2
	       TYPE *,IAM(), ' ** Vakio pools for match ', LTPOOLBET(GAM)-MISS
	       TYPE *,IAM(), TOTALSHR(OFF),' shares in pool '
	       TYPE *,IAM(), ' +++++++++++++++++++++++++++'
	       GOTO 67     ! ONLY FOR 1ST DIV
65	     CONTINUE
	       TYPE*,IAM()
	       TYPE*,IAM(),
     *            'Share values are only indicative with partial results!'
	       TYPE*,IAM()
67	     CONTINUE
	     RETURN
	   ENDIF

	   FOUND = .FALSE.
	   DO 100, OFF=1,LTPOOLFLAGS(GAM)
	      IF (LTPOOLFLAG(OFF,GAM).EQ.0) GOTO 100
	      MISS=OFF/2
	      TYPE *,IAM(), ' +++++++++++++++++++++++++++'
	      TYPE *,IAM(), ' ** Processing Vakio pools for match ',
     *	             LTPOOLBET(GAM)-MISS
	      CALL SPTSHARE(FDB,MISS,BOARD,TOTALSHR(OFF),GAM)
	      TYPE *,IAM(), TOTALSHR(OFF),' shares in pool '
	      TYPE *,IAM(), ' +++++++++++++++++++++++++++'
	      IF (TOTALSHR(OFF).GT.0) FOUND=.TRUE.
	      IF(FSTPRG) GOTO 140
100	   CONTINUE

110	   CONTINUE
	   IF (FOUND) GOTO 120
	   IF (FIRST) THEN
	      FIRST = .FALSE.
	      TYPE*,IAM(),'There were no winners in defined winning divisions.'
	      TYPE*,IAM(),'To get correct shares define more divisions by:'
	      TYPE*,IAM(),'1. restore backup, 2. define divs & 3. reprocess day'
	      TYPE*,IAM(),'Contact on-call support for further instructions.'
	   ENDIF
	   MISS = MISS+1
	   IF (MISS.GT.LTPOOLBET(GAM) .OR.
     *	       MISS.GT.(LTPOOL_MAXSHR/2) ) GOTO 120
	   CALL PRMYESNO('Check next match [Y/N] ?',YESNO)
	   IF (YESNO.NE.1) GOTO 120
	   TYPE *,IAM(), ' +++++++++++++++++++++++++++'
	   TYPE *,IAM(), ' ** Processing Vakio pools for match ',
     *	             LTPOOLBET(GAM)-MISS
	   CALL SPTSHARE(FDB,MISS,BOARD,DUMMY,GAM)
	   TYPE *,IAM(), DUMMY,' shares in pool '
	   TYPE *,IAM(), ' +++++++++++++++++++++++++++'
	   IF (DUMMY.GT.0) FOUND=.TRUE.
	   GOTO 110

120	   CONTINUE
	   GOTO 140
	ENDIF
C
C     PROCESS LOTTO
C
	CALL POOLSHR(FDB,BOARD,BONUS_MASK,TOTALSHR,GAM)
	TYPE *,IAM(), '--------------------------------------------'
C
	MX=5                   !CHECK UP TO MATCH ALL -2 (WITH BONUS)
	IF (LTPOOLFLAGS(GAM).LT.MX) MX=LTPOOLFLAGS(GAM)
	DO 130, OFF=1,MX
	   IF (LTPOOLFLAG(OFF,GAM).NE.0) THEN
	      IF (MOD(OFF,2).NE.0) THEN
	 WRITE(6,950) LTPOOLBET(GAM)-OFF/2,LTPOOLBET(GAM),TOTALSHR(OFF)
	      ELSE
	 WRITE(6,951) LTPOOLBET(GAM)-OFF/2,LTPOOLBET(GAM),TOTALSHR(OFF)
	      ENDIF
	   ENDIF
	   IF(FSTPRG) GOTO 140
130	CONTINUE
C
C
	TYPE *,IAM(), '--------------------------------------------'
C====================================================================
	TYPE *,IAM(), ' '
	IF (LTPOOLFLAG(6,GAM).NE.0.OR.LTPOOLFLAG(7,GAM).NE.0) THEN
	  IF (LTPOOLFLAG(7,GAM).NE.0)
     *	TYPE *,IAM(), ' ** PROCESSING LOTTO POOLS FOR MATCH ',
     *   LTPOOLBET(GAM)-3,'  **'
	  IF (LTPOOLFLAG(6,GAM).NE.0)
     *	TYPE *,IAM(), ' ** PROCESSING LOTTO POOLS FOR MATCH ',
     *	      LTPOOLBET(GAM)-3 ,'+ **'
	CALL POOLSHR3(FDB,BOARD,BONUS_MASK,TOTALSHR(7),TOTALSHR(6),GAM)
	  TYPE *,IAM(), '-----------------------------------------------'
	  IF (LTPOOLFLAG(7,GAM).NE.0)
     *	   TYPE*,IAM(), TOTALSHR(7),' match ',LTPOOLBET(GAM)-3,' boards'
	  IF (LTPOOLFLAG(6,GAM).NE.0)
     *	   TYPE*,IAM(), TOTALSHR(6),' match ',LTPOOLBET(GAM)-3,'+ boards'
	  TYPE *,IAM(), '-----------------------------------------------'
	  TYPE *,IAM(), ' '
	ENDIF
C
140	CONTINUE
C
        DO 150, OFF=1,LTPOOLFLAGS(GAM)
           DIVISIONSHR(OFF)=DIVISIONSHR(OFF)+TOTALSHR(OFF)
150     CONTINUE

	CALL CLOSEQFIL(FDB)
	CURRENT_FILE=CURRENT_FILE+1
	FILE_INDEX=FILE_INDEX_TAB(CURRENT_FILE)
	IF (CURRENT_FILE.EQ.2.AND.CURRENT_FILE.LE.LTPOOLFILES(GAM))
     *	   GOTO 90


        I=1
        DO OFF=1,LTPOOLFLAGS(GAM)
           IF (LTPOOLFLAG(OFF,GAM).NE.0) THEN
              IF (MOD(OFF,2).NE.0) THEN
                 DIVSHR(I)=DIVISIONSHR(OFF)
              ELSE
                 DIVSHR(I)=DIVISIONSHR(OFF)
              ENDIF
              I=I+1
           ENDIF
        ENDDO

	GNUM=SCFGTN(GTYP,GIND)
        IF(GTYP.EQ.TLTO) THEN
           CALL LTDOCALC(GNUM,DRAW,DIVSHR,TMPPOL2,TMPAPL,SWITCH,ST)

           DO I = 1, DLTDIV
                DRWSHV(I)=DLTSHV(I,1)
           END DO
        ELSEIF(GTYP.EQ.TSPT) THEN
            CALL SPDOCALC(GNUM,DRAW,DIVSHR,TMPPOL,.FALSE.)
            DO I = 1, 8
                DRWSHV(I)=DSPSHV(I)
            END DO
        ENDIF
	GNUM=SCFGTN(GTYP,GIND)

	IF(GTYP.EQ.TLTO) CDC = LTOESD(GIND)
	IF(GTYP.EQ.TSPT) CDC = SPTESD(GIND)
	CALL FIGWEK(CDC, WEEK, YEAR)

        WRITE(6,9000) (GLNAMES(I,GNUM),I=1,4), WEEK, YEAR
        WRITE(6,9002) LTPOOL_TOT(GAM), DRAW
        WRITE(6,9004)

	IF (GTYP.EQ.TLTO) THEN
           WRITE(6,9006) (BOARD(I),I=1,LTPOOLBET(GAM))
        ELSE
           WRITE(6,9007) (CNV1X2(BOARD(I)),I=1,LTPOOLBET(GAM))
        ENDIF
        IF (LTPOOL_BONUS(GAM).NE.0)
     *         WRITE(6,9008) (BONUS_TAB(II),II=1,LTPOOL_BONUS(GAM))
        WRITE(6,9010)
        WRITE(6,9011)

C
	I=1
	BET=LTPOOLBET(GAM)
	DO OFF=1,LTPOOLFLAGS(GAM)
           IF (LTPOOLFLAG(OFF,GAM).NE.0) THEN
              IF (MOD(OFF,2).NE.0) THEN
	            WRITE(6,9012) BET-OFF/2,DIVISIONSHR(OFF),
     *                       CSMONY(DRWSHV(I),11,VALUNIT)
              ELSE
      	            WRITE(6,9013) BET-OFF/2,DIVISIONSHR(OFF),
     *                       CSMONY(DRWSHV(I),11,VALUNIT)
              ENDIF
              I=I+1
           ENDIF
	END DO
        WRITE(6,9099)
        WRITE(6,9099)

        I=1
        DO OFF=1,LTPOOLFLAGS(GAM)
           IF (LTPOOLFLAG(OFF,GAM).NE.0) THEN
              IF (MOD(OFF,2).NE.0) THEN
                 DIVISIONSHV(OFF)=DRWSHV(I)
              ELSE
                 DIVISIONSHV(OFF)=DRWSHV(I)
              ENDIF
              I=I+1
           ENDIF
        ENDDO

	RETURN
C
C
950	FORMAT(//,10X,'Match ',I2,'  of ',I2,' =',I7)
951	FORMAT(//,10X,'Match ',I2,'+ of ',I2,' =',I7)

9000    FORMAT(/' Fast win prognosis   ',4A4,4X,'Round',2X,I2,'/',I4)
9002    FORMAT(/' Panels included      ',I10,10x'Draw no:',I4)
9004    FORMAT(/' Winning numbers:')
9006    FORMAT(/,5X,' Actual Numbers: ',10(I2.2,1X))
9007    FORMAT(/,5X,' Actual Numbers: ',20(A1,1X))
9008    FORMAT(/,5X,' Bonus Numbers:  ',5(I2.2,1X))
9010    FORMAT(/,' Prognosis:',10X,'Division',5X,'#wins',14X)
9011    FORMAT(20X,40('-'))
9012    FORMAT(20X,I2,10X,I7,6X,A11)
9013    FORMAT(20X,I2,'+',9X,I7,6X,A11)

9020    FORMAT(/,' Prognosis:',10X,'Division',5X,'#wins')
9022    FORMAT(20X,I2,10X,I7)
9023    FORMAT(20X,I2,'+',9X,I7)

9099    FORMAT(1X,79(' '))
 
	END

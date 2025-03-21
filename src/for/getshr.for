C
C SUBROUTINE GETSHR
C $Log:   GXAFXT:[GOLS]GETSHR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:22:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:28:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - poolshr.for **
C
C
C GETSHR(FDB,BET,MAXNR,WIN,TOTALSHR,IND1,IND2,GAM) ;GET SHARES
C     IN-FDB-FILE DESCRIPTOR BLOCK OF LOTTO POOL FILE.
C     IN-WIN-WINNING COMBINATION I*4 TABLE OF WINNING NUMBERS
C                (NOT ORDERED)
C     IND2,IND1 - TABLE AREA TO BE USED FOR INDEXING
C       BET - NR OF NRS BET   (6 FOR 6 OF 49)
C       MAXNR - MAXIMUM NR OFNUMBERS TO CHOSE FROM (49 FOR 6 OF 49)
C     OUT- TOTALSHR - TABLE WITH SHARES, STRUCTURE AS LTPOOLFLAG
C
C--------------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETSHR(FDB,BET,MAXNR,WIN,BONUS_MASK,TOTALSHR,IND1,
     *	            IND2,GAM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:POOLSEE.DEF'
C
	INTEGER*4 DUMMY, ST, OFFSET, OFF2, BONUS, OFF1, NEXT
	INTEGER*4 OFF, NDRAWN, MISSED, HIT, FASTBIN, GAM, MAXNR, BET
C
	INTEGER*4 FDB(7)
	INTEGER*4 IND2(BET-2,*)   !INDEXES MATCH-2 OUT OF MATCH
	INTEGER*4 IND1(BET-1,*)   !INDEXES MATCH-1 OUT OF MATCH
	INTEGER*4 MISSIND(2,SEEMAX2)  ! 2 OUT OF NOTDRAWN NOT WON
	INTEGER*4 BOARD(SEEBET)     !SORTED TABLE WITH OFFSETS
	INTEGER*4 WIN(*)        !WINNING NUMBERS
	INTEGER*4 BONUS_MASK(*)
	INTEGER*4 NOWIN_NUM(SEEMXNR)      !NOT WINNING NUMBERS
	INTEGER*4 TAB(SEEMXNR)
	INTEGER*4 TOTALSHR(*)       !TOTAL SHARES
	INTEGER*4 OVRSHR(20)/20*0/     !OVERFLOW SHARES, SAME STRUCTURE AS
C                                  LTPOOLFLAG AND TOTALSHR
C
	HIT = FASTBIN(BET,BET-2)
	MISSED = FASTBIN(MAXNR-BET,2)
C
C     DEFINE TABLE OF WINNERS AND NOT WINNERS
C
	NDRAWN=MAXNR-BET      !ACTUAL # OF NOT DRAWN NUMBERS
C
	DO 20, OFF=1,LTPOOLFLAGS(GAM)
	  TOTALSHR(OFF)=0
20	CONTINUE
C
	DO 30, OFF=1,MAXNR
	  TAB(OFF)=OFF
30	CONTINUE
C
	DO 40, OFF=1,BET
	  TAB(WIN(OFF))=0
40	CONTINUE
C
	NEXT=1
C
	DO 50, OFF=1,MAXNR
	  IF (TAB(OFF).EQ.0) GOTO 50
	  NOWIN_NUM(NEXT)=TAB(OFF)
	  NEXT=NEXT+1
50	CONTINUE
C
C
	CALL COMB(IND2,IND1,MISSIND,NDRAWN,BET)   ! GET INDEXES
C
	IF (LTPOOLFLAG(5,GAM).NE.0.OR.LTPOOLFLAG(4,GAM).NE.0) THEN
C
	  DO 80, OFF=1,HIT
	  DO 80, OFF1=1,MISSED
C
	    BONUS=0
C       GET NEW COMBINATION
C
	    DO 60, OFF2=1,BET-2
	      TAB(OFF2)=WIN(IND2(OFF2,OFF))
60	    CONTINUE
	    DO 70, OFF2=BET,BET+1
	      TAB(OFF2)=NOWIN_NUM(MISSIND(OFF2-BET+1,OFF1))
	      IF (TSBIT(BONUS_MASK(1),TAB(OFF2)-1).AND.
     *	          LTPOOLFLAG(4,GAM).NE.0)
     *	          BONUS=-1
70	    CONTINUE
C
C       SORT COMBINATION
C
	    CALL SRT(TAB,BOARD,BET)
C
	    CALL CMBOFF(BOARD,OFFSET,BET)
	    IF (BONUS.NE.0) OFFSET=-OFFSET
	    CALL ABL(OFFSET,SORTQ,ST)
	    IF (ST.NE.0) THEN
	       CALL CMBSORT(FDB,TOTALSHR(5),TOTALSHR(4),GAM)
	       CALL ABL(OFFSET,SORTQ,ST)
	    ENDIF
C
80	  CONTINUE
C
	  CALL CMBSORT(FDB,TOTALSHR(5),TOTALSHR(4),GAM)
	  IF(LTPOOLFLAG(4,GAM).EQ.0)TOTALSHR(5)=TOTALSHR(5)+TOTALSHR(4)
	  IF (LTPOOLFLAG(4,GAM).EQ.0) TOTALSHR(4)=0
	  IF (LTPOOLFLAG(5,GAM).EQ.0) TOTALSHR(5)=0
	ENDIF
C
C     MATCH ALL-1 AND MATCH ALL -1 + BONUS CALCULATION
C
	IF (LTPOOLFLAG(3,GAM).NE.0.OR.LTPOOLFLAG(2,GAM).NE.0) THEN
C
	  DO 110, OFF=1,MAXNR
	    TAB(OFF)=OFF
110	  CONTINUE
C
	  DO 120, OFF=1,BET
	    TAB(WIN(OFF))=0
120	  CONTINUE
C
	  NEXT=1
C
	  DO 130, OFF=1,MAXNR
	    IF (TAB(OFF).EQ.0) GOTO 130
	    NOWIN_NUM(NEXT)=TAB(OFF)
	    NEXT=NEXT+1
130	   CONTINUE
C
	  DO 150, OFF=1,BET
	  DO 150, OFF1=1,NDRAWN
C
C       GET NEW COMBINATION
C
	    BONUS=0
	    DO 140, OFF2=1,BET-1
	      TAB(OFF2)=WIN(IND1(OFF2,OFF))
140	    CONTINUE
	    TAB(BET+1)=NOWIN_NUM(OFF1)
	    IF (TSBIT(BONUS_MASK(1),TAB(BET+1)-1).AND.
     *	             LTPOOLFLAG(2,GAM).NE.0)      BONUS=-1
C
C         SORT COMBINATION
C
	    CALL SRT1(TAB,BOARD,BET)
	    CALL CMBOFF(BOARD,OFFSET,BET)
	    IF (BONUS.NE.0) OFFSET=-OFFSET
C***      TYPE *,IAM(),'Board ',BOARD,' offset ',OFFSET
	    CALL ABL(OFFSET,SORTQ,ST)
	    IF (ST.NE.0) THEN
	      CALL CMBSORT(FDB,TOTALSHR(3),TOTALSHR(2),GAM)
	      CALL ABL(OFFSET,SORTQ,ST)
	    ENDIF
150	  CONTINUE
	  CALL CMBSORT(FDB,TOTALSHR(3),TOTALSHR(2),GAM)
	  IF(LTPOOLFLAG(2,GAM).EQ.0)TOTALSHR(3)=TOTALSHR(3)+TOTALSHR(2)
	  IF (LTPOOLFLAG(2,GAM).EQ.0) TOTALSHR(2)=0
	  IF (LTPOOLFLAG(3,GAM).EQ.0) TOTALSHR(3)=0
	ENDIF
C--------------------------------------------------------------------
C   MATCH BET CALCULATION
C
	IF (LTPOOLFLAG(1,GAM).NE.0) THEN
C
	  CALL CMBOFF(WIN,OFFSET,BET)
	  CALL ABL(OFFSET,SORTQ,ST)
	  CALL CMBSORT(FDB,TOTALSHR(1),DUMMY,GAM)
C***    TYPE *,IAM(),'TOT ',(TOTALSHR(I),I=1,7)
	ENDIF
C
	CALL MATOVR(FDB,WIN,BONUS_MASK,OVRSHR,GAM,BET)!# OVERFLOW MTCH
C***  TYPE *,IAM(),'OVRSHR ',OVRSHR
	DO 160, OFF=1,LTPOOLFLAGS(GAM)
	   TOTALSHR(OFF)=TOTALSHR(OFF)+OVRSHR(OFF)
	   IF (LTPOOLFLAG(OFF,GAM).EQ.0) TOTALSHR(OFF)=0
160	CONTINUE
	RETURN
	END

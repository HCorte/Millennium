C
C SUBROUTINE MATOVR
C $Log:   GXAFXT:[GOLS]MATOVR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:00:48   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:58:44   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - poolshr.for **
C
C====================================================================
C     MATCHOVR(WIN,OVRSHR,GAM)
C     IN-WIN-NUMBERS DRAWN
C        GAM - GAME #
C     OUT- OVRSHR     - TOTAL SHARES FOR OVERFLOWS
C                       STRUCTURE AS TOTALSHR AND LTPOOLFLAG
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MATOVR(FDB,WIN,BONUS_MASK,OVRSHR,GAM,BET)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INTEGER*4 OFFSET,TIMES, OFF1, LENGTH, FILE_INDEX, TIMESB, OFF
	INTEGER*4 ST, RECN, OVERFLOW_BLOK, T0, T1, T2, CURRENT_FILE
	INTEGER*4 BET, GAM, ACT_SECT
	INTEGER*4 BOARD(20)
	INTEGER*4 FDB(*)
	INTEGER*4 OVRSHR(*)
	INTEGER*4 TEMP(2),TEMP1(2)
	INTEGER*4 WINMSK(2),BONUS_MASK(*)
	INTEGER*4 WIN(*)
	INTEGER*4 PFDB(7)            !FDB BLOCK FOR WRITE
	INTEGER*4 OVERFLOWS(2,MAXOVR)!TABLE TO READ OVERFLOWS FROM FILE
	COMMON /CURRFIL/ CURRENT_FILE
C
	WINMSK(1)=0
	WINMSK(2)=0
	CALL MAP(WINMSK,8,WIN,BET)
	CALL FASTSET(0,OVRSHR,LTPOOLFLAGS(GAM))
C
	T2=BET-2
	T1=BET-1
	T0=BET
C
	CALL FASTMOV(LTOVR(1,1,GAM),OVERFLOWS,MAXOVR*2)
	OVERFLOW_BLOK=OVRBLK
C
	IF (CURRENT_FILE.NE.1) THEN
	   ACT_SECT=LTOSEC*256/SECSIZE
C***	   RECN=LTOSEC*LTNUMPAG+1+(GAM-1)*MAXOVR*8/256
	   RECN=ACT_SECT*LTNUMPAG+1+(GAM-1)*MAXOVR*8/SECSIZE
	   CALL READQIO(FDB,RECN,OVERFLOWS,MAXOVR*8,ST)
	   IF (ST.NE.0) THEN
	      CALL FILERR(SFNAMES(1,LP2),2,ST,RECN)
	      RETURN
	   ENDIF
C***	   RECN=LTOSEC*LTNUMPAG+1+LTNUMGAMES*MAXOVR*8/256
	   RECN=ACT_SECT*LTNUMPAG+1+LTNUMGAMES*MAXOVR*8/SECSIZE
	   CALL READQIO(FDB,RECN,OVERFLOW_BLOK,4,ST)
	   IF (ST.NE.0) THEN
	      CALL FILERR(SFNAMES(1,LP2),2,ST,RECN)
	      RETURN
	   ENDIF
	ENDIF
C
	DO 100, OFF=1,MAXOVR-1
	  IF (OVERFLOWS(2,OFF).EQ.0) GOTO 200
	  TEMP(1)=0
	  TEMP(2)=0
	  OFFSET=OVERFLOWS(1,OFF)
	  CALL OFFCMB(BOARD,OFFSET,BET)
	  CALL MAP(TEMP,8,BOARD,BET)
	  CALL BITAND(WINMSK,TEMP,8,TEMP1)
	  CALL BITCNT(TEMP1,8,TIMES)
	  IF (TIMES.LT.T2) GOTO 100
	  CALL BITAND(BONUS_MASK,TEMP,8,TEMP1)
	  CALL BITCNT(TEMP1,8,TIMESB)
	  IF (TIMES.EQ.T2) THEN
	    IF (TIMESB.EQ.0.OR.LTPOOLFLAG(4,GAM).EQ.0) THEN
	      OVRSHR(5)=OVRSHR(5)+OVERFLOWS(2,OFF)
	    ELSE
	      OVRSHR(4)=OVRSHR(4)+OVERFLOWS(2,OFF)
	    ENDIF
	  ELSEIF (TIMES.EQ.T1) THEN
	    IF (TIMESB.EQ.0.OR.LTPOOLFLAG(2,GAM).EQ.0) THEN
	      OVRSHR(3)=OVRSHR(3)+OVERFLOWS(2,OFF)
	    ELSE
	      OVRSHR(2)=OVRSHR(2)+OVERFLOWS(2,OFF)
	    ENDIF
	  ELSEIF (TIMES.EQ.T0) THEN
	    OVRSHR(1)=OVRSHR(1)+OVERFLOWS(2,OFF)
	  ENDIF
100	CONTINUE
C
200	CONTINUE
	IF (OVERFLOW_BLOK.EQ.0) RETURN
C
210	CONTINUE
	IF (CURRENT_FILE.NE.2) THEN
	  CALL OPENW(1,SFNAMES(1,LO1),4,0,0,ST) !OPEN POOL OVERFLOW FILE
	  FILE_INDEX=LO1
	ELSE
	  CALL OPENW(1,SFNAMES(1,LO2),4,0,0,ST) !OPEN POOL OVERFLOW FILE
	  FILE_INDEX=LO2
	ENDIF
	IF (ST.NE.0) THEN
	  CALL FILERR(SFNAMES(1,FILE_INDEX),1,ST,0)
	  TYPE *,IAM(),' correct the problem and continue '
	  CALL GPAUSE
	  GOTO 210
	ENDIF
	LENGTH=MAXOVR*8/256
	CALL IOINIT(PFDB,1,LENGTH*256)     !INITIALISE FOR I/O
C
C     READ FROM POOLOVR.FIL      ;READ OVERFLOWS FROM FILE
C
	DO 230, OFF=1,OVERFLOW_BLOK
	  CALL READW(PFDB,OFF,OVERFLOWS(1,1),ST)
	    IF (ST.NE.0) THEN
	  CALL FILERR(SFNAMES(1,FILE_INDEX),2,ST,OFF)
	      CALL GPAUSE
	    ENDIF
	  IF (OVERFLOWS(1,MAXOVR).NE.GAM) GOTO 230
	  DO 220, OFF1=1,MAXOVR-1
	    IF (OVERFLOWS(2,OFF1).EQ.0) GOTO 230
	    OFFSET=OVERFLOWS(1,OFF1)
	    CALL OFFCMB(BOARD,OFFSET,BET)
	    TEMP(1)=0
	    TEMP(2)=0
	    CALL MAP(TEMP,8,BOARD,BET)
	    CALL BITAND(WINMSK,TEMP,8,TEMP1)
	    CALL BITCNT(TEMP1,8,TIMES)
	    IF (TIMES.LT.T2) GOTO 220
	    CALL BITAND(BONUS_MASK,TEMP,8,TEMP1)
	    CALL BITCNT(TEMP1,8,TIMESB)
	    IF (TIMES.EQ.T2) THEN
	      IF (TIMESB.EQ.0.OR.LTPOOLFLAG(4,GAM).EQ.0) THEN
	        OVRSHR(5)=OVRSHR(5)+OVERFLOWS(2,OFF1)
	      ELSE
	        OVRSHR(4)=OVRSHR(4)+OVERFLOWS(2,OFF1)
	      ENDIF
	    ELSEIF (TIMES.EQ.T1) THEN
	      IF (TIMESB.EQ.0.OR.LTPOOLFLAG(2,GAM).EQ.0) THEN
	        OVRSHR(3)=OVRSHR(3)+OVERFLOWS(2,OFF1)
	      ELSE
	        OVRSHR(2)=OVRSHR(2)+OVERFLOWS(2,OFF1)
	      ENDIF
	    ELSEIF (TIMES.EQ.T0) THEN
	      OVRSHR(1)=OVRSHR(1)+OVERFLOWS(2,OFF1)
	    ENDIF
220	  CONTINUE
230	CONTINUE
C
C     CLEAR NOT DEFINED SHARES
C
	DO 240, OFF=1,LTPOOLFLAGS(GAM)
	   IF (LTPOOLFLAG(OFF,GAM).EQ.0) OVRSHR(OFF)=0
240	CONTINUE
	CALL CLOSEFIL(PFDB)
	RETURN
	END

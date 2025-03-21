C
C SUBROUTINE SPTOVR
C $Log:   GXAFXT:[GOLS]SPTOVR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:16:02   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   08 Aug 1995 12:28:58   HXK
C  Removed TYPE statements (RXK)
C  
C     Rev 1.0   21 Jan 1993 17:41:58   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - sptshare.for **
C
C
C     FIND OVERFLOWS
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SPTOVR(FDB,MISS,WIN,OVR,GAM,BET)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 OFF1, LAST, LENGTH, F_INDEX, ROW, OFF, ST
	INTEGER*4 RECN, OVERFLOW_BLOK, NUMROWS, CURRENT_FILE
	INTEGER*4 BET, GAM, OVR, MISS, ACT_SECT
	INTEGER*4 OFFSET,TIMES
	INTEGER*4 BOARD(20)                !SHOULD BE SEEBET
	INTEGER*4 WIN(*)
	INTEGER*4 FDB(*)
	INTEGER*4 PFDB(7)            !FDB BLOCK FOR WRITE
	INTEGER*4 OVERFLOWS(2,MAXOVR)!TABLE TO READ OVERFLOWS FROM FILE
	COMMON /CURRFIL/ CURRENT_FILE
C
	NUMROWS=LTPOOLBET(GAM)
C
	OVR=0
C
C     TAKE OVERFLOWS FROM MEMORY FIRST
C
	CALL FASTMOV(LTOVR(1,1,GAM),OVERFLOWS,MAXOVR*2)
	OVERFLOW_BLOK=OVRBLK
C
	IF (CURRENT_FILE.NE.1) THEN    !IF READING ANOTHER SET
C                                     OF FILES, READ FROM DISK
C
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
	   CALL READIO(FDB,RECN,OVERFLOW_BLOK,4,ST)
	   IF (ST.NE.0) THEN
	      CALL FILERR(SFNAMES(1,LP2),2,ST,RECN)
	      RETURN
	   ENDIF
	ENDIF
C
C     CHECK OVERFLOWS IN MEMORY NOW
C
	DO 100, OFF=1,MAXOVR-1
	  IF (OVERFLOWS(2,OFF).EQ.0) GOTO 200
	  OFFSET=OVERFLOWS(1,OFF)
C       CALL OFFCMB(BOARD,OFFSET,BET)    ;GET COMBINATIONS
	  CALL OFFSPT(BOARD,BET,OFFSET)
	  TIMES=0
	  DO 90, ROW=1,NUMROWS            !AND FIND # OF MATCHES
	     IF (WIN(ROW).EQ.BOARD(ROW)) TIMES=TIMES+1
90	  CONTINUE
C
	  IF (TIMES.NE.BET-MISS) GOTO 100  !IF WRONG # OF MATCHES SKIP
C
	  OVR=OVR+OVERFLOWS(2,OFF)
100	CONTINUE
C
C
200	CONTINUE
C
C     CHECK FOR OVERFLOWS FLUSHED TO FILE
C
	IF (OVERFLOW_BLOK.EQ.0) RETURN
C
210	CONTINUE
	IF (CURRENT_FILE.EQ.1) THEN   !OPEN RIGHT OVERFLOW FILE
	   CALL OPENW(1,SFNAMES(1,LO1),4,0,0,ST) !OPEN POOL OVR FIL
	   F_INDEX=LO1
	ELSE
	  CALL OPENW(1,SFNAMES(1,LO2),4,0,0,ST) !OPEN POOL OVERFLOW FIL
	  F_INDEX=LO2
	ENDIF
	IF (ST.NE.0) THEN
	  CALL FILERR(SFNAMES(1,F_INDEX),1,ST,0)
	  TYPE *,' correct the problem and continue '
	  CALL GPAUSE
	  GOTO 210
	ENDIF
C
C     READ IT NOW
C
	LENGTH=MAXOVR*8/256
	CALL IOINIT(PFDB,1,LENGTH*256)     !INITIALISE FOR I/O
C
C     READ FROM POOLOVR.FIL      ;READ OVERFLOWS FROM FILE
C
	LAST=OVERFLOW_BLOK
	DO 240, OFF=1,LAST
	CALL READW(PFDB,OFF,OVERFLOWS(1,1),ST)
	  IF (ST.NE.0) THEN
	    CALL FILERR(SFNAMES(1,F_INDEX),2,ST,OFF)
	  ENDIF
	IF (OVERFLOWS(1,MAXOVR).NE.GAM) GOTO 240  !SKIP IF NONE FOUND
	DO 230, OFF1=1,MAXOVR-1
	  IF (OVERFLOWS(2,OFF1).EQ.0) GOTO 240    !SKIP IF NO MORE
	  OFFSET=OVERFLOWS(1,OFF1)
C       CALL OFFCMB(BOARD,OFFSET,BET)   ;GET COMBINATION
	  CALL OFFSPT(BOARD,BET,OFFSET)
C
C        FIND # OF MATCHES
C
	  TIMES=0
	  DO 220, ROW=1,NUMROWS
	     IF (WIN(ROW).EQ.BOARD(ROW)) TIMES=TIMES+1
220	  CONTINUE
C
	  IF (TIMES.NE.BET-MISS) GOTO 230   !IF WRONG # OF MATCHES
	  OVR=OVR+OVERFLOWS(2,OFF1)
230	CONTINUE
240	CONTINUE
C
	CALL CLOSEFIL(PFDB)
	RETURN
	END

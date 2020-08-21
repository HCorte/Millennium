C
C FUNCTION TIMEASCI
C $Log:   GXAFXT:[GOLS]TIMEASCI.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:33:58   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:50:58   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - timeasci.for **
C
C TIMEASCI.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 20/06/90 MK
C
C CONVERTS SEC/MSEC TO ASCI
C
C MODE - INT*4 = 1 TIME IN MSEC (FRACTION OF A SEC TRUNCATED)
C              = 2 TIME IN SEC
C TIME - INT*4     TIME
C TEXT - CHAR*1(8) ASCI STRING HH:MM:SS
C
C              = RETURN CODE =  0  OK
C                            = -1  CONVERSION ERROR
C
C RETURNCODE=TIMEASCI(MODE,TIME,TEXT)
C
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
C Copyright 1990 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	INTEGER*4 FUNCTION TIMEASCI(MODE,TIME,TEXT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER*8 TEXT
	CHARACTER*1 COLON /':'/
	INTEGER*4   MODE
	INTEGER*4   TIME
	INTEGER*4   SECONDS
	INTEGER*4   MINUTES
	INTEGER*4   HOURS
C
	IF(TIME.LT.0) GOTO 900
C
	IF(MODE.EQ.2) THEN
C
C SECONDS
C
	   SECONDS=TIME
C
	ELSEIF(MODE.EQ.1) THEN
C
C MILISECONDS
C
	   SECONDS=TIME/1000
C
	ELSE
	   GOTO 900
	ENDIF
C
	HOURS=SECONDS/3600
	SECONDS=SECONDS-HOURS*3600
	MINUTES=SECONDS/60
	SECONDS=SECONDS-MINUTES*60
C
	WRITE (TEXT,800) HOURS,COLON,MINUTES,COLON,SECONDS
C
800	FORMAT(I2.2,A1,I2.2,A1,I2.2)
C
	TIMEASCI=0
	RETURN
C
900	CONTINUE
	TIMEASCI=-1
	RETURN
C
	END

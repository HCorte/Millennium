C
C SUBROUTINE X2BINVSN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BINVSN.FOV                                 $
C  $Date::   17 Apr 1996 16:08:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2binvsn.for;1 **
C
C X2BINVSN.FOR
C
C V07 25-Aug-95 DAS FIXED TEST OF FIND OF FIRST EVSN         
C V06 02-Aug-95 DAS FIXED PROBLEM WITH V03 TEST OF STNIDX
C V05 27-JUN-95 SCD REMOVE CALLS TO LOKON AND LOKOFF FOR X2X BASELINE
C V04 22-FEB-95 DAS/SCD FIX BUG WITH SKIPPING OVERFLOW SECTION
C V03 09-JAN-95 SCD CHECK FOR STN NUMBER = 0 BECAUSE ONLY A NONZERO 
C		    STATION # IS VALID.
C V02 28-DEC-94 SCD CHANGE SEARCH ORDER.  DO BINARY SEARCH FIRST, THEN DO
C		    LINEAR SEARCH OF OVERFLOW TABLE
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This routine will binary search the X2X_SORTED_EVSN table
C to find the verification sequence number, and will return the
C station number.
C
C Calling sequence:
C
C     CALL X2BINVSN(EVSNLEN,STNEVSN,STATION,STATUS)
C
C Input parameters:
C
C     EVSNLEN  Int*4                       EVSN length
C     STNEVSN  Int*4(X2X_ADRESS_MAXLEN)    Station EVSN
C
C Output parameters:
C
C     STATION Int*4       Station number
C     STATUS  Int*4       Return status (0=found,-1=not found,
C                                       -2=locked)
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2BINVSN(EVSNLEN,STNEVSN,STATION,STATUS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4   EVSNLEN                     !EVSN length
	INTEGER*4   STNEVSN(X2X_EVSN_MAXLEN)    !EVSN to search for
	INTEGER*4   STATION                     !Return station number
	INTEGER*4   STN                         !Test Station number
	INTEGER*4   STATUS                      !Search status
	INTEGER*4   BOT,TOP,TARGET              !Search indices
	INTEGER*4   BEGTOP                      !Bottom of table (skip blanks)
	LOGICAL     INIT                        !Table initialized flag
	INTEGER*4   STNIDX, LENIDX
C
	DATA        BEGTOP / 0 /
	DATA        INIT   /.FALSE./

        COMMON /EVSNCOM/ BEGTOP,INIT		!V02 - Save these across calls
C
C       IF EVSN NUMBER PASSED FROM STATION IS 0, THEN REJECT AS NOT
C       FOUND
C
        IF(STNEVSN(1).EQ.0.AND.STNEVSN(2).EQ.0) THEN
          STATUS = -1
          RETURN
        ENDIF
C
C IF FIRST PASS, SEARCH THROUGH THE SORTED EVSN
C TABLE TO FIND THE HIGHEST VALID STATION NUMBER.
C
	IF(.NOT.INIT .OR. X2X_SORTED_EVSN_UPDATE.EQ.1) THEN
	  DO 100 BEGTOP=X2X_STATIONS,1,-1
	    IF((X2X_SORTED_EVSN(1,BEGTOP).NE.0) .OR.
     *         (X2X_SORTED_EVSN(2,BEGTOP).NE.0)) GOTO 110
100	  CONTINUE
110	  CONTINUE
	  INIT=.TRUE.
	  X2X_SORTED_EVSN_UPDATE=0
	ENDIF
C
C INITIALIZE VARIABLES.
C
	BOT=1
	TOP=BEGTOP+1
	STATUS = 0
C
C ATTEMPT TO LOCK THE SORTED EVSN TABLE.
C
CV05	IF(LOKON(X2X_LOCK_EVSN)) THEN
CV05	  STATUS=-2
CV05	  GOTO 8000
CV05	ENDIF
C
C FIRST, BINARY SEARCH THROUGH SORTED EVSN TABLE.
C
300	CONTINUE
	TARGET=BOT+(TOP-BOT)/2
	STNIDX=X2X_SORTED_EVSN(0,TARGET)
        IF(STNIDX.EQ.0) GOTO 400
	DO 350 LENIDX = 1,X2X_EVSN_MAXLEN
         IF(X2X_SORTED_EVSN(LENIDX,TARGET).NE.STNEVSN(LENIDX)) GOTO 400
350	CONTINUE
	IF(X2XS_EVSN_LEN(STNIDX).EQ.EVSNLEN) THEN !V03
           STATION=STNIDX
           GOTO 7000
	ENDIF
C
C IF ALL POSSIBLE ELEMENTS SEARCHED AND EVSN IS NOT
C FOUND THEN RETURN AN ERROR CODE.
C
400	CONTINUE
	IF(TARGET.EQ.BOT .OR. TARGET.EQ.TOP) THEN
	  STATUS=-1
	  GOTO 500
	ENDIF
C
C FIRST CHECK IF THE FIRST 4 BYTES OF THE EVSN MATCH.
C IF THEY DO, DETERMINE WHICH WAY TO SEARCH.
C
	IF(X2X_SORTED_EVSN(1,TARGET).EQ.STNEVSN(1)) THEN
	  IF(X2X_SORTED_EVSN(2,TARGET).LT.STNEVSN(2)) THEN
	    BOT=TARGET
	    GOTO 300
	  ELSE
	    TOP=TARGET
	    GOTO 300
	  ENDIF
	ENDIF
C
C IF FIRST 4 BYTES DO NOT MATCH AT ALL, SEARCH ACCORDING
C TO EVSN VALUE.
C
	IF(X2X_SORTED_EVSN(1,TARGET).LT.STNEVSN(1)) THEN
	  BOT=TARGET
	  GOTO 300
	ELSE
	  TOP=TARGET
	  GOTO 300
	ENDIF
C
C
C Second, search the overflow area in case the EVSN was added or modified
C online.
C
500     CONTINUE
	DO 200 STNIDX = X2X_STATIONS+1,X2X_STATIONS+X2X_ADDED_EVSN
	   DO 250 LENIDX = 1,X2X_EVSN_MAXLEN
	      IF(X2X_SORTED_EVSN(LENIDX,STNIDX).NE.
     *	         STNEVSN(LENIDX)) GOTO 200
250	   CONTINUE
	   STN = X2X_SORTED_EVSN(0,STNIDX)
	   IF(X2XS_EVSN_LEN(STN).EQ.EVSNLEN .AND. STN .NE. 0) THEN
	     STATION=STN
             STATUS = 0
	     GOTO 7000
	   ENDIF
200	CONTINUE
C
C RELEASE THE RECORD LOCK.
C
7000	CONTINUE
CV05	CALL LOKOFF(X2X_LOCK_EVSN)
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END

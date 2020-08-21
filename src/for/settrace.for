C
C PROGRAM SETTRACE
C $Log:   GXAFXT:[GOLS]SETTRACE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:02:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:36:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - settrace.for **
C
C
C	SETTRACE.FOR
C	____________
C
C	V01 WS 09-APR-91
C
C	SET INPUT/OUTPUT TRACE, FOR TESTING ONLY
C
C=======OPTIONS	/CHECK=NOOVERFLOW
	PROGRAM SETTRACE
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
C
        CHARACTER*20 PASPAS
        EQUIVALENCE(PAS,PASPAS)
        CHARACTER*8 PAS
        INTEGER*4 CNT,VALUE,I4,AGT,ST
	BYTE I1(4)
	EQUIVALENCE (I1,I4)
C
C
	TYPE *,'This program will force the system to provide trace ',
     *		  'transaction for the agents'
C
	AGT=0
	CALL INPNUM('Enter agent number [-1 - all]:',AGT,-1,NUMAGT,ST)
	IF (AGT.EQ.0 .OR. ST.EQ.-1) THEN
	    TYPE *,'Exiting program '
	    GOTO 9000
	ENDIF
C
	CALL INPNUM('Enter value to set to [0,255]',VALUE,0,255,ST)
	I1(1)=VALUE
	I1(2)=VALUE
	I1(3)=VALUE
	I1(4)=VALUE
C
	IF (VALUE.EQ.255 .OR. AGT.EQ.-1) THEN
	    CNT = 0
100    	    CONTINUE
	    CALL PASSWORD(5,PASPAS)
	    IF (PAS .NE. 'SETTRACE') THEN
             CNT = CNT + 1
             IF (CNT .GT. 5) THEN
      		  TYPE*,'You have used up all your chances...bye bye...'
      		  CALL USRCLOS1(     5)
      		  STOP
	     ENDIF
             GOTO 100
	    ENDIF
	ENDIF
CC
	IF (AGT.EQ.-1) THEN
	    CALL FASTSET(I4,IX2XT_TRACE_LIMIT,X2X_TERMS/4) !MONITOR ALL
	ELSE
	    CALL ISBYTE(VALUE,IX2XT_TRACE_LIMIT,AGT-1)
	ENDIF
C
	TYPE *,'done'
C
9000	CONTINUE
	STOP 'bye bye boys'
	END

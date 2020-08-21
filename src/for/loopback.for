C PROGRAM LOOPBACK.FOR
C
C V07 16-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V06 22-AUG-1995 DAS CHANGED TO HANDLE MCP LOOPBACK (TYPE/SUBTYPE)
C V05 08-FEB-1995 SCD Allow SUBNETWORK number of 0 (for DC)
C V04 21-OCT-1994 JWE Set loopback parameters for options other than 4
C V03 19-MAY-1994 TGS ADDED HOST DELAY TIME AND RETRY STATS
C V02 16-DEC-1992 M&M & WS X21UNS EXTRA PARAMETER
C V01 23-MAR-1992 MGM INITIAL RELEASE FOR SWEDEN
C V00 XX-XXX-XXXX XXX INITIAL RELEASE TAKEN FROM SPAIN(STL)
C
C THIS PROGRAM STARTS A LOOPBACK CONDITION WITH A GIVEN TERMINAL.
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
	PROGRAM LOOPBACK
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:REQCOM.DEF'
C
C
	INTEGER*2 TABLE(128) /128*2989/
	BYTE	  BTABLE(256)
	EQUIVALENCE (BTABLE,TABLE)
	INTEGER*2 UNSBEGIN
	INTEGER*4 ST, OPT, EXT, TER, NUM_MESSAGES
	INTEGER*4 LINE, TNUM, LEN
	INTEGER*4 BUF, J
	INTEGER*4 SUBNETWORK /0/
     	DATA UNSBEGIN/Z5620/         !MCP LOOPBACK
C...	DATA UNSBEGIN/ZAF20/         !TERMINAL LOOPBACK (UK)
	INTEGER*4 I4
	INTEGER*2 I2(2)
	BYTE	  I1(4)
	EQUIVALENCE (I4,I2,I1)
	INTEGER*4 RETURN_LEN, OUT_LEN, MES_DELAY
C
	CALL COPYRITE
C
C
C
10	CONTINUE
	SUBNETWORK=-1
C
	TYPE*
	TYPE*,' <<<<< LOOPBACK Network Performace Utility   V03 >>>>>'
	TYPE*,' '
	TYPE*,'       1. Begin Loopback '
	TYPE*,'       2. Stop  Loopback '
	TYPE*
	TYPE*,'       E. Exit '
	TYPE*
C
C
	CALL INPNUM('Enter option [E-Exit]:                         ',
     *	            OPT,1,4,EXT)
	IF(EXT.LT.0) STOP
C
	TER=0
	GOTO (100,200) OPT
C
100	  CONTINUE
C***        CALL INPNUM('Enter delay at host in milliseconds [0-100]  ',
C***     *	              P(LOOPHDLY),0,100,EXT)
C***	  IF(EXT.LT.0) GOTO 10
        CALL INPNUM('Enter delay at terminal in seconds [0-600]   ',
     *	              MES_DELAY,0,600,EXT)
	  IF(EXT.LT.0) GOTO 10
        CALL INPNUM('Enter length of DATA to send in bytes          ',
     *	              OUT_LEN,10,99,EXT)
	  IF(EXT.LT.0) GOTO 10
        CALL INPNUM('Enter length of DATA returning [0=ALL]         ',
     *	              RETURN_LEN,0,99,EXT)
	  IF(EXT.LT.0) GOTO 10
	CALL INPNUM('How many messages per second you want to start ?',
     *	 NUM_MESSAGES,0,100000,ST)
 
	IF (ST.LT.0) NUM_MESSAGES=100000
C
       	  LINE=0
	  TNUM=0
	  IF(MOD(OUT_LEN,2).NE.0) THEN
	     OUT_LEN=OUT_LEN+1
	     TYPE*,'Modified Length Of Data To Send To ',OUT_LEN
	  ENDIF
	  IF(MOD(RETURN_LEN,2).NE.0) THEN
	     RETURN_LEN=RETURN_LEN+1
	     TYPE*,'Modified Length Of Data Returned To  ',RETURN_LEN
	  ENDIF
C
C
120	  CONTINUE
	  LEN=OUT_LEN
C
	  TYPE *
	  TYPE *,'	     Options available: '
	  TYPE *
	  TYPE *,'    1. Start LOOPBACK with  1 terminal '
	  TYPE *,'    2. Change the subnetwork no        ',SUBNETWORK
	  TYPE *,'    3. Start LOOPBACK with a subnetwork'
	  TYPE *,'    4. Start single loopback message from the terminals'
	  TYPE *
C
	  CALL INPNUM('Enter option [E-Exit]: ',OPT,1,4,EXT)
	  IF(EXT.LT.0) GOTO 10
C
	  P(LOOPOUT)=OUT_LEN
	  P(LOOPIN)=RETURN_LEN
	  P(LOOPDLAY)=MES_DELAY
C
	  IF(OPT.EQ.1) THEN
	     CALL INPNUM('Enter terminal number [E-Exit]: ',
     *	                  TER,1,X2X_TERMS,EXT)
	     IF(EXT.LT.0) GOTO 120
	     LINE=0
	     TNUM=TER
	  ENDIF
C
	  IF(OPT.EQ.2) THEN
	     CALL INPNUM('Enter SUBNETWORK no ',SUBNETWORK,0,		!V03
     *							RE_MAX_SUBNET,ST)
	     GOTO 120
	  ENDIF
C
C
	  IF(OPT.EQ.3) THEN
	     TNUM=-1
	     LINE=-1
	  ENDIF
C
	TABLE(1)=UNSBEGIN
	
	I4=MES_DELAY
	BTABLE(4)=I1(2)
	BTABLE(3)=I1(1)	
	BTABLE(5)=0			       !RESERVE FOR FUTURE
	BTABLE(6)=0			       !RESERVE FOR FUTURE
	BTABLE(7)=0			       !RESERVE FOR FUTURE
	BTABLE(8)=0			       !RESERVE FOR FUTURE
	I4=RETURN_LEN
	BTABLE(9)=I1(1)
	I4=OUT_LEN
	BTABLE(10)=I1(1)
	TABLE(6)=0
	TABLE(7)=0
	TABLE(8)=0
	TABLE(9)=0
	TABLE(10)=0
C
C SEND MESSAGE
C
	CALL GETBUF(BUF)
	IF(BUF.LE.0) THEN
	  TYPE*,'Buffer allocation error'
	  PAUSE
	ENDIF
C
C TRANSFER MESSAGE TO BUFFER
C
	HPRO(OUTLEN,BUF)=10+LEN+MOD(LEN,2)
	HPRO(MSGNUM,BUF)=0
	HPRO(TRCODE,BUF)=TYPUNS
	IF (X2X_I4_STATION) THEN
	    PRO(LINENO,BUF)=LINE
	    PRO(TERNUM,BUF)=TNUM
	ELSE
	    HPRO(LINENO,BUF)=LINE
	    HPRO(TERNUM,BUF)=TNUM
	ENDIF
	DO 150 J=1,5+LEN/2+MOD(LEN,2)
	   HPRO(INPTAB*2-2+J,BUF)=TABLE(J)
150	CONTINUE
CC	TYPE *,BUF,LEN,'BPRO ',(BPRO(J,BUF),J=(INPTAB-1)*4+1,
CC   *		      (INPTAB-1)*4+30)
C
C QUEUE BUFFER TO THE OUTPUT QUEUE
C
	IF(LINE.LT.0) THEN
	  IF (SUBNETWORK.LT.0) THEN
	    TYPE *,'Invalid subnetwork '
	    GOTO 120
	  ENDIF
	  CALL X2XUNS(BUF,NUM_MESSAGES,SUBNETWORK)
	ELSE IF(LINE.EQ.0) THEN
	  TYPE *,'Sending single message '
	  CALL QUETRA(LOG,BUF)
	ENDIF
C
	GOTO 120
C
C STOP LOOPBACK
C
200	CONTINUE
	P(LOOPDLAY)=0
	P(LOOPIN)=0
	P(LOOPOUT)=0
	GOTO 10
C
C CLEAR STATS
C
	END

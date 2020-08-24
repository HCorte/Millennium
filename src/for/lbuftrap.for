C
C *** SUBROUTINE LBUFTRAP ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]LBUFTRAP.FOV                                 $
C  $Date::   17 Apr 1996 13:48:20                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - lbuftrap.for ***
C
C V09 05-JUN-95 WJK Changed length calculation for READS (see comment)
C V08 19-APR-95 DAS Changed length calculation for writes (see comment)
C V07 12-DEC-94 GPR Integrate UK changes into X2X Baseline
C V06 27-MAY-92 JWE DISCARD LESS THEN MINUMUM LENGTH, GREATER THAN MAX
C		    BAD TRAP DATA...
C V05 19-MAY-92 JWE TRY AGAIN WITH THE ERROR HANDLING
C V04 11-MAY-92 JWE IMPROVE ERROR MESSAGES AND DISCARD BUFFERS ON CONGESTION...
C V03 29-APR-92 JWE REMOVE MAXB AND REALLY REMOVED LIMTAB.DEF
C V02 28-APR-92 DAS REMOVED LIMTAB.DEF 
C V01 16-SEP-90 MRM RELEASED FOR VAX
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	THE CONTROL OF THE TASK WILL BE SENT HERE WHEN
C	A BUFFER HAS BEEN SENT OR RECEIVED. CLEAN THEM ALL (READ AND WRITE).
C	EXECUTE ONLY ACTIONS BROKEN BY LUN (MAC STUFF).
C	ACTIONS RELATED TO THE CONNECTION WILL BE EXECUTED IN LANACT.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE LBUFTRAP(TEMP_TRAP_DATA)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'					  !V07
	INCLUDE 'INCLIB:CONCOM.DEF'					  !V07
C
        INCLUDE '($SSDEF)'
	INCLUDE '($XMDEF)'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	ARR,
     *			BLAN,
     *			BUF,
     *			BUFSIZ,
     *			CNT,
     *			CONN,
     *			DSAP,
     *			FRAME,
     *			K,
     *			LAN,
     *			LBUF_DATA_EQV,			! PASSED BY AST TRAP
     *			LENGTH,
     *			LIM,
     *			QUE,
     *			QUECNT,
     *			SAP,
     *			SLOT,
     *			SSAP,
     *			TEMP,
     *                  TEMP_TRAP_DATA,
     *                  LAST_TIME_NOTIFY /0/,				  !V07
     *                  LAST_TIME_NOTIFY1 /0/,				  !V07
     *                  LAST_TIME_NOTIFY2 /0/,				  !V07
     *                  LAST_TIME_NOTIFY3 /0/				  !V07
C
	INTEGER*2	LBUF_DATA(2)
C
	LOGICAL*4	READTRAP,			! INDICATE WHICH IO TRAP
     *			WRITETRAP			! DELIVERED THIS AST
C
        EQUIVALENCE	(LBUF_DATA_EQV, LBUF_DATA(1))
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C EXTRACT DATA FROM THE AST TRAP INFO.
C
	LBUF_DATA_EQV = TEMP_TRAP_DATA
	SLOT = LBUF_DATA(TRPDTA_SLOT)			! IOSB SLOT
	BUF  = LBUF_DATA(TRPDTA_BUFFER)			! LANCOM BUFFER NUMBER
C
	IF (BUF .GT. 0) THEN
	  READTRAP  = .TRUE.
	  WRITETRAP = .FALSE.
	ELSEIF (BUF .LT. 0) THEN
	  READTRAP  = .FALSE.
	  WRITETRAP = .TRUE.
	ELSE
	  CALL OPS('*** LBUFTRAP - BAD TRAP DATA ***', BUF, 0)		  !V07
	  GOTO 9999
	ENDIF
C	    
	BUF = ABS(BUF)
C
C EXTRACT THE INFORMATION BASED ON LANCOM BUFFER NUMBER.
C
	SAP = TRAP_INFO(TRPINF_SAP, BUF)
	LAN = TRAP_INFO(TRPINF_LAN, BUF)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHECK STATUS OF I/O (WRITE).
C
	IF (WRITETRAP) THEN
	  WRITECNT(LAN, SAP) = WRITECNT(LAN, SAP) - 1
C
C V08     THE AMOUNT OF DATA TRANSFERED BY THE DEC CONTROLER DOES NOT
C V08     INCLUDE THE HEADER INFORMATION
C.VO8.....LENGTH = WRITE_IOSB(SLOT, SAP, LAN).XSIZE + CTRLEND
C
	  LENGTH = WRITE_IOSB(SLOT, SAP, LAN).XSIZE
D         CALL OPS('LENGTH RECIEVED LBUFTRAP: ',LENGTH,0)
C
	  IF (WRITE_IOSB(SLOT, SAP, LAN).STAT .EQ. SS$_DATACHECK) THEN
	    CALL OPS('*** LBUFFFTRAP - COLLISION ON NETWORK WRITE ***',
     *               SAP, LAN)
	    CALL GOWRITE(BUF)
	    GOTO 9999
C
	  ELSEIF (WRITE_IOSB(SLOT, SAP, LAN).STAT .NE. SS$_NORMAL) THEN
	    TEMP = WRITE_IOSB(SLOT, SAP, LAN).STAT
	    CALL OPS('*** LBUFTRAP - WRITE TRAP BAD STATUS ***',
     *               TEMP, LAN)
C
	    IF (TEMP .NE. 0) THEN
	      CALL VMS_ERROR_OPS(TEMP)
C
	    ELSEIF (WRITE_IOSB(SLOT, SAP, LAN).PARM .EQ.
     *              XM$M_STS_BUFFAIL) THEN
	      CALL OPS('*** LBUFTRAP - ATTEMPT TO ALLOCATE A ' //
     *                 'SYSTEM RECEIVE BUFFER FAILED ***', SAP, LAN)
C
	    ELSEIF (WRITE_IOSB(SLOT, SAP, LAN).PARM .EQ.
     *              XM$M_STS_TIMO) THEN
	      CALL OPS('*** LBUFTRAP - TIMEOUT OCCURED ***', SAP, LAN)
C
	    ELSEIF (WRITE_IOSB(SLOT, SAP, LAN).PARM .EQ.
     *              XM$M_ERR_FATAL) THEN
	      CALL OPS('*** LBUFTRAP - HARDWARE OR SOFTWARE ERROR ' //
     *                 'OCCURRED ON CONTROLLER PORT ***', SAP, LAN)
C
	    ELSE
	      CALL OPS('*** LBUFTRAP - UNKNOWN ERROR ***',
     *                 LAN, WRITE_IOSB(SLOT, SAP, LAN).PARM)
	      CALL LAN_BUFFER_DUMP(BUF)
	    ENDIF
C
	    CALL LANRELB(BUF)
	    GOTO 9999
	  ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHECK STATUS OF I/O (READ).
C
	ELSEIF (READTRAP) THEN
	  READCNT(LAN, SAP) = READCNT(LAN, SAP) - 1
C V09     THE LENGTH RETURNED BY THE DEC CONTOLER DOES NOT INCLUDE
C V09     THE HEADER BYTES.
C V09	  LENGTH = READ_IOSB(SLOT, SAP, LAN).XSIZE + CTRLEND
          LENGTH = READ_IOSB(SLOT, SAP, LAN).XSIZE      ! V09
C
	  IF (READ_IOSB(SLOT, SAP, LAN).STAT .EQ. SS$_DATACHECK) THEN
	    CALL OPS('*** LBUFTRAP - COLLISION ON NETWORK READ ***',
     *               SAP, LAN)
	    CALL LANRELB(BUF)
	    GOTO 9999
C
	  ELSEIF (READ_IOSB(SLOT, SAP, LAN).STAT .NE. SS$_NORMAL) THEN
	    IF (LANSAPSTS(SAP) .EQ. SAPUP) THEN
	      TEMP = READ_IOSB(SLOT, SAP, LAN).STAT
	      CALL OPS('*** LBUFTRAP - READ TRAP BAD STATUS ***',
     *                 TEMP, LAN)
C
	      IF (TEMP .NE. 0) THEN
		CALL VMS_ERROR_OPS(TEMP)
C
	      ELSEIF (READ_IOSB(SLOT, SAP, LAN).PARM .EQ.
     *                XM$M_STS_BUFFAIL) THEN
		CALL OPS('*** LBUFTRAP - ATTEMPT TO ALLOCATE A ' //
     *                   'SYSTEM RECEIVE BUFFER FAILED ***', SAP, LAN)
C
	      ELSEIF (READ_IOSB(SLOT, SAP, LAN).PARM .EQ.
     *                XM$M_STS_TIMO) THEN
		CALL OPS('*** LBUFTRAP - TIMEOUT OCCURED ***', SAP, LAN)
C
	      ELSEIF (READ_IOSB(SLOT, SAP, LAN).PARM .EQ.
     *                XM$M_ERR_FATAL) THEN
		CALL OPS('*** LBUFTRAP - HARDWARE OR SOFTWARE ERROR ' //
     *                   'OCCURRED ON CONTROLLER PORT ***', SAP, LAN)
C
	      ELSE
		CALL OPS('*** LBUFTRAP - UNKNOWN ERROR ***',
     *                   LAN, READ_IOSB(SLOT, SAP, LAN).PARM)
		CALL LAN_BUFFER_DUMP(BUF)
	      ENDIF
	    ENDIF
C
	    CALL LANRELB(BUF)
	    GOTO 9999
	  ENDIF
C
C MOVE DATA FROM DRIVER INTO COMPATIBLE LANPRO FORMAT.
C
	  CALL MOVBYT(READ_DATA(SLOT, SAP, LAN).DEST(1), 1,
     *                LANBUF(1, BUF), DABEG, DAEND - DABEG + 1)
C
	  CALL MOVBYT(READ_DATA(SLOT, SAP, LAN).SOURCE(1), 1,
     *                LANBUF(1, BUF), SABEG, SAEND - SABEG + 1)
C
	  CALL I4TOBUF2(LENGTH, LANBUF(1, BUF), TYPEBEG - 1)
C
	  CALL MOVBYT(READ_DATA(SLOT, SAP, LAN).SSAP, 1,
     *                LANBUF(1, BUF), DSAPBEG, DSAPEND - DSAPBEG + 1)
C
	  CALL MOVBYT(READ_DATA(SLOT, SAP, LAN).DSAP, 1,
     *                LANBUF(1, BUF), SSAPBEG, SSAPEND - SSAPBEG + 1)
C
	  CALL MOVBYT(READ_DATA(SLOT, SAP, LAN).CTL, 1,
     *                LANBUF(1, BUF), CTRLBEG, CTRLEND - CTRLBEG + 1)
C
C FILL IN DRIVER INFORMATION (TO BE COMPATIBLE).
C
	  LANBUF(LANDLEN, BUF) = READ_IOSB(SLOT, SAP, LAN).XSIZE - 3
	  LANBUF(LANDORG, BUF) = READ_DATA(SLOT, SAP, LAN).DSAP
	  LANBUF(LANDDES, BUF) = READ_DATA(SLOT, SAP, LAN).SSAP
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHECK FOR VALID BUFFER SIZE & EXTRACT DATA FROM MESSAGE.
C
	BUFSIZ = LENGTH
C
	IF (BUFSIZ .GE. 0 .AND. BUFSIZ .LE. ETHLENDT) THEN
	  FRAME = BLANBUF(FRTYPBEG, BUF)
	  SSAP  = BLANBUF(SSAPBEG,  BUF)
	  DSAP  = BLANBUF(DSAPBEG,  BUF)
C
C CHECK FOR VALID SSAP AND DSAP.
C
	  IF (SSAP .LE. 0 .OR. SSAP .GT. MAXSAP .OR.
     *        DSAP .LE. 0 .OR. DSAP .GT. MAXSAP) THEN
	    IF (LANTEST .EQ. 1)
     *        CALL OPS('*** LBUFTRAP - ILLEGAL SAP ***', SSAP, DSAP)
	    CALL LANRELB(BUF)
	    CALL GOREAD(LAN, DSAP)
	    GOTO 9999
	  ENDIF
C
	  CONN = CONNECTION(SSAP, DSAP)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PROCESS READ TRAP ...
C
	  IF (READTRAP) THEN				! READ BUFFER
	    IF (LANTEST .EQ. 2)
     *        TYPE 9000, BUF, (BLANBUF(K, BUF), K = 1, 128)
D	    ARR = LANTARR(CONN)
D	    TYPE *,'*** LBUFTRAP - READ *** [', BUF, BUFSIZ, SSAP, ']'
D	    TYPE *,'*** LBUFTRAP - READ *** [', LANTIMER, ARR, ']'
C
C FROM HERE OR FOR HERE ?
C
	    IF (LANSAPSTS(SSAP) .NE. SAPDOWN) THEN
D	      TYPE *, '*** LBUFTRAP - SENT FROM HERE *** [',
D    *                SSAP, LANSAPSTS, ']'
	      CALL LANRELB(BUF)
	      GOTO 2000
	    ELSEIF (LANSAPSTS(DSAP) .EQ. SAPDOWN) THEN
D	      TYPE *, '*** LBUFTRAP - NOT FOR HERE *** [',
D    *                SSAP, DSAP, LAN, FRAME, ']'
	      CALL LANRELB(BUF)
	      GOTO 2000
	    ENDIF
C
C CHECK FOR A VALID LAN NUMBER.
C
	    CALL ILBYTE(BLAN, LANBUF(1, BUF), TLANBEG - 1)
C
	    IF (BLAN .NE. LAN) THEN
C
C	      ***** Start V07 changes *****
C
              IF (P(ACTTIM).LT.LAST_TIME_NOTIFY1) LAST_TIME_NOTIFY1=
     *                        P(ACTTIM)-31
	      IF (P(ACTTIM).GT.LAST_TIME_NOTIFY1+30) THEN
		 CALL OPS('*** LBUFTRAP - INVALID LAN ID FROM SAP ***',
     *			  SSAP, DSAP)
		 LAST_TIME_NOTIFY1=P(ACTTIM)
	      ENDIF
C
C	      ***** End V07 changes *****
C
	      CALL LANRELB(BUF)
	      GOTO 2000
	    ENDIF
C
C CHECK FOR VALID FRAME TYPE.
C
	    IF (FRAME .LE. 0 .OR. FRAME .GT. MAXFRAME) THEN
C
C	      ***** Start V07 changes *****
C
	      IF (P(ACTTIM).LT.LAST_TIME_NOTIFY2) LAST_TIME_NOTIFY2=
     *			   P(ACTTIM)-31
	      IF (P(ACTTIM).GT.LAST_TIME_NOTIFY2+30) THEN
	         CALL OPS('*** LBUFTRAP - ILLEGAL FRAME TYPE ***',
     *                    FRAME, SSAP)
		 LAST_TIME_NOTIFY2=P(ACTTIM)
	      ENDIF
C
C	      ***** End V07 changes *****
C
	      CALL LANRELB(BUF)
	      GOTO 2000
	    ENDIF
C
C FRAME COUNTER - READ SIDE
C
	    LANFRAMES(FRAME, FRAMESIN, CONN) =
     *      LANFRAMES(FRAME, FRAMESIN, CONN) + 1
C
C SAP TOO SLOW
C
	    QUE = QUESAP(DSAP)
C
	    IF (NUMSAP .GT. 0) THEN
	      LIM = (LANBNUM - NUMEXTRA) / NUMSAP
	    ELSE
	      LIM = 0 
	    ENDIF
C
	    CNT = QUECNT(LANAPP(1, QUE))
C
	    IF (CNT .GT. LIM) THEN
C
C	      ***** Start V07 changes *****
C
	      IF (P(ACTTIM).LT.LAST_TIME_NOTIFY) LAST_TIME_NOTIFY=
     *			      P(ACTTIM)-31
	      IF (P(ACTTIM).GT.LAST_TIME_NOTIFY+30) THEN
		LAST_TIME_NOTIFY=P(ACTTIM)
	        CALL OPS('*** LBUFTRAP - DATA OVERFLOW ***', DSAP, CNT)
	      ENDIF
C
C	      ***** End V07 changes *****
C
D	      TYPE *, '*** LBUFTRAP - CNT DSAP QUE *** [',
D    *                CNT, DSAP, QUE, ']'
	      CALL LANACT(EVNDOVF, SSAP, DSAP, BUF)
	      GOTO 2000
	    ENDIF
C
C BRANCH BASED OF FRAME TYPE.
C
	    GOTO (100, 200, 300, 400, 500,
     *            600, 700, 800, 900, 1000) FRAME
C
C ILLEGAL FRAME TYPE.
C
 	    GOTO 2000
C
C FIND REQUEST
C
100	    CONTINUE
	    CALL LANACT(EVNFIND, SSAP, DSAP, BUF)
	    GOTO 2000
C
C FIND RESPONSE
C
200	    CONTINUE
	    CALL LANACT(EVNFCONF, SSAP, DSAP, BUF)
	    GOTO 2000
C
C CONNECT REQUEST
C
300	    CONTINUE
	    CALL LANACT(EVNCIND, SSAP, DSAP, BUF)
	    GOTO 2000
C
C CONNECT CONFIRM
C
400	    CONTINUE
	    CALL LANACT(EVNCCONF, SSAP, DSAP, BUF)
	    GOTO 2000
C
C DISCONNECT REQUEST
C
500	    CONTINUE
	    CALL LANACT(EVNDIND, SSAP, DSAP, BUF)
	    GOTO 2000
C
C DISCONNECT CONFIRM
C
600	    CONTINUE
	    CALL LANACT(EVNDCONF, SSAP, DSAP, BUF)
	    GOTO 2000
C
C CLEAR FRAME
C
700	    CONTINUE
	    CALL LANACT(EVNCLEAR, SSAP, DSAP, BUF)
	    GOTO 2000
C
C DATA FRAME
C
800	    CONTINUE
C
C	    ***** Start V07 changes *****
C
C*UK*	    IF (QUECNT(LANFREE) .LT. 5) THEN		    ! Use 10 percent
	    IF (QUECNT(LANFREE) .LT. LANBNUM / 10) THEN	    ! instead of 5
	      IF (P(ACTTIM).LT.LAST_TIME_NOTIFY) LAST_TIME_NOTIFY=
     *			      P(ACTTIM)-31
              IF (P(ACTTIM).GT.LAST_TIME_NOTIFY+30) THEN
	        CALL OPS('*** LBUFTRAP - RECEIVE DATA OVERRUN ***',
     *                   QUECNT(LANFREE), QUECNT(LANFREE))
		LAST_TIME_NOTIFY=P(ACTTIM)
	      ENDIF
C
C	      ***** End V07 changes *****
C
	      CALL LANRELB(BUF)
	    ELSE
	      CALL LANACT(EVNDTAIND, SSAP, DSAP, BUF)
	    ENDIF
	    GOTO 2000
C
C POLL IND
C
900	    CONTINUE
	    CALL LANACT(EVNPIND, SSAP, DSAP, BUF)
	    GOTO 2000
C
C POLL CONF
C
1000	    CONTINUE
	    CALL LANACT(EVNPCONF, SSAP, DSAP, BUF)
	    GOTO 2000
C
C SETUP ANOTHER OUTSTANDING READ.
C
2000	    CONTINUE
	    CALL GOREAD(LAN, DSAP)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PROCESS WRITE TRAP ...
C
	  ELSEIF (WRITETRAP) THEN			! WRITE BUFFER
C
C CHECK IF THIS IS FROM YOUR SAP
C
	    IF (LANSAPSTS(SSAP) .EQ. SAPDOWN) THEN
	      CALL OPS('*** LBUFTRAP - ILLEGAL SSAP STATE ***',
     *                 SSAP, LAN)
	    ELSE
C
C FRAME COUNTER - WRITE SIDE
C
	      IF (FRAME .GT. 0 .AND. FRAME .LE. MAXFRAME .AND.
     *            CONN  .GE. 0 .AND. CONN  .LE. MAXCON)
     *          LANFRAMES(FRAME, FRAMESOUT, CONN) =
     *          LANFRAMES(FRAME, FRAMESOUT, CONN) + 1
D	      TYPE *, '*** LBUFTRAP - WRITE *** [',
D    *                BUF, LANTIMER, DSAP, ']'
D	      CONN = CONNECTION(SSAP, DSAP)
D	      TYPE *, '*** LBUFTRAP - TOUT ARR ***[',
D    *                LANTOUT(CONN), LANTARR(CONN), ']'
	    ENDIF
C
C RETURN THE BUFFER TO FREE LIST.
C
	    CALL LANRELB(BUF)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C HANDLE ILLEGAL TRAP ...
C
	  ELSE
	    CALL OPS('*** LBUFTRAP - COMPLETE TRAP ***', BUF, 0)	!V07
	    CALL LANRELB(BUF)
	  ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C UNUSED BUFFER (DOUBLE BUFFERING)
C
	ELSEIF (BUFSIZ .EQ. 0) THEN
D	  TYPE *, '*** LBUFTRAP - DBL-BUF SIZE ***[',
D    *            BUFSIZ, BUF, ']'
	  CALL LANRELB(BUF)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SOMETHING IS NOT QUITE OK HERE
C
	ELSE
C
            IF (LANTEST .EQ. 2)
     *        TYPE 9000, BUF, (BLANBUF(K, BUF), K = 1, 128)
C
D	  TYPE *, '*** LBUFTRAP - ILLEGAL BUFFER SIZE ***[',
D    *            BUFSIZ, BUF, ']'
	  CALL OPS('*** LBUFTRAP - ILLEGAL BUFFER SIZE ***',
     *            LAN, BUFSIZ)
	  CALL LANRELB(BUF)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS
C
9000	FORMAT(X, '++++++++++ BUFFER IN READTRAP ++++++++++', /,
     *         X, I5, 8(/, X, 16Z3.2))
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END

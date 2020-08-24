C
C SUBROUTINE VZSCREEN
C
C V03 21-SEP-1999 UXN VIS_BOLD ADDED.
C V02 13-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C VZSCREEN does all the work
C
	SUBROUTINE VZSCREEN(TYPWAIT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
C
	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($IODEF)'
	INCLUDE '($SSDEF)'
	INCLUDE '(SMG$ROUTINES)'
	INCLUDE '($SMGDEF)'
	INCLUDE '($TRMDEF)'
C
C
C Set some equivalences for ease of reference
C
	INTEGER*4	I4OLDPAG(20,24)             ! OLD SCREEN
	CHARACTER*1	C1OLDPAG(80,24)
	CHARACTER*80	CXOLDPAG(24)
	EQUIVALENCE    (I4OLDPAG, C1OLDPAG, CXOLDPAG, OLD(1,1))
C
	INTEGER*4	I4NEWPAG(20,24)		    ! NEW SCREEN
	CHARACTER*1	C1NEWPAG(80,24)
	CHARACTER*80	CXNEWPAG(24)
	EQUIVALENCE    (I4NEWPAG, C1NEWPAG, CXNEWPAG, NEW(1,1))
C
C
	INTEGER*4	I4TIMBUF(2)		    ! CURRENT TIME IN NEW SCR
	CHARACTER*1	C1TIMBUF(8)
	CHARACTER*8	CXTIMBUF
	EQUIVALENCE    (I4TIMBUF, C1TIMBUF, CXTIMBUF, NEW(15,1))
C
	INTEGER*4	I4CMDBUF(20)		    ! COMMAND LINE
	CHARACTER*1	C1CMDBUF(80)
	CHARACTER*80	CXCMDBUF
	EQUIVALENCE    (I4CMDBUF, C1CMDBUF, CXCMDBUF)
C
	INTEGER*4	DELAY			    ! INTERVAL (MSECS)
	EQUIVALENCE    (DELAY, DEL)
	INTEGER*4	MIN_DELAY		    ! MINIMUM DELAY
	PARAMETER	(MIN_DELAY = 1000)
C
	INTEGER*4	CLRFLG			    ! 1 = CLEAR SCREEN
	EQUIVALENCE    (CLRFLG, CLR)
C
	INTEGER*4	STPFLG			    ! 1 = RETURN TO CALLER
	EQUIVALENCE    (STPFLG, STP)
C
	INTEGER*4	CMDLEN			    ! COMMAND LENGTH (BYTES)
	EQUIVALENCE    (CMDLEN, LQ)
C
C The following are various parameters for screen stuff (they do not
C include the preceding escape character)
C
	CHARACTER*4	CLRSCRN
	DATA		CLRSCRN(1:1)/Z1B/,CLRSCRN(2:4)/'[2J'/
	CHARACTER*3	HOMECUR
	DATA		HOMECUR(1:1)/Z1B/,HOMECUR(2:3)/'[H'/
	CHARACTER*4     DISP_BOLD
	DATA            DISP_BOLD(1:1)/Z1B/,DISP_BOLD(2:4)/'[1m'/
	CHARACTER*4     DISP_NORM
	DATA            DISP_NORM(1:1)/Z1B/,DISP_NORM(2:4)/'[0m'/
	CHARACTER*4     DISP_STR
	CHARACTER*1	NULLCHR
	DATA		NULLCHR(1:1)/Z00/
C
	INTEGER*4	MYLUN
	PARAMETER      (MYLUN = 5)
C
	INTEGER*4	CRTCHAN/-1/
	INTEGER*4	EVFTIM/-1/
	INTEGER*4	EVFQIO/-1/
	INTEGER*4	EVFMASK/0/
	LOGICAL*4	GOTINP/.TRUE./
C
	INTEGER*4	VAXDELAYNORM(2)
	INTEGER*4	VAXDELAYSMAL(2)
	INTEGER*4	VAXDELAY(2)
C
	INTEGER*4	 READFUNCOD
	PARAMETER	(READFUNCOD = IO$_READVBLK + IO$M_CVTLOW +
     *                                IO$M_TRMNOECHO)
	INTEGER*4	 WRITEFUNCOD
	PARAMETER	(WRITEFUNCOD= IO$_WRITEVBLK + IO$M_CANCTRLO +
     *                                IO$M_NOFORMAT)
C
	INTEGER*2	OUIOSB(8)
C
	INTEGER*4	ST, TYPWAIT, ALLFLAGS, LSTLIN, LIN, X
C
	INTEGER*4	PASTEBOARD_ID, KEYBOARD_ID
C
	INTEGER*4	INPUTAST
	EXTERNAL	INPUTAST
C
	INTEGER*4       VIS_BOLD,OLD_VIS_BOLD(24)
	COMMON /SCREEN/ VIS_BOLD(24)
C
C
	IF(EVFTIM.LE.0)THEN
	  CALL LIB$GET_EF(EVFTIM)
	  IF(EVFTIM.LT.0)THEN
	    TYPE *,'CANNOT GET TIME EVENT FLAG'
	    CALL GSTOP(GEXIT_SUCCESS)
	  ENDIF
	  EVFMASK = IBSET(0, MOD(EVFTIM,32))
C
	  CALL LIB$GET_EF(EVFQIO)
	  IF(EVFQIO.LT.0)THEN
	    TYPE *,'CANNOT GET QIO EVENT FLAG'
	    CALL GSTOP(GEXIT_SUCCESS)
	  ENDIF
	  EVFMASK = IBSET(EVFMASK, MOD(EVFQIO,32))
	ENDIF
C
C Create pasteboard and virtual display
C Assign this crt to me, then assign 2 event flags.
C (also, initialize timer value)
C
	IF(CRTCHAN.LT.0)THEN
	  ST = SYS$ASSIGN('SYS$INPUT',CRTCHAN,,)
	  IF(.NOT.ST)CALL LIB$STOP(%VAL(ST))
C
	  ST = 0
	  ST = 1
	  ST = -1
	  ST = SMG$CREATE_PASTEBOARD(PASTEBOARD_ID, 'SYS$OUTPUT')
	  IF(.NOT.ST)CALL LIB$SIGNAL(%VAL(ST))
	  ST = SMG$CREATE_VIRTUAL_KEYBOARD(KEYBOARD_ID,'SYS$INPUT')
	  IF(.NOT.ST)CALL LIB$SIGNAL(%VAL(ST))
	  ST = SMG$ENABLE_UNSOLICITED_INPUT(PASTEBOARD_ID, INPUTAST,
     *                                      EVFQIO)
	  IF(.NOT.ST)CALL LIB$SIGNAL(%VAL(ST))
C
	  ST = SYS$BINTIM( '0 ::4.00', VAXDELAYNORM)      !3 SEC DELAY
	  IF(.NOT.ST)CALL LIB$STOP(%VAL(ST))
	  ST = SYS$BINTIM( '0 ::0.50', VAXDELAYSMAL)	  !SHORT DELAY
	  VAXDELAY(1) = VAXDELAYNORM(1)
	  VAXDELAY(2) = VAXDELAYNORM(2)
	ENDIF
C
C First, clear out OLDPAG and fill lines 1-23 of NEWPAG with blanks,
C then set command to MENU and call VPAGE.
C
	CALL FASTSET('20202020'X, I4OLDPAG(1,1),20*24)
	CALL FASTSET('20202020'X, I4NEWPAG(1,1),20*23)
	CALL FASTSET(0,VIS_BOLD,24)
	CALL FASTSET(0,OLD_VIS_BOLD,24)
	CXCMDBUF = 'MENU'
	CXNEWPAG(24) = CXCMDBUF
	CMDLEN   = 4
	GOTO 2000
C
C
C This is the main loop.  We wait for either input or a time interval
C then call VPAGE.FOR to execute the command
C
1000	CONTINUE
	IF(TYPWAIT.EQ.1)THEN
C
	  ST = SMG$READ_STRING(KEYBOARD_ID,CXCMDBUF,,LEN(CXCMDBUF),
     *                         TRM$M_TM_TRMNOECHO,,,CMDLEN)
	  IF(.NOT.ST)CALL LIB$STOP(%VAL(ST))
	ELSE
	  ST = SYS$SETIMR(%VAL(EVFTIM), VAXDELAY,,%VAL(1),)
	  IF(.NOT.ST)CALL LIB$STOP(%VAL(ST))
	  VAXDELAY(1) = VAXDELAYNORM(1)                !RESET DELAY
	  VAXDELAY(2) = VAXDELAYNORM(2)
C
	  ST = SYS$WFLOR(%VAL(EVFQIO), %VAL(EVFMASK))
	  IF(.NOT.ST)CALL LIB$STOP(%VAL(ST))
	  ST = SYS$READEF(%VAL(EVFQIO), ALLFLAGS)
	  IF(ST.EQ.SS$_WASSET)THEN
	    CALL SYS$CLREF(%VAL(EVFQIO))
	    ST = SMG$READ_STRING(KEYBOARD_ID,CXCMDBUF,,LEN(CXCMDBUF),
     *                           TRM$M_TM_TRMNOECHO,,,CMDLEN)
	    GOTINP = .TRUE.
	    CXNEWPAG(24) = CXCMDBUF
	  ELSE IF(ST.EQ.SS$_WASCLR)THEN
	    GOTINP = .FALSE.
	    CMDLEN = 0
	  ELSE
	    TYPE *,'ERROR WITH READEF'
	    CALL LIB$STOP(%VAL(ST))
	  ENDIF
	  ST = SYS$CANTIM(%VAL(1),)
	ENDIF
C
C Come here when timer has elapsed or command has been entered.
C
C Call VPAGE to format a screen into NEWPAG
C
2000	CONTINUE
	CALL PAGE
	CXNEWPAG(1)(41:80) = VISHEAD
	CALL TIME(CXTIMBUF)
C
C If routine set CLRFLG, clear the screen first
C
	IF(CLRFLG.NE.0)THEN
	  CLRFLG = 0
	  ST = SYS$QIOW ( , %VAL(CRTCHAN), %VAL(WRITEFUNCOD),
     *                   OUIOSB,,,
     *                   %REF(CLRSCRN), %VAL(LEN(CLRSCRN)),,
     *                   %VAL(0),,)
	  CALL FASTSET(0,I4OLDPAG(1,1),20*24)
	  VAXDELAY(1) = VAXDELAYSMAL(1)		!SMALL DELAY ONLY
	  VAXDELAY(2) = VAXDELAYSMAL(2)
	  GOTO 1000
	ENDIF
C
C If routine set STPFLG, just return to caller
C
	IF(STPFLG.NE.0)THEN
	  STPFLG = 0
	  GOTO 9000
	ENDIF
C
C Move cursor to beginning of screen
C
	ST = SYS$QIOW ( , %VAL(CRTCHAN), %VAL(WRITEFUNCOD),
     *                   OUIOSB,,,
     *                   %REF(HOMECUR), %VAL(LEN(HOMECUR)),,
     *                   %VAL(0),,)
C
C Now compare l line by line.  If a line is different, output the
C appropriate number of line feeds before outputting the new line.
C
	LSTLIN = 1
	DO 3900 LIN = 1, 23
	  IF(VIS_BOLD(LIN)) THEN
	     DISP_STR = DISP_BOLD
	  ELSE
	     DISP_STR = DISP_NORM
	  ENDIF
	  ST = SYS$QIOW ( , %VAL(CRTCHAN), %VAL(WRITEFUNCOD),
     *                   OUIOSB,,,
     *                   %REF(DISP_STR), %VAL(LEN(DISP_STR)),,
     *                   %VAL(0),,)
	  IF(CXNEWPAG(LIN).NE.CXOLDPAG(LIN).OR.
     *       VIS_BOLD(LIN).NE.OLD_VIS_BOLD(LIN))THEN
	    X = LIN - LSTLIN
	    ST = SYS$QIOW ( , %VAL(CRTCHAN), %VAL(WRITEFUNCOD),
     *                       OUIOSB,,,
     *                       %REF(CXNEWPAG(LIN)), %VAL(80),,
     *                       %VAL(ISHFT(X,16)),,)
	    CXOLDPAG(LIN) = CXNEWPAG(LIN)
	    OLD_VIS_BOLD(LIN) = VIS_BOLD(LIN)
	    LSTLIN = LIN
	  ENDIF
3900	CONTINUE
C
C Now position the cursor at the beginning of the last line
C
	X = 24 - LSTLIN
	IF(X.GT.0)THEN
	  ST = SYS$QIOW ( , %VAL(CRTCHAN), %VAL(WRITEFUNCOD),
     *                     OUIOSB,,,
     *                     %REF(NULLCHR), %VAL(1),,
     *                     %VAL(ISHFT(X,16)),,)
	ENDIF
	CALL FASTSET(0,VIS_BOLD,24)
C
C Now wait again
C
	GOTO 1000
C
C
C Come here if STPFLG is set
C
9000	CONTINUE
	ST = SYS$CANCEL(%VAL(CRTCHAN))
	RETURN
	END

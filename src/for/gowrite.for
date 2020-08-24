C
C *** SUBROUTINE GOWRITE ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]GOWRITE.FOV                                  $
C  $Date::   17 Apr 1996 13:27:40                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - gowrite.for ***
C
C V02 19-MAY-92 JWE CHECK BOUNDS OF LAN BEFORE WE USE THE VALUE TO DETERMINE LUN
C V01 07-SEP-90 MRM RELEASED FOR VAX
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C   Get SSAP from the buffer, then LAN and LUN.
C   Put the buffer in the right IOWRITE queue.
C   Check status, if busy ok, if not start it.
C   If something goes wrong, dump the buffer.
C
C Calling Sequence:
C   CALL GOWRITE(BUF)
C
C Input:
C   BUF - ETHR BUFFER, READY TO GO
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE GOWRITE(BUF)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:LANCOM.DEF'
        INCLUDE 'INCLIB:NMADEF.DEF'
C
        INCLUDE '($IODEF)'
	INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	BLENGTH,
     *			BUF,
     *			BYT,
     *			CONN,
     *			CTL,
     *			DESCRIP_P4(2),
     *			DSAP,
     *			INFOLEN,
     *			IOSTAT,
     *			K,
     *			LAN,
     *			LUN,
     *			OFF,
     *			SLOT,
     *			SSAP
C
	BYTE		ADDR(6)
C
	STRUCTURE	/OUTPUT_P5/
	  BYTE	DEST(6)
	  BYTE	SSAP
	END STRUCTURE
C
	RECORD /OUTPUT_P5/ DESTADDR
	RECORD /P4_STRUCT/ WRITE_PARM
C
C EXTERNAL DECLARATIONS
C
	EXTERNAL	LBUFTRAP			! AST FOR READ TRAPS
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C EXTRACT INFORMATION FROM FORMATTED BUFFER.
C
	CALL ILBYTE(SSAP, LANBUF(1, BUF), SSAPBEG - 1)
	SSAP = IAND(SSAP, '0000007F'X)
C
	CALL ILBYTE(DSAP, LANBUF(1, BUF), DSAPBEG - 1)
	DSAP = IAND(DSAP, '0000007F'X)
C
	CALL ILBYTE(CTL,  LANBUF(1, BUF), CTRLBEG - 1)
C
C GET LAN NUMBER & CHECK VALIDITY.
C
	CALL ILBYTE(LAN,  LANBUF(1, BUF), TLANBEG - 1)
	IF (LAN .LE. 0 .OR. LAN .GT. MAXLAN) THEN
	  CALL OPS('*** GOWRITE - INVALID LAN ***', LAN, 0)
	  CALL LANRELB(BUF)
	  GOTO 9999
	ENDIF
C
C GET LUN NUMBER & CHECK VALIDITY.
C
	LUN = LUNLAN(SSAP, LAN)
	IF (LUN .LE. 0) THEN
	  CALL OPS('*** GOWRITE - INVALID LUN ***', LUN, 0)
	  CALL LANRELB(BUF)
	  GOTO 9999
	ENDIF
C
C CHECK STATUS OF LAN AND SAP.
C
	IF (LANOPN(SSAP, LAN) .NE. LSOPN .OR.
     *      LOCLAN(SSAP, LAN) .NE. LANUP .OR.
     *      LANLAN(DSAP, LAN) .NE. LANUP) THEN
D	  TYPE *, '*** DROPPED FRAME ***'
D	  TYPE *, 'LAN:', LAN, ', LUN:', LUN, ', BUF:', BUF
D	  TYPE *, 'SSAP:', SSAP, ', DSAP:', DSAP
D	  IF (LANOPN(SSAP, LAN) .NE. LSOPN) TYPE *, 'NOT OPENED'
D	  IF (LOCLAN(SSAP, LAN) .NE. LANUP) TYPE *, 'LOCLAN NOT UP'
D	  IF (LANLAN(DSAP, LAN) .NE. LANUP) TYPE *, 'LANLAN NOT UP'
	  CALL OPS('*** GOWRITE - ' //
     *             'FRAME DROPPED WHEN SENDING TO DSAP ***',
     *              DSAP, LAN)
	  CALL LANRELB(BUF)
	  GOTO 9999
	ENDIF
C
C GET THE LENGTH OF THE DATA PACKET.
C
	CALL MOV2TOI4(INFOLEN, LANBUF(1, BUF), TYPEBEG - 1)
	INFOLEN = INFOLEN + MACHDLEN			! TOTAL BUFFER LEN
C
C IF LENGTH IS LESS THAN THE MINIMUM, PAD THE PACKET WITH ZEROS.
C
	BLENGTH = MAX0(INFOLEN, ETHLENMN)		! AT LEAST 60 BYTES
C
	IF (INFOLEN .LT. ETHLENMN) THEN			! INCLUDING HEADER
	  DO 100 BYT = INFOLEN, ETHLENMN - 1		! PAD ZEROES
	    CALL ISBYTE(0, LANBUF(1, BUF), BYT)
100	  CONTINUE
	ENDIF
C
	BLENGTH = MIN0(MAX0(INFOLEN - 20, ETHMINDT), ETHLENMF)
C
	LANBUF(LANOWN,BUF) = OWNWRITE
C
C EXTRACT DATA FROM BUFFER TO BUILD DRIVER HEADERS.
C
	WRITE_PARM.DSAP   = DSAP
	WRITE_PARM.CTL    = CTL
	WRITE_PARM.FILLER = 0
C
	DESCRIP_P4(1)     = 3
	DESCRIP_P4(2)     = %LOC(WRITE_PARM)
C
C SETUP P5 PARAMETER FOR DRIVER (DESTINATION ADDRESS)
C
	CALL MOVBYT(LANBUF(1, BUF), DABEG, DESTADDR.DEST,
     *              1, DAEND - DABEG + 1)
	DESTADDR.SSAP = SSAP
C
C GET SLOT NUMBER TO STORE STATUS OF I/O.
C
        WRITE_SLOT(SSAP, LAN) = MOD(WRITE_SLOT(SSAP, LAN), MAXWRITE) + 1
C
        SLOT = WRITE_SLOT(SSAP, LAN)
C
        WRITE_DATA(TRPDTA_SLOT,   SLOT, SSAP, LAN) = SLOT
        WRITE_DATA(TRPDTA_BUFFER, SLOT, SSAP, LAN) = -BUF
C
	TRAP_INFO(TRPINF_SAP, BUF) = SSAP
        TRAP_INFO(TRPINF_LAN, BUF) = LAN
C
	IF (LANTEST .EQ. 2) THEN
	  OFF = FRTYPBEG
	  WRITE(6, 910) BLENGTH, BUF,
     *                  (BLANBUF(K, BUF), K = OFF, OFF + BLENGTH - 19)
910	  FORMAT(X, 10('+'), ' BUFFER IN GOWRITE ', 10('+'),
     *           X, 'LENGTH: ', I5, /,
     *           X, 'BUFFER: ', I5, 1600(/, 12Z3.2))
        ENDIF
C
C START THE WRITE PROCEED.
C
D       CALL OPS(' LENGTH SENT GOWRITE: ',BLENGTH,0)
	IOSTAT = SYS$QIO(,					! EVENT FLAG
     *                   %VAL(LUN),				! CHANNEL
     *                   %VAL(IO$_WRITEVBLK),			! FUNCTION CODE
     *                   %REF(WRITE_IOSB(SLOT, SSAP, LAN)),	! STATUS BLOCK
     *                   %REF(LBUFTRAP),			! AST ADDRESS
     *                   WRITE_DATA_EQV(SLOT, SSAP, LAN),	! AST PARAMETER
     *                   BLANBUF(FRTYPBEG, BUF),		! BUFFER ADDRESS
     *                   %VAL(BLENGTH),				! BUFFER LENGTH
     *                   ,					! P3 PARAM
     *                   DESCRIP_P4,				! P4 PARAM
     *                   DESTADDR,				! P5 PARAM
     *                   )					! P6 PARAM
C
	IF (.NOT. IOSTAT) THEN
	  IF (LANSAPSTS(SSAP) .EQ. SAPUP)
     *      CALL OPS('*** GOWRITE - ERROR STARTING WRITE ***',
     *               IOSTAT, SSAP)
	  GOTO 9999
	ENDIF
C
	WRITECNT(LAN, SSAP) = WRITECNT(LAN, SSAP) + 1
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END

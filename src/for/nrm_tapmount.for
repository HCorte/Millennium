C
C SUBROUTINE TAPMOUNT
C $Log:   GXAFXT:[GOLS]TAPMOUNT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:27:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:48:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_tapeio.for **
C
C TAPEIO.FOR
C
C V06 01-AUG-91 MP  If QIO returns bad status, return it to caller.
C
C V05 01-AUG-91 MP  Code uses V05 instead of V04, so this is just a 
C		    space reserved.
C V04 04-JUN-91 TKO Do physical I/O - not logical I/O
C		    Mount tape as nomount_verify
C		    When reading or writing, if device unavailable
C		      encountered, retry one more time. (See below)
C V03 21-MAY-91 TKO Check to be sure tape is mounted when opened.
C                   Add routine TAPCHECK
C V02 24-APR-91 TKO Check for # of bytes to read/write greater than 65535.
C		    Add NOFTLSIG checking
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C **** Notes on changes for V05 ****
C
C Normally, we expect tapes to be mounted on the system using the following:
C

C	MOUNT/FOREIGN/NOMOUNT_VERIFY/NOASSIST XXXX
C
C The /NOMOUNT_VERIFY qualifier will guarantee that if a tape drive is
C offline when a read or write occurs, the driver will return to the caller
C with a device unavailable error (1A4).  If we mounted the tape with the
C mount verify, the driver would instead notify the operator of the need
C for a tape change and wait for the tape to be changed (forever, if necessary).
C Thus, when manually mounting a tape (or disk, for that matter) it should
C always be mounted using the /NOMOUNT_VERIFY option.
C
C Unfortunately, whenever a tape drive is taken offline and then put back
C online again, even if the tape is ready to go the driver will return
C 'device unavailable (1A4)' on the first access after coming back online.
C This serves as some kind of warning.  Because we do not desire this, I have
C changed the routines below, in V05, to automatically retry exactly once
C when a device unavailable status is returned.
C
C In order to guarantee that the driver performs in this way for both SCSI and
C other type tape drives, I had to change all Virtual I/O's to Physical I/O's.
C
C **** End of notes for V05 ****
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C	 TAPMOUNT- MOUNT A TAPE
C	 TAPDISMT- DISMOUNT A TAPE
C	 TAPCHECK- CHECK TO SEE IF TAPE IS MOUNTED
C	 TAPOPEN - OPEN A TAPE AND INITIALIZE FDB
C	   - SET BLOCK SIZE
C	 TAPCLOS - CLOSE A TAPE
C	 TAPUNLD - UNLOAD A TAPE
C
C        RTAPEW  - READ BLOCK WAIT FOR COMPLETION
C        WTAPEW  - WRITE BLOCK WAIT FOR COMPLETION
C        WTAPEQ  - WRITE BLOCK WITHOUT WAIT
C
C        WEOT    - WRITE END OF TAPE MARK
C        XREWIND - REWIND TAPE
C        BACK    - BACKSPACE TAPE ONE RECORD
C        FORWRD  - FORWARD SPACE TAPE ONE RECORD
C        BEOT    - BACKSPACE FILE MARK
C        FEOT    - FORWARD SPACE FILE MARK
C
C CALLING SEQUENCE
C	 CALL TAPMOUNT ('NAME',DENSITY,STATUS)
C	 CALL TAPDISMT ('NAME',STATUS)
C	 CALL TAPCHECK ('NAME',STATUS)
C	 CALL TAPOPEN  (FDB,'NAME',STATUS)
C	 CALL TAPINT   (FDB,DUMMY,NUMBYTES)
C	 CALL TAPCLOS  (FDB,STATUS)
C
C        CALL RTAPEW   (FDB,BUF,STATUS)
C        CALL WTAPEW   (FDB,BUF,STATUS)
C        CALL WTAPEQ   (FDB,BUF,OWNBUF,STATUS)
C        CALL WEOT     (FDB,STATUS)
C        CALL XREWIND  (FDB,STATUS)
C	 CALL TAPUNLD  (FDB,STATUS)
C        CALL BACK     (FDB,STATUS)
C        CALL FORWRD   (FDB,STATUS)
C        CALL BEOT     (FDB,STATUS)
C        CALL FEOT     (FDB,STATUS)
C
C PARAMETER DESCRIPTIONS
C
C        FDB      - FILE DESCRIPTOR BLOCK (7 WORDS)
C	 'NAME'   - NAME OF TAPE AS A CHARACTER STRING
C	 DENSITY  - TAPE DENSITY (1600 OR 6250) 0 = 6250
C	 DUMMY	  - FOR COMPATABILITY WITH CONCURRENT ONLY - IS IGNORED
C        NUMBYTES - MAXIMUM # OF BYTES IN A BLOCK
C        UNIT     - LOGICAL UNIT NUMBER
C        RECSIZ   - RECORD SIZE IN BYTES
C        BUF      - BUFFER FOR TRANSFER
C        OWNBUF   - OWN BUFFER FOR DOUBLE BUFFERING FAST WRITES
C        STATUS   - I/O STATUS   (0 - NO ERROR)
C                 (ON QUICK WRITE STATUS RETURNED IS FOR
C                  PREVIOUS WRITE)
C
C
C
C
C*********************************************************************
C
C *** TAPMOUNT	  MOUNT A TAPE
C
C*********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TAPMOUNT (DEVNAM, DENSITY, STATUS)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($MNTDEF)'
C
	CHARACTER   DEVNAM*(*)
	INTEGER*4   DENSITY
	INTEGER*4   STATUS
C
	INTEGER*4   ST
C
C
C The following structure is used to pass itemlist elements to SYS$MOUNT
C
	STRUCTURE /MOUNT_STRUC/
	  INTEGER*2	BUFLEN		    !LENGTH OF BUFFER
	  INTEGER*2	ITMCOD		    !ITEM CODE
	  INTEGER*4	BUFADR		    !BUFFER ADDRESS
	  INTEGER*4	RLNADR		    !RETURN LENGTH ADDRESS (NOT USED)
	END STRUCTURE
C
	RECORD	/MOUNT_STRUC/ ITEMLIST(10)  !UP TO 10 DIFFERENT ITEMS
C
	INTEGER*4	BLOCKSIZE	    !BLOCKSIZE BUFFER
	INTEGER*4	DENSBUF		    !DENSITY BUFFER
	INTEGER*4	FLAGS		    !MOUNTING FLAGS
C
	INTEGER*4	NOFTLSIG
	EXTERNAL	NOFTLSIG
C
C
	CALL LIB$ESTABLISH(NOFTLSIG)	    !No fatal errors
C
C Set default values for mounting
C
	IF(DENSITY.EQ.0)THEN
	  DENSBUF = 6250
	ELSE IF(DENSITY.EQ.6250)THEN
	  DENSBUF = 6250
	ELSE IF(DENSITY.EQ.1600)THEN
	  DENSBUF = 1600
	ELSE
	  TYPE *,IAM(),'BAD TAPE DENSITY = ',DENSITY
	  CALL LIB$SIGNAL(%VAL(0))
	  STATUS = -1
	  GOTO 9000
	ENDIF
C
	BLOCKSIZE = 65532		    !(MAX ALLOWED BY OS)
	FLAGS     = MNT$M_FOREIGN	    ! /FOREIGN
     *             +MNT$M_NOASSIST	    ! /NOASSIST
     *		   +MNT$M_MULTI_VOL	    ! /MULTI_VOLUME
     *             +MNT$M_TAPE_DATA_WRITE   ! /CACHE=TAPE_DATA
     *             +MNT$M_NOMNTVER	    ! /NOMOUNT_VERIFY
C
C Set all item codes and other information
C
	ITEMLIST(1).BUFLEN = 4
	ITEMLIST(1).ITMCOD = MNT$_BLOCKSIZE	!MAXIMUM BLOCK SIZE
	ITEMLIST(1).BUFADR = %LOC(BLOCKSIZE)
C
	ITEMLIST(2).BUFLEN = 4
	ITEMLIST(2).ITMCOD = MNT$_DENSITY	!TAPE DENSITY
	ITEMLIST(2).BUFADR = %LOC(DENSBUF)
C
	ITEMLIST(3).BUFLEN = LEN(DEVNAM)
	ITEMLIST(3).ITMCOD = MNT$_DEVNAM	!NAME OF DEVICE
	ITEMLIST(3).BUFADR = %LOC(DEVNAM)
C
	ITEMLIST(4).BUFLEN = 4
	ITEMLIST(4).ITMCOD = MNT$_FLAGS		!CONTROL FLAGS
	ITEMLIST(4).BUFADR = %LOC(FLAGS)
C
	ITEMLIST(5).BUFLEN = 0
	ITEMLIST(5).ITMCOD = 0			!END OF LIST
C
C Mount the tape
C
	ST = SYS$MOUNT(ITEMLIST)
	IF(.NOT.ST) THEN
	  TYPE *,IAM(),'FILE ', DEVNAM, ' MOUNT ERROR'
	  CALL LIB$SIGNAL(%VAL(ST))
	  STATUS = -1
	  GOTO 9000
	ENDIF
C
	STATUS = 0
C
9000	CONTINUE
	RETURN
	END

C
C FUNCTION DISKOPENY
C $Log:   GXAFXT:[GOLS]DISKOPENY.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:53:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   09 Mar 1994 12:57:50   JXP
C  Initial revision.
C  
C     Rev 1.0   21 Jan 1993 16:05:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskio.for **
C
C
C
C
C *** DISKOPENY
C
C -----------------------------------------------------------------------------
C	This routine is called by an OPEN statement using the USEROPEN parameter
C	so that the channel number of the opened unit can be returned through
C	the common block.
C
C	Inputs:
C		fab - longword passed to us by RMS
C		rab - longword passed to us by RMS
C		lun - longword passed to us by RMS
C
C	Outputs:
C		LUNARRAY is set with channel # for this lun
C		XSTAT in common block set to error code
C
C	Returns:
C		longword indicating the status of the create
C -----------------------------------------------------------------------------
C
	INTEGER*4 FUNCTION DISKOPENY(FAB, RAB, LUN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DISKIO.DEF'
C
	INCLUDE	'($SYSSRVNAM)'
	INCLUDE '($FABDEF)'
	INCLUDE '($RABDEF)'
	INCLUDE '($RMSDEF)'
C
	INTEGER*4 LUN
C
	RECORD /FABDEF/ FAB
	RECORD /RABDEF/ RAB
C
C Set the block I/O bit in the FAC
C
	FAB.FAB$B_FAC = FAB.FAB$B_FAC .OR. FAB$M_BIO .OR. FAB$M_GET
C
C Set complete access privileges
C
	FAB.FAB$B_SHR = FAB.FAB$B_SHR .OR. FAB$M_UPI
C
C Now open the file and connect to the record stream
C
	DISKOPENY = SYS$OPEN(FAB)
	IF(.NOT.DISKOPENY) THEN
	  CALL LIB$SIGNAL(%VAL(DISKOPENY))
	  GOTO 9000
	ENDIF
C
	DISKOPENY = SYS$CONNECT(RAB)
	IF(.NOT.DISKOPENY) THEN
	  CALL LIB$SIGNAL(%VAL(DISKOPENY))
	  GOTO 9000
	ENDIF
C
9000	CONTINUE
	RETURN
	END

C FUNCTION DISKOPEN
C
C V03 04-JUL-2000 OXK RAB -> RAB64
C V02 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V01 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C
C -----------------------------------------------------------------------------
C	This routine is called by an OPEN statement using the USEROPEN parameter
C	so that the channel number of the opened unit can be returned through
C	the common block.
C	Inputs:
C		fab - longword passed to us by RMS
C		RAB - longword passed to us by RMS
C		lun - longword passed to us by RMS
C	Outputs:
C		LUNARRAY is set with channel # for this lun
C		XSTAT in common block set to error code
C	Returns:
C		longword indicating the status of the create
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
	INTEGER*4 FUNCTION DISKOPEN(FAB, RAB, LUN)
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
	FAB.FAB$B_FAC = FAB.FAB$B_FAC .OR. FAB$M_BIO
C
C Set complete access privileges
C
	FAB.FAB$B_SHR = FAB.FAB$B_SHR .OR. FAB$M_UPI
C
C Now open the file and connect to the record stream
C
	DISKOPEN = SYS$OPEN(FAB)
	IF(.NOT.DISKOPEN) THEN
	  CALL LIB$SIGNAL(%VAL(DISKOPEN))
	  GOTO 9000
	ENDIF
C
	DISKOPEN = SYS$CONNECT(RAB)
	IF(.NOT.DISKOPEN) THEN
	  CALL LIB$SIGNAL(%VAL(DISKOPEN))
	  GOTO 9000
	ENDIF
C
9000	CONTINUE
	RETURN
	END

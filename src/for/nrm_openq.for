C SUBROUTINE OPENQ
C
C V03 13-MAR-1998 UXN Initialization for FAB corrected.
C V02 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C V01 21 Jan 1993 DAB Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C -----------------------------------------------------------------------------
C	This routine calls SYS$OPEN
C	so that the channel number of the opened unit can be returned through
C	the common block.
C	Inputs:
C		lun - longword passed to us by RMS
C		fname - file name
C		FAB  - fab record area
C		mode - QREAD or QWRITE
C	Outputs:
C		LUNARRAY is set with channel # for this lun
C		ST - longword indicating the status of the OPEN
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
	SUBROUTINE OPENQ(LUN, FNAME, FAB, MODE, ST)
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DISKQIO.DEF'
C
	INCLUDE	'($SYSSRVNAM)'
	INCLUDE '($RMSDEF)'
C
	INTEGER*4	LUN
	CHARACTER*(*)	FNAME
	RECORD	/FABDEF/ FAB
	INTEGER*4	MODE
	INTEGER*4	ST
C
	INTEGER*4 CHAN
C
	CHARACTER*63	DFLTNAM
C
C -----------------------------------------------------------------------------
C
	CALL LIB$MOVC5(0,0,0,SIZEOF(FAB),FAB)
C
C	SET FAB ID AND LENGTH
C
	FAB.FAB$B_BID = FAB$C_BID
	FAB.FAB$B_BLN = FAB$C_BLN
C
	FAB.FAB$B_ACMODES = FAB.FAB$B_ACMODES .OR.
     *			      ISHFT(0, FAB$V_CHAN_MODE)
 
C	FILE NAME
	FAB.FAB$L_DNA	= %LOC(DFLTNAM)
C
	FAB.FAB$B_FAC = FAB.FAB$B_FAC
     *			 .OR. ISHFT(1, FAB$V_BIO)
     *			 .OR. ISHFT(0, FAB$V_BRO)
     *			 .OR. ISHFT(0, FAB$V_DEL)
     *			 .OR. ISHFT(1, FAB$V_GET)
     *			 .OR. ISHFT(1, FAB$V_PUT)
     *			 .OR. ISHFT(0, FAB$V_TRN)
     *			 .OR. ISHFT(0, FAB$V_UPD)
C	FILE NAME
	FAB.FAB$L_FNA	= %LOC(FNAME)
	FAB.FAB$B_FNS   = LEN(FNAME)
C
	FAB.FAB$L_FOP=FAB.FAB$L_FOP .OR. FAB$M_UFO  ! SET USEROPEN BIT
C
	FAB.FAB$B_SHR=FAB.FAB$B_SHR .OR. FAB$M_SHRPUT
	FAB.FAB$B_SHR=FAB.FAB$B_SHR .OR. FAB$M_SHRGET
	FAB.FAB$B_SHR=FAB.FAB$B_SHR .OR. FAB$M_SHRDEL
	FAB.FAB$B_SHR=FAB.FAB$B_SHR .OR. FAB$M_SHRUPD
	FAB.FAB$B_SHR=FAB.FAB$B_SHR .OR. FAB$M_MSE
	FAB.FAB$B_SHR=FAB.FAB$B_SHR .OR. FAB$M_UPI
C
	ST = SYS$OPEN(FAB)
	IF(.NOT.ST) THEN
	    CALL LIB$SIGNAL(%VAL(ST))
	ENDIF
C
	CHAN=FAB.FAB$L_STV
	IF(LUN.LT.1 .OR. LUN.GT.MAXLUNARRAY)THEN
	  TYPE *,IAM(),'BAD LUN IN VAX_DISKQIO/DISKQOPEN = ',LUN
	  GOTO 9000
	ENDIF
C
C Save important info about opened file: channel, file size
C
	LUNARRAY(LUN) = CHAN
C
	FABFILSZ(LUN) = FAB.FAB$L_ALQ
C
9000	CONTINUE
	RETURN
	END

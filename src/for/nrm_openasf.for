C
C SUBROUTINE OPENASF
C $Log:   GXAFXT:[GOLS]OPENASF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:18:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:11:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_asfsubs.for **
C
C ASFSUBS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C These subroutines provide a method of handling reads and
C writes to the ASF.
C
C To open the ASF:
C
C     CALL OPENASF(XLUN)
C
C     Input:    XLUN: Logical unit number
C                     (This will open ASF.FIL on the current pack)
C
C To read an agent's record
C
C     CALL READASF(AGT,AGTREC,ST)
C
C     Input:      AGT: Slot # in the ASF
C     Output:  AGTREC: Array to contain full agent record
C
C To write a record:
C
C     CALL WRITASF(AGT,AGTREC,ST)
C
C     Input:      AGT: Slot # in the ASF
C     Output:  AGTREC: Array containing agent record
C
C (Note that the writes are buffered, therefore you must call
C  CLOSASF to write out the last bunch of records).
C
C To close the file:
C
C     CALL CLOSASF
C
C     *** This MUST be called if any writes were done ***
C
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE OPENASF(XLUN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:ASFSUBS.DEF'
C
	INTEGER*4 XLUN, ST
C
	LUN=XLUN
	CALL OPENW(LUN,SFNAMES(1,ASF),4,0,0,ST)
	IF(ST.NE.0)THEN
	  CALL FILERR(SFNAMES(1,ASF),1,ST,0)
	  TYPE *,IAM(),'  ASFSUBS--> ERROR OPENING  ASF.FIL'
	ENDIF
C
	CALL IOINIT(FDB,LUN,ASFSEC*RECSPERBKT*256)
C
	BKTNUM=-1                   !FORCE A READ
	BKTCHG=0                    !NOTHING HAS BEEN CHANGED
C
	RETURN
	END

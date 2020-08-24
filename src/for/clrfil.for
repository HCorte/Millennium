C
C PROGRAM CLRFIL
C $Log:   GXAFXT:[GOLS]CLRFIL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:36:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:54:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - clrfil.for **
C
C CLRFIL.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This will clear a contiguous file
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
	PROGRAM CLRFIL
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER*60	FILENAME
	INTEGER*4	ST
C
	CALL COPYRITE
C
C
	CALL WIMG(5,'ENTER FILE NAME')
	ACCEPT 101,FILENAME
101	FORMAT(A)
C
	CALL SUBCLRFIL(1, FILENAME, ST)
	IF(ST.NE.0)THEN
	  CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
	CALL GSTOP(GEXIT_SUCCESS)
C
	END

C
C SUBROUTINE NEWFIL
C $Log:   GXAFXT:[GOLS]NEWFIL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:11:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   09 Jan 1994 18:27:54   HXK
C  New call parameter, KEEP, which determines whether file should be
C  allocated.
C  
C     Rev 1.0   21 Jan 1993 17:06:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_newfil.for **
C
C NEWFIL.FOR
C
C V02 24-OCT-91 TKO CALL SUBCLRFIL to clear the file
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This will create a contiguous file and initialize it to 0's
C
C LUN      is the logical unit # to use
C FILENAME must be a character string
C SECTORS  is the number of VAX sectors
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
	SUBROUTINE NEWFIL(LUN, FILENAME, SECTORS, KEEP, ST)
	IMPLICIT NONE
C
	INTEGER*4	LUN
	CHARACTER	FILENAME*(*)
	INTEGER*4	SECTORS
	INTEGER*4	ST
C
        LOGICAL         KEEP
C
	IF(SECTORS.LT.1)THEN
	  TYPE *,'NEWFIL: NUMBER OF SECTORS < 1'
	  ST = -1
	  GOTO 9000
	ENDIF
C
C	Create the file using CFILX only if we're not using the existing one
C
	IF(.NOT.KEEP) THEN
	   CALL CFILX(FILENAME, 0, 0, SECTORS*2, 0,0,0, ST)
	   IF(ST.NE.0) THEN
	     GOTO 9000
	   ENDIF
	ENDIF
C
C Now re-open the file and fill it with zeroes
C
	CALL SUBCLRFIL(LUN, FILENAME, ST)
C
9000	CONTINUE
	RETURN
C
	END

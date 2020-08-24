C
C SUBROUTINE IREAD
C $Log:   GXAFXT:[GOLS]IREAD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:40:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:42:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshrndi4.for **
C
C HSHRNDI4.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C B04 04-OCT-88 TKO  SET MYBLK & MYLUN TO 0 ON FIRST CALL TO IREAD
C                    OR IWRITE TO INVALIDATE BLOCK IN MEMORY
C B03 14-SEP-88 BAB  force new findkey per #tko88005
C B02 11-MAR-88 TKO  IF USER CALLED ITUBSIZE, TREAT AS ERROR
C B01 25-MAY-86 TKO  INITIAL RELEASE
C
C This is a set of subroutines to do random reads/writes
C to a hashed file.
C
C
C
C
C *** IREAD
C
C This will get the record corresponding to the indicated keys.
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
	SUBROUTINE IREAD(XKEYS,I4REC,LUN,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 XKEYS(2)             !  INPUT: (1)=I2KEY (AS I*4)
C                                    ;         (2),I4KEY
	INTEGER*4 I4REC(*)             ! OUTPUT: TO HOLD RECORD
	INTEGER*4 LUN                  !  INPUT: LOGICAL UNIT #
	INTEGER*4 STATUS               ! OUTPUT: 0=OK, ELSE ERROR #
C
	INTEGER*4 RECI4LEN, I4KEY, I2KEY
C
C
C
	I2KEY=XKEYS(1)
	I4KEY=XKEYS(2)
C
	CALL IREADR(LUN,I4KEY,I2KEY,I4REC,RECI4LEN,STATUS)
C
	RETURN
	END

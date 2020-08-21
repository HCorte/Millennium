C
C SUBROUTINE READTCF
C $Log:   GXAFXT:[GOLS]READTCF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:39:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:27:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - readtmf.for **
C
C
C
C SUBROUTINE TO READ TRANSACTION FROM CARRYOVER FILE
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE READTCF(LOGREC,LU,EOT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:HSHCOM.DEF'
	INTEGER*4 TCFBUF(I4BUCSIZ),LOGREC(*), ST, LU
	LOGICAL EOT
C
C
	CALL ISREAD(LOGREC,LU,TCFBUF,ST)
	IF(ST.EQ.ERREND) THEN
	  EOT=.TRUE.
	  RETURN
	ENDIF
C
C
	IF(ST.NE.0) THEN
	  TYPE*,'Carryover read error ',ST
	  CALL GPAUSE
	  EOT=.TRUE.
	  RETURN
	ENDIF
	RETURN
	END

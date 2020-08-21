C
C SUBROUTINE READTMF
C $Log:   GXAFXT:[GOLS]READTMF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:39:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:27:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - readtmf.for **
C
C READTMF.FOR
C
C V02 14-FEB-91 KWP ADDED ROUTINE READDRW, ADDED LU FOR READTCF
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C A SET OF SUBROUTINES TO READ FROM THE TMF OR TCF FILES
C
C READTMF  - READS A RECORD FROM THE TMF FILE
C READDRW  - READS A RECORD FROM A SAVDRW FILE
C READTCF  - READS A RECORD FROM THE TCF FILE
C READTAPE - READS A RECORD FROM A TMFDMP OR TMFLOG TAPE
C
C
C SUBROUTINE TO READ TRANSACTIONS FROM TM FILE
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
	SUBROUTINE READTMF(LOGREC,SER,EOT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*4 LOGREC(*), ST, DUMMY, EOTCNT, SER
	LOGICAL EOT
	DATA EOTCNT/0/
C
C
10	CONTINUE
	CALL RLOG(SER,LOGREC,DUMMY,ST)
	SER=SER+1
	IF(ST.GT.0) GOTO 10
	IF(ST.LT.0) THEN
	  EOT=.TRUE.
	  RETURN
	ENDIF
C
C CHECK FOR END OF FILE
C
	IF(LOGREC(1).EQ.0) THEN
	  EOTCNT=EOTCNT+1
	  IF(EOTCNT.GT.5000) THEN
	    EOT=.TRUE.
	    RETURN
	  ENDIF
	  GOTO 10
	ELSE
	  EOTCNT=0
	ENDIF
	RETURN
	END

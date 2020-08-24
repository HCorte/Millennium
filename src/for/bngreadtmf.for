C
C SUBROUTINE BNGREADTMF
C $Log:   GXAFXT:[GOLS]BNGREADTMF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:20:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   10 Dec 1994 15:55:56   HXK
C  Initial revision.
C  
C
C BNGREADTMF.FOR
C
C
C BNGREADTMF  - READS A RECORD FROM THE TMF FILE FOR BINGO
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BNGREADTMF(LOGREC,SER,EOT)
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
	  IF(EOTCNT.GT.2000) THEN
	    EOT=.TRUE.
	    RETURN
	  ENDIF
	  GOTO 10
	ELSE
	  EOTCNT=0
	ENDIF
	RETURN
	END

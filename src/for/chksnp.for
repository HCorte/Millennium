C
C SUBROUTINE CHKSNP
C $Log:   GXAFXT:[GOLS]CHKSNP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:33:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   12 Dec 1995 15:08:10   PXB
C  Changed for new menu in vision
C  
C     Rev 1.1   11 Jun 1993 17:18:34   HXK
C  ADDED AGTINF.DEF, PRMAGT.DEF
C  
C     Rev 1.0   21 Jan 1993 15:51:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_chksnp.for **
C
C CHKSNP.FOR
C
C V02 25-OCT-91 GCAN INCREASED NUMBER OF SNAPSHOTS TO 120
C V01 06-NOV-90 DSL  INITIAL RELEASE MASS
C
C
C
C SUBROUTINE TO DETERMINE ID SNAPSHOT IS AVAILABLE FOR THIS VISION
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
	SUBROUTINE CHKSNP(KEY,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
C
	INTEGER*4 I, ST, KEY
C
	ST=-1
	DO 10 I=1,168
	  IF(SNAPS(I,PASS).EQ.KEY) GOTO 20
C**   IF(SNAPS(I,PASS).EQ.0) RETURN
10	CONTINUE

	RETURN
C
C
20	CONTINUE

	ST=0

	RETURN
	END

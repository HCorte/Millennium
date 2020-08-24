C
C SUBROUTINE ASCASC
C $Log:   GXAFXT:[GOLS]ASCASC.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:12:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:39:44   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_ascasc.for **
C
C ASCASC.FOR
C
C V01 12-DEC-90 TKO  INITIAL RELEASE
C
C
C       CONVERT AN ASCII STRING TO ASCII IN PLACE, SETTING BLANKS FOR NON-
C       PRINTING CHARACTERS
C
C       CALL ASCASC(STRING,ERR)
C                   STRING=ASCII INPUT/OUTPUT STRING
C                   ERR   = 0 IF ALL ASCII CHARS, -1 IF NON ASCII
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
	  SUBROUTINE ASCASC(STRING,ERR)
	  IMPLICIT NONE
C
	  CHARACTER STRING*(*)
	  INTEGER*4 ERR
C
	  INTEGER*4 I
C
C
C
C
	  ERR=0
	  DO 1010 I=1,LEN(STRING)
	    IF(ICHAR(STRING(I:I)).LT.32) THEN
	      STRING(I:I)=' '
	      ERR=-1
	    ENDIF
1010	  CONTINUE
C
C
8000	  RETURN
C
	  END

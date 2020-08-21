C
C SUBROUTINE PRMYESNO
C $Log:   GXAFXT:[GOLS]PRMYESNO.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:32:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:22:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_inpyesno.for **
C
C INPYESNO.FOR
C
C V02 02-APR-01 CS   INCLUDED 'S[im]' (YES) FOR PORTUGUESE MESSAGES
C V01 02-APR-91 TKO  INITIAL RELEASE
C
C This program is used to output a message and wait for a yes/no/exit
C response.
C
C Calling sequence:
C
C	CALL INPTEXT(OUTPUTSTRING, YESNOFLAG)
C
C INPUT:
C	OUTPUTSTRING	This is the text string to output as a prompt.  It
C			must be a character string whose length can be
C			determined by the LEN(X) function.
C
C OUTPUT:
C	YESNOFLAG	1 = Y[es]  was entered (S[im] for Portugal)
C		        2 = N[o]   was entered (N[ao] for Portugal)
C			3 = E[xit] was entered
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
	SUBROUTINE PRMYESNO(OUTPUTSTRING, YESNOFLAG)
	IMPLICIT NONE
C
	CHARACTER   OUTPUTSTRING*(*)
	INTEGER*4   YESNOFLAG
C
	CALL XXXYESNO(.TRUE., OUTPUTSTRING, YESNOFLAG)
	RETURN
	END

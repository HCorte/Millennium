C
C SUBROUTINE NQIMAGE
C $Log:   GXAFXT:[GOLS]NQIMAGE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:14:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:09:18   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_nqimage.for **
C
C NQIMAGE.FOR
C
C V02 08-MAR-91 TKO CALL LISTSIZE
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C SUBROUTINE TO BUILD SYSTEM QUEUE IMAGES FOR VISION
C QUEUE SNAPSHOT.
C
C CALLING SEQUENCE:
C     CALL  NQIMAGE(LIST,IMAGE,LENGTH)
C INPUT
C     LIST   - WS LIST DATA STRUCTURE.
C     LENGTH - LENGTH OF QUEUE IMAGE
C OUTPUT
C     IMAGE  - QUEUE IMAGE
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
	SUBROUTINE   NQIMAGE(LIST,IMAGE,LENGTH)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
C
	INTEGER*4   LIST(*)
	INTEGER*4   LENGTH
	INTEGER*4   IMAGE(LENGTH)
 
	INTEGER*4 I, LIMIT, TOP, USED
	INTEGER*4 J, IND
C
C CLEAR QUEUE IMAGE
C
	DO 10 I=1,LENGTH
	  IMAGE(I)=0
10	CONTINUE
C
C GET PARAMETERS FROM TOP OF LIST
C
	LIMIT = LIST(GLIST_MAX_OFFSET) - GLIST_START
	IF (LIMIT .LE. 0) THEN
	    IMAGE(1) = -1
	    RETURN
	ENDIF
	TOP = LIST(GLIST_TOP)
	CALL LISTSIZE( LIST, USED )
C
C
C BUILD NEW QUEUE IMAGE
C
	IMAGE(1)=USED
	IF (USED.EQ.0)     GOTO 9000
	IF (LENGTH .EQ. 1) GOTO 9000
C
	J=2
	IND = TOP
	DO 20 I = 1, USED
	  IND = IND + 1
	  IF(IND.GT.LIST( GLIST_MAX_OFFSET))THEN
	    IND = GLIST_START
	  ENDIF
	  IMAGE(J) = LIST(IND)
	  J = J + 1
	  IF( J.GT.LENGTH ) GOTO 9000
20	CONTINUE
C
9000	CONTINUE
	RETURN
	END

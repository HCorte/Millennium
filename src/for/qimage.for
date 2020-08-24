C
C SUBROUTINE QIMAGE
C $Log:   GXAFXT:[GOLS]QIMAGE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:35:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:24:46   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_qimage.for **
C
C QIMAGE.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C SUBROUTINE TO BUILD SYSTEM QUEUE IMAGES FOR VISION
C QUEUE SNAPSHOT.
C
C CALLING SEQUENCE:
C     CALL QIMAGE(LIST,IMAGE,LENGTH)
C INPUT
C     LIST   - PE LIST DATA STRUCTURE.
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
	SUBROUTINE QIMAGE(LIST,IMAGE,LENGTH)
	IMPLICIT NONE
	INTEGER*4 LIST(*), LENGTH, IMAGE(LENGTH)
 
	CALL NQIMAGE(LIST,IMAGE,LENGTH)
 
	RETURN
	END

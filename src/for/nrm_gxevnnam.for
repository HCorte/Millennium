C
C FUNCTION GXEVNNAM
C $Log:   GXAFXT:[GOLS]GXEVNNAM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:28:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:34:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gxevnnam.for;3 **
C
C GXEVNNAM.FOR
C
C V01 16-APR-92 MRM INITIAL RELEASE.
C
C This function should be called by any program which utilizes
C common event flag clusters.  This will allow the event cluster
C name to be unique per system, allowing multiple system to run
C on the same machine.
C
C Calling sequence:
C
C	GXEVNNAM(EVNNAM)
C
C Input parameters:
C
C	EVNNAM	    CHARACTER*(*)	Common event name
C
C Output parameters:
C
C	GXEVNNAM    CHARACTER*20	Common event name with project
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
	FUNCTION GXEVNNAM
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'	
C
	INTEGER*4  PROJ_PREFIX		    !Project prefix
        INTEGER*4  PROJ_PREFIX_LEN	    !Length of prefix
	CHARACTER  GXEVNNAM*4		    !Output event name (with project)
	CHARACTER  CPROJ_PREFIX*4	    !Project prefix
C
	EQUIVALENCE (PROJ_PREFIX,CPROJ_PREFIX)
C
C GET THE PROJECT PREFIX.
C
	CALL GETPRFX(PROJ_PREFIX,PROJ_PREFIX_LEN)
C
C CREATE THE NEW COMMON EVENT FLAG NAME.
C
	GXEVNNAM = CPROJ_PREFIX(1:PROJ_PREFIX_LEN)
C
	RETURN
	END

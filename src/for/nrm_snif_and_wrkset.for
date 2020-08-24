C
C SUBROUTINE SNIF_AND_WRKSET
C $Log:   GXAFXT:[GOLS]SNIF_AND_WRKSET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:10:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:39:30   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_snif.for **
C
C NRM_SNIF.FOR
C
C V01 09-MAY-91 MP  INITIAL RELEASE
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
C-----------------------------------------------------------------------
C
C SUBROUTINE SNIF_AND_WRKSET() FINDS AND SETS WORKING SET SIZE (CURRENTLY
C				WITHOUT LOCKING)
C
C INTEGER*4 FUNCTION SNIF(LOCK_FLAG)
C
C INPUT:
C	INTEGER*4 LOCK_FLAG -
C		  IF = 1    CAUSES IDENTIFIED PAGES TO BE LOCKED IN
C			    MEMORY
C		  IF = 0    DOES NOT LOCK PAGES IN MEMORY
C OUTPUT:
C	INTEGER*4 FUNCTION VALUE -
C			    NUMBER OF PAGES FOUND IN THE VIRTUAL ADDRESS
C			    SPACE
C
C THIS FUNCTION TOUCHES ALL VIRTUAL PAGES IN THE PROCESS'S SPACE AND
C RETURNS THE NUMBER OF PAGES TOUCHED. THE VALUE RETURNED MAY BE USED
C AS AN ESTIMATE OF THE WORKING SET SIZE WHEN ADJUSTING THE WORKING SET.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C SNIF_AND_WRKSET()
C
C THIS SUBROUTINE CALLS 'SNIF' TO FIND THE SIZE OF THE WORKING SET NEEDED
C AND SETS THE WORKING SET TO THE REQUIRED VALUE
C
C-------------------------------------------------------------------------
C
C=======OPTIONS/CHECK=NOOVERFLOW
	SUBROUTINE  SNIF_AND_WRKSET()
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   SNIF
	EXTERNAL    SNIF
C
	INTEGER*4   PAGES_SNIFED, MAXEXTENT
C
	CALL GET_WSEXTENT(MAXEXTENT)
	CALL WRKSET(MAXEXTENT)	    ! THIS COULD BE IMPROVED BY
				    ! USING THE VALUE FROM EXTENT
	PAGES_SNIFED = SNIF(0)
C
	PAGES_SNIFED = MAX(PAGES_SNIFED,1000)
	CALL WRKSET(PAGES_SNIFED)
C
	RETURN
	END

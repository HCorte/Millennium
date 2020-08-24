C
C SUBROUTINE CLOSLOD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CLOSLOD.FOV                                  $
C  $Date::   17 Apr 1996 12:36:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2lodsub.for;1 **
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     ========================================================
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CLOSLOD(LUN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2LODSUB.DEF'
C
	INTEGER*4  ST
C
	IF(BKTCHG(LUN).NE.0)THEN
	  CALL WRITEW(FDB(1,LUN),BKTNUM(LUN),BUCKET(1,1,LUN),ST)
	  IF(ST.NE.0)THEN
	    CALL OS32ER(5,X2XNAM(LUN),'WRITEW',ST,BKTNUM(LUN))
	    CALL GPAUSE
	    RETURN
	  ENDIF
	ENDIF
C
	CALL CLOSEFIL(FDB(1,LUN))
C
	RETURN
	END

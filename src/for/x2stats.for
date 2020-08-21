C
C SUBROUTINE X2STATS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STATS.FOV                                  $
C  $Date::   17 Apr 1996 16:35:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2stats.for;1 **
C
C X2STATS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C     X2STATS.FTN
C     ___________
C
C V01 16-JUL-91 WS   INITIAL RELEASE
C
C     CALL X2STATS             UPDATE SOME STATS
C
C     IN:
C     NONE
C     OUT:
C     NONE
C
C
C COPYRITF.DEF+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C
C CONFIDENTIAL PROPRIETARY INFORMATION
C This item is the property of GTECH Corporation, W. Greenwich, Rhode
C Island, and contains confidential and trade secret information.  It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH.  Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published or disclosed, in whold or in part, directly
C or indirectly, except as expressly authorized by an officer of
C GTECH pursuant to written agreement.
C COPYRITF.DEF-------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2STATS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
        INTEGER*4 NEXT_POINTER
C
	NEXT_POINTER=X2XS_STATS_PNT+1
	IF (NEXT_POINTER.GT.2) NEXT_POINTER=1
C
C     CLEAR ACTIVE COUNTS
C
	CALL FASTSET(0,X2XS_CNT_ACTIVE_LAST(1,NEXT_POINTER),
     *	               X2X_STATIONS/2)
C
C
	IF (NEXT_POINTER.EQ.1) THEN
	   CALL FASTSET(0,X2XS_ACK_CNT_LAST1,X2X_STATIONS/4)
	   CALL FASTSET(0,X2XS_ERR_CNT_LAST1,X2X_STATIONS/4)
	ELSE
C
	   CALL FASTSET(0,X2XS_ACK_CNT_LAST2,X2X_STATIONS/4)
	   CALL FASTSET(0,X2XS_ERR_CNT_LAST2,X2X_STATIONS/4)
	ENDIF
C
C
	X2XS_STATS_PNT=NEXT_POINTER
	RETURN
	END

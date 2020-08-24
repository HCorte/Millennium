C
C SUBROUTINE X2MSGSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2MSGSTN.FOV                                 $
C  $Date::   17 Apr 1996 16:24:18                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2rcvbuf.for;1 **
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++
C
C     X2MSGSTN(MESSAGE,STATION_NO) ;SET STATION NO IN X2XPRO MESSAGE
C
C     IN:
C     STATION_NO
C     OUT:
C     MESSAGE        - STATION_NO SET IN THE MESSAGE
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2MSGSTN(MESSAGE,STATION_NO)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INTEGER*2 MESSAGE(*)
	INTEGER*4 STATION_NO
C
	CALL I4TOBUF2(STATION_NO,MESSAGE,X2PRO_STATION-1)
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	   TYPE *,'X2MSGSTN ',STATION_NO
	RETURN
	END

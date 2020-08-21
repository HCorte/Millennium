C
C SUBROUTINE X2SETFLG
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2SETFLG.FOV                                 $
C  $Date::   17 Apr 1996 16:34:02                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2sndbuf.for;2 **
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     X2SETFLG(DSAP,FLAG)
C     IN:
C     DSAP     -  DESTINATION SAP
C     OUT:
C     FLAG    -  TRANSPORT OUTPUT FLAG
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
	SUBROUTINE X2SETFLG(DSAP,FLAG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2TDBH.DEF'
C
	INTEGER*4  FLAG, DSAP
C
C
	FLAG=X2TDBHF_DEFAULT
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	    TYPE *,'X2SETFLAG ',DSAP,FLAG
	RETURN
	END

C
C SUBROUTINE X2ERROR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2ERROR.FOV                                  $
C  $Date::   17 Apr 1996 16:16:02                                         $
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
C++++++++++++++++++++++++++++++++++++++++
C
C     X2ERROR SUBROUTINE
C     X2ERROR(ERROR,PAR0,PAR1,PAR2)  ;REPORT ERROR, TEMPORARY ROUTINE
C     IN:
C        ERROR       - ERROR REPROTED
C        PAR1        - FIRST PARAMETER (USUALY SAP)
C        PAR2        - ADDITIONAL PARAMETER
C        PAR3        - ADDITIONAL PARAMETER
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
	SUBROUTINE X2ERROR(ERROR,PAR1,PAR2,PAR3)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
C	INCLUDE 'INCLIB:X2XPTL.DEF'
	INCLUDE 'INCLIB:X2PTLMES.DEF'
C
	INTEGER*4    PAR3, PAR2, PAR1, ERROR
	INTEGER*4    STATUS
	CHARACTER*40 WRITE_FORMAT
	CHARACTER*40 DLL_MESSAGE
	
	DATA WRITE_FORMAT/ '(1X,''status '',I,'' params '',Z8,Z8,Z8)'/
C
	   IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	      TYPE *,'X2XERROR ',ERROR
C
	IF (ERROR.EQ.X2ERR_DLL) THEN
	  CALL X2DLLNAME(PAR3,DLL_MESSAGE,STATUS)
	  IF(STATUS.EQ.0) THEN
	    CALL OPS(DLL_MESSAGE,PAR1,PAR2)
	  ELSE
	    CALL OPS1(X2X_PTLMES(ERROR),ERROR,PAR1,PAR2,PAR3,WRITE_FORMAT)
	  ENDIF
	ELSE  
	  IF (ERROR.EQ.X2ERR_TDBH_CMD_MAINTENANCE) RETURN
	  CALL OPS1(X2X_PTLMES(ERROR),ERROR,PAR1,PAR2,PAR3,WRITE_FORMAT)
	ENDIF
C
	RETURN
	END

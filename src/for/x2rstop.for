C
C SUBROUTINE X2RSTOP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RSTOP.FOV                                  $
C  $Date::   17 Apr 1996 16:32:46                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2rstart.for;1 **
C
C
C++++++++++++++++++++++++++++++++++++++++++++++
C
C     STOP RELAY PROCESS (OR SUBPROCESS)
C
C
C     X2RSTOP(BUFFER,PROCESS,DESTINATION)
C
C     IN:
C     PROCESS        - PROCESS # TO BE STOPPED
C     DESTINATION    - STATION/GROUP TO BE STOPPED
C
C     OUT:
C     BUFFER         - SET UP FOR X2XREL
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
	SUBROUTINE X2RSTOP(BUFFER,PROCESS,DESTINATION)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2RCMD.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
C
	INTEGER*4  INPTAB2
	PARAMETER (INPTAB2=INPTAB*2-1)
C
	INTEGER*2 BUFFER(*)
	INTEGER*4 DESTINATION, PROCESS
C
	BUFFER(TRCODE)=TYPX2X_RELAY_CMD
C
	CALL ISBYTE(X2RCMD_STOP,BUFFER(INPTAB2),X2ROFF_CMD-1)
	CALL ISBYTE(PROCESS,BUFFER(INPTAB2),X2ROFF_PROCESS-1)
	CALL I4TOBUF4(DESTINATION,BUFFER(INPTAB2),X2ROFF_DEST-1)
C
	RETURN
	END

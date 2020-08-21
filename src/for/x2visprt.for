C
C SUBROUTINE X2VISPRT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2VISPRT.FOV                                 $
C  $Date::   17 Apr 1996 16:40:46                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_x2vissub.for;1 **
C
C
C
C
C ===========================================================
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
	SUBROUTINE X2VISPRT(SCRIDX,LEVEL,PRT,LEN,ERR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
C
	INTEGER*4   SCRIDX          !Screen
	INTEGER*4   LEVEL           !Level of input
	INTEGER*4   PRT             !Station port number
	INTEGER*4   STNPRT          !Port from memory
	INTEGER*4   LEN             !Length of input
	INTEGER*4   ERR             !Return error code
C
	ERR=0
C
C IF INPUT DATA HAS BEEN PASSED, CHECK TO ENSURE THAT
C THE PORT IS DEFINED.
C
	IF(PRT.NE.0) THEN
	  IF(PRT.LT.0 .OR. PRT.GT.X2X_MAXPORT) PRT=1
	  X2FLDINF(XPRTIDX)=PRT
	  X2SCRN(X2SCRN_KEYLEV3+LEVEL,SCRIDX)=XPRTIDX
C
C ATTEMPT TO GET THE PORT BASED ON OTHER INFORMATION.
C
	ELSE
	  IF(X2FLDINF(XTERIDX).NE.0) THEN
	    CALL ILBYTE(STNPRT,IX2XT_STATION_PORT,X2FLDINF(XTERIDX)-1)
	    X2FLDINF(XPRTIDX)=STNPRT
	  ENDIF
	ENDIF
C
	RETURN
	END

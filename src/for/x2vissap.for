C
C SUBROUTINE X2VISSAP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2VISSAP.FOV                                 $
C  $Date::   17 Apr 1996 16:40:54                                         $
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
	SUBROUTINE X2VISSAP(SCRIDX,LEVEL,SAP,LEN,ERR)
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
	INTEGER*4   SAP             !SAP number
	INTEGER*4   LEN             !Length of input
	INTEGER*4   ERR             !Return error code
C
	ERR=0
C
C IF INPUT DATA HAS BEEN PASSED, CHECK TO ENSURE THAT
C THE SAP IS DEFINED.
C
	IF(SAP.NE.0) THEN
	  IF(SAP.LT.0 .OR. SAP.GT.X2X_SAP) SAP=1
	  X2FLDINF(XSAPIDX)=SAP
	  X2SCRN(X2SCRN_KEYLEV3+LEVEL,SCRIDX)=XSAPIDX
C
C ATTEMPT TO GET THE SAP BASED ON OTHER INFORMATION.
C
	ELSE
	  IF(X2FLDINF(XLOCIDX).NE.0) THEN
	    X2FLDINF(XSAPIDX)=X2XPL_SAP(X2FLDINF(XLOCIDX))
	  ENDIF
	ENDIF
C
	RETURN
	END

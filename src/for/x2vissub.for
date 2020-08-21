C
C SUBROUTINE X2VISSUB
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2VISSUB.FOV                                 $
C  $Date::   17 Apr 1996 16:41:08                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
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
	SUBROUTINE X2VISSUB(SCRIDX,LEVEL,SUB,LEN,ERR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
C
	INTEGER*4   SCRIDX          !Screen
	INTEGER*4   LEVEL           !Level of input
	INTEGER*4   SUB             !Subnetwork number
	INTEGER*4   LEN             !Length of input
	INTEGER*4   ERR             !Return error code
C
C 	If input data has been passed, check to ensure that
C 	the subnetwork is valid
C
D	TYPE *,'SUB= ',SUB

	IF((SUB.GT.0).AND.(SUB.LE.X2X_MAX_SUBNETWORK+1)) THEN
	  X2FLDINF(XSUBIDX)=SUB
	  X2SCRN(X2SCRN_KEYLEV3+LEVEL,SCRIDX)=XSUBIDX
	  ERR=0
C
C	Attempt to get the subnetwork number based on other information.
C
	ELSE
	  IF(X2FLDINF(XSAPIDX).NE.0) THEN
	    IF((X2XE_SUBNETWORK(X2FLDINF(XSAPIDX)).GE.0).AND.
     *	       (X2XE_SUBNETWORK(X2FLDINF(XSAPIDX)).LE.
     *		X2X_MAX_SUBNETWORK)) THEN
	      X2FLDINF(XSUBIDX)=X2XE_SUBNETWORK(X2FLDINF(XSAPIDX))+1
	    ENDIF
	  ENDIF
	ENDIF

D	TYPE *,'X2FLDINF(XSUBIDX) = ',X2FLDINF(XSUBIDX)
C
	RETURN
	END

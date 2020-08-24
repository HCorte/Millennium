C
C SUBROUTINE X2VISLOC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2VISLOC.FOV                                 $
C  $Date::   17 Apr 1996 16:40:32                                         $
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
	SUBROUTINE X2VISLOC(SCRIDX,LEVEL,LOC,LEN,ERR)
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
	INTEGER*4   LOC             !Local port number
	INTEGER*4   LEN             !Length of input
	INTEGER*4   ERR             !Return error code
C
	ERR=0
C
C IF INPUT DATA HAS BEEN PASSED, CHECK TO ENSURE THAT
C THE LOCAL PORT IS DEFINED.
C
	IF(LOC.NE.0) THEN
	  IF(LOC.LT.0 .OR. LOC.GT.X2X_LOCAL_PORTS) LOC=1
	  IF(X2XPL_STATE(LOC).NE.0) THEN
	    X2FLDINF(XLOCIDX)=LOC
	    X2SCRN(X2SCRN_KEYLEV3+LEVEL,SCRIDX)=XLOCIDX
	  ELSE
	    ERR=-1
	  ENDIF
C
C ATTEMPT TO GET THE LOCAL PORT BASED ON OTHER INFORMATION.
C
	ELSE
	  IF(X2FLDINF(XNETIDX).NE.0) THEN
	    X2FLDINF(XLOCIDX)=
     *	      X2XPN_NETWORK_TO_LOCAL(X2FLDINF(XNETIDX))
	  ENDIF
	ENDIF
C
	RETURN
	END

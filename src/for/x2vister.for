C
C SUBROUTINE X2VISTER
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2VISTER.FOV                                 $
C  $Date::   17 Apr 1996 16:41:12                                         $
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
	SUBROUTINE X2VISTER(SCRIDX,LEVEL,TER,LEN,ERR)
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
	INTEGER*4   TER             !Terminal number
	INTEGER*4   LEN             !Length of input
	INTEGER*4   ERR             !Return error code
	INTEGER*4   STATE           !Terminal state
	INTEGER*4   I
C
	ERR=0
C
C IF INPUT DATA HAS BEEN PASSED, CHECK TO ENSURE THAT
C THE TERMINAL IS DEFINED.
C
	IF(TER.NE.0) THEN
	  IF(TER.LT.0 .OR. TER.GT.X2X_TERMS) TER=1
	  CALL ILBYTE(STATE,IX2XT_STATE,TER-1)
	  IF(STATE.EQ.X2XTS_NOT_DEF) THEN
	    ERR=-1
	  ELSE
	    X2FLDINF(XTERIDX)=TER
	    X2SCRN(X2SCRN_KEYLEV3+LEVEL,SCRIDX)=XTERIDX
	  ENDIF
C
C ATTEMPT TO GET THE TERMINAL NUMBER BASED ON OTHER INFORMATION.
C
	ELSE
	  IF(X2FLDINF(XAGTIDX).NE.0) THEN
	    DO 100 I=1,NUMAGT
	      IF(AGTTAB(AGTNUM,I).EQ.X2FLDINF(XAGTIDX)) THEN
	        X2FLDINF(XTERIDX)=I
	        GOTO 110
	      ENDIF
100	    CONTINUE
110	    CONTINUE
	  ENDIF
	ENDIF
C
	RETURN
	END

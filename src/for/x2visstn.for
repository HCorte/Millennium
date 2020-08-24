C
C SUBROUTINE X2VISSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2VISSTN.FOV                                 $
C  $Date::   17 Apr 1996 16:41:02                                         $
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
C X2VISSUB.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added AGTINF.DEF, moved VISCOM.DEF for Finland.
C
C V02 07-OCT-94 SCD MAKE XGVT AND XSTN BITS MUTUALLY EXCLUSIVE - Integrate 
C		    UK changes into X2X Baseline
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This routine contains the subroutines which are used to
C load information into the X2VIS common based on some
C input information.
C
C Routines contained in this module include:
C
C     CALL X2VISSTN(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISTER(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISAGT(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISNET(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISLOC(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISSAP(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISPRT(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISGRP(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISBOT(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISTOP(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISFOR(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISBAK(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISSRT(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISDRP(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISUP(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISADR(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISREL(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISMNT(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
C     CALL X2VISIDX(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
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
	SUBROUTINE X2VISSTN(SCRIDX,LEVEL,STN,LEN,ERR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
C
	INTEGER*4   SCRIDX          !Screen
	INTEGER*4   LEVEL           !Level of input
	INTEGER*4   STN             !Station number
	INTEGER*4   LEN             !Length of input
	INTEGER*4   ERR             !Return error code
	INTEGER*4   I

	LOGICAL     TESTBIT         !Test for valid bits - V02
C
	ERR=0
C
C IF INPUT DATA HAS BEEN PASSED, CHECK TO ENSURE THAT
C THE STATION IS DEFINED.
C
	IF(STN.NE.0) THEN
	  IF(STN.LT.0 .OR. STN.GT.X2X_STATIONS) STN=1
          X2FLDINF(XSTNIDX)=STN
          X2SCRN(X2SCRN_KEYLEV3+LEVEL,SCRIDX)=XSTNIDX
C
C ATTEMPT TO GET THE STATION NUMBER BASED ON OTHER INFORMATION.
C
	ELSE
	  IF(X2FLDINF(XTERIDX).NE.0) THEN
	    X2FLDINF(XSTNIDX)=X2XT_STATION_NO(X2FLDINF(XTERIDX))
	  ELSE IF(X2FLDINF(XAGTIDX).NE.0) THEN
	    DO 100 I=1,NUMAGT
	      IF(AGTTAB(AGTNUM,I).EQ.X2FLDINF(XAGTIDX)) THEN
	        X2FLDINF(XSTNIDX)=X2XT_STATION_NO(I)
	        GOTO 110
	      ENDIF
100	    CONTINUE
110	    CONTINUE
	  ENDIF
	ENDIF
C
C ***** Start V02 changes *****
C	STATION, GVT and IDU bits are mutually exclusive.  If GVT or IDU 
c	bits are set, then clear them.

	TESTBIT=BJTEST(X2SCRN(X2SCRN_FILFLDS,SCRIDX),XGVTIDX)
	IF(TESTBIT) X2SCRN(X2SCRN_FILFLDS,SCRIDX) = 
     *		    JIBCLR(X2SCRN(X2SCRN_FILFLDS,SCRIDX),XGVTIDX)
	TESTBIT=BJTEST(X2SCRN(X2SCRN_FILFLDS,SCRIDX),XIDUIDX)
	IF(TESTBIT) X2SCRN(X2SCRN_FILFLDS,SCRIDX) = 
     *		    JIBCLR(X2SCRN(X2SCRN_FILFLDS,SCRIDX),XIDUIDX)

C ***** End V02 changes *****
C
	RETURN
	END

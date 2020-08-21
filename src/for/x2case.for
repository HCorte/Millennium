C
C SUBROUTINE X2CASE
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CASE.FOV                                   $
C  $Date::   17 Apr 1996 16:11:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_x2visupd.for;1 **
C
C V04 06-OCT-94 SCD ADD GVTID AND SATELLINE IDU COMMANDS - Integrate UK 
C		    changes into X2X Baseline
C V03 24-AUG-94 GPR Process subnetwork - Integrate UK changes into X2X Baseline
C V02 18-AUG-94 GPR Handle ascii data for x25 address - Integrate UK changes 
C		    into X2X Baseline
C
C ===============================================================
C This subroutine will call the appropriate load routine based
C on the vision field index.
C
C Calling sequence:
C
C     CALL X2CASE(SCRIDX,LEVEL,FLDIDX,FLDDTA,FLDASC,FLDLEN,ERR)
C
C Input parameters:
C
C     SCRIDX      Int*4       Screen
C     LEVEL       Int*4       Level of input
C     FLDIDX      Int*4       Field index relative to X2VIS.DEF
C     FLDDTA      Int*4       Data input from X2LINTER
C     FLDASC      Char*       Ascii Data input from X2LINTER	  !V02
C     FLDLEN      Int*4       Length of data
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
	SUBROUTINE X2CASE(SCRIDX,LEVEL,FLDIDX,FLDDTA,FLDASC,	!V02
     *		          FLDLEN,ERR)				!V02
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XPRM.DEF'				!V02
	INCLUDE 'INCLIB:X2VIS.DEF'
C
        CHARACTER   FLDASC*(*)      !String for ascii data      !V02
	INTEGER*4   SCRIDX          !Screen index
	INTEGER*4   LEVEL           !Level of input
	INTEGER*4   FLDIDX          !Field index
	INTEGER*4   FLDDTA          !Field data
	INTEGER*4   FLDLEN          !Length of field data
	INTEGER*4   ERR             !Error code
C
	IF(FLDIDX.EQ.XSTNIDX) THEN
	  CALL X2VISSTN(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XTERIDX) THEN
	  CALL X2VISTER(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XGRPIDX) THEN
	  CALL X2VISGRP(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XSAPIDX) THEN
	  CALL X2VISSAP(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XFORIDX) THEN
	  CALL X2VISFOR(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XBAKIDX) THEN
	  CALL X2VISBAK(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XTOPIDX) THEN
	  CALL X2VISTOP(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XBOTIDX) THEN
	  CALL X2VISBOT(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XNETIDX) THEN
	  CALL X2VISNET(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XLOCIDX) THEN
	  CALL X2VISLOC(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XPRTIDX) THEN
	  CALL X2VISPRT(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XAGTIDX) THEN
	  CALL X2VISAGT(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XSRTIDX) THEN
	  CALL X2VISSRT(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XDRPIDX) THEN
	  CALL X2VISDRP(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XUPDIDX) THEN
	  CALL X2VISUP(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XADRIDX) THEN
	  CALL X2VISADR(SCRIDX,LEVEL,FLDASC,FLDLEN,ERR)	    !V02
	ELSE IF(FLDIDX.EQ.XRELIDX) THEN
	  CALL X2VISREL(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XMNTIDX) THEN
	  CALL X2VISMNT(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XIDXIDX) THEN
	  CALL X2VISIDX(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)
	ELSE IF(FLDIDX.EQ.XSUBIDX) THEN			    !V03
	  CALL X2VISSUB(SCRIDX,LEVEL,FLDDTA,FLDLEN,ERR)	    !V03
	ELSE IF(FLDIDX.EQ.XGVTIDX) THEN			    !V04
	  CALL X2VISGVT(SCRIDX,LEVEL,FLDASC,FLDLEN,ERR)	    !V04
	ELSE IF(FLDIDX.EQ.XIDUIDX) THEN			    !V04
	  CALL X2VISIDU(SCRIDX,LEVEL,FLDASC,FLDLEN,ERR)	    !V04
	ELSE
	  ERR=-1
	ENDIF
C
	RETURN
	END

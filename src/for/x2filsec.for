C
C FUNCTION X2FILSEC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2FILSEC.FOV                                 $
C  $Date::   17 Apr 1996 16:17:02                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2filsec.for;1 **
C
C X2FILSEC.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This function will return the number of sectors for the
C input X2X file.
C
C Input parameters:
C
C     FILE        Int*4   File number
C
C Output parameters:
C
C     X2FILSEC    Int*4   Number of sectors
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
	INTEGER*4 FUNCTION X2FILSEC(FILE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XGBL.DEF'
	INCLUDE 'INCLIB:X2XNPC.DEF'
	INCLUDE 'INCLIB:X2XLPC.DEF'
	INCLUDE 'INCLIB:X2XSPC.DEF'
	INCLUDE 'INCLIB:X2XRCD.DEF'
	INCLUDE 'INCLIB:X2XRCL.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
	INCLUDE 'INCLIB:X2XTER.DEF'
	INCLUDE 'INCLIB:X2XBLD.DEF'
	INCLUDE 'INCLIB:X2XBRO.DEF'
	INCLUDE 'INCLIB:X2XTTN.DEF'
	INCLUDE 'INCLIB:X2XGRP.DEF'
C
	INTEGER*4   FILE            !File number
C
	X2FILSEC=0
	IF(FILE.EQ.XGBL) THEN
	  X2FILSEC=X2XGBL_SECT
	ELSE IF(FILE.EQ.XNPC) THEN
	  X2FILSEC=X2XNPC_SECT
	ELSE IF(FILE.EQ.XLPC)THEN
	  X2FILSEC=X2XLPC_SECT
	ELSE IF(FILE.EQ.XSPC) THEN
	  X2FILSEC=X2XSPC_SECT
	ELSE IF(FILE.EQ.XRCD) THEN
	  X2FILSEC=X2XRCD_SECT
	ELSE IF(FILE.EQ.XRCL) THEN
	  X2FILSEC=X2XRCL_SECT
	ELSE IF(FILE.EQ.XSCL) THEN
	  X2FILSEC=X2XSCL_SECT
	ELSE IF(FILE.EQ.XSTN) THEN
	  X2FILSEC=X2XSTN_SECT
	ELSE IF(FILE.EQ.XTER) THEN
	  X2FILSEC=X2XTER_SECT
	ELSE IF(FILE.EQ.XBLD) THEN
	  X2FILSEC=X2XBLD_SECT
	ELSE IF(FILE.EQ.XBRO) THEN
	  X2FILSEC=X2XBRO_SECT
	ELSE IF(FILE.EQ.XGRP) THEN
	  X2FILSEC=X2XGRP_SECT
	ELSE IF(FILE.EQ.XTTN) THEN
	  X2FILSEC=X2XTTN_SECT
	ELSE
	  X2FILSEC=-1
	ENDIF
C
	RETURN
	END

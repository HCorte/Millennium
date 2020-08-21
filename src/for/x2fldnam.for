C
C FUNCTION X2FLDNAM
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2FLDNAM.FOV                                 $
C  $Date::   17 Apr 1996 16:17:08                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2fldnam.for;1 **
C
C X2FLDNAM.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This function will return the name of the field being modified
C given the file number and the field number.
C
C Input parameters:
C
C     FILE        Int*4   File number
C     FIELD       Int*4   Field number relative to file
C
C Output parameters:
C
C     X2FLDNAM    Char    Name of modified field
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
	CHARACTER*15 FUNCTION X2FLDNAM(FILE,FIELD)
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
	INCLUDE 'INCLIB:X2XBRO.DEF'
	INCLUDE 'INCLIB:X2XTTN.DEF'
	INCLUDE 'INCLIB:X2XGRP.DEF'
C
	INTEGER*4   FILE            !File number
	INTEGER*4   FIELD           !Field number
C
	X2FLDNAM=' '
	IF(FIELD.LE.0) THEN
	  X2FLDNAM=' '
	ELSE IF(FILE.EQ.XGBL.AND.FIELD.LE.X2XGBL_ENTRIES) THEN
	  X2FLDNAM=X2XGBL_FIELD(FIELD)
	ELSE IF(FILE.EQ.XNPC.AND.FIELD.LE.X2XNPC_ENTRIES) THEN
	  X2FLDNAM=X2XNPC_FIELD(FIELD)
	ELSE IF(FILE.EQ.XLPC.AND.FIELD.LE.X2XLPC_ENTRIES) THEN
	  X2FLDNAM=X2XLPC_FIELD(FIELD)
	ELSE IF(FILE.EQ.XSPC.AND.FIELD.LE.X2XSPC_ENTRIES) THEN
	  X2FLDNAM=X2XSPC_FIELD(FIELD)
	ELSE IF(FILE.EQ.XRCD.AND.FIELD.LE.X2XRCD_ENTRIES) THEN
	  X2FLDNAM=X2XRCD_FIELD(FIELD)
	ELSE IF(FILE.EQ.XRCL.AND.FIELD.LE.X2XRCL_ENTRIES) THEN
	  X2FLDNAM=X2XRCL_FIELD(FIELD)
	ELSE IF(FILE.EQ.XSCL.AND.FIELD.LE.X2XSCL_ENTRIES) THEN
	  X2FLDNAM=X2XSCL_FIELD(FIELD)
	ELSE IF(FILE.EQ.XSTN.AND.FIELD.LE.X2XSTN_ENTRIES) THEN
	  X2FLDNAM=X2XSTN_FIELD(FIELD)
	ELSE IF(FILE.EQ.XTER.AND.FIELD.LE.X2XTER_ENTRIES) THEN
	  X2FLDNAM=X2XTER_FIELD(FIELD)
	ELSE IF(FILE.EQ.XBRO.AND.FIELD.LE.X2XBRO_ENTRIES) THEN
	  X2FLDNAM=X2XBRO_FIELD(FIELD)
	ELSE IF(FILE.EQ.XGRP.AND.FIELD.LE.X2XGRP_ENTRIES) THEN
	  X2FLDNAM=X2XGRP_FIELD(FIELD)
	ELSE IF(FILE.EQ.XTTN.AND.FIELD.LE.X2XTTN_ENTRIES) THEN
	  X2FLDNAM=X2XTTN_FIELD(FIELD)
	ENDIF
C
	RETURN
	END

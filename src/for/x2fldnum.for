C
C FUNCTION X2FLDNUM
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2FLDNUM.FOV                                 $
C  $Date::   17 Apr 1996 16:17:14                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2fldnum.for;1 **
C
C X2FLDNUM.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This function will return the field number of a file given the
C physical field number and the file number.
C
C Input parameters:
C
C     FILE        Int*4       File number
C     FIELD       Int*4       Physical field number relative to file
C
C Output parameters:
C
C     X2FLDNUM    Int*4       Logical display field number
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
	INTEGER*4 FUNCTION X2FLDNUM(FILE,FIELD)
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
	INCLUDE 'INCLIB:X2XGRP.DEF'
	INCLUDE 'INCLIB:X2XTTN.DEF'
C
	INTEGER*4   FILE            !File number
	INTEGER*4   FIELD           !Field number
	INTEGER*4   I
C
	X2FLDNUM=0
	I=0
C
C EXTRACT THE FIELD TYPE AND LENGTH.
C
	IF(FIELD.LE.0) THEN
	  X2FLDNUM=0
	ELSE IF(FILE.EQ.XGBL.AND.FIELD.LE.X2XGBL_INDEX(X2XGBL_ENTRIES)) THEN
	  DO 100 I=1,X2XGBL_ENTRIES
	    IF(FIELD.EQ.X2XGBL_INDEX(I)) THEN
	      X2FLDNUM=I
	      GOTO 8000
	    ENDIF
100	  CONTINUE
	ELSE IF(FILE.EQ.XNPC.AND.FIELD.LE.X2XNPC_INDEX(X2XNPC_ENTRIES)) THEN
	  DO 110 I=1,X2XNPC_ENTRIES
	    IF(FIELD.EQ.X2XNPC_INDEX(I)) THEN
	      X2FLDNUM=I
	      GOTO 8000
	    ENDIF
110	  CONTINUE
	ELSE IF(FILE.EQ.XLPC.AND.FIELD.LE.X2XLPC_INDEX(X2XLPC_ENTRIES)) THEN
	  DO 120 I=1,X2XLPC_ENTRIES
	    IF(FIELD.EQ.X2XLPC_INDEX(I)) THEN
	      X2FLDNUM=I
	      GOTO 8000
	    ENDIF
120	  CONTINUE
	ELSE IF(FILE.EQ.XSPC.AND.FIELD.LE.X2XSPC_INDEX(X2XSPC_ENTRIES)) THEN
	  DO 130 I=1,X2XSPC_ENTRIES
	    IF(FIELD.EQ.X2XSPC_INDEX(I)) THEN
	      X2FLDNUM=I
	      GOTO 8000
	    ENDIF
130	  CONTINUE
	ELSE IF(FILE.EQ.XRCD.AND.FIELD.LE.X2XRCD_INDEX(X2XRCD_ENTRIES)) THEN
	  DO 140 I=1,X2XRCD_ENTRIES
	    IF(FIELD.EQ.X2XRCD_INDEX(I)) THEN
	      X2FLDNUM=I
	      GOTO 8000
	    ENDIF
140	  CONTINUE
	ELSE IF(FILE.EQ.XRCL.AND.FIELD.LE.X2XRCL_INDEX(X2XRCL_ENTRIES)) THEN
	  DO 150 I=1,X2XRCL_ENTRIES
	    IF(FIELD.EQ.X2XRCL_INDEX(I)) THEN
	      X2FLDNUM=I
	      GOTO 8000
	    ENDIF
150	  CONTINUE
	ELSE IF(FILE.EQ.XSCL.AND.FIELD.LE.X2XSCL_INDEX(X2XSCL_ENTRIES)) THEN
	  DO 160 I=1,X2XSCL_ENTRIES
	    IF(FIELD.EQ.X2XSCL_INDEX(I)) THEN
	      X2FLDNUM=I
	      GOTO 8000
	    ENDIF
160	  CONTINUE
	ELSE IF(FILE.EQ.XSTN.AND.FIELD.LE.X2XSTN_INDEX(X2XSTN_ENTRIES)) THEN
	  DO 170 I=1,X2XSTN_ENTRIES
	    IF(FIELD.EQ.X2XSTN_INDEX(I)) THEN
	      X2FLDNUM=I
	      GOTO 8000
	    ENDIF
170	  CONTINUE
	ELSE IF(FILE.EQ.XTER.AND.FIELD.LE.X2XTER_INDEX(X2XTER_ENTRIES)) THEN
	  DO 180 I=1,X2XTER_ENTRIES
	    IF(FIELD.EQ.X2XTER_INDEX(I)) THEN
	      X2FLDNUM=I
	      GOTO 8000
	    ENDIF
180	  CONTINUE
	ELSE IF(FILE.EQ.XBRO.AND.FIELD.LE.X2XBRO_INDEX(X2XBRO_ENTRIES)) THEN
	  DO 190 I=1,X2XBRO_ENTRIES
	    IF(FIELD.EQ.X2XBRO_INDEX(I)) THEN
	      X2FLDNUM=I
	      GOTO 8000
	    ENDIF
190	  CONTINUE
	ELSE IF(FILE.EQ.XGRP.AND.FIELD.LE.X2XGRP_INDEX(X2XGRP_ENTRIES)) THEN
	  DO 200 I=1,X2XGRP_ENTRIES
	    IF(FIELD.EQ.X2XGRP_INDEX(I)) THEN
	      X2FLDNUM=I
	      GOTO 8000
	    ENDIF
200	  CONTINUE
	ELSE IF(FILE.EQ.XTTN.AND.FIELD.LE.X2XTTN_INDEX(X2XTTN_ENTRIES)) THEN
	  DO 210 I=1,X2XTTN_ENTRIES
	    IF(FIELD.EQ.X2XTTN_INDEX(I)) THEN
	      X2FLDNUM=I
	      GOTO 8000
	    ENDIF
210	  CONTINUE
	ENDIF
C
C PROGRAM EXIT.
C
8000	CONTINUE
	X2FLDNUM=I
	RETURN
	END

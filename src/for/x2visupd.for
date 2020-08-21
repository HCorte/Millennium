C
C SUBROUTINE X2VISUPD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2VISUPD.FOV                                 $
C  $Date::   17 Apr 1996 16:41:24                                         $
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
C X2VISUPD.FOR
C
C V02 18-AUG-94 GPR HANDLE ASCII DATA AS FIELD DATA FOR ADDRESSES - Integrate 
C		    UK changes into X2X Baseline
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will fill the X2X vision common information
C with as many parameters are possible based on the screen.
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
CV02	SUBROUTINE X2VISUPD(SCRIDX,LEVEL,FLDIDX,FLDDTA,FLDLEN,ERR)
	SUBROUTINE X2VISUPD(SCRIDX,LEVEL,FLDIDX,FLDDTA,FLDASC,	    !V02
     *			    FLDLEN,ERR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
C
	CHARACTER   FLDASC*(*)		!String for ascii data - V02
	INTEGER*4   SCRIDX              !Current screen (KEYLIST)
	INTEGER*4   LEVEL               !Input level
	INTEGER*4   FLDIDX              !Field index (X2VIS.DEF)
	INTEGER*4   FLDDTA              !Field data
	INTEGER*4   FLDLEN              !Length of field
	INTEGER*4   ERR                 !Error condition
	INTEGER*4   LASTSCR / 0 /       !Last screen used
	INTEGER*4   I
	LOGICAL     TESTBIT             !Test for valid parameters
	LOGICAL     CLEAR               !Clear common flag
C
C IF THE SCREEN HAS CHANGED OR THE PRIMARY KEY HAS BEEN
C MODIFIIED, CLEAR THE X2 COMMON.
C
	CLEAR=.FALSE.
	IF(SCRIDX.LE.0.OR.SCRIDX.GT.X2SCRN_CNT) GOTO 8000
	IF(SCRIDX.NE.LASTSCR) CLEAR=.TRUE.
        IF(X2SCRN(X2SCRN_DEFKEY,SCRIDX).NE.0) THEN
          IF(LEVEL.EQ.1 .AND.
     *	    FLDIDX.EQ.X2SCRN(X2SCRN_DEFKEY,SCRIDX) .AND.
     *	    X2FLDINF(X2SCRN(X2SCRN_DEFKEY,SCRIDX)).NE.FLDDTA)
     *	      CLEAR=.TRUE.
	ENDIF
C
	IF(CLEAR) THEN
	  DO 50 I=1,X2FLDCNT
	    X2FLDINF(I)=0
50	  CONTINUE
	  X2SCRN(X2SCRN_INPLEV1,SCRIDX)=0
	  X2SCRN(X2SCRN_INPLEV2,SCRIDX)=0
	  X2SCRN(X2SCRN_INPLEV3,SCRIDX)=0
	ENDIF
	LASTSCR=SCRIDX
C
C LOAD THE INPUT SCREEN VALUE INTO THE APPROPRIATE SLOT.
C ZERO OUT THE DATA FIELD AS IT HAS ALREADY BEEN LOADED.
C
CV02	CALL X2CASE(SCRIDX,LEVEL,FLDIDX,FLDDTA,FLDLEN,ERR)
	CALL X2CASE(SCRIDX,LEVEL,FLDIDX,FLDDTA,FLDASC,		!V02
     *		    FLDLEN,ERR)					!V02
	FLDDTA=0
	IF(ERR.NE.0) GOTO 8000
C
C LOOP FOR ALL VALID PARAMETERS. LOOP TWICE TO ALLOW FOR
C ASSUMPTIONS BASED ON OTHER DETERMINED INFORMATION.
C
	DO 100 I=1,2
	  DO 110 FLDIDX=1,X2FLDCNT
	    TESTBIT=BJTEST(X2SCRN(X2SCRN_FILFLDS,SCRIDX),FLDIDX)
	    IF(TESTBIT) THEN
CV02	      CALL X2CASE(SCRIDX,LEVEL,FLDIDX,FLDDTA,FLDLEN,ERR)
	      CALL X2CASE(SCRIDX,LEVEL,FLDIDX,FLDDTA,FLDASC,	!V02
     *			  FLDLEN,ERR)				!V02
	      IF(ERR.NE.0) GOTO 8000
	    ENDIF
110	  CONTINUE
100	CONTINUE
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END

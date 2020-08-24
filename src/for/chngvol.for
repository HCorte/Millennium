C
C SUBROUTINE CHNGVOL
C $Log:   GXAFXT:[GOLS]CHNGVOL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:34:40   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   15 Jan 1996 19:09:00   HXK
C  Allow global change of Verify File volume names
C  
C     Rev 1.0   21 Jan 1993 15:52:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - chngvol.for **
C
C CHNGVOL.FOR
C
C V01 13-APR-92 HDB  RELEASED FOR NETHERLANDS
C
C
C SUBROUTINE TO BATCH CHANGE GLOBAL VOLUME NAMES 
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C	discr	: allow user to batch change volume names in file
C		  lists. User has option to change either system
C		  or game volume names or both.
C
C	input   : NONE	 via common...
C	output	: NONE   all error handling internal
C	
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CHNGVOL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
C	
	INTEGER*4 OPTION	!menu option selected
	INTEGER*4 EXIT		!did the user exit
	INTEGER*4 STATUS	!return status for calls
	INTEGER*4 FRMNAME	!from volume name
	INTEGER*4 TONAME	!to volume name
	INTEGER*4 I		!loop counter
	INTEGER*4 FLAG		!used for call to YESNO
	INTEGER*4 NRM_YES	!return from NRM_YESNO
	INTEGER*4 NRM_NO	!return from NRM_YESNO
	PARAMETER (NRM_YES=1)
	PARAMETER (NRM_NO=2)
	CHARACTER FNAMES(20,MAXFIL),GFNAMES(20,MAXGAM),GVNAMES(20,MAXGAM)
	EQUIVALENCE (FNAMES,SCFSFN)
	EQUIVALENCE (GFNAMES,SCFGFN)
	EQUIVALENCE (GVNAMES,SCFGVN)

	COMMON SCFREC

100	CALL CLRSCR(5)
	WRITE(5,900)
	CALL INPNUM('Enter option ',OPTION,0,15,EXIT)
	IF(EXIT.LT.0) GOTO 600	
C	WRITE(5,910)
	CALL WIMG(5,'Enter from name ')
	READ(5,910) FRMNAME
	CALL WIMG(5,'Enter to name ')
	READ(5,920) TONAME
	CALL CLRSCR(5)
	IF(OPTION.EQ.1) WRITE(5,930) FRMNAME,TONAME
	IF(OPTION.EQ.2) WRITE(5,940) FRMNAME,TONAME
	IF(OPTION.EQ.3) WRITE(5,950) FRMNAME,TONAME
	CALL WIMG(5,' Apply the changes ? [YES/NO]')
	CALL YESNO(FLAG)
	IF(FLAG.NE.NRM_YES) THEN
	    WRITE(5,960)
	    CALL XWAIT(2,2,STATUS)
	    GOTO 100
	END IF
C
C make the actual changes to the volume names
C
	IF(OPTION.EQ.1.OR.OPTION.EQ.3) THEN
	    DO 110 I=1,MAXFIL
	    IF(SCFSFN(1,I).EQ.FRMNAME) THEN
		SCFSFN(1,I) = TONAME
		FNAMES(5,I) =':'
	    ENDIF
110	    CONTINUE
	ENDIF

C	CHANGE THE GAME FILE NAMES
	IF(OPTION.EQ.2.OR.OPTION.EQ.3) THEN
	    DO 120 I=1,MAXGAM
	       IF(SCFGFN(1,I).EQ.FRMNAME) THEN
		  SCFGFN(1,I) = TONAME
		  GFNAMES(5,I) =':'
	       ENDIF
	       IF(SCFGVN(1,I).EQ.FRMNAME) THEN
		  SCFGVN(1,I) = TONAME
		  GVNAMES(5,I) =':'
	       ENDIF
120	    CONTINUE
	ENDIF
	GOTO 100	

600	CONTINUE

900	FORMAT(' - - Change volume name - -',//,
     *	       ' 1 - change system volume name',/,
     *	       ' 2 - change game volume name',/,
     *	       ' 3 - change both',//,
     *	       ' E - Exit')
910	FORMAT(A4)
920	FORMAT(A4)
930	FORMAT(' change all system volnames from: ',A4,' to ',A4)
940	FORMAT(' change all game volnames from: ',A4,' to ',A4)
950	FORMAT(' change all volnames from: ',A4,' to ',A4)
960	FORMAT(' no changes made')
	END

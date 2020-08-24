C
C PROGRAM X2GBLMEN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GBLMEN.FOV                                 $
C  $Date::   17 Apr 1996 16:18:28                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2gblmen.for;1 **
C
C X2GBLMEN.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This program provides an interface into the Global parameters
C file.  This menu will allow: modifications, saving configuration,
C and restoring configurations.
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
	PROGRAM X2GBLMEN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XGBL.DEF'
C
	INTEGER*4   OPT             !Menu option
	INTEGER*4   ST,EXT          !Status/Exit
	INTEGER*4   REC /0/         !Record to modify
	INTEGER*4   LOCK            !Record lock check
	INTEGER*4   LOCKCNT /0/     !Lock counter
	CHARACTER   PROMPT*60       !Input prompt string
	CHARACTER   X2FILNAM*20     !File name function
	CHARACTER   NULL*60         !Null string
	CHARACTER   NULLEQV(60)*1   !Null string
C
	DATA        NULLEQV/60*Z00/
	EQUIVALENCE (NULL,NULLEQV)
C
C OPEN THE GLOBAL PARAMETERS FILE.
C
	CALL OPENX(1,X2FILNAM(XGBL),4,0,0,ST)
	CALL IOINIT(X2XGBL_FDB,1,X2XGBL_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XGBL),'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
C
C CALL COPYRIGHT.
C
	CALL COPYRITX(5)
C
C CLEAR THE SCREEN AND DISPLAY THE MENU.
C
100	CONTINUE
	CALL CLRSCR(5)
	WRITE(5,9000)
C
C READ THE MENU OPTION.
C
	PROMPT=NULL
	WRITE (PROMPT,9010)
	CALL INPNUM(PROMPT,OPT,1,3,EXT)
	IF(EXT.EQ.-9) THEN
	  CALL X2XHLP('X2GBLMEN.HLP')
	  GOTO 100
	ELSE IF(EXT.LT.0) THEN
	  GOTO 8000
	ENDIF
C
C READ THE RECORD NUMBER.
C
	IF(OPT.NE.1) THEN
	  PROMPT=NULL
	  WRITE (PROMPT,9020)
	  CALL INPNUM(PROMPT,REC,1,100,EXT)
	  IF(EXT.LT.0) GOTO 100
	  REC=REC+1
	ENDIF
C
C READ THE CURRENT CONFIGURATION.
C
	LOCKCNT=0
104	CONTINUE
	CALL READL(X2XGBL_FDB,1,X2XGBL_REC,ST,LOCK)
	IF(LOCK.NE.0) THEN
	  LOCKCNT=LOCKCNT+1
	  CALL XWAIT(1,2,ST)
	  IF(LOCKCNT.LT.10) GOTO 104
	  WRITE(5,9034)
	  CALL XWAIT(2,2,ST)
	  GOTO 8000
	ENDIF
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XGBL),'READL',ST,1)
	  CALL GPAUSE
	ENDIF
C
C BRANCH TO APPROPRIATE ROUTINE.
C
	IF(OPT.EQ.1) THEN
	  CALL X2GBLMOD
	ELSE IF(OPT.EQ.2) THEN
	  CALL X2GBLSAV(REC)
          CALL X2UNLOCK(XGBL,X2XGBL_FDB,1,X2XGBL_REC)
	ELSE IF(OPT.EQ.3) THEN
	  CALL X2GBLRES(REC)
	ENDIF
	GOTO 100
C
C PROGRAM EXIT.
C
8000	CONTINUE
	CALL CLOSEFILE(X2XGBL_FDB)
C
C     ================== Format Statements =====================
C
9000	FORMAT(////,T26,'GTECH Distributed Network',/,
     *	          T29,'Global Setup Menu',//,
     *	          T20,'  1. Modify Global parameters',/,
     *	          T20,'  2. Backup current parameters',/,
     *	          T20,'  3. Restore parameters from Backup',/,
     *	          T20,'  E. Exit',//)
9010	FORMAT(25(' '),'Enter option [1-3] ')
9020	FORMAT(25(' '),'Enter record number   ')
9034	FORMAT(25(' '),'Record is locked')
	END

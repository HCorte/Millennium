C
C PROGRAM X2BLDMEN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BLDMEN.FOV                                 $
C  $Date::   17 Apr 1996 16:09:24                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2bldmen.for;1 **
C
C X2BLDMEN.FOR
C
C V02 03-FEB-94 GPR USE I5 FORMAT FOR STATION AND TERMINAL TYPE-OUTS
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This program provides an interface into the Auto Add
C file.  This menu will allow: addition, modification,
C and deletion of auto add records.
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
	PROGRAM X2BLDMEN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XBLD.DEF'
C
	INTEGER*4   OPT             !Menu option
	INTEGER*4   ST,EXT          !Status/Exit
	INTEGER*4   REC             !Record to modify
	INTEGER*4   LOCK            !Record lock check
	INTEGER*4   LOCKCNT /0/     !Count of locked records
	CHARACTER   PROMPT*60       !Input prompt string
	CHARACTER   X2FILNAM*20     !File name function
C
C OPEN THE AUTO BUILD FILE.
C
	CALL OPENX(1,X2FILNAM(XBLD),4,0,0,ST)
	CALL IOINIT(X2XBLD_FDB,1,X2XBLD_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XBLD),'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
C
C DISPLAY COPYRIGHT.
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
	WRITE (PROMPT,9010)
	CALL INPNUM(PROMPT,OPT,1,5,EXT)
	IF(EXT.EQ.-9) THEN
	  CALL X2XHLP('X2BLDMEN.HLP')
	  GOTO 100
	ELSE IF(EXT.LT.0) THEN
	  GOTO 8000
	ENDIF
C
C READ THE RECORD NUMBER.
C
	IF(OPT.NE.4 .AND. OPT.NE.5) THEN
	  WRITE (PROMPT,9020)
	  CALL INPNUM(PROMPT,REC,X2XBLD_RANGE(1,1),
     *	              X2XBLD_RANGE(2,1),EXT)
	  IF(EXT.LT.0) GOTO 100
	ENDIF
C
C IF ADD A RECORD ENSURE IT DOES NOT ALREADY EXIST.
C
	IF(OPT.EQ.1) THEN
	  LOCKCNT=0
102	  CONTINUE
	  CALL READL(X2XBLD_FDB,REC,X2XBLD_REC,ST,LOCK)
	  IF(LOCK.NE.0) THEN
	    LOCKCNT=LOCKCNT+1
	    CALL XWAIT(1,2,ST)
	    IF(LOCKCNT.LT.10) GOTO 102
	    WRITE(5,9034)
	    CALL XWAIT(3,2,ST)
	    GOTO 8000
	  ENDIF
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XBLD),'READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XBLD_STN.GT.0) THEN
	    WRITE(5,9032) REC,CHAR(7)
	    CALL X2UNLOCK(XBLD,X2XBLD_FDB,REC,X2XBLD_REC)
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
	  CALL X2BLDADD(REC)
C
C IF MODIFY A RECORD, ENSURE IT ALREADY EXISTS.
C
	ELSE IF(OPT.EQ.2) THEN
	  LOCKCNT=0
104	  CONTINUE
	  CALL READL(X2XBLD_FDB,REC,X2XBLD_REC,ST,LOCK)
	  IF(LOCK.NE.0) THEN
	    LOCKCNT=LOCKCNT+1
	    CALL XWAIT(1,2,ST)
	    IF(LOCKCNT.LT.10) GOTO 104
	    WRITE(5,9034)
	    CALL XWAIT(3,2,ST)
	    GOTO 8000
	  ENDIF
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XBLD),'READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XBLD_STN.NE.REC) THEN
	    WRITE(5,9030) REC,CHAR(7)
	    CALL X2UNLOCK(XBLD,X2XBLD_FDB,REC,X2XBLD_REC)
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
	  CALL X2BLDMOD(REC)
C
C IF DELETE A RECORD, ENSURE IT ALREADY EXISTS.
C
	ELSE IF(OPT.EQ.3) THEN
	  LOCKCNT=0
106	  CONTINUE
	  CALL READL(X2XBLD_FDB,REC,X2XBLD_REC,ST,LOCK)
	  IF(LOCK.NE.0) THEN
	    LOCKCNT=LOCKCNT+1
	    CALL XWAIT(1,2,ST)
	    IF(LOCKCNT.LT.10) GOTO 106
	    WRITE(5,9034)
	    CALL XWAIT(3,2,ST)
	    GOTO 8000
	  ENDIF
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XBLD),'READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XBLD_STN.NE.REC) THEN
	    WRITE(5,9030) REC,CHAR(7)
	    CALL X2UNLOCK(XBLD,X2XBLD_FDB,REC,X2XBLD_REC)
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
	  CALL X2BLDDEL(REC)
C
C DISPLAY STATION INFORMATION.
C
	ELSE IF(OPT.EQ.4) THEN
	  CALL X2BLDLIS
C
C RUN AUTOBUILD
C
	ELSE IF(OPT.EQ.5) THEN
	  CALL X2BLDFIL
	ENDIF
	GOTO 100
C
C PROGRAM EXIT.
C
8000	CONTINUE
	CALL CLOSEFILE(X2XBLD_FDB)
C
C     ================== Format Statements =====================
C
9000	FORMAT(////,T26,'GTECH Distributed Network',/,
     *	          T28,'Auto Build Setup Menu',//,
     *	          T20,'  1. Add a station record',/,
     *	          T20,'  2. Modify a station record',/,
     *	          T20,'  3. Delete a station record',/,
     *	          T20,'  4. List auto build records',/,
     *	          T20,'  5. Run X2X autobuild facility',/,
     *	          T20,'  E. Exit',//)
9010	FORMAT(25(' '),'Enter option [1-5] ')
9020	FORMAT(25(' '),'Enter station         ')
9030	FORMAT(25(' '),'Station ',I5,' does not exist',A)		! V02
9032	FORMAT(25(' '),'Station ',I5,' already exists',A)		! V02
9034	FORMAT(25(' '),'Record is locked ')
	END

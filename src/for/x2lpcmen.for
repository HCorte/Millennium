C
C PROGRAM X2LPCMEN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LPCMEN.FOV                                 $
C  $Date::   17 Apr 1996 16:22:54                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2lpcmen.for;1 **
C
C X2LPCMEN.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This program provides an interface into the Local Port
C Configuration file.  This menu will allow: addition, modification,
C and deletion of host addresses.
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
	PROGRAM X2LPCMEN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XLPC.DEF'
C
	INTEGER*4   OPT             !Menu option
	INTEGER*4   ST,EXT          !Status/Exit
	INTEGER*4   REC             !Record to modify
	INTEGER*4   LOCK            !Lock check
	INTEGER*4   LOCKCNT /0/     !Lock counter
	CHARACTER   PROMPT*60       !Input prompt string
	CHARACTER   X2FILNAM*20     !File name function
	CHARACTER   NULL*60         !Null string
	CHARACTER   NULLEQV(60)*1   !Null string
C
	DATA        NULLEQV/60*Z00/
	EQUIVALENCE (NULL,NULLEQV)
C
C OPEN THE LOCAL PORT CONFIGURATION FILE.
C
	CALL OPENX(1,X2FILNAM(XLPC),4,0,0,ST)
	CALL IOINIT(X2XLPC_FDB,1,X2XLPC_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XLPC),'OPENX',ST,0)
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
	PROMPT=NULL
	WRITE (PROMPT,9010)
	CALL INPNUM(PROMPT,OPT,1,4,EXT)
	IF(EXT.EQ.-9) THEN
	  CALL X2XHLP('X2LPCMEN.HLP')
	  GOTO 100
	ELSE IF(EXT.LT.0) THEN
	  GOTO 8000
	ENDIF
C
C READ THE RECORD NUMBER.
C
	IF(OPT.NE.4) THEN
	  PROMPT=NULL
	  WRITE (PROMPT,9020)
	  CALL INPNUM(PROMPT,REC,X2XLPC_RANGE(1,1),
     *	              X2XLPC_RANGE(2,1),EXT)
	  IF(EXT.LT.0) GOTO 100
	ENDIF
C
C IF ADD A RECORD ENSURE IT DOES NOT ALREADY EXIST.
C
	LOCKCNT=0
	IF(OPT.EQ.1) THEN
104	  CONTINUE
	  CALL READL(X2XLPC_FDB,REC,X2XLPC_REC,ST,LOCK)
	  IF(LOCK.NE.0) THEN
	    LOCKCNT=LOCKCNT+1
	    CALL XWAIT(1,2,ST)
	    IF(LOCKCNT.LT.10) GOTO 104
	    WRITE(5,9034)
	    CALL XWAIT(2,2,ST)
	    GOTO 8000
	  ENDIF
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XLPC),'READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XLPC_REC(1).GT.0) THEN
	    WRITE(5,9032) REC,CHAR(7)
	    CALL X2UNLOCK(XLPC,X2XLPC_FDB,REC,X2XLPC_REC)
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
	  CALL X2LPCADD(REC)
C
C IF MODIFY A RECORD, ENSURE IT ALREADY EXISTS.
C
	ELSE IF(OPT.EQ.2) THEN
106	  CONTINUE
	  CALL READL(X2XLPC_FDB,REC,X2XLPC_REC,ST,LOCK)
	  IF(LOCK.NE.0) THEN
	    LOCKCNT=LOCKCNT+1
	    CALL XWAIT(1,2,ST)
	    IF(LOCKCNT.LT.10) GOTO 106
	    WRITE(5,9034)
	    CALL XWAIT(2,2,ST)
	    GOTO 8000
	  ENDIF
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XLPC),'READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XLPC_REC(1).NE.REC) THEN
	    WRITE(5,9030) REC,CHAR(7)
	    CALL X2UNLOCK(XLPC,X2XLPC_FDB,REC,X2XLPC_REC)
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
	  CALL X2LPCMOD(REC)
C
C IF DELETE A RECORD, ENSURE IT ALREADY EXISTS.
C
	ELSE IF(OPT.EQ.3) THEN
108	  CONTINUE
	  CALL READL(X2XLPC_FDB,REC,X2XLPC_REC,ST,LOCK)
	  IF(LOCK.NE.0) THEN
	    LOCKCNT=LOCKCNT+1
	    CALL XWAIT(1,2,ST)
	    IF(LOCKCNT.LT.10) GOTO 108
	    WRITE(5,9034)
	    CALL XWAIT(2,2,ST)
	    GOTO 8000
	  ENDIF
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XLPC),'READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XLPC_REC(1).NE.REC) THEN
	    WRITE(5,9030) REC,CHAR(7)
	    CALL X2UNLOCK(XLPC,X2XLPC_FDB,REC,X2XLPC_REC)
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
	  CALL X2LPCDEL(REC)
C
C LIST LOCAL PORT.
C
	ELSE IF(OPT.EQ.4) THEN
	  CALL X2LPCLIS
	ENDIF
	GOTO 100
C
C PROGRAM EXIT.
C
8000	CONTINUE
	CALL CLOSEFILE(X2XLPC_FDB)
C
C     ================== Format Statements =====================
C
9000	FORMAT(////,T26,'GTECH Distributed Network',/,
     *	          T26,'Local Port Configuration',//,
     *	          T20,'  1. Add a Local Port',/,
     *	          T20,'  2. Modify a Local Port',/,
     *	          T20,'  3. Delete a Local Port',/,
     *	          T20,'  4. List Local Port',/,
     *	          T20,'  E. Exit',//)
9010	FORMAT(25(' '),'Enter option [1-4] ')
9020	FORMAT(25(' '),'Enter port index    ')
9030	FORMAT(25(' '),'Port ',I4,' does not exist',A)
9032	FORMAT(25(' '),'Port ',I4,' already exists',A)
9034	FORMAT(25(' '),'Record is locked')
	END

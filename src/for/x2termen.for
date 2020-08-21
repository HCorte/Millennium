C
C PROGRAM X2TERMEN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2TERMEN.FOV                                 $
C  $Date::   17 Apr 1996 16:38:12                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2termen.for;1 **
C
C X2TERMEN.FOR
C
C V02 03-AUG-92 NJA FIXED PROBLEM WITH OVERFLOWS.
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program provides an interface into the Statistics Report
C Code file.  This menu will allow: addition, modification,
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
	PROGRAM X2TERMEN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XTER.DEF'
C
	INTEGER*4   OPT             !Menu option
	INTEGER*4   ST,EXT          !Status/Exit
	INTEGER*4   REC             !Record to modify
	INTEGER*4   LOCK            !Lock check
	INTEGER*4   LOCKCNT         !Lock count
	CHARACTER   PROMPT*60       !Input prompt string
	CHARACTER   X2FILNAM*20     !File name function
	CHARACTER   NULL*60         !Null string
	CHARACTER   NULLEQV(60)*1   !Null string
C
	DATA        NULLEQV/60*Z00/
	EQUIVALENCE (NULL,NULLEQV)
C
C OPEN THE STATISTICS REPORT CODE FILE.
C
	CALL OPENX(1,X2FILNAM(XTER),4,0,0,ST)
	CALL IOINIT(X2XTER_FDB,1,X2XTER_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XTER),'OPENX',ST,0)
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
	  CALL X2XHLP('X2TERMEN.HLP')
	  GOTO 100
	ELSE IF(EXT.LT.0) THEN
	  GOTO 8000
	ENDIF
C
C READ THE RECORD NUMBER.
C
	IF(OPT.NE.4) THEN
	  WRITE (PROMPT,9020)
	  CALL INPNUM(PROMPT,REC,X2XTER_RANGE(1,1),
     *	              X2XTER_RANGE(2,1),EXT)
	  IF(EXT.LT.0) GOTO 100
	ENDIF
C
C IF ADD A RECORD ENSURE IT DOES NOT ALREADY EXIST.
C
	LOCKCNT=0
	IF(OPT.EQ.1) THEN
104	  CONTINUE
	  CALL READL(X2XTER_FDB,REC,X2XTER_REC,ST,LOCK)
	  IF(LOCK.NE.0) THEN
	    LOCKCNT=LOCKCNT+1
	    CALL XWAIT(1,2,ST)
	    IF(LOCKCNT.LT.10) GOTO 104
	    WRITE(5,9034)
	    CALL XWAIT(2,2,ST)
	    GOTO 8000
	  ENDIF
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XTER),'READL',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XTER_REC(1).GT.0) THEN
	    WRITE(5,9032) REC,CHAR(7)
	    CALL X2UNLOCK(XTER,X2XTER_FDB,REC,X2XTER_REC)
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
	  CALL X2TERADD(REC)
C
C IF MODIFY A RECORD, ENSURE IT ALREADY EXISTS.
C
	ELSE IF(OPT.EQ.2) THEN
106	  CONTINUE
	  CALL READL(X2XTER_FDB,REC,X2XTER_REC,ST,LOCK)
	  IF(LOCK.NE.0) THEN
	    LOCKCNT=LOCKCNT+1
	    CALL XWAIT(1,2,ST)
	    IF(LOCKCNT.LT.10) GOTO 106
	    WRITE(5,9034)
	    CALL XWAIT(2,2,ST)
	    GOTO 8000
	  ENDIF
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XTER),'READL',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XTER_REC(1).NE.REC) THEN
	    WRITE(5,9030) REC,CHAR(7)
	    CALL X2UNLOCK(XTER,X2XTER_FDB,REC,X2XTER_REC)
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
	  CALL X2TERMOD(REC)
C
C IF DELETE A RECORD, ENSURE IT ALREADY EXISTS.
C
	ELSE IF(OPT.EQ.3) THEN
108	  CONTINUE
	  CALL READL(X2XTER_FDB,REC,X2XTER_REC,ST,LOCK)
	  IF(LOCK.NE.0) THEN
	    LOCKCNT=LOCKCNT+1
	    CALL XWAIT(1,2,ST)
	    IF(LOCKCNT.LT.10) GOTO 108
	    WRITE(5,9034)
	    CALL XWAIT(2,2,ST)
	    GOTO 8000
	  ENDIF
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XTER),'READL',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XTER_REC(1).NE.REC) THEN
	    WRITE(5,9030) REC,CHAR(7)
	    CALL X2UNLOCK(XTER,X2XTER_FDB,REC,X2XTER_REC)
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
	  CALL X2TERDEL(REC)
C
C LIST TERMINAL INFORMATION.
C
	ELSE IF(OPT.EQ.4) THEN
	  CALL X2TERLIS
	ENDIF
	GOTO 100
C
C PROGRAM EXIT.
C
8000	CONTINUE
	CALL CLOSEFILE(X2XTER_FDB)
C
C     ================== Format Statements =====================
C
9000	FORMAT(////,T26,'GTECH Distributed Network',/,
     *	          T25,'Terminal Configuration Menu',//,
     *	          T20,'  1. Add a terminal to a port',/,
     *	          T20,'  2. Modify a terminal on a port',/,
     *	          T20,'  3. Delete a terminal on a port',/,
     *	          T20,'  4. List terminals',/,
     *	          T20,'  E. Exit',//)
9010	FORMAT(25(' '),'Enter option [1-4]     ')
9020	FORMAT(25(' '),'Enter terminal number  ')
9030	FORMAT(25(' '),'Terminal  ',I5,' does not exist',A)
9032	FORMAT(25(' '),'Terminal  ',I5,' already exists',A)
9034	FORMAT(25(' '),'Record is locked ')
	END

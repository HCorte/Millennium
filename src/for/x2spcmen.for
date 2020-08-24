C
C PROGRAM X2SPCMEN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2SPCMEN.FOV                                 $
C  $Date::   17 Apr 1996 16:35:16                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2spcmen.for;1 **
C
C X2SPCMEN.FOR
C
C V02 03-FEB-94 GPR USE I5 FORMAT FOR STATION AND TERMINAL TYPE-OUTS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program provides an interface into the Station Port
C Configuration file.  This menu will allow: addition,
C modification, and deletion of host addresses.
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
	PROGRAM X2SPCMEN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XSPC.DEF'
C
	INTEGER*4   OPT             !Menu option
	INTEGER*4   ST,EXT          !Status/Exit
	INTEGER*4   REC,EXSREC      !Record to add/modify
	INTEGER*4   STATION,PORT    !Input data
	CHARACTER   PROMPT*60       !Input prompt string
	CHARACTER   X2FILNAM*20     !File name function
	CHARACTER   NULL*60         !Null string
	CHARACTER   NULLEQV(60)*1   !Null string
C
	DATA        NULLEQV /60*Z00/
	EQUIVALENCE (NULL,NULLEQV)
C
C OPEN THE STATISTICS REPORT CODE FILE. (BUFFERED I/O)
C
	CALL OPENX2X(X2FILNAM(XSPC),4)
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
	  CALL X2XHLP('X2SPCMEN.HLP')
	  GOTO 100
	ELSE IF(EXT.LT.0) THEN
	  GOTO 8000
	ENDIF
C
C READ THE STATION NUMBER.
C
	IF(OPT.NE.4) THEN
	  PROMPT=NULL
	  WRITE (PROMPT,9020)
	  CALL INPNUM(PROMPT,STATION,X2XSPC_RANGE(1,1),
     *	              X2XSPC_RANGE(2,1),EXT)
	  IF(EXT.LT.0) GOTO 100
C
C READ THE PORT NUMBER.
C
	  PROMPT=NULL
	  WRITE (PROMPT,9022)
	  CALL INPNUM(PROMPT,PORT,X2XSPC_RANGE(1,2),
     *	              X2XSPC_RANGE(2,2),EXT)
	  IF(EXT.LT.0) GOTO 100
	ENDIF
C
C IF ADD A RECORD ENSURE IT DOES NOT ALREADY EXIST.
C
	IF(OPT.EQ.1) THEN
	  CALL X2FNDSPC(STATION,PORT,ST,REC,EXSREC)
	  IF(ST.NE.0) THEN
	    IF(ST.EQ.-99) THEN
	      WRITE(5,9034)
	    ELSE
	      WRITE(5,9032) PORT,STATION,CHAR(7)
	    ENDIF
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
	  CALL X2SPCADD(STATION,PORT,REC)
C
C IF MODIFY A RECORD, ENSURE IT ALREADY EXISTS.
C
	ELSE IF(OPT.EQ.2) THEN
	  CALL X2FNDSPC(STATION,PORT,ST,REC,EXSREC)
	  IF(ST.NE.-1) THEN
	    IF(ST.EQ.-99) THEN
	      WRITE(5,9034)
	    ELSE
	      WRITE(5,9030) PORT,STATION,CHAR(7)
	    ENDIF
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
	  CALL X2SPCMOD(EXSREC)
C
C IF DELETE A RECORD, ENSURE IT ALREADY EXISTS.
C
	ELSE IF(OPT.EQ.3) THEN
	  CALL X2FNDSPC(STATION,PORT,ST,REC,EXSREC)
	  IF(ST.NE.-1) THEN
	    IF(ST.EQ.-99) THEN
	      WRITE(5,9034)
	    ELSE
	      WRITE(5,9030) PORT,STATION,CHAR(7)
	    ENDIF
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
	  CALL X2SPCDEL(EXSREC)
C
C LIST STATION PORTS
C
	ELSE IF(OPT.EQ.4) THEN
	  CALL X2SPCLIS
	ENDIF
	GOTO 100
C
C PROGRAM EXIT.
C
8000	CONTINUE
	CALL CLOSX2X(4)
C
C     ================== Format Statements =====================
C
9000	FORMAT(////,T26,'GTECH Distributed Network',/,
     *	          T25,'Station Port Configuration Menu',//,
     *	          T20,'  1. Add a port to a station',/,
     *	          T20,'  2. Modify a port on a station',/,
     *	          T20,'  3. Delete a port on a station',/,
     *	          T20,'  4. List station ports',/,
     *	          T20,'  E. Exit',//)
9010	FORMAT(25(' '),'Enter option [1-4] ')
9020	FORMAT(25(' '),'Enter station number  ')
9022	FORMAT(25(' '),'Enter port number     ')
9030	FORMAT(25(' '),'Port  ',I4,' on station ',I5,
     *	               ' does not exist',A)				! V02
9032	FORMAT(25(' '),'Port  ',I4,' on station ',I5,
     *	               ' already exists',A)				! V02
9034	FORMAT(25(' '),'Record is locked')
	END

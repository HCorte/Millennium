C
C PROGRAM X2SCLMEN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2SCLMEN.FOV                                 $
C  $Date::   17 Apr 1996 16:33:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2sclmen.for;1 **
C
C X2SCLMEN.FOR
C
C V02 11-DEC-95 JAC ADDED COPY FUNCTION X2SCLCOPY.FOR
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program provides an interface into the Station Class
C file.  This menu will allow: addition, modification, COPYING
C and deletion of host addresses.
C
C Calling Sequence:
C
C	N/A
C
C Input Parameters (SUBROUTINES):
C
C	X2SCLADD(REC)
C	X2SCLDEL(REC)
C	X2SCLMOD(REC)
C	X2SCLCOPY(REC,TO_REC)
C	X2SCLLIS()
C	X2UNLOCK()
C	X2XHLP()
C	X2XFILNAM()
C
C Output Parameters:
C
C	NONE
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2SCLMEN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
C
        INTEGER*4   ANS             !Input response
	INTEGER*4   OPT             !Menu option
	INTEGER*4   ST,EXT          !Status/Exit
	INTEGER*4   REC             !Record to modify
	INTEGER*4   TO_REC          !Record to copy to
	INTEGER*4   LOCK            !Lock check
	INTEGER*4   LOCKCNT /0/     !Lock counter
        INTEGER*4   X2XSCL_REC2(128)  !2nd buffer used for copy function
	CHARACTER   PROMPT*60       !Input prompt string
	CHARACTER   PROMPT2*70      !Input prompt string
	CHARACTER   X2FILNAM*20     !File name function
	CHARACTER   NULL*60         !Null string
	CHARACTER   NULLEQV(60)*1   !Null string
C
	DATA        NULLEQV/60*Z00/
	EQUIVALENCE (NULL,NULLEQV)
C
C OPEN THE STATISTICS REPORT CODE FILE.
C
	CALL OPENX(1,X2FILNAM(XSCL),4,0,0,ST)
	CALL IOINIT(X2XSCL_FDB,1,X2XSCL_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XSCL),'OPENX',ST,0)
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
C				    Display "Enter option [1-5] "
C			    1-Add, 2-Modify, 3-Delete, 4-Copy, 5-List
C			    H-Help (EXT = -9), E-Exit (EXT = -1)
	WRITE (PROMPT,9010)
	CALL INPNUM(PROMPT,OPT,1,5,EXT)
C			    If help is selected
	IF(EXT.EQ.-9) THEN
	  CALL X2XHLP('X2SCLMEN.HLP')
	  GOTO 100
C			    If exit is selected
	ELSE IF(EXT.LT.0) THEN
	  GOTO 8000
	ENDIF
C
C
C IF OPT = STATION CLASS LISTING.
C
	IF(OPT.EQ.5) THEN
          LOCKCNT=0
	  CALL X2SCLLIS
          GOTO 100
	ENDIF 
C
C
C READ THE RECORD NUMBER.
C				If we are NOT doing a listing...
	PROMPT=NULL
        IF(OPT.EQ.4) THEN
C				DISPLAY "Copy station clas from No? "
          WRITE (PROMPT,9025)
          CALL INPNUM(PROMPT,REC,X2XSCL_RANGE(1,1),
     *                X2XSCL_RANGE(2,1),EXT)
          PROMPT = NULL
C				DISPLAY "Copy to station class No.? "
          WRITE (PROMPT,9028)
          CALL INPNUM(PROMPT,TO_REC,X2XSCL_RANGE(1,1),
     *                X2XSCL_RANGE(2,1),EXT)
        ELSE
C				Display "Enter station class list " 
	  WRITE (PROMPT,9020)
	  CALL INPNUM(PROMPT,REC,X2XSCL_RANGE(1,1),
     *	              X2XSCL_RANGE(2,1),EXT)
	ENDIF
C				If a letter was input	
        IF(EXT.LT.0) THEN
          GOTO 100
        ENDIF
C
C
C				    Read the data -> X2XSCL_REC
C
	LOCKCNT=0
104	CONTINUE
	CALL READL(X2XSCL_FDB,REC,X2XSCL_REC,ST,LOCK)
	IF(LOCK.NE.0) THEN
	  LOCKCNT=LOCKCNT+1
	  CALL XWAIT(1,2,ST)
	  IF(LOCKCNT.LT.10) GOTO 104
C				    Record is locked; close and quit
	  WRITE(5,9034)
	  CALL XWAIT(2,2,ST)
	  GOTO 8000
	ENDIF
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XSCL),'READL',ST,1)
	  CALL GPAUSE
	ENDIF
C
C
C
C IF option = ADD A RECORD, ensure it does not already exist.
C
	IF(OPT.EQ.1) THEN
C		X2XSCL_REC(1) = X2XSCL_CLASS = 0 if record is undefined 
	  IF(X2XSCL_CLASS.GT.0) THEN
C					Display "Class already exists"
	    WRITE(5,9032) REC,CHAR(7)
	    CALL X2UNLOCK(XSCL,X2XSCL_FDB,REC,X2XSCL_REC)
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
C					else.... add the record
	  CALL X2SCLADD(REC)
C
C
C
C IF OPT = MODIFY, DELETE OR COPY A RECORD, ensure it already exists.
C
	ELSE IF((OPT.EQ.2).OR.(OPT.EQ.3).OR.(OPT.EQ.4)) THEN
	  IF(X2XSCL_CLASS.NE.REC) THEN
C					Display "Class does not exist"
	    WRITE(5,9030) REC,CHAR(7)
	    CALL X2UNLOCK(XSCL,X2XSCL_FDB,REC,X2XSCL_REC)
	    CALL XWAIT(2,2,ST)
	    GOTO 100
	  ENDIF
C				    else.... Modify, Delete or Copy the record
          IF (OPT.EQ.2) THEN
	     CALL X2SCLMOD(REC)

          ELSE IF (OPT.EQ.3) THEN
             CALL X2SCLDEL(REC)
C
C			          - N E W -   C O P Y   F U N C T I O N

          ELSE IF (OPT.EQ.4) THEN
C                                  check if destination record already exists
	    LOCKCNT=0
106	    CONTINUE
	    CALL READL(X2XSCL_FDB,TO_REC,X2XSCL_REC2,ST,LOCK)
	    IF(LOCK.NE.0) THEN
	      LOCKCNT=LOCKCNT+1
	      CALL XWAIT(1,2,ST)
	      IF(LOCKCNT.LT.10) GOTO 106
C                                       Record is Locked; close and quit
	      WRITE(5,9034)
	      CALL XWAIT(2,2,ST)
	      GOTO 8000
	    ENDIF
	    IF(ST.NE.0) THEN
	      CALL OS32ER(5,X2FILNAM(XBRO),'READL',ST,1)
	      CALL GPAUSE
	    ENDIF


	    IF(X2XSCL_REC2(1).GT.0) THEN
C 			    Ensure operator wishes to overwrite this record. 
C			    Display "Record already exists, overwrite?"
	      WRITE(5,*)
	      WRITE(5,*)
	      WRITE(PROMPT2,9033) TO_REC,CHAR(7)
	      CALL WIMG(5,PROMPT2)
	      CALL YESNO(ANS)
C				    If no, exit
	      IF(ANS.EQ.2) THEN
                WRITE(5,9040) TO_REC
	        CALL XWAIT(2,2,ST)
                GOTO 100 
	      ENDIF
            ENDIF
C 				    Record to be overwritten
            CALL X2SCLCOPY(REC,TO_REC)

          ENDIF   !/*   End of If Modify, Delete or Copy   */  
	ENDIF    !/*  End of IF options  */

	GOTO 100
C
C
C PROGRAM EXIT.
C
8000	CONTINUE
	CALL CLOSEFILE(X2XSCL_FDB)
C
C     ================== Format Statements =====================
C
9000	FORMAT(////,T26,'GTECH Distributed Network',/,
     *	          T29,'Station Class Menu',//,
     *	          T20,'  1. Add a station class',/,
     *	          T20,'  2. Modify a station class',/,
     *	          T20,'  3. Delete a station class',/,
     *	          T20,'  4. Copy a station class',/,
     *	          T20,'  5. Station class List',/,
     *	          T20,'  E. Exit',//)
9010	FORMAT(25(' '),'Enter option [1-5], "E" or "H" ')
9020	FORMAT(25(' '),'Enter station class   ')
9025	FORMAT(25(' '),'Copy station class from No.? ')
9028	FORMAT(25(' '),'Copy to station class No.? ')
9030	FORMAT(25(' '),'Class ',I4,' does not exist',A)
9032	FORMAT(25(' '),'Class ',I4,' already exists',A)
9033	FORMAT(25(' '),'Record ',I4,' exists, overwrite? [Y/N] ',A)
9034	FORMAT(25(' '),'Record is locked')
9040    FORMAT(25(' '),'Record 'I4,' has not been overwritten ')
	END

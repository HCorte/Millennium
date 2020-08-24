C
C PROGRAM BLDX2X
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]BLDX2X.FOV                                   $
C  $Date::   17 Apr 1996 12:20:04                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - bldx2x.for;1 **
C
C BLDX2X.FOR
C
C V03 06_FEB-95 DAS REMOVED DELETE TERMINAL OPTION 
C V02 28_AUG-95 WJK REMOVE THE USE OF RUNTSK TO START PROCESSES.  ON THE ALPHA
C                   RUNTSK CAUSES BLDX2X AND THE CHOSEN TASK WILL RUN IN 
C		    PARALLEL.  SPAWN WAIT FUNCTION IS USED.
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This program will act as the main menu for updating the
C X.2X parameters for the GTECH Distributed Network.
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
	PROGRAM BLDX2X
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   FLAG            !Outstanding modifications flag
	INTEGER*4   OPT,EXT         !Input option/exit flag
	INTEGER*4   ST,I            !Status
	INTEGER*4   ANS             !Input response
	INTEGER*4   TSKNAM(2)       !Terminal name
	INTEGER*4   I4DEV(2)        !I4 device name
	INTEGER*4   DUMMY(2)        !Dummy parameter
	INTEGER*4   FILNAM(4)       !File to be loaded
	CHARACTER   PROMPT*65       !Input prompt string
	CHARACTER   DEVICE*8        !Device name
	CHARACTER   NULEQV(65)*1    !Null character
	CHARACTER   NULL*65         !Null character
	CHARACTER   CFILNAM*16      !Task to be executed
	CHARACTER   CTSKNAM*8       !Terminal name
	CHARACTER   X2FILNAM*20     !File name function
	CHARACTER   RUNTSKCMD*18    !V02 Run task name command string
	INTEGER*4   SPAWN_FLG	    !V02 SPAWN optional behavior flag
	PARAMETER   (SPAWN_FLG = 0) !V02 put calling process in hibernation
				    !until subprocess completes
C
	DATA         NULEQV / 65*'00'X /
	EQUIVALENCE (DEVICE,I4DEV)
	EQUIVALENCE (FILNAM,CFILNAM)
	EQUIVALENCE (TSKNAM,CTSKNAM)
	EQUIVALENCE (NULL,  NULEQV)
C
C CLEAR THE SCREEN, DISPLAY THE MENU, AND GET THE
C MENU OPTION.
C
100	CONTINUE
	PROMPT=NULL
	WRITE (PROMPT,9010)
	CALL CLRSCR(5)
	WRITE(5,9000)
	WRITE(5,9002)
	CALL INPNUM(PROMPT,OPT,1,18,EXT)
C
C USER WISHES TO EXIT, CHECK TO SEE IF ANY OUTSTANDING MODIFICATIONS
C HAVE BEEN PERFORMED.  IF ERRORS STILL EXIST, DISPLAY A WARNING.
C
	IF(EXT.LT.0 .AND. EXT.NE.-9) THEN
	  CALL X2CHKEXT(FLAG)
	  IF(FLAG.EQ.0) THEN
	    CALL GSTOP(GEXIT_SUCCESS)
	  ELSE
	    WRITE(5,9040) X2FILNAM(FLAG)
	    WRITE (PROMPT,9020)
	    CALL WIMG(5,PROMPT)
	    CALL YESNO(ANS)
	    IF(ANS.EQ.1) THEN
	      CALL GSTOP(GEXIT_SUCCESS)
	    ELSE
	      GOTO 100
	    ENDIF
	  ENDIF
	ENDIF
C
C LOAD AND RUN TASK BASED ON INPUT FUNCTION CODE.
C
	IF(EXT.EQ.-9) THEN
	  CALL X2XHLP('BLDX2X.HLP/2')
	  GOTO 100
C
C       Start of V02 changes
C
	ELSE IF(OPT.EQ.1) THEN
	  CTSKNAM = 'X2GBLMEN'
	ELSE IF (OPT.EQ.2) THEN
	  CTSKNAM = 'X2NPCMEN'
	ELSE IF (OPT.EQ.3) THEN
	  CTSKNAM = 'X2LPCMEN'
	ELSE IF (OPT.EQ.4) THEN
	  CTSKNAM = 'X2RCDMEN'
	ELSE IF (OPT.EQ.5) THEN
	  CTSKNAM = 'X2RCLMEN'
	ELSE IF (OPT.EQ.6) THEN
	  CTSKNAM = 'X2SCLMEN'
	ELSE IF (OPT.EQ.7) THEN
	  CTSKNAM = 'X2SPCMEN'
	ELSE IF (OPT.EQ.8) THEN
	  CTSKNAM = 'X2TERMEN'
	ELSE IF (OPT.EQ.9) THEN
	  CTSKNAM = 'X2STNMEN'
	ELSE IF (OPT.EQ.10) THEN
	  CTSKNAM = 'X2BROMEN'
	ELSE IF (OPT.EQ.11) THEN
	  CTSKNAM = 'X2GRPMEN'
	ELSE IF (OPT.EQ.12) THEN
	  CTSKNAM = 'X2BLDNET'
	ELSE IF (OPT.EQ.13) THEN
	  CTSKNAM = 'X2TTNMEN'
	ELSE IF (OPT.EQ.14) THEN
	  CTSKNAM = 'X2CHKMEN'
	ELSE IF (OPT.EQ.15) THEN
	  CTSKNAM = 'VISION  '
	ELSE IF (OPT.EQ.16) THEN
	  CTSKNAM = 'HASF    '
	ELSE IF (OPT.EQ.17) THEN
	  CTSKNAM = 'X2RELLOC'
C....	ELSE IF (OPT.EQ.18) THEN
C....	  CTSKNAM = 'X2DELTER'
	ENDIF
	RUNTSKCMD = 'RUN GXTSK:'//CTSKNAM
	CALL LIB$SPAWN(%DESCR(RUNTSKCMD(1:LEN(RUNTSKCMD))),,SPAWN_FLG,,,,,,,,,)
C
C       End of V02 changes
C
	GOTO 100
C
C     =================== Format Statements ===================
C
9000	FORMAT(/, T26,'GTECH Distributed Network',/,
     *	          T24,'Transport Level Update Program',///,
     *	          T05,'  1. Global Parameters.........',
     *	          T39,'  2. Network Ports.............',/,
     *	          T05,'  3. Local Ports...............',
     *	          T39,'  4. Report Codes..............',/,
     *	          T05,'  5. Report Class..............',
     *	          T39,'  6. Station Class.............')
9002	FORMAT(   T05,'  7. Station Port Configuration',
     *	          T39,'  8. Terminal Configuration....',/,
     *	          T05,'  9. Station Configuration.....',
     *	          T39,' 10. Relay Configuration.......',/,
     *	          T05,' 11. Group Configuration.......',
     *	          T39,' 12. Auto Build from ASF.......',/,
     *	          T05,' 13. TITN Parameters...........',
     *	          T39,' 14. Perform Edit Checks.......',/,
     *	          T05,' 15. VISION....................',
     *	          T39,' 16. HASF......................',/,
     *	          T05,' 17. Release record locks......',//,
C... *	          T39,' 18. Auto Delete a Terminal....',//,
     *	          T05,'  H. Help......................',
     *	          T39,'  E. Exit......................',///)
9010	FORMAT(25(' '),'Enter option [1-18]  ')
9020	FORMAT(10(' '),'Edit check not successfully completed ',
     *	               '- exit [Y/N] ')
9040	FORMAT(10X,'File ',A20,' has not been checked ')
	END

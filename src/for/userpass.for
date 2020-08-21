C PROGRAM USERPASS
C  
C V04 19 May 1996 HXK Wojtek's security stuff added
C V03 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C V02 01-AUG-1990 XXX RELEASED FOR VAX
C V01 18-MAY-1988 DSL Initial release for password security system
C                    Use this routine to set up and maintain USER
C                    file.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
	 OPTIONS /CHECK=NOOVERFLOW
	 PROGRAM USERPASS
	 IMPLICIT NONE
C
	 INCLUDE 'INCLIB:SYSPARAM.DEF'
	 INCLUDE 'INCLIB:RECUSE.DEF'
	 INCLUDE 'INCLIB:PRMLVL.DEF'
C
	 INTEGER*4  SUPLOC
	 PARAMETER (SUPLOC=1)   !Location of superuser in LEVELS file
C
	 CHARACTER*6 PASSENT,DEFTPASS
	 INTEGER*4 SECLEV,STATUS, ACT, ST, I, NOCHECK0, EXIT
	 INTEGER*4 UID,LEV,SIND
	 CHARACTER*20 PASPAS
	 DATA DEFTPASS/'DONUTS'/
	 EQUIVALENCE (PASPAS,PASSENT,EXIT)
	 COMMON /NOCHECK0/ NOCHECK0
C
	 CALL COPYRITE
C
	 NOCHECK0=-1
C
C Get  security levels and set default flag for ( no LEVELS file)
C
	 CALL BUILDLEV(UINDEX,STATUS)
         IF (STATUS.EQ.1) THEN
           GOTO 5
         ENDIF
C
C Prompt user to sign on with user ID and password
C
	 CALL CLRSCR(5)
	 CALL SGNON(UINDEX,SECLEV,UID,SIND,STATUS)
	 CALL USRCLOS1(     2)
C
C If status equals to 1 then USER.FIL or LEVEL.FIL  not on pack
C Ask for default password
C
 5	 CONTINUE
C
	 IF (STATUS.EQ.1) THEN
10	   CONTINUE
           CALL BUILDALL(UINDEX,STATUS)
           LEV=SUPLOC
           CALL CLRSCR(5)
           CALL PASSWORD(5,PASPAS)
           IF(EXIT.EQ.'EXIT') THEN
	     GOTO 200
           ENDIF
           IF(PASSENT.EQ.DEFTPASS.AND.PASSENT.NE.'        ') GOTO 30
           GOTO 10
	 ENDIF
C
C Get proper group level
C
	 DO 20 I=1,16
	    IF (SECLEV.EQ.LEVELS(I)) THEN
	       LEV=I
	       GOTO 30
	    ENDIF
 20	 CONTINUE
C
C Security level not declared in levels file
C
	 CALL CLRSCR(5)
	 WRITE(5,902)
	 GOTO 200
C
C Print main menu to screen
C
30	 CALL CLRSCR(5)
	 WRITE(5,900)
	 CALL INPNUM('Please enter the desired action   : ',ACT,1,7,ST)
C
C Exit from program
C
	 IF(ST.EQ.-1) THEN
	   GOTO 200
	 ENDIF
C
C Determine if the choice is allowed at this level
C
	 CALL CHKUMENU(ACT,LEV,ST)
	 IF (ST.NE.0) THEN
	   CALL CLRSCR(5)
	   WRITE(5,901)
	   CALL XWAIT(2,2,ST)
	   GOTO 30
	 ENDIF
	 CALL CLRSCR(5)
C
C Goto add a user routine
C
	 IF(ACT.EQ.1) THEN
	   CALL ADDUSER(UID)
	   GOTO 30
C
C Goto modify a user routine
C
	 ELSEIF(ACT.EQ.2) THEN
	   CALL MODUSER(UID)
	   GOTO 30
C
C Goto delete a user routine
C
	 ELSEIF(ACT.EQ.3) THEN
	   CALL DELUSER(UID)
	   GOTO 30
C
C Goto display a user routine
C
	 ELSEIF(ACT.EQ.4) THEN
	   CALL DISUSER
	   GOTO 30
C
C Goto print a user routine
C
	 ELSEIF(ACT.EQ.5) THEN
	   CALL PRTUSER(UID)
	   GOTO 30
C
C Goto inquire about the user file routine
C
	 ELSEIF(ACT.EQ.6) THEN
	   CALL INQUSER(UID)
	   GOTO 30
C
C Goto initial file routine
C
	 ELSEIF(ACT.EQ.7) THEN
	   CALL INITUSER
	   GOTO 30
C
	 ELSE
	   GOTO 30
	 ENDIF
C
C Exit
C
200      CONTINUE
	 CALL USRCLOS1(     2)
	 CALL USRCLOS1(     5)
	 CALL USRCLOS1(     6)
         CALL XWAIT(1,2,ST)
         CALL CLRSCR(5)
         CALL GSTOP(GEXIT_SUCCESS)
C
C
900	  FORMAT(//,15X,'     < < < <  U S E R   M E N U  > > > >',
     *	         //,15X,'    1.  Add user to USER file ',
     *	         //,15X,'    2.  Modify user in USER file ',
     *	         //,15X,'    3.  Delete user in USER file '
     *	        ,//,15X,'    4.  Display a user record '
     *	        ,//,15X,'    5.  Print a user record '
     *	        ,//,15X,'    6.  Inquire about the user file'
     *	        ,//,15X,'    7.  Initialize USER file  '
     *	        ,//,15X,'            E  -  EXIT',//)
901   FORMAT(' This request cannot be granted on this security level')
902   FORMAT(' Security level not declared in LEVELS file')
      END

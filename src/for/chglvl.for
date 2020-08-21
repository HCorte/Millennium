C
C PROGRAM CHGLVL
C $Log:   GXAFXT:[GOLS]CHGLVL.FOV  $
C  
C     Rev 1.1   19 May 1996 17:44:54   HXK
C  Wojtek's security stuff added
C  
C     Rev 1.0   21 Jan 1993 15:48:24   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - chglvl.for **
C
C CHGLVL.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C CHGLVL
C
C V01  31-MAY-89 BEN Initial release (Michigan)
C
	  OPTIONS /CHECK=NOOVERFLOW
	  PROGRAM CHGLVL
	  IMPLICIT NONE
C
	  INCLUDE 'INCLIB:SYSPARAM.DEF'
	  INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	  INCLUDE 'INCLIB:GLOBAL.DEF'
	  INCLUDE 'INCLIB:CONCOM.DEF'
	  INCLUDE 'INCLIB:PRMLVL.DEF'
	  INCLUDE 'INCLIB:RECUSE.DEF'
          INCLUDE 'INCLIB:DESLVL.DEF'
C
	  INTEGER*4  SUPLOC
	  PARAMETER (SUPLOC=1)
C
	  INTEGER*4 MSEL, ST, NOCHECK0, LEV, I
	  INTEGER*4 UID, SECLEV, STATUS, SIND, EXIT
C
	  CHARACTER*6 PASSENT,DEFTPASS
	  CHARACTER*20 PASPAS
C
	  DATA DEFTPASS/'DONUTS'/
	  EQUIVALENCE (PASPAS,PASSENT,EXIT)
	  COMMON /NOCHECK0/ NOCHECK0
C
C This is the beginning
C
	 CALL COPYRITE
	 CALL XWAIT(1,2,ST)
C
	 NOCHECK0=-1
C
C Get security levels
C
         CALL BUILDLEV(LINDEX,STATUS)
         IF (STATUS.EQ.1) THEN
            GOTO 5
         ENDIF
C
C Prompt user to sign on with user ID and password
C
	 CALL CLRSCR(5)
	 CALL SGNON(LINDEX,SECLEV,UID,SIND,STATUS)
	 CALL USRCLOS1(     2)
C
C If status equals to 1 then USER.FIL or LEVEL.FIL  not on pack
C Ask for default password
C
 5	 CONTINUE
C
         IF (STATUS.EQ.1) THEN
10	   CONTINUE
	   CALL BUILDALL(LINDEX,STATUS)
           LEV=SUPLOC
           CALL CLRSCR(5)
           CALL PASSWORD(5,PASPAS)
           IF(EXIT.EQ.'EXIT') THEN
	     GOTO 200
           ENDIF
           IF(PASSENT.EQ.DEFTPASS.AND.PASSENT.NE.'        ') GOTO 100
           GOTO 10
         ENDIF
C
C Get proper group level
C
         DO 20 I=1,16
           IF (SECLEV.EQ.LEVELS(I)) THEN
             LEV=I
             GOTO 100
           ENDIF
20       CONTINUE
C
C If level not in the file, exit
C
         CALL CLRSCR(5)
         WRITE(5,903)
	 GOTO 200
C
C Print main menu to screen
C
100	 CALL CLRSCR(5)
C
	 WRITE(5,900)			    !level line
	 WRITE(5,901)			    !menu display
C
	 CALL INPNUM('Please enter the desired action   : ',MSEL,1,5,ST)
C
	 IF(ST.EQ.-1) THEN
	   GOTO 200
	 ENDIF
C
C Test if level allows access. 
C
         CALL CHKCMENU(MSEL,LEV,ST)
         IF (ST.NE.0) THEN
           CALL CLRSCR(5)
           WRITE(5,902)
           CALL XWAIT(1,2,ST)
           GOTO 100
         ENDIF
C
	 IF(MSEL.EQ.1) THEN
	   CALL ADDLVL
	   GOTO 100
	 ELSEIF(MSEL.EQ.2) THEN
	   CALL DELLVL
	   GOTO 100
	 ELSEIF(MSEL.EQ.3) THEN
	   CALL VIEWLVL
	   GOTO 100
	 ELSEIF(MSEL.EQ.4) THEN
	   CALL DISPLVL
	   GOTO 100
	 ELSEIF(MSEL.EQ.5) THEN
	   CALL INILVL      
	   GOTO 100
	 ELSE
	   GOTO 100
	 ENDIF
C
C Exit
C
200	CONTINUE
	CALL XWAIT(1,2,ST)
        CALL CLRSCR(5)
        CALL GSTOP(GEXIT_SUCCESS)
C
900    FORMAT(///20X,' <<<    LEVEL FILE EDITOR   >>>   ')
901    FORMAT(//20X,'       1. Add level             ',/,
     *	         20X,'       2. Delete level          ',/,
     *	         20X,'       3. Display level         ',/,
     *	         20X,'       4. Display options       ',/,
     *	         20X,'       5. Initialize level file ',/,
     *	         20X,'                                ',/,
     *	         20X,'            E - Exit            '///)
902    FORMAT(' This request cannot be granted on this security level')
903    FORMAT(' Security level not declared in LEVELS file')
       END

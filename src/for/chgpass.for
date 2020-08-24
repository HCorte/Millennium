C
C SUBROUTINE CHGPASS
C $Log:   GXAFXT:[GOLS]CHGPASS.FOV  $
C  
C     Rev 1.1   19 May 1996 17:45:00   HXK
C  Wojtek's security stuff added
C  
C     Rev 1.0   21 Jan 1993 15:48:30   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_chgpass.for **
C
C CHGPASS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 06-NOV-90 DSL  INITIAL RELEASE MASS
C
C
C     CALLING SEQUENCE :
C                      INPUT: ID - user id
C                          INDEX - function index
C
C                     OUTPUT: SLEVEL - group level
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
C=======OPTIONS /CHECK=NOOVERFLOW
	 SUBROUTINE CHGPASS(ID,INDEX,SLEVEL,SIND)
	 IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
C
	CHARACTER *4 EXIT,EXIT1,EXIT2
	CHARACTER *6 CHANGE
	CHARACTER*8 PASSOLD,PASSNEW,PASSNEW1,SPWORD,UPWORD
	CHARACTER*20 PASOLD,PASNEW,PASNEW1
	INTEGER *4 ID,SLEVEL,NIBINX,SIND, STAT, ST
	INTEGER *4 INDEX
	INTEGER *4 DATES(3)
	EQUIVALENCE(PASOLD,PASSOLD,EXIT)
	EQUIVALENCE(PASNEW,PASSNEW,EXIT1,CHANGE)
	EQUIVALENCE(PASNEW1,PASSNEW1,EXIT2)
C
C Unscramble password
C
	CALL ENCPAS(ID,PASWORD1,UPWORD)
C
C Ask for old password
C
 4	CONTINUE
	CALL CLRSCR(5)
	WRITE(5,900)
900	FORMAT(' Enter old')
	CALL PASSWORD(5,PASOLD)
	CALL CHKPAS(PASSOLD,ST)
	IF(ST.NE.0) GOTO 4
	IF(EXIT.EQ.'EXIT') THEN
	   CALL CLRSCR(5)
	   CALL GSTOP(GEXIT_OPABORT)
	ENDIF
C
C Test if old password is correct
C
	IF (PASSOLD.NE.UPWORD) THEN
	   SIND=1
	   WRITE(5,904)
904	   FORMAT(' Passwords do not match')
	   CALL XWAIT(2,2,STAT)
           CALL PRINTSON(INDEX,SIND)
	   GOTO 4
	ENDIF
C
C Ask for new password
C
 6	CONTINUE
	CALL CLRSCR(5)
	WRITE(5,9011)
9011	FORMAT(' Enter new')
	CALL PASSWORD(5,PASNEW)
	CALL CHKPAS(PASSNEW,ST)
	IF(ST.NE.0) GOTO 6
	IF(EXIT1.EQ.'EXIT') THEN
	  CALL CLRSCR(5)
	  CALL GSTOP(GEXIT_OPABORT)
	ENDIF
C
C Enter new password again for verification
C
 7	CONTINUE
	CALL CLRSCR(5)
	WRITE(5,902)
902	FORMAT(' Enter new a second time')
	CALL PASSWORD(5,PASNEW1)
	CALL CHKPAS(PASSNEW1,ST)
	IF(ST.NE.0) GOTO 7
	IF(EXIT2.EQ.'EXIT') THEN
	  CALL CLRSCR(5)
	  CALL GSTOP(GEXIT_OPABORT)
	ENDIF
C
C Test if new passwords are identical
C
	IF(PASSNEW.NE.PASSNEW1) THEN
	  WRITE(5,903)
 903	  FORMAT(' New passwords do not verify')
	  CALL XWAIT(2,2,STAT)
	  SIND=1
          CALL PRINTSON(INDEX,SIND)
	  GOTO 6
	ENDIF
C
C Verify that password is not equal to 'CHANGE'
C
	IF(CHANGE.EQ.'CHANGE') THEN
	   WRITE(5,888)
 888	   FORMAT(' CHANGE is a reserved word')
	   CALL XWAIT(2,2,ST)
	   GOTO 6
	ENDIF
C
C Verify that password is not equal to 'EXIT'
C
	IF(EXIT1.EQ.'EXIT') THEN
	   WRITE(5,889)
 889	   FORMAT(' EXIT is a reserved word')
	   CALL XWAIT(2,2,ST)
	   GOTO 6
	ENDIF
C
C Scramble password
C
	CALL ENCPAS(ID,PASSNEW,SPWORD)
C
C Set old password to new password  and set group level
C Set USERCHA variable
C
	 PASWORD1=SPWORD
	 USERCHA=ID
C
C Levels are stored in nibbles  extract and put in SECLEV
C
	 NIBINX=INDEX-1
	 CALL GETNIBLE(NIBINX,USERSECL,SLEVEL)
C
C Update DATECHA variable
C
	 CALL XDAT(DATES)
	 DATECHA(1)=DATES(2)
	 DATECHA(2)=DATES(3)
	 DATECHA(3)=DATES(1)
C
C Write new pasword in USER file
C
	 CALL WRITEW(SONFDB,ID,USEREC,ST)
	 IF (ST.NE.0) THEN
	    WRITE(5,702) ST
 702	    FORMAT(1X,' user write error  st- ',I4)
	    CALL GSTOP(GEXIT_FATAL)
	 ENDIF
C
C Return to SGNON routine with good status
C
	 RETURN
	 END

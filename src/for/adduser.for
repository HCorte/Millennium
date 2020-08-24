C
C SUBROUTINE ADDUSER
C
C V04 09-JUL-1999 UXN Password expiration date added.
C V03 19-MAY-1996 HXK Wojtek's security stuff added
C V02 01-AUG-1990 XXX RELEASED FOR VAX
C V01 17-MAY-1988 DSL Initial release adds users to USER file
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
	 SUBROUTINE ADDUSER(UID)
	 IMPLICIT NONE
C
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
C
	 INTEGER *4 UID,REC,FLAG,ADD
	 INTEGER *4 DATES(3), NIBBLE, I, EXT, ST
	 INTEGER*4 LEV(24),USEFDB(7)
	 CHARACTER *4 EXIT,EXIT1
	 CHARACTER *6 CHANGE
	 CHARACTER *8  PWORD,PWORD1,SCPWORD
	 CHARACTER *20 UNAME,PWPAS,PWPAS1
	 CHARACTER *55 STRING
	 EQUIVALENCE(PWPAS,PWORD,EXIT,CHANGE)
	 EQUIVALENCE(PWPAS1,PWORD1,EXIT1)
	 INTEGER*4 VALUE
C
C Ask if the user is sure he/she wants this operation
C
         CALL WIMG(5,' Are you sure you want to add a record < y/n > ?')
	 CALL YESNO(ADD)
	 IF (ADD.NE.1) RETURN
C
C Open USER file
C
	 CALL OPENX(2,'GXTSK:USER.FIL',4,0,0,ST)
	 CALL IOINIT(USEFDB,2,USESEC*256)
	 IF (ST.NE.0) THEN
	    WRITE(5,899) ST
 899	    FORMAT(' user file open error  st - ',I4)
	    CALL XWAIT(2,2,ST)
	    CALL USRCLOS1(     2)
	    RETURN
	 ENDIF
C
C Display user id on screen
C
10	 CONTINUE
	 CALL CLRSCR(5)
	 CALL INPNUM(' Enter the user ID number of your choice : ',REC,
     *	              1,1000,EXT)
	 IF(EXT.LT.0) THEN
	   CALL USRCLOS1(     2)
	   RETURN
	 ENDIF
C
C Read USER file until you hit first empty record
C
	 FLAG=0
	 CALL READW(USEFDB,REC,USEREC,ST)
	 IF (ST.NE.0) THEN
	    WRITE(5,900) REC,ST
 900	    FORMAT(' user file read error  rec -',I4,' st - ',I4)
	    CALL USRCLOS1(     2)
	    RETURN
	 ENDIF
C
C Determine if record is available
C
	 IF (USERID.NE.0) THEN
	   CALL CLRSCR(5)
	   WRITE(5,901) REC
 901	   FORMAT(' User number ',I4,' is reserved - choose another')
	   CALL XWAIT(2,2,ST)
	   GOTO 10
	 ENDIF
C
C Ask for password  twice and verify
C
 11	 CONTINUE
	 CALL CLRSCR(5)
	 CALL PASSWORD(5,PWPAS)
	 CALL CHKPAS(PWORD,ST)
	 IF(ST.NE.0) GOTO 11
C
C Ask for passowrd a second time
C
	 CALL CLRSCR(5)
	 WRITE(5,9098)
 9098	 FORMAT(1X,' Enter again to verify')
	 CALL PASSWORD(5,PWPAS1)
	 CALL CHKPAS(PWORD1,ST)
	 IF(ST.NE.0) GOTO 10
	 IF(PWORD.NE.PWORD1) THEN
	   WRITE(5,9099)
 9099	   FORMAT(1X,' Passwords do not match')
	   CALL XWAIT(2,2,ST)
	   GOTO 11
	 ENDIF
C
C Verify that password is not equal to 'CHANGE'
C
	IF(CHANGE.EQ.'CHANGE') THEN
	   WRITE(5,888)
 888	   FORMAT(' CHANGE is a reserved word')
	   CALL XWAIT(2,2,ST)
	   GOTO 11
	ENDIF
C
C Verify that password is not equal to 'EXIT'
C
	IF(EXIT.EQ.'EXIT') THEN
	   WRITE(5,889)
 889	   FORMAT(' EXIT is a reserved word')
	   CALL XWAIT(2,2,ST)
	   GOTO 11
	ENDIF
C
C Ask for user name
C
 20	  CONTINUE
	 CALL WIMG(5,'Enter user name : ')
	 READ(5,902) UNAME
 902	 FORMAT(A20)
	 IF (UNAME.EQ.'                    ') GOTO 20
	 IF (UNAME.EQ.'E                   ') THEN
	   WRITE(5,9002)
 9002	   FORMAT(' User name must be entered')
	   CALL XWAIT(2,2,ST)
	   GOTO 20
	 ENDIF
C
C Ask for and security levels
C
	 DO 30 I=1,OINDEX-1
 29	    WRITE (STRING,903) FUNCNAME(I)
	    CALL INPNUM(STRING,LEV(I),0,15,EXT)
	    IF(EXT.LT.0) THEN
	       WRITE(5,898)
 898	       FORMAT(' You must enter a security level')
	       CALL XWAIT(2,2,ST)
	       GOTO 29
	    ENDIF
 30	 CONTINUE
 903	 FORMAT('Enter security level for  :  ',A8)
C
C Ask password expiration date
C
	 EXPDAY = 0 
         CALL INPNUM('Enter password expiration time in days [E-no change]',
     *                VALUE,0,99999,ST)
         IF(ST.EQ.0)  EXPDAY = VALUE
C
C Display values entered on screen
C
	 CALL CLRSCR(5)
	 WRITE(5,905)
	 WRITE(5,9055) REC
	 WRITE(5,907) UNAME
	 DO 40 I=1,OINDEX-1
	   WRITE(5,908) FUNCNAME(I),LEV(I)
 40	 CONTINUE
         IF(EXPDAY.LE.0) THEN
            WRITE(5,910)
         ELSE
            WRITE(5,909) EXPDAY
         ENDIF
C
C Format statements for above displays
C
 905	 FORMAT('       User information',/)
 9055	 FORMAT(' User ID        : ',I4,/)
 907	 FORMAT(' User name      : ',A20)
 908	 FORMAT(' Security level : ',A8,' is ',I4)
 909     FORMAT(' Password expires in ',I4,' days')
 910     FORMAT(' Password never expires')
C
C Ask if values are ok
C
	 CALL WIMG(5,' Are they correct < y/n > ? ')
	 CALL YESNO(FLAG)
	 IF (FLAG.EQ.1) THEN
C
C Fill user numbers
C
	    USERID=REC
	    USERADD=UID
	    USERCHA=UID
C
C Fill in dates
C
	    CALL XDAT(DATES)
C
C Fill in date record added
C
	    DATEADD(1)=DATES(2)
	    DATEADD(2)=DATES(3)
	    DATEADD(3)=DATES(1)
C
C Fill in date record changed
C
	    DATECHA(1)=DATES(2)
	    DATECHA(2)=DATES(3)
	    DATECHA(3)=DATES(1)
C
C Encryt and fill in password      fill in user name
C
	    CALL ENCPAS(REC,PWORD,SCPWORD)
	    PASWORD1=SCPWORD
	    USERNAM=UNAME
C
C Fill in levels
C
	    DO 50 NIBBLE=0,OINDEX-2
C***	       CALL PUTNIB(NIBBLE,USERSECL,LEV(NIBBLE+1))
	       CALL SETNIBLE(LEV(NIBBLE+1),USERSECL,NIBBLE+1)
 50	    CONTINUE
C
C Write record
C
	    CALL WRITEW(USEFDB,REC,USEREC,ST)
	    IF (ST.NE.0) THEN
	       WRITE(5,911) REC,ST
 911	       FORMAT(' user file write error  rec - ',I4,' st - ',I4)
	       CALL USRCLOS1(     2)
	       RETURN
	    ENDIF
	 ELSE
	    CALL CLRSCR(5)
	    GOTO 10
	 ENDIF
C
C Close files and return
C
	 CALL USRCLOS1(     2)
	 RETURN
	 END

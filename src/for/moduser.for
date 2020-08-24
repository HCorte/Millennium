C SUBROUTINE MODUSER
C
C V04 17-MAY-2000 OXK Ability to change password added; cleanup
C V03 09-JUL-1999 UXN EXPDAY added.
C V02 19-MAY-1996 HXK Wojtek's security stuff added
C V01 01-AUG-1990 XXX RELEASED FOR VAX
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MODUSER(UID)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
C
	INTEGER*4    UID,REC,FLAG, STS, I, NIBBLE, EXT, ST
	INTEGER*4    DATES(3),IND,INDEX,SLEVEL,SIND,YNFLG
	INTEGER*4    LEVS(24),USEFDB(7),LEV(24)
	CHARACTER*4  EXIT,EXIT1
	CHARACTER*6  CHANGE
	CHARACTER*8  PWORD,PWORD1,SCPWORD
	CHARACTER*20 UNAME,PWPAS,PWPAS1
	CHARACTER*55 STRING
	INTEGER*4    VALUE
	INTEGER*4    PWDCHG

	EQUIVALENCE(PWPAS,PWORD,EXIT,CHANGE)
	EQUIVALENCE(PWPAS1,PWORD1,EXIT1)

C
C Open USER file
C
	CALL OPENX(2,'GXTSK:USER.FIL',4,0,0,ST)
	CALL IOINIT(USEFDB,2,USESEC*256)
	IF (ST.NE.0) THEN
	    WRITE(6,899) ST
	    CALL XWAIT(2,2,ST)
	    CALL USRCLOS1(     2)
	    RETURN
	ENDIF
C
C Find user record to change
C
3	CONTINUE
	CALL CLRSCR(6)
	CALL INPNUM(' Please enter USER ID to change : ',REC,1,9999,EXT)
	IF (EXT.LT.0) THEN
	    CALL USRCLOS1 (     2)
	    RETURN
	ENDIF
C
C Read USER file with inputed ID number
C
	FLAG=0
	CALL READW(USEFDB,REC,USEREC,ST)
	IF (ST.NE.0) THEN
	    WRITE(6,900) REC,ST
	    CALL USRCLOS1(     2)
	    RETURN
	ENDIF
C
C Test is to verify that this record if filled
C
	IF(USERID.EQ.0) THEN
	    WRITE(6,897) REC
	    CALL XWAIT(2,2,ST)
	    GOTO 3
	ENDIF
C
C Try to change password...
C
10	CONTINUE
	PWDCHG = 0
	CALL INPYESNO('Do you want to change password [Y/N]? ',YNFLG)
	IF(YNFLG.NE.1) GOTO 20
C
C Ask for password  twice and verify
C
11      CONTINUE
	CALL CLRSCR(6)
	CALL PASSWORD(6,PWPAS)
	CALL CHKPAS(PWORD,ST)
	IF(ST.NE.0) GOTO 11
C
C Ask for passowrd a second time
C
	CALL CLRSCR(6)
	WRITE(6,9098)
	CALL PASSWORD(6,PWPAS1)
	CALL CHKPAS(PWORD1,ST)
	IF(ST.NE.0) GOTO 10
	IF(PWORD.NE.PWORD1) THEN
	    WRITE(6,9099)
	    CALL XWAIT(2,2,ST)
	    GOTO 11
	ENDIF
C
C Verify that password is not equal to 'CHANGE'
C
	IF(CHANGE.EQ.'CHANGE') THEN
	    WRITE(6,888)
	    CALL XWAIT(2,2,ST)
	    GOTO 11
	ENDIF
C
C Verify that password is not equal to 'EXIT'
C
	IF(EXIT.EQ.'EXIT') THEN
	    WRITE(6,889)
	    CALL XWAIT(2,2,ST)
	    GOTO 11
	ENDIF
C
C Encryt and fill in password      fill in user name
C
	CALL ENCPAS(REC,PWORD,SCPWORD)
	PASWORD1=SCPWORD
	PWDCHG = 1

C
C Ask for  user name
C
20	CONTINUE
	CALL WIMG(6,' Enter new user name : < E - no change > ')
	READ(5,902) UNAME
	IF (UNAME.EQ.'                    ') GOTO 20
	IF (UNAME.NE.'E                   ') USERNAM=UNAME
C
C Get levels for original record  because privilege group didn't
C change
C
	DO NIBBLE=0,OINDEX-2
	    CALL GETNIBLE(LEVS(NIBBLE+1),USERSECL,NIBBLE+1)
	ENDDO
C
C Ask for and security levels
C
	DO 30 I=1,OINDEX-1
	    WRITE (STRING,903) FUNCNAME(I),LEVS(I)
	    CALL INPNUM(STRING,LEV(I),0,15,EXT)
	    IF(EXT.LT.0) THEN
		LEV(I)=LEVS(I)
		GOTO 30
	    ENDIF
30	CONTINUE
C
C Ask password expiration date
C
	CALL INPNUM('Enter password expiration time in days [E-no change]',
     *                  VALUE,0,99999,ST)
	IF(ST.EQ.0)  EXPDAY = VALUE
C
C Display values entered on screen
C
	CALL CLRSCR(6)
	WRITE(6,905)
	WRITE(6,904) REC
	WRITE(6,907) USERNAM
	DO I=1,OINDEX-1
	    WRITE(6,908) FUNCNAME(I),LEV(I)
	ENDDO
	IF(EXPDAY.LE.0) THEN
	    WRITE(6,910)
	ELSE
	    WRITE(6,909) EXPDAY 
	ENDIF

	IF (PWDCHG.EQ.1) WRITE(6,911)
C
C
C Ask if values are ok
C
	CALL WIMG(6,' Are they correct < y/n > ? ')
	CALL YESNO(FLAG)
	IF (FLAG.EQ.1) THEN
C
C Fill user number
C
	    USERCHA=UID
C
C Fill in dates
C
	    CALL XDAT(DATES)
C
C Fill in date record changed
C
	    DATECHA(1)=DATES(2)
	    DATECHA(2)=DATES(3)
	    DATECHA(3)=DATES(1)
C
C Fill in security levels
C
	    DO NIBBLE=0,OINDEX-2
		CALL SETNIBLE(LEV(NIBBLE+1),USERSECL,NIBBLE+1)
	    ENDDO
C
C Write record
C
	    CALL WRITEW(USEFDB,REC,USEREC,ST)
	    IF (ST.NE.0) THEN
		WRITE(6,912) REC,ST
		CALL USRCLOS1(     2)
		RETURN
	    ENDIF
	 ELSE
	    CALL USRCLOS1(     2)
	    RETURN
	 ENDIF
C
C Close files and return
C
	CALL USRCLOS1(     2)
	RETURN
C ----- FORMAT STATEMENTS -----
897	FORMAT(1X,I4,' - user  not found ')
888	FORMAT(' CHANGE is a reserved word')
889	FORMAT(' EXIT is a reserved word')
899	FORMAT(' user file open error  st - ',I4)
900	FORMAT(' user file read error  rec -',I4,' st - ',I4)
902	FORMAT(A20)
903	FORMAT(' Enter security level (E-no change) for ',A8,1X,I2)
905	FORMAT(' Modified user information',/)
904	FORMAT(' User id          : ',I4)
907	FORMAT(' User name        : ',A20)
908	FORMAT(' Security level   : ',A8,' is ',I4)
909	FORMAT(' Password expires in ',I4,' days')
910	FORMAT(' Password never expires')
911	FORMAT(' PASSWORD CHANGED!!!')
912	FORMAT(' user file write error  rec - ',I4,' st - ',I4)
9098	FORMAT(1X,' Enter again to verify')
9099	FORMAT(1X,' Passwords do not match')
	END

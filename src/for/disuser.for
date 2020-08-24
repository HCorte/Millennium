C SUBROUTINE DISUSER
C
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
	 SUBROUTINE DISUSER
	 IMPLICIT NONE
C
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
C
	 INTEGER*4 REC,FLAG,TEMP
	 INTEGER*4 USEFDB(7), NIBBLE, EXT, ST
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
 10	 CONTINUE
	 CALL CLRSCR(6)
       CALL INPNUM(' Please enter USER ID to display : ',REC,1,9999,EXT)
	 IF (EXT.LT.0) THEN
	    CALL USRCLOS1 (     2)
	    RETURN
	 ENDIF
C
C Read USER file until you hit first empty record
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
	    GOTO 10
	 ENDIF
C
C Display values entered on screen
C
	 CALL CLRSCR(6)
	 WRITE(6,905) USERID
	 WRITE(6,907) USERNAM
	 WRITE(6,9077) USERADD
	 WRITE(6,9078) USERCHA
	 WRITE(6,9079) DATEADD(1),DATEADD(2),DATEADD(3)
	 WRITE(6,9080) DATECHA(1),DATECHA(2),DATECHA(3)
         IF(EXPDAY.LE.0) THEN
            WRITE(6,910)
         ELSE
            WRITE(6,909) EXPDAY
         ENDIF

	 DO 40 NIBBLE=0,OINDEX-2
C***	   CALL GETNIB(NIBBLE,USERSECL,TEMP)
	   CALL GETNIBLE(TEMP,USERSECL,NIBBLE+1)
	   WRITE(6,908) FUNCNAME(NIBBLE+1),TEMP
 40	 CONTINUE
C
C Ask if values are ok
C
	 CALL WIMG(6,'Do you want to return to menu ? < y/n > ? ')
	 CALL YESNO(FLAG)
	 IF (FLAG.EQ.1) THEN
	    CALL USRCLOS1(     2)
	    RETURN
	 ELSE
	    CALL CLRSCR(6)
	    GOTO 10
	 ENDIF
C
C Format statements
C
897	    FORMAT(1X,I4,' - user  not found ')
899	    FORMAT(' user file open error  st - ',I4)
900	    FORMAT(' user file read error  rec -',I4,' st - ',I4)

905	 FORMAT('    User information  USER ID: ',I4,/)
907	 FORMAT(' User name               : ',A20)
9077	 FORMAT(' User who added record   : ',I4)
9078	 FORMAT(' User who changed record : ',I4)
9079	 FORMAT(' Date record added       : ',I2,'/',I2,'/',I2)
9080	 FORMAT(' Date record changed     : ',I2,'/',I2,'/',I2)
908	 FORMAT(' Security level for      : ',A8,' is ',I4)
909      FORMAT(' Password expires in ',I4,' days')
910      FORMAT(' Password never expires')
	 END

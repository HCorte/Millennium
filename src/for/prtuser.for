C
C SUBROUTINE PRTUSER
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PRTUSER(UID)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
C
	INTEGER *4 UID, REC, FLAG, TEMP, COPY
	INTEGER*4 USEFDB(7), NIBBLE, EXT, ST
C
C Open USER file
C
	CALL OPENX(2,'GXTSK:USER.FIL',4,0,0,ST)
	CALL IOINIT(USEFDB,2,USESEC*256)
	IF (ST.NE.0) THEN
	  WRITE(5,901) ST
901 	  FORMAT(' user file open error  st - ',I4)
	  CALL XWAIT(2,2,ST)
	  CALL USRCLOS1(     2)
	  CLOSE(UNIT=6)
	  RETURN
	ENDIF
C
C Find user record to print
C
10 	CONTINUE
	CALL CLRSCR(5)
        CALL INPNUM(' Please enter USER ID to print : ',REC,1,9999,EXT)
	IF (EXT.LT.0) THEN
	  CALL USRCLOS1(     2)
	  RETURN
	ENDIF
C
C Get the number of report copies
C
        CALL INPNUM(' Enter number of report copies : ',COPY,0,5,EXT)
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
	  WRITE(5,902) REC,ST
902 	  FORMAT(' user file read error  rec -',I4,' st - ',I4)
	  CALL XWAIT(2,2,ST)
	  CALL USRCLOS1(     2)
	  RETURN
	ENDIF
C
C Test is to verify that this record if filled
C
	IF(USERID.EQ.0) THEN
	  WRITE(5,903) REC
903 	  FORMAT(1X,I4,' - user  not found ')
	  CALL XWAIT(2,2,ST)
	  GOTO 10
	ENDIF
C
C Open logical unit 6 to print file
C
	CALL ROPEN('GXTSK:USER.REP',8,ST)
        IF (ST.NE.0) THEN
          WRITE(5,900) ST
900       FORMAT(' open error  st - ',I4)
	  CALL USRCLOS1(     8)
	  CALL XWAIT(2,2,ST)
          RETURN
        ENDIF
C
C Generate report
C
	WRITE(8,905) USERID
	WRITE(8,906) USERNAM
	WRITE(8,907) USERADD
	WRITE(8,908) USERCHA
	WRITE(8,909) DATEADD(1),DATEADD(2),DATEADD(3)
	WRITE(8,910) DATECHA(1),DATECHA(2),DATECHA(3)
         IF(EXPDAY.LE.0) THEN
            WRITE(8,912)
         ELSE
            WRITE(8,913) EXPDAY
         ENDIF
	DO 20 NIBBLE=0,OINDEX-2
	   CALL GETNIBLE(TEMP,USERSECL,NIBBLE+1)
	   WRITE(8,911) FUNCNAME(NIBBLE+1),TEMP
20	CONTINUE
C
C Print user record
C
        CALL SPOOL('GXTSK:USER.REP',COPY,ST)
C
	CALL CLRSCR(5)
	CALL WIMG(5,'Do you want to return to menu ? < y/n > ? ')
	CALL YESNO(FLAG)
	IF (FLAG.EQ.1) THEN
	  CALL USRCLOS1(     2)
	  CALL USRCLOS1(     8)
	  RETURN
	ELSE
	  CALL CLRSCR(5)
	  CALL USRCLOS1(     8)
	  GOTO 10
	ENDIF
C
905	 FORMAT('1',10X,///,'    User information  USER ID: ',I4,/)
906	 FORMAT(' ',10X,'User name               : ',A20)
907	 FORMAT(' ',10X,'User who added record   : ',I4)
908	 FORMAT(' ',10X,'User who changed record : ',I4)
909	 FORMAT(' ',10X,'Date record added       : ',I2,'/',I2,'/',I2)
910 	 FORMAT(' ',10X,'Date record changed     : ',I2,'/',I2,'/',I2)
911	 FORMAT(' ',10X,'Security level for      : ',A8,' is ',I4)
912      FORMAT(' ',10X,'Password expires in ',I4,' days')
913      FORMAT(' ',10X' Password never expires')
	 END

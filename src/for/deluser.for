C
C SUBROUTINE DELUSER
C $Log:   GXAFXT:[GOLS]DELUSER.FOV  $
C  
C     Rev 1.1   19 May 1996 17:45:16   HXK
C  Wojtek's security stuff added
C  
C     Rev 1.0   21 Jan 1993 16:03:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_deluser.for **
C
C DELUSER.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C PROGRAM DELUSER
C
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
	 SUBROUTINE DELUSER(UID)
	 IMPLICIT NONE
C
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
C
	 INTEGER *4 UID,REC,FLAG,TEMP, N, NIBBLE, EXT, ST
	 INTEGER*4 USEFDB(7)
	 CHARACTER *8  PWORD
	 CHARACTER *20 UNAME
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
C Find user record to change
C
 3	 CONTINUE
	 CALL CLRSCR(5)
       CALL INPNUM(' Please enter USER ID to delete : ',REC,1,9999,EXT)
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
	    WRITE(5,900) REC,ST
 900	    FORMAT(' user file read error  rec -',I4,' st - ',I4)
	    CALL USRCLOS1(     2)
	    RETURN
	 ENDIF
C
C Test is to verify that this record if filled
C
	 IF(USERID.EQ.0) THEN
	    WRITE(5,897) REC
 897	    FORMAT(1X,I4,' - user  not found ')
	    CALL XWAIT(2,2,ST)
	    GOTO 3
	 ENDIF
C
C Display values entered on screen
C
	 CALL CLRSCR(5)
	 WRITE(5,905)
	 WRITE(5,906) REC
	 WRITE(5,907) USERNAM
	 DO 40 NIBBLE=0,OINDEX-2
C***	   CALL GETNIB(NIBBLE,USERSECL,TEMP)
	   CALL GETNIBLE(TEMP,USERSECL,NIBBLE+1)
	   WRITE(5,908) FUNCNAME(NIBBLE+1),TEMP
 40	 CONTINUE
C
C Format statements for above displays
C
 905	 FORMAT(' User information to delete ',/)
 906	 FORMAT(' User number    : ',I8)
 907	 FORMAT(' User name      : ',A20)
 908	 FORMAT(' Security level : ',A8,' is ',I4)
C
C Ask if values are ok
C
       CALL WIMG(5,'Are you sure you want to delete record < y/n > ? ')
	 CALL YESNO(FLAG)
	 IF (FLAG.EQ.1) THEN
C
C Wipe out user record
C
	    DO 85 N=1,64
	       USEREC(N)=0
 85	    CONTINUE
C
C Write record
C
	    CALL WRITEW(USEFDB,REC,USEREC,ST)
	    IF (ST.NE.0) THEN
	       WRITE(5,910) REC,ST
 910	       FORMAT(' user file write error  rec - ',I4,' st - ',I4)
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
	 END

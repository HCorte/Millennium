C
C SUBROUTINE INITUSER
C $Log:   GXAFXT:[GOLS]INITUSER.FOV  $
C  
C     Rev 1.1   19 May 1996 17:45:38   HXK
C  Wojtek's security stuff added
C  
C     Rev 1.0   21 Jan 1993 16:39:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_inituser.for **
C
C INITUSER.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C PROGRAM INITUSER
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
	 SUBROUTINE INITUSER
	 IMPLICIT NONE
C
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
C
	 INTEGER *4 FLAG,SIZ,I, REC, IOS, ST
	 INTEGER*4 USEFDB(7)
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
C Use inquire statement to get size of file
C
	 CALL VAXGETFSIZ(2,SIZ)
C***	 INQUIRE(UNIT=2,SIZE=SIZ,IOSTAT=IOS)
C***	 IF(IOS.NE.0) THEN
C***	   WRITE(5,888)
C*** 888	   FORMAT(' Not able to inquire about size of file')
C***	   CALL WAIT(2,2,ST)
C***	   CALL USRCLOS1(     2)
C***	   RETURN
C***	 ENDIF
C
C Ask to verify initialization
C
       CALL WIMG(5,'Are you sure you want to initialize file <y/n> ? ')
	 CALL YESNO(FLAG)
	 IF (FLAG.NE.1) THEN
	    CALL USRCLOS1(     2)
	    RETURN
	 ENDIF
C
C Read USER file until you hit first empty record
C
	 FLAG=0
	 DO 10 REC=1,SIZ
	 CALL READW(USEFDB,REC,USEREC,ST)
	 IF (ST.EQ.144.OR.ST.EQ.136) GOTO 20
	 IF (ST.NE.0) THEN
	    WRITE(5,900) REC,ST
 900	    FORMAT(' user file read error  rec -',I4,' st - ',I4)
	    CALL USRCLOS1(     2)
	    RETURN
	 ENDIF
C
C Wipe out user record
C
	 DO 23 I=1,64
	   USEREC(I)=0
 23	 CONTINUE
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
C
C Increment read next record
C
 10	CONTINUE
C
C Close files and return
C
 20	 CONTINUE
	 CALL USRCLOS1(     2)
	 RETURN
	 END

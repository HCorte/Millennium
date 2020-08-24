C
C SUBROUTINE INQUSER
C $Log:   GXAFXT:[GOLS]INQUSER.FOV  $
C  
C     Rev 1.1   19 May 1996 17:45:44   HXK
C  Wojtek's security stuff added
C  
C     Rev 1.0   21 Jan 1993 16:40:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_inquser.for **
C
C INQUSER.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C PROGRAM INQUSER
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
	 SUBROUTINE INQUSER(UID)
	 IMPLICIT NONE
C
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
C
	 INTEGER *4 UID,REC,CNT,ANS,TEMP
	 INTEGER *4 LEVS(24),FLAG,SIZ
	 INTEGER*4 USEFDB(7), NIBBLE, IOS, ST
	 FLAG=0
C
C Open USER file
C
	 CALL OPENX(2,'USER.FIL',4,0,0,ST)
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
C
C Read USER file until you hit first empty record
C
	CNT=1
	DO 10 REC=1,SIZ
	 CALL READW(USEFDB,REC,USEREC,ST)
	 IF (ST.NE.0) THEN
	    WRITE(5,900) REC,ST
 900	    FORMAT(' user file read error  rec -',I4,' st - ',I4)
	    CALL USRCLOS1(     2)
	    RETURN
	 ENDIF
C
C If record is empty return
C
	 IF(USERID.LE.0) THEN
	    GOTO 10
	 ENDIF
C
C Display values entered on screen
C
	 DO 40 NIBBLE=0,OINDEX-2
C***	   CALL GETNIB(NIBBLE,USERSECL,LEVS(NIBBLE+1))
	   CALL GETNIBLE(LEVS(NIBBLE+1),USERSECL,NIBBLE+1)
 40	 CONTINUE
	 IF(FLAG.EQ.0) THEN
	    WRITE(5,904)
	    FLAG=1
	 ENDIF
C
C Display detailed summary line
C
	 WRITE(5,905) USERID,USERNAM,LEVS(1),LEVS(2),LEVS(3),LEVS(4)
C
C     *	              LEVS(3),LEVS(4),LEVS(5),LEVS(6),LEVS(7),
C     *	              LEVS(8),LEVS(9),LEVS(10),LEVS(11),LEVS(12),
C     *	              LEVS(13),LEVS(14),LEVS(15),LEVS(16),LEVS(17),
C     *	              LEVS(18)
C
C Ask if you want to continue
C
	IF(CNT.GE.19) THEN
	 CALL WIMG(5,'Hit enter to continue............')
	 READ(5,906) ANS
 906	 FORMAT(A1)
	 CNT=1
	 FLAG=0
	 CALL CLRSCR(5)
	ELSE
	 CNT=CNT+1
	ENDIF
 10	CONTINUE
C
C Close files and return
C
	 CALL WIMG(5,'Hit enter to go to menu..........')
	 READ(5,907) ANS
 907	 FORMAT(A1)
	 CALL USRCLOS1(     2)
	 RETURN
C
C Format statements for above displays
C
C 904	 FORMAT('   UID  ',4X,'User name',6X,
C     *        'Vs Hs Us Pl Pn Gk Un Cm Dc An Bl Dr De Vh Ag Ma Cl Xo')
C
 904	 FORMAT('   UID  ',4X,'User name',6X,
     *         '  Visn Hasf Usps Chlv')
 905	 FORMAT(1X,I4,2X,A20,4(3X,I2))
	 END

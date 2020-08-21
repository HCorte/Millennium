C PROGRAM DMPUSER
C
C V02 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V01 22-MAY-2000 OXK INITIAL RELEASE
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
	PROGRAM DMPUSER
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
C
	INTEGER*4 UID, I
	INTEGER*4 USEFDB(7), ST
	INTEGER*4 LEVS(OINDEX)

	INTEGER*4 INLUN/2/
	INTEGER*4 OUTLUN/3/
C
C Open USER file
C
	CALL OPENX(INLUN,'GXTSK:USER.FIL',4,0,0,ST)
	CALL IOINIT(USEFDB,INLUN,USESEC*256)
	IF (ST.NE.0) THEN
	    WRITE(6,990) ST
            CALL XWAIT(2,2,ST)
	    CALL USRCLOS1(INLUN)
	    CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
C Open REPORT file
C
        CALL ROPEN('GXTSK:DMPUSER.REP',OUTLUN,ST)
	IF (ST.NE.0) THEN
	    CALL USRCLOS1(OUTLUN)
	    GOTO 800
	ENDIF
	WRITE(OUTLUN,890)
C
C GO THROUGH THE USER FILE...
C
	UID=0
10	CONTINUE
	UID=UID+1
	CALL FASTSET(0,LEVS,OINDEX)
	CALL READW(USEFDB,UID,USEREC,ST)
	IF (ST.NE.0) THEN
	    IF (ST.EQ.144) THEN
		WRITE(6,999) UID
	    ELSE
	        WRITE(6,995) UID,ST
	    ENDIF
	    CALL USRCLOS1(INLUN)
	    GOTO 800
	ENDIF
C
C Test is record filled
C
	IF(USERID     .EQ.0 .AND.
     *	   USERSECL(1).EQ.0 .AND.
     *	   USERSECL(2).EQ.0 .AND.
     *	   USERSECL(3).EQ.0 .AND.
     *	   USERADD    .EQ.0 .AND.
     *	   USERCHA    .EQ.0 .AND.
     *	   DATEADD(1) .EQ.0 .AND.
     *	   DATECHA(1) .EQ.0 .AND.
     *	   EXPDAY     .EQ.0) THEN
	   GOTO 10
	ENDIF

	IF (DATEADD(3).LT.80)   DATEADD(3) = DATEADD(3) + 100
	IF (DATEADD(3).LT.1000) DATEADD(3) = DATEADD(3) + 1900
	IF (DATECHA(3).LT.80)   DATECHA(3) = DATECHA(3) + 100
	IF (DATECHA(3).LT.1000) DATECHA(3) = DATECHA(3) + 1900

	DO I=0,OINDEX-2
	    CALL GETNIBLE(LEVS(I+1),USERSECL,I+1)
	ENDDO

	WRITE(OUTLUN,900)
     *	     USERID,			      ! USERID
     *	     USERNAM,			      ! USERNAME
C     *	     USERSECL,			      ! FUNCTION SECURITY LEVELS
     *	     (LEVS(I),I=1,OINDEX-1),	      ! FUNCTION SECURITY LEVELS
     *	     USERADD,			      ! CREATED BY
     *	     USERCHA,			      ! CHANGED BY
     *	     DATEADD(3),DATEADD(1),DATEADD(2),! CREATED
     *	     DATECHA(3),DATECHA(1),DATECHA(2),! LAST CHANGED
     *	     EXPDAY			      ! EXPIRY IN XX DAYS
	GOTO 10

800	CONTINUE
	WRITE(6,*)'DONE'
C

890	FORMAT(2X,
     *  'UID Username            Visn Hasf Usps Chlv  Add.By  Mod.By  ',
     *  'Added    Modified  Pwd.Exp')
900	FORMAT(1X,I4,X,A20,X,<OINDEX-1>(I2,3X),2(I4,4X),2(I4,I2.2,I2.2,X),X,I4)
990	FORMAT(' USER.FIL open error  st - ',I4)
995	FORMAT(' USER.FIL read error  rec -',I4,' st - ',I4)
999	FORMAT(' end of USER.FIL encountered while reading rec ',I4)
	END

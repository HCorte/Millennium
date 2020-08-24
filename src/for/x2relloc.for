C PROGRAM X2RELLOC
C
C V02 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C This program will release all record locks which
C have occurred in the X2X database.
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
	PROGRAM X2RELLOC
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XFIL.DEF'
C
	INTEGER*4   ALLREC(256)     !Record buffer
	INTEGER*4   FDB(7)          !File descriptor block
	INTEGER*4   FILE            !File number
	INTEGER*4   REC /0/         !Record number
	INTEGER*4   X2FILSEC        !File sector size function
	INTEGER*4   ST, FILE_INDEX
	CHARACTER   X2FILNAM*20     !File name function
	CHARACTER   PROMPT*60       !Output prompt
	CHARACTER   INTERUP         !Screen interrupt buffer

C
C DISPLAY COPYRIGHT.
C
	CALL COPYRITX(5)
C
C CLEAR THE SCREEN AND DISPLAY THE MENU.
C
	CALL CLRSCR(5)
	WRITE(5,9000)
C
C LOOP THROUGH ALL X2X NETWORK FILES.
C
	DO 100 FILE_INDEX=1,X2XFIL_MAX_FILES
	  FILE=X2XFIL_FILE_LIST(FILE_INDEX)
	  WRITE(5,9010) X2FILNAM(FILE)
	  REC=0
	  CALL OPENX(1,X2FILNAM(FILE),4,0,0,ST)
	  CALL IOINIT(FDB,1,X2FILSEC(FILE)*256)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XNPC),'OPENX',ST,0)
	    CALL GPAUSE
	  ENDIF
C
C READ THROUGH ENTIRE FILE SKIPPING EMPTY SLOTS.
C
200	  CONTINUE
	  REC=REC+1
	  CALL READW(FDB,REC,ALLREC,ST)
	  IF(ST.NE.0.AND.ST.NE.144) THEN
	    CALL OS32ER(5,X2FILNAM(XNPC),'READW',ST,REC)
	    CALL GPAUSE
	  ENDIF
C
C IF NO MORE RECORDS, CLOSE THE CURRENT FILE.
C
	  IF(ST.EQ.144) THEN
	    CALL CLOSEFIL(FDB)
	    REC=0
	    GOTO 100
	  ENDIF
C
C SKIP EMPTY RECORDS.
C
	  IF(ALLREC(1).EQ.-1) THEN
	    ALLREC(1)=0
	  ELSE IF(ALLREC(1).GE.0) THEN
	    GOTO 200
	  ENDIF
C
C REWRITE THE RECORD WITH CLEARS ANY LOCKS.
C
	  ALLREC(1)=IAND(ALLREC(1),'7FFFFFFF'X)
	  CALL WRITEW(FDB,REC,ALLREC,ST)
	  IF(ST.NE.0.AND.ST.NE.144) THEN
	    CALL OS32ER(5,X2FILNAM(XNPC),'WRITEW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  IF(ALLREC(1).NE.0) THEN
	    WRITE(5,9012) REC
	  ELSE
	    WRITE(5,9014) REC
	  ENDIF
C
C READ THE NEXT RECORD
C
	  GOTO 200
100	CONTINUE
C
C RELEASE COMPLETE.
C
	WRITE(5,9020)
	WRITE (PROMPT,9030)
	CALL WIMG(5,PROMPT)
	READ(5,9040) INTERUP
C
C     ===================== Format Statements ====================
C
9000	FORMAT(////,T26,'GTECH Distributed Network',/,
     *	            T29,'Clear Record Locks',//)
9010	FORMAT(5X,'Unlocking records in file ',A20)
9012	FORMAT(8X,'Record unlocked:  ',I3)
9014	FORMAT(8X,'Deleted record cleared:  ',I3)
9020	FORMAT(5X,'Unlock complete ',//)
9030	FORMAT(15(' '),'    Press RETURN to continue         ')
9040	FORMAT(A)
	END

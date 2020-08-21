C
C PROGRAM X2PRTREP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2PRTREP.FOV                                 $
C  $Date::   17 Apr 1996 16:26:56                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2prtrep.for;1 **
C
C X2PRTREP.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C THIS PROGRAM WILL EXTRACT THE SET OF PORTS WHICH HAS BEEN
C ASSIGNED TO THE STATION FROM THE STATION CONFIGURATION FILE
C AND WILL WRITE THE LIST OF PORTS TO THE FILE - X2PRTREP.FIL
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2PRTREP
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
C
	INTEGER*4 ASGMTAB(X2XSTN_MAXDEF+2,X2X_STATIONS)
	INTEGER*4 DATETAB(3), OFF1, OFF, FULLREC, REC, I, ST
	CHARACTER*1 HEADER(128,4), REPLIN(128)
	CHARACTER*128 CHEADER(4), CREPLIN
	EQUIVALENCE  (CHEADER,HEADER)
	EQUIVALENCE  (CREPLIN,REPLIN)
C
	DATA HEADER /126*' ',Z0D,Z0A,
     *	             126*' ',Z0D,Z0A,
     *	             126*' ',Z0D,Z0A,
     *	             126*' ',Z0D,Z0A/
     *	     REPLIN /126*' ',Z0D,Z0A/
	CALL COPYRITE
C
	TYPE *
	TYPE *,'This program will create the list of addresses that'
	TYPE *,'are stored in the station as default assignment'
	TYPE *
C
C OPEN THE STATION CONFIGURATION FILE
C
	CALL OPENW(1,X2XSTN_NAME,4,0,0,ST)
	CALL IOINIT(X2XSTN_FDB,1,X2XSTN_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2XSTN_NAME,'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
C
C OPEN A PORT REPORT FILE
C
	CALL ROPEN1('X2PRTREP.FIL',3,128,ST)
	IF (ST .NE. 0) THEN
	  TYPE *, 'X2PRTREP.FIL OPEN ERROR -> ',ST
	  CALL GPAUSE
	ENDIF
	CALL XDAT(DATETAB)
	IF(DATETAB(1) .LT. 77) DATETAB(1) = DATETAB(1) + 100
	DATETAB(1) = DATETAB(1) + 1900
	WRITE (CHEADER(1),9509)
	WRITE(3,9109) CHEADER(1)
	WRITE (CHEADER(2),9209) DATETAB(2),DATETAB(3),DATETAB(1)
	WRITE(3,9109) CHEADER(2)
	WRITE (CHEADER(3),9609)
	WRITE(3,9109) CHEADER(3)
C
C LOOP THROUGH THE STATION CONFIGURATION FILE
C
	REC = 0
	FULLREC = 0
100	CONTINUE
	  REC=REC+1
	  CALL READW(X2XSTN_FDB,REC,X2XSTN_REC,ST)
	  IF(ST.EQ.144) GO TO 300
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2XSTN_NAME,'READW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XSTN_STN .LE. 0) GOTO 100
C
C EXTRACT X2XSTN_DEF_PORT  TABLE
C
	  FULLREC = FULLREC + 1
	  ASGMTAB(1,FULLREC) = X2XSTN_STN
	  ASGMTAB(2,FULLREC) = X2XSTN_MAXDEF
	  DO 199 OFF = 1, X2XSTN_MAXDEF
	    ASGMTAB(OFF+2 ,FULLREC) = X2XSTN_DEF_PORT(OFF)
199	  CONTINUE
	  GO TO 100
300	  CONTINUE
	  IF (FULLREC .GT. 0) THEN
	    WRITE (CHEADER(4),9309) FULLREC
	    WRITE(3,9109) CHEADER(4)
	    DO 400 OFF = 1, FULLREC
	      DO 350 OFF1 = 1, 126
	        REPLIN(OFF1) = ' '
350	      CONTINUE
	    WRITE (CREPLIN,9409) (ASGMTAB(I,OFF), I=1,X2XSTN_MAXDEF+2)
	    WRITE(3,9109,IOSTAT=ST) CREPLIN
	    IF(ST.NE.0) THEN
	      TYPE *, 'X2PRTREP.FIL FILE WRITE ERROR -> ',ST
	      CALL GPAUSE
	    ENDIF
400	    CONTINUE
	  ENDIF
C
C CLOSE THE FILES AND CALL GSTOP(GEXIT_SUCCESS)
C
	CALL CLOSEFIL(X2XSTN_FDB)
	CALL USRCLOS1(     3)
C
	CALL GSTOP(GEXIT_SUCCESS)
9109	FORMAT(A128)
9209	FORMAT('creation date - ',I2,'/',I2,'/',I4)
9309	FORMAT('number of data records - ',I5)
9409	FORMAT(I7,1X,I1,1X,7(I9,1X))
9509	FORMAT('file name - X2PRTREP.FIL')
9609	FORMAT('software revision number - V01')
	END

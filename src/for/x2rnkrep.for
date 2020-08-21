C
C PROGRAM X2RNKREP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RNKREP.FOV                                 $
C  $Date::   17 Apr 1996 16:31:52                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2rnkrep.for **
C
C X2RNKREP.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C THIS PROGRAM WILL EXTRACT THE RANK WHICH HAS BEEN ASSIGNED
C TO THE STATION FROM THE STATION CONFIGURATION FILE AND WILL
C WRITE THE LIST OF RANKS TO THE FILE - X2RNKREP.FIL
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2RNKREP
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
	INTEGER*4 RANKTAB(4,X2X_STATIONS)
	INTEGER*4 DATETAB(3), OFF1, OFF, FULLREC, REC, ST
	INTEGER*4 BCDADDRESS(2)
	INTEGER*4 START, LENGTH, ERR
	CHARACTER*1 HEADER(32,4), REPLIN(32)
	CHARACTER*32 CHEADER(4), CREPLIN
	EQUIVALENCE (CHEADER,HEADER)
	EQUIVALENCE (CREPLIN,REPLIN)
C
	DATA HEADER /30*' ',Z0D,Z0A,
     *	             30*' ',Z0D,Z0A,
     *	             30*' ',Z0D,Z0A,
     *	             30*' ',Z0D,Z0A/
     *	     REPLIN /30*' ',Z0D,Z0A/
	CALL COPYRITE
C
	TYPE *
	TYPE *,'This proggram wil create the list of the ranks'
	TYPE *,'assigned to the stations'
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
C OPEN A RANK REPORT FILE
C
	CALL ROPEN1('X2RNKREP.FIL',3,32,ST)
	IF (ST .NE. 0) THEN
	  TYPE *, 'X2RNKREP.FIL OPEN ERROR -> ',ST
	  CALL GPAUSE
	ENDIF
	CALL XDAT(DATETAB)
	IF(DATETAB(1).LT.77) DATETAB(1) = DATETAB(1) + 100
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
C EXTRACT X2XSTN_ADDRES AND X2XSTN_RANK
C
	  FULLREC = FULLREC + 1
	  RANKTAB(1,FULLREC) = X2XSTN_STN
	  RANKTAB(2,FULLREC) = X2XSTN_ADDRES(1)
	  RANKTAB(3,FULLREC) = X2XSTN_ADDRES(2)
	  RANKTAB(4,FULLREC) = X2XSTN_RANK
	  GO TO 100
300	  CONTINUE
	  IF (FULLREC .GT. 0) THEN
	    WRITE (CHEADER(4),9309) FULLREC
	    WRITE(3,9109) CHEADER(4)
	    START = 1
	    LENGTH = 9
	    DO 400 OFF = 1, FULLREC
	      DO 350 OFF1 = 1, 30
	        REPLIN(OFF1) = ' '
350	      CONTINUE
	    WRITE (CREPLIN,9409) RANKTAB(1,OFF), RANKTAB(4,OFF)
	    BCDADDRESS(1) = RANKTAB(2,OFF)
	    BCDADDRESS(2) = RANKTAB(3,OFF)
	    CALL HTOA(REPLIN(9),START,LENGTH,BCDADDRESS,ERR)
	    IF (ERR .NE. 0) THEN
	      TYPE *, 'HTOA CONVERSION ERROR -> ',ERR
	      CALL GPAUSE
	    ENDIF
	    WRITE(3,9109,IOSTAT=ST) CREPLIN
	    IF(ST.NE.0) THEN
	      TYPE *, 'X2RNKREP.FIL FILE WRITE ERROR -> ',ST
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
9109	FORMAT(A32)
9209	FORMAT('creation date - ',I2,'/',I2,'/',I4)
9309	FORMAT('number of data records - ',I5)
9409	FORMAT(I7,T19,I3)
9509	FORMAT('file name - X2RNKREP.FIL')
9609	FORMAT('software revision number - V01')
	END

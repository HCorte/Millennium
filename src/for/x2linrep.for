C
C PROGRAM X2LINREP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LINREP.FOV                                 $
C  $Date::   17 Apr 1996 16:20:54                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2linrep.for **
C
C X2LINREP.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C THIS PROGRAM WILL EXTRACT THE LINE ADDRESS FROM THE NETWORK
C PORT CONFIGURATION FILE AND WILL WRITE THE LIST OF ADDRESSES
C TO THE FILE - X2LINREP.FIL
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2LINREP
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
	INCLUDE 'INCLIB:X2XNPC.DEF'
C
	INTEGER*4 LINETAB(3,X2X_NETWORK_PORTS)
	INTEGER*4 DATETAB(3)
	INTEGER*4 BCDADDRESS(2)
	INTEGER*4 START, LENGTH, ERR
	INTEGER*4 OFF1, OFF, FULLREC, REC, ST
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
	TYPE *,'This program will create the list addresses of each'
	TYPE *,'network line'
	TYPE *
C
C OPEN THE NETWORK PORT CONFIGURATION FILE
C
	CALL OPENW(1,X2XNPC_NAME,4,0,0,ST)
	CALL IOINIT(X2XNPC_FDB,1,X2XNPC_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2XNPC_NAME,'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
C
C OPEN A LINE REPORT FILE
C
	CALL ROPEN1('X2LINREP.FIL',3,32,ST)
	IF (ST .NE. 0) THEN
	  TYPE *, 'X2LINREP.FIL OPEN ERROR -> ',ST
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
C LOOP THROUGH THE NETWORK PORT CONFIGURATION FILE
C
	REC = 0
	FULLREC = 0
100	CONTINUE
	  REC=REC+1
	  CALL READW(X2XNPC_FDB,REC,X2XNPC_REC,ST)
	  IF(ST.EQ.144) GO TO 300
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2XNPC_NAME,'READW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XNPC_PORT .LE. 0) GOTO 100
C
C EXTRACT X2XNPC_ADDRESS
C
	  FULLREC = FULLREC + 1
	  LINETAB(1,FULLREC) = X2XNPC_PORT
	  LINETAB(2,FULLREC) = X2XNPC_ADDRES(1)
	  LINETAB(3,FULLREC) = X2XNPC_ADDRES(2)
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
	    WRITE (CREPLIN,9409) LINETAB(1,OFF)
	    BCDADDRESS(1) = LINETAB(2,OFF)
	    BCDADDRESS(2) = LINETAB(3,OFF)
	    CALL HTOA(REPLIN(9),START,LENGTH,BCDADDRESS,ERR)
	    IF (ERR .NE. 0) THEN
	      TYPE *, 'HTOA CONVERSION ERROR -> ',ERR
	      CALL GPAUSE
	    ENDIF
	    WRITE(3,9109,IOSTAT=ST) CREPLIN
	    IF(ST.NE.0) THEN
	      TYPE *, 'X2LINREP.FIL FILE WRITE ERROR -> ',ST
	      CALL GPAUSE
	    ENDIF
400	    CONTINUE
	  ENDIF
C
C CLOSE THE FILES AND CALL GSTOP(GEXIT_SUCCESS)
C
	CALL CLOSEFIL(X2XNPC_FDB)
	CALL USRCLOS1(     3)
C
	CALL GSTOP(GEXIT_SUCCESS)
9109	FORMAT(A32)
9209	FORMAT('creation date - ',I2,'/',I2,'/',I4)
9309	FORMAT('number of data records - ',I5)
9409	FORMAT(I7,T19,I3)
9509	FORMAT('file name - X2LINREP.FIL')
9609	FORMAT('software revision number - V01')
	END

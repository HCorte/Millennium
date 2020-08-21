C
C PROGRAM X2LODPRT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LODPRT.FOV                                 $
C  $Date::   17 Apr 1996 16:21:50                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2lodprt.for;1 **
C
C X2LODPRT.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS) 
C
C
C THIS PROGRAM WILL LOAD SETS OF PORTS FROM THE X2PRTREP.FIL
C TO THE FIELDS IN THE STATION CONFIGURATION FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2LODPRT
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
	INTEGER*4 ASGMTAB(X2XSTN_MAXDEF+2)
	INTEGER*4 OFF1, NUMPRT, REC, OFF, FULLREC, I, ST
	CHARACTER*1 REPLIN(128)
C
	CALL COPYRITE
C
	TYPE *
	TYPE *,'This program loads addresses assigned to each station'
	TYPE *,'to station configuration file '
	TYPE *
C
C OPEN THE PORT REPORT FILE - X2PRTREP.FIL
C
	OPEN(3,FILE='X2PRTREP.FIL',STATUS='NEW',IOSTAT=ST,RECL=128)
	IF (ST .NE. 0) THEN
	  TYPE *, 'X2PRTREP.FIL OPEN ERROR -> ',ST
	  CALL GPAUSE
	ENDIF
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
C READ THE HEADER OF X2PRTREP.FIL
C
	READ(3,9109) (REPLIN(I), I = 1,128)
	READ(3,9109) (REPLIN(I), I = 1,128)
	READ(3,9109) (REPLIN(I), I = 1,128)
	READ(3,9109) (REPLIN(I), I = 1,128)
	READ  (REPLIN,9309) FULLREC
C
C CHANGE THE PROPER RECORDS IN THE STATION CONFIGURATION FILE
C
	IF (FULLREC .GT. 0) THEN
	  DO 200 OFF = 1, FULLREC
	    READ(3,9109) (REPLIN(I), I = 1, 128)
	    READ  (REPLIN,9409) (ASGMTAB(I), I = 1, X2XSTN_MAXDEF + 2)
	    REC = ASGMTAB(1)
	    NUMPRT = ASGMTAB(2)
	    CALL READW(X2XSTN_FDB,REC,X2XSTN_REC,ST)
	    IF(ST.NE.0) THEN
	      CALL OS32ER(5,X2XSTN_NAME,'READW',ST,REC)
	      CALL GPAUSE
	    ENDIF
C
C LOAD X2XSTN_DEF_PORT  TABLE
C
	    DO 150 OFF1 = 1, NUMPRT
	      X2XSTN_DEF_PORT(OFF1) = ASGMTAB(OFF1 + 2)
150	    CONTINUE
	    CALL WRITEW(X2XSTN_FDB,REC,X2XSTN_REC,ST)
200	  CONTINUE
	ENDIF
C
C CLOSE THE FILES AND CALL GSTOP(GEXIT_SUCCESS)
C
	CALL CLOSEFIL(X2XSTN_FDB)
	CALL USRCLOS1(     3)
C
	CALL GSTOP(GEXIT_SUCCESS)
9109	FORMAT(128A1)
9309	FORMAT(T26,I5)
9409	FORMAT(I7,1X,I1,1X,7(I9,1X))
	END

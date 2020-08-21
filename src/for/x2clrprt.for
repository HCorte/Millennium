C
C PROGRAM X2CLRPRT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CLRPRT.FOV                                 $
C  $Date::   17 Apr 1996 16:13:48                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2clrprt.for;1 **
C
C X2CLRPRT.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2CLRPRT
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
	INTEGER*4 OFF, ANS, SECOND, FIRST, REC, ST
C
	CALL COPYRITE
C
	TYPE *
	TYPE *,'<<<<<WFIXX2X >>>>>'
	TYPE *
C
	CALL OPENW(1,X2XSTN_NAME,4,0,0,ST)
	CALL IOINIT(X2XSTN_FDB,1,X2XSTN_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2XSTN_NAME,'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
C
C LOOP THROUGH THE STATION FILE;
C CLEAR X2XSTN_DEF_PORT  AND  X2XSTN_DIAL_PORT  TABLES
C
	REC = 0
	FIRST = 0
	SECOND = 0
	CALL WIMG(5,'CLEAR X2XSTN_DEF_PORT  [Y/N] ....')
	CALL YESNO(ANS)
	IF(ANS.EQ.1) FIRST = 1
	CALL WIMG(5,'CLEAR X2XSTN_DIAL_ PORT  [Y/N] ....')
	CALL YESNO(ANS)
	IF(ANS.EQ.1) SECOND = 1
	IF(FIRST + SECOND .EQ. 0) GO TO 499
100	CONTINUE
	  REC=REC+1
	  CALL READW(X2XSTN_FDB,REC,X2XSTN_REC,ST)
	  IF(ST.EQ.144) GO TO 499
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2XSTN_NAME,'READW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XSTN_REC(1).LE.0) GOTO 100
C
C CLEAR  X2XSTN_DEF_PORT  TABLE
C
	  IF(FIRST .NE. 1) GO TO 250
	  DO 199 OFF = 1, X2XSTN_MAXDEF
	    X2XSTN_DEF_PORT(OFF) = 0
199	  CONTINUE
C
C CLEAR  X2XSTN_DIAL_PORT  TABLE
C
250	  CONTINUE
	  IF(SECOND .NE. 1) GO TO 350
	  DO 299 OFF = 1, X2XSTN_MAXDIAL
	    X2XSTN_DIAL_PORT(OFF) = 0
299	  CONTINUE
350	  CONTINUE
	  CALL WRITEW(X2XSTN_FDB,REC,X2XSTN_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2XSTN_NAME,'READW',ST,REC)
	    CALL GPAUSE
	  ENDIF
	  GOTO 100
C
C CLOSE THE FILE AND CALL GSTOP(GEXIT_SUCCESS)
C
499	CONTINUE
	CALL CLOSEFIL(X2XSTN_FDB)
C
	CALL GSTOP(GEXIT_SUCCESS)
	END

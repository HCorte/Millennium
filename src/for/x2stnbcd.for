C
C PROGRAM X2STNBCD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STNBCD.FOV                                 $
C  $Date::   17 Apr 1996 16:35:56                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2stnbcd.for **
C
C X2STNBCD.FOR
C
C V02 03-FEB-94 GPR USE I5 FORMAT FOR STATION AND TERMINAL TYPE-OUTS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program generates a list of station addresses
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
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2STNBCD
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
C
	INTEGER*4   ST              !Status
	INTEGER*4   REC             !Record
	INTEGER*4   ERRCNT /0/      !Error count
	INTEGER*4   OFF, ERR
	LOGICAL*1   REPORT /.TRUE./ !Error report flag
	CHARACTER   ASCIADDR(16)    !ASCI address
C
	CHARACTER*12 X2BCDFIL /'X2STNBCD.FIL'/
C
C OPEN THE STATION FILE.
C
40      CONTINUE
	CALL OPENW(1,SFNAMES(1,XSTN),4,0,0,ST)
	CALL IOINIT(X2XSTN_FDB,1,X2XSTN_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2XSTN_NAME,'OPENX',ST,0)
	  CALL GPAUSE
	  GOTO 40
	ENDIF
C
C OPEN THE BCD LIST FILE.
C
50	CONTINUE
	CALL OPENW(2,X2BCDFIL,4,0,0,ST)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2BCDFIL,'OPENX',ST,0)
	  CALL GPAUSE
	  GOTO 50
	ENDIF
	REWIND (2)
C
C DISPLAY COPYRIGHT.
C
	CALL COPYRITX(5)
C
C READ THE STATIONS RECORDS, EXTRACT STATION NUMBER AND BCD ADDRESS
C
	DO 100 REC=1,X2X_STATIONS
C
	  CALL READW(X2XSTN_FDB,REC,X2XSTN_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2XSTN_NAME,'READW',ST,X2XSTN_STN)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XSTN_STN.LE.0) THEN
	    IF(REPORT) THEN
	       WRITE(5,9030) REC,X2XSTN_STN,CHAR(7)
	       CALL WIMG(5,'Do you want to disable error log?[Y/N]')
	       CALL YESNO(ST)
	       IF(ST.EQ.1) REPORT=.FALSE.
	    ENDIF
	    ERRCNT=ERRCNT+1
	    GOTO 100
	  ENDIF
C
C CONVERT BCD ADDRESS
C
	  CALL HTOA(ASCIADDR,1,X2XSTN_ADDLEN,X2XSTN_ADDRES,ERR)
	  IF(ERR.NE.0) THEN
	    WRITE(5,9032) X2XSTN_STN,X2XSTN_ADDRES,X2XSTN_ADDLEN
	    CALL GPAUSE
	    GOTO 100
	  ENDIF
C
C WRITE TO THE LIST FILE
C
	  WRITE(2,9033) X2XSTN_STN,
     *                X2XSTN_ADDLEN,(ASCIADDR(OFF),OFF=1,X2XSTN_ADDLEN)
C
100	CONTINUE
C
C PROGRAM EXIT.
C
	IF(ERRCNT.GT.0) THEN
	   WRITE(5,9034) ERRCNT,CHAR(7)
	ENDIF
C
	CALL CLOSEFILE(X2XSTN_FDB)
	CALL USRCLOS1(2)
	CALL USRCLOS1(5)
C
	TYPE *,IAM(),'OK'
	CALL GSTOP(GEXIT_SUCCESS)
C
C     ================== Format Statements =====================
C
9030	FORMAT(1X,'Station ',I5,' has invalid station number: ',I8,A/)	! V02
9032	FORMAT(1X,'Station ',I5,' has invalid station address: ',
     *	       2(Z8,1X),'(',I4,')')					! V02
9033	FORMAT(I8.8,1X,I2.2,1X,16A1)
9034	FORMAT(1X,I5,'  Station(s) have invalid station numbers',A)	! V02
C
	END

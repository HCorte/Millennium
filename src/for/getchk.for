C
C PROGRAM GETCHK
C $Log:   GXAFXT:[GOLS]GETCHK.FOV  $
C  
C     Rev 1.1   17 May 1996 11:42:54   HXK
C  Update from Wojtek, Siew Mun
C  
C     Rev 1.0   21 Jan 1993 16:24:30   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - getchk.for **
C
C GETCHK.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01  11-JAN-90  GCAN  MODIFIED TO HANDLE EXTERNAL TO INTERNAL
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
	PROGRAM GETCHK
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
C
	INTEGER*4 I, CHK, CHKERR, INPVER, INTSER, JUL, CHECK
	INTEGER*4 SCRAM, SER, CDC, EXT, FUN
	INTEGER*2 DBUF(12)
C
C
	CALL COPYRITE
C
10	CONTINUE
	TYPE*,' '
	TYPE*,' 1 - Internal --> External serial number '
	TYPE*,' 2 - External --> Internal serial number '
	TYPE*,' '
	CALL INPNUM('Enter desired function: ',FUN,1,2,EXT)
	IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
	IF(FUN.EQ.1) THEN
	   CALL INPNUM('Enter CDC date      :',CDC,1,9999,EXT)
	   IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	   CALL INPNUM('Enter SERIAL number :',SER,1,999999999,EXT)
	   IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	   CALL OUTGEN(CDC,SER,SCRAM,CHECK)
	   DBUF(VCDC)=CDC
	   CALL CDATE(DBUF)
	   JUL=DBUF(VJUL)
	   WRITE(5,101)
	   WRITE(5,100) JUL,SCRAM,CHECK
	   WRITE(5,101)
	ELSE
	   CALL INPNUM('Enter CDC date              : ',
     *	               CDC,1,9999,EXT)
	   IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
	   CALL INPNUM('Enter External serial number: ',
     *	               SCRAM,0,99999999,EXT)
	   IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
	   CALL INPNUM('Enter Check digits          : ',
     *	               CHECK,0,999,EXT)
	   IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
	   CHKERR=INPVER(CDC,SCRAM,INTSER,CHECK)
	   IF(CHKERR.NE.0) THEN
	      TYPE*,'**** Check digit errror **** '
	      TYPE*,' Trying to find it myself '
	      CHK=0
	      DO 20 I=0,999
	         CHKERR=INPVER(CDC,SCRAM,INTSER,CHK)
	         IF(CHKERR.EQ.0) THEN
	            WRITE(5,101)
	            WRITE(5,300) INTSER
	            WRITE(5,101)
	         ENDIF
	         CHK=CHK+1
20	      CONTINUE
	      GOTO 10
	   ENDIF
	   WRITE(5,101)
	   WRITE(5,200) INTSER
	   WRITE(5,101)
	ENDIF
	GOTO 10
100	FORMAT(' External serial number -:  ',I3.3,' - ',
     *	 I9.8,' - ',I3.3)
101	FORMAT(/)
200	FORMAT(' Internal serial number -: ',I9.8)
300	FORMAT(' ** Internal serial could be -: ',I9.8,' ** ')
	END

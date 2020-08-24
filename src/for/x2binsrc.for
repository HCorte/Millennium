C
C SUBROUTINE X2BINSRC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   DKA100:[X2XBASELINES.EURO_BASE]X2BINSRC.FOV                   $
C  $Date::   29 May 1998 10:39:08                                             $
C  $Revision::   1.2                                                          $
C  $Author::   GAH                                                            $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V04 27-MAY-97 GAH Bug fix:  Changed comparion logic to accounted for signed
C                             integer implementation of addresses
C V03 19-APR-96 DXG Bug fix:  Changed .AND. to .OR. in search of address table
C V02 12-DEC-94 GPR Integrate UK changes into X2X Baseline
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This routine will binary search the X2X_SORTED_ADR table
C to find the input station address, and will return the
C station number.
C
C Calling sequence:
C
C     CALL X2BINSRC(ADRLEN,STNADR,STATION,STATUS)
C
C Input parameters:
C
C     ADRLEN  Int*4                       Address length
C     STNADR  Int*4(X2X_ADRESS_MAXLEN)    Station address
C
C
C Output parameters:
C
C     STATION Int*4       Station number
C     STATUS  Int*4       Return status (0=found,-1=not found,
C                                       -2=locked)
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2BINSRC(ADRLEN,STNADR,STATION,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4   ADRLEN                      !Address length
	INTEGER*4   STNADR(X2X_ADRESS_MAXLEN)   !Address to search for
	INTEGER*4   STATION                     !Return station number
	INTEGER*4   STATUS                      !Search status
	INTEGER*4   BOT,TOP,TARGET              !Search indices
	INTEGER*4   BEGTOP                      !Bottom of table (skip blanks)
	INTEGER*4   STNIDX
	LOGICAL     INIT                        !Table initialized flag
	INTEGER*4   ADR_LENGTH   		!V02
C
	DATA        BEGTOP / 0 /
	DATA        INIT   /.FALSE./
C
C IF FIRST PASS, SEARCH THROUGH THE SORTED ADDRESS
C TABLE TO FIND THE FIRST VALID STATION NUMBER.
C
	IF(.NOT.INIT .OR. X2X_SORTED_UPDATE.EQ.1) THEN
	  DO 100 BEGTOP=X2X_STATIONS,1,-1
	    IF(X2X_SORTED_ADR(1,BEGTOP).NE.0 .OR.
     *	       X2X_SORTED_ADR(2,BEGTOP).NE.0 ) GOTO 110			  !V02
100	  CONTINUE
	  BEGTOP=0							  !V02
110	  CONTINUE
	  INIT=.TRUE.
	  X2X_SORTED_UPDATE=0
	ENDIF
C
C
C INITIALIZE VARIABLES.
C
C	***** Start V02 changes *****
C
	IF (BEGTOP.LE.0) THEN
	    STATUS=-4
	    GOTO 8000
	ENDIF
C
C	***** End V02 changes *****
C
	BOT=1
	TOP=BEGTOP+1
	STATUS=0
C
C BINARY SEARCH THROUGH SORTED ADDRESS TABLE.
C
200	CONTINUE
	TARGET=BOT+(TOP-BOT)/2
	STNIDX=X2X_SORTED_ADR(0,TARGET)
C
C	***** Start V02 changes *****
C
	ADR_LENGTH=0
	IF (STNIDX.NE.0) ADR_LENGTH = X2XS_ADRESS_LEN(STNIDX)
	IF(X2X_SORTED_ADR(1,TARGET).EQ.STNADR(1) .AND.
     *	   X2X_SORTED_ADR(2,TARGET).EQ.STNADR(2) .AND.
     *	   ADR_LENGTH .EQ.ADRLEN)   THEN
	  STATION=STNIDX
	  STATUS=0
	  IF (STNADR(1).NE.X2XS_ADRESS(1,STNIDX) .OR.	    !V03 .AND. -> .OR.
     *	      STNADR(2).NE.X2XS_ADRESS(2,STNIDX)) STATUS=-3
	  GOTO 7000
	ENDIF
C
C	***** End V02 changes *****
C
C IF ALL POSSIBLE ELEMENTS SEARCHED AND ADDRESS IS NOT
C FOUND, RETURN AN ERROR CODE.
C
	IF(TARGET.EQ.BOT .OR. TARGET.EQ.TOP) THEN
	  STATUS=-1
	  GOTO 7000
	ENDIF
C
C FIRST CHECK IF THE FIRST 4 BYTES OF THE ADDRESS MATCH.
C IF THEY DO, DETERMINE WHICH WAY TO SEARCH.
C
C BEGIN V04
	IF(X2X_SORTED_ADR(1,TARGET).EQ.STNADR(1)) THEN
	  IF(X2X_SORTED_ADR(2,TARGET).GE.0.AND.STNADR(2).GE.0.AND.
     +       X2X_SORTED_ADR(2,TARGET).LT.STNADR(2) .OR.
     +       X2X_SORTED_ADR(2,TARGET).LT.0.AND.STNADR(2).LT.0.AND.
     +       X2X_SORTED_ADR(2,TARGET).LT.STNADR(2) .OR.
     +       X2X_SORTED_ADR(2,TARGET).GE.0.AND.STNADR(2).LT.0) THEN
C END V04
C	  IF(X2X_SORTED_ADR(2,TARGET).LT.STNADR(2)) THEN  ! V03
	    BOT=TARGET
	    GOTO 200
	  ELSE
	    TOP=TARGET
	    GOTO 200
	  ENDIF
	ENDIF
C
C IF FIRST 4 BYTES DO NOT MATCH AT ALL, SEARCH ACCORDING
C TO ADDRESS VALUE.
C
C BEGIN V04
	IF(X2X_SORTED_ADR(1,TARGET).GE.0.AND.STNADR(1).GE.0.AND.
     +     X2X_SORTED_ADR(1,TARGET).LT.STNADR(1) .OR.
     +     X2X_SORTED_ADR(1,TARGET).LT.0.AND.STNADR(1).LT.0.AND.
     +     X2X_SORTED_ADR(1,TARGET).LT.STNADR(1) .OR.
     +     X2X_SORTED_ADR(1,TARGET).GE.0.AND.STNADR(1).LT.0) THEN
C END V04
C	IF(X2X_SORTED_ADR(1,TARGET).LT.STNADR(1)) THEN  ! V03
	  BOT=TARGET
	  GOTO 200
	ELSE
	  TOP=TARGET
	  GOTO 200
	ENDIF
C
C RELEASE THE RECORD LOCK.
C
7000	CONTINUE
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END

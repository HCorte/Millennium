C
C PROGRAM X2LDSTRN.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LDSTRN.FOV                                 $
C  $Date::   17 Apr 1996 16:20:48                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C X2X Upgrade: 22-FEB-96 wsm Rearranged order of PRMAGT.DEF, AGTINF.DEF, 
C                            RECAGT.DEF for Finland.
C
C V01 13-DEC-94 GPR RELEASED FOR UK
C
C THIS PROGRAM WILL FILL UP X2STN_RANK FIELD
C IN THE STATION CONFIGURATION FILE   (SCF)
C
C
C     LOWRANK        INT*4        THE LOWEST  RANK ASSIGNED
C     HGHRANK        INT*4        THE HIGHEST RANK ASSIGNED
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2LDSTRN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
C
	INTEGER*4 LOWRANK, HGHRANK
	INTEGER*4 RANKTAB(NUMAGT), STFIDX(1,NUMAGT)
	INTEGER*4 COUNT, MXVAL, MINVAL, REC
	INTEGER*4 CURIDX, CURVAL, BETIDX, OFF, ST
	CHARACTER X2FILNAM*20
	CHARACTER TASTN*5
	EQUIVALENCE (ASFBYT(SXSTN), TASTN)
	PARAMETER(LOWRANK =  3)     !THE LOWEST  RANK TO BE ASSIGNED
	PARAMETER(HGHRANK = 10)     !THE HIGHEST RANK TO BE ASSIGNED
C
	CALL COPYRITE
C
C   SET VARIABLES
C
	CALL FASTSET(0, RANKTAB, NUMAGT)
	CALL FASTSET(0, STFIDX, 1*NUMAGT)
	COUNT = 0
	MXVAL = 0
	MINVAL = 1000000
C
C   OPEN AGENT SALES FILE
C
C***  CALL OPENASF(ASF)
	CALL ASFOPEN(ASF)
C
C   LOOP THROUGH AND READ AGENT SALES FILE BY AGENT RECORD NUMBER
	DO 1660 REC = 1, NUMAGT
	  CALL READASF(REC, ASFREC, ST)
	  IF (ST .NE. 0) THEN
	    TYPE *, ' ASF reading error st - ', ST
	    CALL GSTOP(GEXIT_SUCCESS)
	  ENDIF
	  CURIDX = CTOI(TASTN, OFF)
C***    TYPE *, '   off = ', OFF, ' curidx =', CURIDX
	  IF (CURIDX .EQ. 0) GO TO 1660
	  COUNT = COUNT + 1
	  STFIDX(1,COUNT) = CURIDX
	  CURVAL = 0
	  DO 1550 BETIDX = 1, MAXGAM
	    CURVAL = CURVAL + ASFBIL(GSCNT, BETIDX, 2)
     *	                    + ASFBIL(GCCNT, BETIDX, 2)
     *	                    + ASFBIL(GVCNT, BETIDX, 2)
1550	  CONTINUE
	  RANKTAB(COUNT) = CURVAL
	  IF (CURVAL .GT. MXVAL) MXVAL = CURVAL
	  IF (CURVAL .LT. MINVAL) MINVAL = CURVAL
C***    TYPE *, REC, RANKTAB(REC), STFIDX(1,REC)
1660	CONTINUE
C
C   CLOSE AGENT SALES FILE
C
	CALL CLOSASF
C
C   RANKS VALUE CALCULATION
C
	IF (MXVAL .LE. MINVAL) THEN
	  TYPE *, '   the sum of the count fields in the ASF',
     *	  '  has the same value for all retailers'
	  TYPE *, '   the ranking is not possible'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
	DO 1770 OFF = 1, COUNT
	  RANKTAB(OFF) = LOWRANK + INT(FLOAT((RANKTAB(OFF) - MINVAL)
     *	  *(HGHRANK - LOWRANK)) / FLOAT(MXVAL - MINVAL) + 0.5)
C***    TYPE *, OFF, RANKTAB(OFF), STFIDX(1,OFF)
1770	CONTINUE
	CALL I4SHELL(RANKTAB, COUNT, STFIDX, 1)
C
C   OPEN STATION CONFIGURATION FILE
C
	CALL OPENX2X(X2FILNAM(XSTN), 1)
	DO 1880 OFF = 1, COUNT
C***    TYPE *, OFF, RANKTAB(OFF), STFIDX(1,OFF)
	  CURIDX = STFIDX(1, OFF)
	  CALL READX2X(1, CURIDX, X2XSTN_REC, ST)
	  X2XSTN_RANK = RANKTAB(OFF)
	  CALL WRITX2X(1, CURIDX, X2XSTN_REC, ST)
1880	CONTINUE
	CALL CLOSX2X(1)
	CALL GSTOP(GEXIT_SUCCESS)
	END

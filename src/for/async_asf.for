   	PROGRAM TEST
	IMPLICIT NONE
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE	'INCLIB:GLOBAL.DEF'
	INCLUDE	'INCLIB:PRMAGT.DEF'
	INCLUDE	'INCLIB:AGTINF.DEF'
	INCLUDE	'INCLIB:RECAGT.DEF'
	INCLUDE	'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
	INCLUDE	'INCLIB:X2XSCL.DEF'
C
	CHARACTER   X2FILNAM*20
	INTEGER*4   STATUS
	INTEGER*4   STATION
	INTEGER*4   STATION_CLASS
	INTEGER*4   TERMINAL
        INTEGER*4   OFFSET
        INTEGER*4   IDROP
        CHARACTER*2 CXDROP
C
        INTEGER*4   CLASS3_OFFSET
        PARAMETER   (CLASS3_OFFSET = 0000)
C
	CHARACTER   AGENT_STRING*(ALENGTH)
	EQUIVALENCE (AGENT_STRING, ASFBYT)
C
	CALL COPYRITE
C
	CALL OPENASF(1)
C
	TYPE *,'Begining agent file update'
C
C Read all terminal records & update the station addresses from station
C file
C
	DO 1000 TERMINAL = 1, NUMAGT
C
	    IF(MOD(TERMINAL, 696) .EQ. 0)
	1	TYPE *,TERMINAL,' terminals processed'
C
C	    Read TERMINAL reocrd which has no STATION address
C
	    CALL READASF(TERMINAL, ASFREC, STATUS)
	    IF(STATUS .NE. 0)THEN
		WRITE(UNIT = 5, FMT = '(1X, A, ''READASF error'',
	1	    I, '' TERMINAL'', I)') IAM(), STATUS, TERMINAL
		GOTO 1000
	    ENDIF
C
C
C           Replace non printable characters with blanks
C
C
            DO 500 OFFSET=1,ALENGTH
               IF (AGENT_STRING(OFFSET:OFFSET) .LT. ' ')       
	1	   AGENT_STRING(OFFSET:OFFSET) = ' '
500         CONTINUE
     	       
C
C
C	    Try to read the station record #.  If there is no station # then
C	    we skip the record.
C
	    READ(UNIT = AGENT_STRING(SSCLS:ESCLS),
	1	FMT = '(BN, I)', ERR = 1000) STATION_CLASS
C	
C
            IF (STATION_CLASS .EQ. 1 .OR. STATION_CLASS .EQ. 10)
     1          GOTO 1000
C
C
            CXDROP = AGENT_STRING (SDROP:EDROP)
	    IDROP  = ICHAR(CXDROP(1:1)) - 63
            IF (IDROP .LT. 1 .OR. IDROP .GT. X2X_MAXTERMS) GOTO 1000
C
C    
            READ(UNIT = AGENT_STRING(SXSTN+1:EXSTN),
	1	FMT = '(BN, I)', ERR = 1000) STATION
C
            STATION_CLASS = 3
            STATION       = STATION + CLASS3_OFFSET 
C
	    WRITE(UNIT = AGENT_STRING(SSCLS:ESCLS),
	1	  FMT = '(I<LSCLS>)', IOSTAT = STATUS)
	2	  STATION_CLASS
C
	    WRITE(UNIT = AGENT_STRING(SXSTN:EXSTN),
	1	  FMT = '(I<LXSTN>)', IOSTAT = STATUS)
	2	  STATION      
C
C	    Put the station address into the ASF record
C
C
	    CALL WRITASF(TERMINAL, ASFREC, STATUS)
	    IF(STATUS .NE. 0)THEN
		WRITE(UNIT = 5, FMT = '(1X, A, ''WRITASF error'',
     1  	    I, '' TERMINAL'', I)') IAM(), STATUS, TERMINAL
		GOTO 1000
	    ENDIF
C
1000	CONTINUE
C
	CALL GSTOP(GEXIT_SUCCESS)
	END

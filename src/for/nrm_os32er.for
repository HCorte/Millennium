C
C SUBROUTINE OS32ER
C $Log:   GXAFXT:[GOLS]OS32ER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:20:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:13:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_os32er.for **
C
C OS32ER.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C  B02  26-NOV-88  MRM  Fix bug with unknown error code.
C  B01  06-SEP-88  MRM  Initial Release
C
C     This subroutine will display a standard error message
C     given the error number and the calling routine.  The
C     possible routines are:  all enhanced FORTRAN I/O calls
C     (OPEN, READ, WRITE, CLOSE), OPENW, DISKIO, TAPEIO, CFILW,
C     DFILW, RENAME, and all hash file subroutines.
C
C     Calling Sequence:
C
C         CALL OS32ER(CONLU,FILNAM,SUBNAM,ERROR,RECORD)
C
C     Input parameters:
C
C         CONLU       Int*4       Previously opened output
C                                 logical unit.
C         FILNAM      Char*(*)    File name or device.
C         SUBNAM      Char*(*)    Name of subroutine executed to
C                                 produce I/O error (refer to STNERR.DEF).
C         ERROR       Int*4       Error number to report.
C         RECORD      Int*4       Record number where error ocurred
C                                 (not used for open and close errors).
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
	SUBROUTINE OS32ER(CONLU,FILNAM,XSUBNAM,ERROR,RECORD)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:NRM_OS32ER.DEF'
	INCLUDE 'INCLIB:STNERR.DEF'
C
	INTEGER*4       CONLU,              !Output logical unit
     *	                SUBTYP,             !Type of error condition
     *	                ERROR,              !O/S 32 error number
     *	                RECORD,             !Record number
     *	                TEMP_ERR,           !Temp array index
     *	                ERRINDX,            !Actual index into error messages
     *	                XRFIND,             !Remap search index
     *	                ASC,                !ASCII numbers
     *	                UPPERZ,             !Upper case 'Z' ASC value
     *	                OFFSET,             !ASC offset between upper/lower
     *	                POS,                !Substring index
     *	                I                   !Array index
C
	INTEGER*4       MAXXRF              !Cross reference table size
	PARAMETER      (MAXXRF = 12)
C
	INTEGER*2       MAPOPNW(MAXXRF),    !OPENW map into OS32ER table
     *	                XRFOPNW(MAXXRF),    !OPENW cross reference table
     *	                MAPSVC1(MAXXRF),    !SVC1 map into OS32ER table
     *	                XRFSVC1(MAXXRF),    !SVC1 cross reference table
     *	                MAPCFIL(MAXXRF),    !CFILW map into OS32ER table
     *	                XRFCFIL(MAXXRF),    !CFILW cross reference table
     *	                MAPDFIL(MAXXRF),    !DFILW map into OS32ER table
     *	                XRFDFIL(MAXXRF),    !DFILW cross reference table
     *	                MAPRENM(MAXXRF),    !RENAME map into OS32ER table
     *	                XRFRENM(MAXXRF)     !RENAME cross reference table
C
	LOGICAL		GTECH_ERR	    !GTECH error type
	CHARACTER       FILNAM*(*),         !Output file name
     *                  XSUBNAM*(*),
     *	                SUBNAM*(10),        !Subroutine where error occurred
     *	                BELL*1  /Z07/       !Ring Bell
C
	DATA            MAPOPNW / MAXXRF*99/
	DATA           (MAPOPNW(I),XRFOPNW(I),I=1,10)
     *	                        / 51,02,52,03,54,04,55,05,  !OPENW errors
     *	                          56,06,58,07,62,08,63,09,
     *	                          66,11,67,12 /
C
	DATA            MAPSVC1 / MAXXRF*99/
	DATA           (MAPSVC1(I),XRFSVC1(I),I=1,7)        !SVC1 errors
     *	                        / 25,129,26,130,27,132,
     *	                          -3,136,28,144,29,160,
     *	                          30,192 /
C
	DATA            MAPCFIL / MAXXRF*99/
	DATA           (MAPCFIL(I),XRFCFIL(I),I=1,9)        !CFILW errors
     *	                        / 50,01,00,02,52,03,53,04,
     *	                          55,05,57,07,65,10,66,11,
     *	                          68,13 /
C
	DATA            MAPDFIL / MAXXRF*99/
	DATA           (MAPDFIL(I),XRFDFIL(I),I=1,8)        !DFILW errors
     *	                        / 52,03,53,04,56,06,57,07,
     *	                          62,08,65,10,66,11,68,13 /
C
	DATA            MAPRENM / MAXXRF*99/
	DATA           (MAPRENM(I),XRFRENM(I),I=1,6)        !RENAME errors
     *	                        / 51,02,53,04,60,07,64,09,
     *	                          65,10,66,11 /
C
C
C     Check the input subroutine name for lower case characters,
C     and it they exist, convert them to upper case.
C
	SUBNAM = XSUBNAM
C
	GTECH_ERR = .TRUE.
	UPPERZ = ICHAR('Z')
	OFFSET = ICHAR('a') - ICHAR('A')
	DO 50 POS = 1,LEN(SUBNAM)
	    ASC = ICHAR(SUBNAM(POS:POS))
	    IF (ASC .GT. UPPERZ) THEN
	      SUBNAM(POS:POS) = CHAR(ASC-OFFSET)
	    END IF
50	CONTINUE
C
C
C     Based on the input subroutine name, search through the
C     subroutine type table to determine the correct offset.
C
	XRFIND = 1
	SUBTYP = 1
100	CONTINUE
	IF (SUBTYP .LT. ERRUNKN) THEN
	    IF (SUBNAM .NE. TYPTBL(SUBTYP)) THEN
	        SUBTYP = SUBTYP + 1
	        GOTO 100
	    END IF
	END IF
	IF(SUBTYP .EQ. ERRUNKN) GTECH_ERR = .FALSE.
C
C     If an SVC1 error, determine the index into the FORTRAN
C     error description table.
C
	TEMP_ERR = ERROR
	IF (SUBTYP .GE. ERRREADW .AND.
     *	    SUBTYP .LE. ERRFEOT) THEN
	    XRFIND = 1
110	    CONTINUE
	    IF (XRFIND .LT. MAXXRF) THEN
	        IF (ERROR .NE. XRFSVC1(XRFIND)) THEN
	            XRFIND = XRFIND + 1
	            GOTO 110
	        END IF
	    END IF
	    TEMP_ERR = MAPSVC1(XRFIND)
	END IF
C
C
C     If an OPENW error, determine the index into the FORTRAN
C     error description table.
C
	IF (SUBTYP .EQ. ERROPENW) THEN
	    XRFIND = 1
120	    CONTINUE
	    IF (XRFIND .LT. MAXXRF) THEN
	        IF (ERROR .NE. XRFOPNW(XRFIND)) THEN
	            XRFIND = XRFIND + 1
	            GOTO 120
	        END IF
	    END IF
	    TEMP_ERR = MAPOPNW(XRFIND)
	END IF
C
C
C     If a CFILW error, determine the index into the FORTRAN
C     error description table.
C
	IF (SUBTYP .EQ. ERRCFILW) THEN
	    XRFIND = 1
130	    CONTINUE
	    IF (XRFIND .LT. MAXXRF) THEN
	        IF (ERROR .NE. XRFCFIL(XRFIND)) THEN
	            XRFIND = XRFIND + 1
	            GOTO 130
	        END IF
	    END IF
	    TEMP_ERR = MAPCFIL(XRFIND)
	END IF
C
C
C     If a DFILW error, determine the index into the FORTRAN
C     error description table.
C
	IF (SUBTYP .EQ. ERRDFILW) THEN
	    XRFIND = 1
140	    CONTINUE
	    IF (XRFIND .LT. MAXXRF) THEN
	        IF (ERROR .NE. XRFDFIL(XRFIND)) THEN
	            XRFIND = XRFIND + 1
	            GOTO 140
	        END IF
	    END IF
	    TEMP_ERR = MAPDFIL(XRFIND)
	END IF
C
C
C     If a RENAME error, determine the index into the FORTRAN
C     error description table.
C
	IF (SUBTYP .EQ. ERRRENAME) THEN
	    XRFIND = 1
150	    CONTINUE
	    IF (XRFIND .LT. MAXXRF) THEN
	        IF (ERROR .NE. XRFRENM(XRFIND)) THEN
	            XRFIND = XRFIND + 1
	            GOTO 150
	        END IF
	    END IF
	    TEMP_ERR = MAPRENM(XRFIND)
	END IF
C
C IF GTECH SYSTEM ERROR, DISPLAY THE APPROPRIATE ERROR MESSAGE.
C
	IF(GTECH_ERR) THEN
C
C Check for a valid error code.
C
	  IF (TEMP_ERR .GT. OSXREF(OS32TOP)) TEMP_ERR=OSXREF(OS32TOP)
C
C
C Search the cross reference table to obtain the
C actual index into the error message table.
C
	  ERRINDX = 1
200	  IF (ERRINDX .LT. OS32TOP) THEN
	      IF (OSXREF(ERRINDX) .NE. TEMP_ERR) THEN
	          ERRINDX = ERRINDX + 1
	          GOTO 200
	      END IF
	  END IF
C
C Determine if the record # is to be printed,
C and use the appropriate format statement.
C
	  WRITE (CONLU,9015)
 	  IF (DISREC(SUBTYP) .NE. 0) THEN
	      WRITE (CONLU,9000) FILNAM,  SUBNAM,
     *	                         ERROR,   OSDESC(ERRINDX),
     *	                         RECORD,  BELL
	  ELSE
	      WRITE (CONLU,9010) FILNAM,  SUBNAM,
     *	                         ERROR,   OSDESC(ERRINDX),
     *	                         BELL
	  END IF
C
C VMS ERROR CODE.
C
	ELSE
	  WRITE (CONLU,9015)
	  WRITE (CONLU,9020) FILNAM, SUBNAM, ERROR, RECORD, BELL
	  CALL LIB$SIGNAL(%VAL(ERROR))
	ENDIF
	RETURN
C
C     ==================== Format Statements ====================
C
9000	FORMAT(1X,A18,1X,A10,1X,'ERR',1X,I7,3X,
     *	         A35,/,31X,'REC=',I7,A,/)
9010	FORMAT(1X,A18,1X,A10,1X,'ERR',1X,I7,3X,A35,A,/)
9015	FORMAT(/,20('='),' ERROR CONDITION ',20('='))
9020	FORMAT(1X,A18,1X,A10,1X,'ERR',1X,I7,3X,'REC=',I7,A)
	END

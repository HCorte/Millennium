C
C SUBROUTINE OPENXY
C $Log:   GXAFXT:[GOLS]OPENXY.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:19:06   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   02 Sep 1994 18:31:34   HXK
C  Merge of May,June RFSS batch
C  
C     Rev 1.1   28 Apr 1994 19:10:22   HXK
C  ADDED READONLY WHEN SHARIND .EQ. 0
C                  AND READIND .NE. 0
C  
C     Rev 1.0   09 Mar 1994 13:00:00   JXP
C  Initial revision.
C  
C     Rev 1.0   21 Jan 1993 17:12:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskio.for **
C
C
C
C
C **** OPENXY
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE OPENXY(LUN, FILENAME, SHRIND, READIND, DUM2, ST)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    'INCLIB:DISKIO.DEF'
	INCLUDE	'($SYSSRVNAM)'
	INCLUDE '($FABDEF)'
	INCLUDE '($STSDEF)'
	INCLUDE '($RMSDEF)'
C
	INTEGER*4   LUN
	CHARACTER   FILENAME*(*)
	INTEGER*4   SHRIND
	INTEGER*4   READIND
	INTEGER*4   DUM2
	INTEGER*4   ST
	
	LOGICAL*1    ISTHERE
	CHARACTER*20 ISDIRECT
	CHARACTER*20 ORGTYPE
	CHARACTER*20 RECTYPE
C
	INTEGER*4   DISKOPEN
	EXTERNAL    DISKOPEN
	INTEGER*4   DISKOPENY
	EXTERNAL    DISKOPENY
C
C
C
C
C
C If the file doesn't exist, just return with status = -1
C
	INQUIRE(FILE=FILENAME, EXIST=ISTHERE,
     *	            IOSTAT=ST,
     *	          DIRECT=ISDIRECT,
     *	          ORGANIZATION=ORGTYPE,
     *	          RECORDTYPE=RECTYPE)
	IF(.NOT.ISTHERE)THEN
	  ST = -1
	  TYPE *,IAM(),'FILE NOT FOUND: ', FILENAME
C***	  CALL LIB$SIGNAL(%VAL(RMS$_FNF))
	  GOTO 9000
	ENDIF
C
C Save the file name for RENAME(LUN, NAME, ...) option
C
	FNAMES(LUN) = FILENAME
C
C Now open the file
C
	IF(ISDIRECT.EQ.'YES' .AND.
     *	     ORGTYPE.EQ.'RELATIVE' .AND.
     *	     RECTYPE.EQ.'FIXED')THEN
C
	  IF(SHRIND.EQ.0.AND.READIND.EQ.0)THEN
   	    OPEN(UNIT=LUN, FILE=FILENAME, ORGANIZATION='RELATIVE',
     1	           ACCESS='DIRECT', STATUS='OLD', IOSTAT=ST,
     2	           USEROPEN=DISKOPEN, SHARED)
	  ELSEIF(SHRIND.EQ.0.AND.READIND.NE.0)THEN
   	    OPEN(UNIT=LUN, FILE=FILENAME, ORGANIZATION='RELATIVE',
     1	           ACCESS='DIRECT', STATUS='OLD', IOSTAT=ST,
     2	           USEROPEN=DISKOPENY, SHARED, READONLY)
	 ELSE
   	    OPEN(UNIT=LUN, FILE=FILENAME, ORGANIZATION='RELATIVE',
     1	           ACCESS='DIRECT', STATUS='OLD', IOSTAT=ST,
     2	           USEROPEN=DISKOPEN, SHARED)
	  ENDIF
	ELSE
	  IF(SHRIND.EQ.0)THEN
	    OPEN(UNIT=LUN, FILE=FILENAME, STATUS='OLD',
     *	           IOSTAT=ST, SHARED, READONLY)
	  ELSE
	    OPEN(UNIT=LUN, FILE=FILENAME, STATUS='OLD',
     *	           IOSTAT=ST, SHARED)
	  ENDIF
	ENDIF
C
	IF(ST)THEN
	  ST = 0
	ENDIF
C
C
9000	CONTINUE
	RETURN
	END

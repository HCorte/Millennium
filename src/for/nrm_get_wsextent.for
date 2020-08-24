C
C SUBROUTINE GET_WSEXTENT
C  
C V02 25-FEB-98 UXN 1000 substracted from WSEXTENT
C
C     Rev 1.0   17 Apr 1996 13:24:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:30:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_wrkset.for **
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C GET_WSEXTENT(MAXEXTENT) - RETURNS WORKING SET EXTENT
C
C-----------------------------------------------------------------------
C
C
C=======OPTIONS/CHECK=NOOVERFLOW
	SUBROUTINE GET_WSEXTENT(MAXEXTENT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($JPIDEF)'
C
	INTEGER*4   MAXEXTENT
C
	STRUCTURE   /JPISTRUC/
	    INTEGER*2	BUFLEN
	    INTEGER*2	ITMCOD
	    INTEGER*4	BUFADR
	    INTEGER*4	LENADR
	END STRUCTURE
C
	RECORD	    /JPISTRUC/ ITEMLIST(2)
C
	INTEGER*4   RETURN_LEN
C
	INTEGER*4   ST
C
C
C
C Get the PID of my MASTER
C
	ITEMLIST(1).BUFLEN = 4
	ITEMLIST(1).ITMCOD = JPI$_WSEXTENT
	ITEMLIST(1).BUFADR = %LOC(MAXEXTENT)
	ITEMLIST(1).LENADR = %LOC(RETURN_LEN)
C
	ITEMLIST(2).BUFLEN = 0			!TO TERMINATE LIST
	ITEMLIST(2).ITMCOD = 0			!TO TERMINATE LIST
C
	ST = SYS$GETJPIW( ,%VAL(0),,ITEMLIST(1),,,)
	IF(.NOT.ST)THEN
	  CALL LIB$SIGNAL(%VAL(ST))
	  GOTO 10000				!NORMAL PROMPT
	ENDIF
C
C Oct 7, 94 MP: There is a bug in VMS 6.*: MAXEXTENT in sub-process is
C		4 greater that it should be. Since there could be
C		multiple levels of sub-processes, subtract 1000
C
	MAXEXTENT = MAXEXTENT - 1000
C
D	TYPE *,IAM(),'MAXIMUM EXTENT WORKING SET= ',MAXEXTENT
10000	CONTINUE
C
	RETURN
	END

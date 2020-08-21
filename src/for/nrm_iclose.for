C
C SUBROUTINE ICLOSE
C $Log:   GXAFXT:[GOLS]ICLOSE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:34:58   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:37:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshfili4.for **
C
C
C
C
C *** ICLOSE
C
C This is called to close an indirect file.  If the file was
C opened for buffered writes (i.e., IINIB was called), use
C ICLOSB instead.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ICLOSE(LUN,IOBUF,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 LUN                    !  INPUT: LOGICAL UNIT #
	INTEGER*4 IOBUF(*)               !SCRATCH: I/O BUFFER
	INTEGER*4 STATUS                 ! OUTPUT: 0=OK, ELSE ERROR CODE
C
C
C
C
	IF(FCB(FCBLUN,LUN).NE.LUN)THEN
	  STATUS=ERRERR                          !NOT OPEN
	  GO TO 9000
	ENDIF
C
C If ISWRIT was ever called on this lun, write out current block.
C
	STATUS=0
	IF(FCB(FCBMOD,LUN).EQ.SEQWRI .AND.
     *	   FCB(FCBBLK,LUN).NE.0)THEN
	  CALL TUBWRITE(LUN,IOBUF,STATUS)
	ENDIF
C
C***      CALL CLOSE(LUN)
C***      CLOSE(UNIT=LUN)
	CALL CLOSEFIL(FCB(FCBFDB,LUN))
	FCB(FCBLUN,LUN)=0
C
9000	CONTINUE
	RETURN
	END

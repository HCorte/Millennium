C
C SUBROUTINE WRITHASH
C $Log:   GXAFXT:[GOLS]WRITHASH.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:05:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:09:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshsubi4.for **
C
C
C
C
C
C *** WRITHASH
C
C Call this routine to perform a tub write of the hash file.
C
C Note that this routine does not actually do a write.  It simply
C sets the tub changed flag so that the next read will force a
C write.
C
C To force a write, you must call TUBWRITE
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE WRITHASH(LUN,BKTNUM,IOBUF,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 LUN               !LOGICAL UNIT #
	INTEGER*4 BKTNUM            !BUCKET NUMBER TO WRITE
	INTEGER*4 IOBUF(*)          !I/O BUFFER (TUB)
	INTEGER*4 STATUS            !STATUS (AS RETURNED FROM READW)
	INTEGER*4 TUBOFF, TUBNUM
C
C
C
	TUBNUM=(BKTNUM-1)/FCB(FCBTUBSIZ,LUN) + 1
	TUBOFF=MOD(BKTNUM-1,FCB(FCBTUBSIZ,LUN))*I4BUCSIZ
	IF(TUBNUM.NE.FCB(FCBTUBBLK,LUN) .OR.
     *	   TUBOFF.NE.FCB(FCBTUBOFF,LUN) )THEN
	  TYPE *,IAM(),'WRITHASH- TRYING TO WRITE TUB NOT IN I/O BUFFER'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	FCB(FCBTUBCHG,LUN)=1
C
	STATUS=0
C
	RETURN
	END

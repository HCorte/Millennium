C
C SUBROUTINE FE_SPE_TIME
C $Log:   GXAFXT:[GOLS]FE_SPE_TIME.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:09:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:17:14   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gnfeavg2.for **
C
*
* ========================================================
* SUBROUTINE FE_SPE_TIME
*
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE FE_SPE_TIME(MESTYP,SUBTYP,MSGID,
     *	                       MSGSEQ,TYP,CNT,QUADTA,SAP)
	IMPLICIT NONE
*
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
*
	INCLUDE 'INCLIB:GNSMES.DEF'
*
	INTEGER*4   MESTYP              !Message type
	INTEGER*4   SUBTYP              !Message subtype
	INTEGER*4   MSGID               !PC message identifier
	INTEGER*4   MSGSEQ              !Message sequence number
	INTEGER*4   TYP(4)              !Type of qualification data
	INTEGER*4   CNT(4)              !Number of qualification parameters
	INTEGER*4   QUADTA(20,4)        !Qualification parameters
	INTEGER*4   SAP                 !SAP number
	INTEGER*4   TIME                !Time - secs from midnight
	INTEGER*4   MESLEN		!Length of message
	INTEGER*4   I
*
	DO 100 I=1,CNT(2)
	  TIME=QUADTA(I,2)
	  CALL BLDFEAVG(SAP,TIME,MESTYP,SUBTYP,MSGID,
     *	                MSGSEQ,MESLEN)
100	CONTINUE
	RETURN
	END

C
C SUBROUTINE NP_SPE_TIME
C $Log:   GXAFXT:[GOLS]NP_SPE_TIME.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:14:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:09:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gnnetpr3.for **
C
*
* ========================================================
* SUBROUTINE NP_SPE_TIME
*
	SUBROUTINE NP_SPE_TIME(MESTYP,SUBTYP,MSGID,
     *	                       MSGSEQ,TYP,CNT,QUADTA,SAP,PORT)
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
	INTEGER*4   MESLEN		!Message length
	INTEGER*4   TYP(4)              !Type of qualification data
	INTEGER*4   CNT(4)              !Number of qualification parameters
	INTEGER*4   QUADTA(20,4)        !Qualification parameters
	INTEGER*4   SAP                 !SAP number
	INTEGER*4   PORT                !Port number
	INTEGER*4   TIME                !Time - secs from midnight
	INTEGER*4   I
*
	DO 100 I=1,CNT(3)
	  TIME=QUADTA(I,3)
	  CALL BLDNETPRT(SAP,PORT,TIME,MESTYP,SUBTYP,MSGID,
     *	                 MSGSEQ,MESLEN)
100	CONTINUE
	RETURN
	END

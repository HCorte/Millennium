C
C SUBROUTINE FC_ALL_PORT
C $Log:   GXAFXT:[GOLS]FC_ALL_PORT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:08:40   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:15:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gnfecap2.for **
C
*
* ========================================================
* SUBROUTINE FC_ALL_PORT
*
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE FC_ALL_PORT(MESTYP,SUBTYP,MSGID,
     *	                       MSGSEQ,TYP,CNT,QUADTA,SAP)
	IMPLICIT NONE
*
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
*
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GNSMES.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
*
	INTEGER*4   MESTYP              !Message type
	INTEGER*4   SUBTYP              !Message subtype
	INTEGER*4   MSGID               !PC message identifier
	INTEGER*4   MSGSEQ              !Message sequence number
	INTEGER*4   TYP(4)              !Type of qualification data
	INTEGER*4   CNT(4)              !Number of qualification parameters
	INTEGER*4   QUADTA(20,4)        !Qualification parameters
	INTEGER*4   SAP                 !SAP number
	INTEGER*4   PORT                !Time - secs from midnight
	INTEGER*4   MESLEN		!Length of message
*
	DO 100 PORT=1,X2X_SAP_PORTS
	  CALL BLDFECAP(SAP,PORT,MESTYP,SUBTYP,MSGID,
     *	                MSGSEQ,MESLEN)
100	CONTINUE
	RETURN
	END

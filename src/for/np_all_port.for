C
C SUBROUTINE NP_ALL_PORT
C $Log:   GXAFXT:[GOLS]NP_ALL_PORT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:13:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:08:26   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gnnetpr2.for **
C
*
* ========================================================
* SUBROUTINE NP_ALL_PORT
*
	SUBROUTINE NP_ALL_PORT(MESTYP,SUBTYP,MSGID,
     *	                       MSGSEQ,TYP,CNT,QUADTA,SAP)
	IMPLICIT NONE
*
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
*
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
	INTEGER*4   PORT                !PORT number
*
	DO 100 PORT=1,X2X_SAP_PORTS
	  IF(TYP(3).EQ.GNHDRMES_TYP_RANGE) THEN
	    CALL NP_RNG_TIME(MESTYP,SUBTYP,MSGID,
     *	                     MSGSEQ,TYP,CNT,QUADTA,SAP,PORT)
	  ELSE IF(TYP(3).EQ.GNHDRMES_TYP_SPECIFIC) THEN
	    CALL NP_SPE_TIME(MESTYP,SUBTYP,MSGID,
     *	                     MSGSEQ,TYP,CNT,QUADTA,SAP,PORT)
	  ELSE
	    CALL NP_ALL_TIME(MESTYP,SUBTYP,MSGID,
     *	                     MSGSEQ,TYP,CNT,QUADTA,SAP,PORT)
	  ENDIF
100	CONTINUE
	RETURN
	END

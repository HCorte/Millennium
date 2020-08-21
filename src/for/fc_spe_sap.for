C
C SUBROUTINE FC_SPE_SAP
C $Log:   GXAFXT:[GOLS]FC_SPE_SAP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:08:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:16:24   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gnfecap1.for **
C
*
* ========================================================
* SUBROUTINE FC_SPECIFIC_SAP
*
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE FC_SPE_SAP(MESTYP,SUBTYP,MSGID,
     *	                       MSGSEQ,TYP,CNT,QUADTA)
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
	INTEGER*4   I
*
	DO 100 I=1,CNT(1)
	  SAP=QUADTA(I,1)
	  IF(TYP(2).EQ.GNHDRMES_TYP_RANGE) THEN
	    CALL FC_RNG_PORT(MESTYP,SUBTYP,MSGID,
     *	                     MSGSEQ,TYP,CNT,QUADTA,SAP)
	  ELSE IF(TYP(2).EQ.GNHDRMES_TYP_SPECIFIC) THEN
	    CALL FC_SPE_PORT(MESTYP,SUBTYP,MSGID,
     *	                     MSGSEQ,TYP,CNT,QUADTA,SAP)
	  ELSE
	    CALL FC_ALL_PORT(MESTYP,SUBTYP,MSGID,
     *	                     MSGSEQ,TYP,CNT,QUADTA,SAP)
	  ENDIF
100	CONTINUE
	RETURN
	END

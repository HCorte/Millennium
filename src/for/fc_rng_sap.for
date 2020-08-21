C
C SUBROUTINE FC_RNG_SAP
C $Log:   GXAFXT:[GOLS]FC_RNG_SAP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:08:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:16:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gnfecap1.for **
C
* GNFECAP1.FOR
*
* V01 01-AUG-90 XXX RELEASED FOR VAX
*
* V01 16-JUN-90 MRM INITIAL RELEASE.
*
* GNOS front end network port summary qualification 1 switch routines.
* Qualification 1 for Front end average messages refer
* to SAP values.
*
* The routines contained in this source file are:
*
*     FC_RNG_SAP
*     FC_SPE_SAP
*     FC_ALL_SAP
*
* Input parameters:
*
*     MESTYP      Int*4           GNOS message type
*     SUBTYP      Int*4           GNOS message subtype
*     MSGID       Int*4           PC assigned message identifier
*     MSGSEQ      Int*4           Message sequence number
*     TYP         Int*4(4)        Qualification parameter type
*     CNT         Int*4(4)        Number of qualification parameters
*     QUADTA      Int*4(20,4)     Qualification parameters
*
* ========================================================
* SUBROUTINE FC_RNG_RANGE
*
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* This item is the property of GTECH Corporation, Providence, Rhode
* Island, and contains confidential and trade secret information. It
* may not be transferred from the custody or control of GTECH except
* as authorized in writing by an officer of GTECH. Neither this item
* nor the information it contains may be used, transferred,
* reproduced, published, or disclosed, in whole or in part, and
* directly or indirectly, except as expressly authorized by an
* officer of GTECH, pursuant to written agreement.
*
* Copyright 1990 GTECH Corporation. All rights reserved.
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE FC_RNG_SAP(MESTYP,SUBTYP,MSGID,
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
*
	DO 100 SAP=QUADTA(1,1),QUADTA(2,1)
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

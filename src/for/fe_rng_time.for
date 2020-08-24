C
C SUBROUTINE FE_RNG_TIME
C $Log:   GXAFXT:[GOLS]FE_RNG_TIME.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:09:14   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:17:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gnfeavg2.for **
C
* GNFEAVG2.FOR
*
* V01 01-AUG-90 XXX RELEASED FOR VAX
*
* V01 16-JUN-90 MRM INITIAL RELEASE.
*
* GNOS front end average qualification 2 switch routines.
* Qualification 2 for front end average messages refer
* to time values.
*
* The routines contained in this source file are:
*
*     FE_RNG_TIME
*     FE_SPE_TIME
*     FE_ALL_TIME
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
*     SAP         Int*4           SAP number
*
* ========================================================
* SUBROUTINE FE_RNG_TIME
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
	SUBROUTINE FE_RNG_TIME(MESTYP,SUBTYP,MSGID,
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
*
	DO 100 TIME=QUADTA(1,2),QUADTA(2,2)
	  CALL BLDFEAVG(SAP,TIME,MESTYP,SUBTYP,MSGID,
     *	                MSGSEQ,MESLEN)
100	CONTINUE
	RETURN
	END

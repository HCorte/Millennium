C
C SUBROUTINE FC_RNG_PORT
C $Log:   GXAFXT:[GOLS]FC_RNG_PORT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:08:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:15:58   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gnfecap2.for **
C
* GNFECAP2.FOR
*
* V01 01-AUG-90 XXX RELEASED FOR VAX
*
* V01 16-JUN-90 MRM INITIAL RELEASE.
*
* GNOS front end port capacity qualification 2 switch routines.
* Qualification 2 for netport port messages refer
* to port values.
*
* The routines contained in this source file are:
*
*     FC_RNG_PORT
*     FC_SPE_PORT
*     FC_ALL_PORT
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
* SUBROUTINE FC_RNG_PORT
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
	SUBROUTINE FC_RNG_PORT(MESTYP,SUBTYP,MSGID,
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
	INTEGER*4   PORT                !Port number
	INTEGER*4   MESLEN		!Length of message
*
	DO 100 PORT=QUADTA(1,2),QUADTA(2,2)
	  CALL BLDFECAP(SAP,PORT,MESTYP,SUBTYP,MSGID,
     *	                MSGSEQ,MESLEN)
100	CONTINUE
	RETURN
	END

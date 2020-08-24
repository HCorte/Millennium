C
C SUBROUTINE NOTIFY1
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NOTIFY1.FOV                                  $
C  $Date::   17 Apr 1996 14:13:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - net_netsub2.for ***
C  
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	SUBROUTINE TO NOTYFY THE OPERATOR IN A CASE OF SOME
C       SPECIAL EVENT IN THE NETWORK.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NOTIFY1(ADR, TYPE, VALUE, WAY)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:TASKID.DEF'
C
C PARAMETER DECLARATIONS
C
	INTEGER*4	NETMBS
C
	PARAMETER	(NETMBS = 10)
C
C LOCAL DECLARATIONS
C
	REAL*8		NAME
C
	INTEGER*4	ADR,
     *			HNAME(2),
     *			HTSKNAM(2, NUMTSK),
     *			K,
     *			MESS(EDLEN),
     *			NUM,
     *			TYPE,
     *			VALUE,
     *			WAY
C
	EQUIVALENCE (HNAME,   NAME)
	EQUIVALENCE (HTSKNAM, TSKNAM)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	CALL GETNAM(NAME)
C
	NUM = 0
C
	DO 100 K = 1, NUMTSK
	  IF (HNAME(1) .EQ. HTSKNAM(1, K) .AND.
     *        HNAME(2) .EQ. HTSKNAM(2, K)) THEN
	    NUM = K
	    GOTO 200
	  ENDIF
100	CONTINUE
C
200	CONTINUE
 	MESS(1) = NUM
	MESS(2) = TENET
	MESS(3) = NETMBS + TYPE				! NETWORK MESSAGE BASE
	MESS(4) = ADR
	MESS(5) = VALUE
	MESS(6) = WAY
C
	CALL QUEMES(MESS)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END

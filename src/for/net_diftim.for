C
C SUBROUTINE NET_DIFTIM
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NET_DIFTIM.FOV                               $
C  $Date::   17 Apr 1996 14:11:18                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - net_diftim.for ***
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
C	CALCULATE DIFFERENCE IN TIME BETWEEN BUFFER PASSED IN DCN BUFFER 
C	AND CURRENT TIME.
C
C Calling Sequence:
C	CALL NET_DIFTIM(BUFFER, DIFF)
C
C Input:
C	BUFFER - DCN BUFFER STRUCTURE
C
C Output:
C	DIFF - DIFFERENCE OF TIME
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NET_DIFTIM(BUFFER, DIFF)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DN_LINK.DEF'
C
	INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
	DOUBLE PRECISION	VAX_REAL_TIME
C
	INTEGER*4		DAY	/86400/,	! SECONDS PER DAY
     *				DIFF,
     *				HOUR 	/ 3600/,	! SECONDS PER HOUR
     *				MINUTE	/   60/,	! SECONDS PER MINUTE
     *				STATUS
C
	INTEGER*2		CURRENT_TIME(7),
     *				INITIAL_TIME(7)
C
	RECORD /DN_BUFFER_STRUCT/ BUFFER		! BUFFER ARGUMENT
C
C COMMON DECLARATIONS
C
	COMMON /LOCAL_COMMON/ VAX_REAL_TIME
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C GET INITIAL & CURRENT TIMES.
C
	STATUS = SYS$NUMTIM(INITIAL_TIME, BUFFER.TIME)
C
	STATUS = SYS$GETTIM(VAX_REAL_TIME)
C
	STATUS = SYS$NUMTIM(CURRENT_TIME, VAX_REAL_TIME)
C
C CALCULATE DIFFERENCE BETWEEN TIME IN SECONDS
C
	DIFF = (HOUR   * CURRENT_TIME(4)
     *       +  MINUTE * CURRENT_TIME(5)
     *       +           CURRENT_TIME(6))
     *       - (HOUR   * INITIAL_TIME(4)
     *       +  MINUTE * INITIAL_TIME(5)
     *       +           INITIAL_TIME(6))
C
C IF DIFF < 0, THEN ADD 86400 (SECONDS IN A DAY).
C THIS WILL GIVE CORRECT DIFF TIMES WHEN CROSSING MIDNIGHT BOUNDARY.
C
	IF (DIFF .LT. 0) DIFF = DIFF + DAY
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END

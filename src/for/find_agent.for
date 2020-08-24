C  GXSRC:FIND_AGENT.FOR
C  
C  $Log:   GXAFXT:[GOLS]FIND_AGENT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:10:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   16 Jan 1995 12:34:02   DJO
C  Changed to check if agent commons have not been initialized yet.
C  
C     Rev 1.1   16 Dec 1994 11:37:32   DJO
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    16 Dec 1994 11:34:56   DJO
C  Initial revision.
C  
C SUBROUTINE TO FIND TERMINAL NUMBER GIVEN AGENT NUMBER
C USING SORTED AGENT LOOKUP TABLES IN AGTCOM.
C
C
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE FIND_AGENT(ANUM, TNUM, ST)
	IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
C
	INTEGER*4 ANUM, TNUM, ST, TOP, BOT, INDEX
C
C FIND TERMINAL NUMBER
C
	ST=-1
	TNUM=0
        IF(AGT_LOOKUP_CNT.LE.0) RETURN   !RETURN IF AGT_LOOKUP_CNT IS ZERO
        TOP = 1
        BOT = AGT_LOOKUP_CNT
C
10	CONTINUE
        INDEX = (TOP + BOT) / 2
C
        IF(AGT_LOOKUP_AGT(INDEX) .EQ. ANUM) THEN
          TNUM = AGT_LOOKUP_TER(INDEX)
	  ST = 0
	  RETURN
        ENDIF
C
        IF(AGT_LOOKUP_AGT(INDEX) .GT. ANUM) BOT = INDEX - 1
	IF(AGT_LOOKUP_AGT(INDEX) .LT. ANUM) TOP = INDEX + 1
        IF(TOP .LE. BOT) GOTO 10
C
C AGENT NOT FOUND
C
        RETURN
	END


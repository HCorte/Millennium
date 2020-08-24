C
C *** SUBROUTINE AGT_TO_TERM***
C *** Provides conversion of the agent number into terminal number ***
C
C $Log::   GXAFXT:[GOLS]AGT_TO_TERM.FOV                                   $
C  
C     Rev 1.0   17 Apr 1996 12:09:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   03 Jan 1994 17:02:28   JPJ
C  Initial revision.
C  
C     Rev 1.1   10 Sep 1993 18:46:54   MP
C  Report an error if number of agents less than 1
C  
C     Rev 1.0   08 Sep 1993  9:44:06   MP
C  Initial revision.
C  
C     Rev 1.0   08 Sep 1993 15:11:20   MP
C  Initial revision.
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
	SUBROUTINE AGT_TO_TERM (INIT_FLAG, AGENT_NR, TERM_NR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
C
	LOGICAL   INIT_FLAG		! input:
					! if TRUE, just sort agent nrs
					! if FALSE, do binary search
	INTEGER*4 AGENT_NR		! input: agent nr (INIT_FLAG=.FALSE.)
	INTEGER*4 TERM_NR		! output:
					! nr terms (INIT_FLAG=.TRUE.)
					! term nr (INIT_FLAG=.FALSE.)
					! 0 if not found
C
	INTEGER*4 
     *	          TERMS(NUMAGT),	! term numbers
     *	          AGENTS(NUMAGT),	! agents #s sorted
     *		  AGTCNT/0/		! agent count
C
	INTEGER*4 IDX, IDX_LOW, IDX_HIGH
C
        INTEGER*4       MES_BUF(33)
        CHARACTER*132   MES_CBUF
        EQUIVALENCE     (MES_BUF, MES_CBUF)
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
C
	IF(INIT_FLAG) THEN
C
C	  Build sorted agent tablse with corresponding terminal numbers
C
	  DO 1000 IDX=1,NUMAGT
	    IF(AGTTAB(AGTNUM,IDX) .EQ. 0) GOTO 1000
C
	    AGTCNT = AGTCNT + 1
	    AGENTS(AGTCNT) = AGTTAB(AGTNUM, IDX)
	    TERMS(AGTCNT)  = IDX
1000	  CONTINUE
C
C	  Do sort
C
	  CALL I4SHELL(TERMS,AGTCNT,AGENTS,1)
C
	  TERM_NR = AGTCNT
C
	  IF(AGTCNT.LE.0) THEN
	    CALL FASTSET(BLANK, MES_BUF,33)
	    WRITE(MES_CBUF,9010) IAM(), AGTCNT
9010	    FORMAT(A,'AGT_TO_TERM: Bad AGTCNT:',I)
	    CALL WRITEBRK(MES_CBUF)
	    GOTO 10000
	  ENDIF
C
	  GOTO 10000					! return
	ENDIF
C
	IF(AGTCNT.LE.0) THEN
	    CALL FASTSET(BLANK, MES_BUF,33)
	    WRITE(MES_CBUF,9010) IAM(), AGTCNT
	    CALL WRITEBRK(MES_CBUF)
	    GOTO 10000
	ENDIF
C
C Find a terminal nr
C
	TERM_NR = 0					! initialize
	IDX_LOW = 1
	IDX_HIGH = AGTCNT
C
2000	CONTINUE
C
	IDX = (IDX_LOW + IDX_HIGH) / 2
C
	IF(AGENTS(IDX) .EQ. AGENT_NR) THEN		! found
	  TERM_NR = TERMS(IDX)
	  GOTO 10000
	ENDIF
C
	IF(AGENTS(IDX) .GT. AGENT_NR) THEN		! check lower half
	  IDX_HIGH = IDX - 1
	ENDIF
C
	IF(AGENTS(IDX) .LT. AGENT_NR) THEN		! check upper half
	  IDX_LOW = IDX + 1
	ENDIF
C
	IF(IDX_LOW .GT. IDX_HIGH) GOTO 10000		! not found
C
	GOTO 2000					! continue
C
10000	CONTINUE
C
	RETURN
C
	END

C
C SUBROUTINE OPENQW
C $Log:   GXAFXT:[GOLS]OPENQW.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:18:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:12:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskqio.for **
C
C VAX_DISKQIO.FOR
C
C V03 01-AUG-91 MP  Fixed IO status returned from routines
C
C V02 24-APR-91 MP  CHANGED 'CLOSEFIL' TO 'CLOSEQFIL' IN COMMENTS
C		    AND OUTPUT STRINGS
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This contains subroutines analogous to diskio.  Note, however, that
C OPENQW only works for a byte array (not a character string).  Call OPENQX
C if you want to call with a character string.
C
C Also note that OPENQW on the VAX assumes that the file is contiguous.  To
C open any other type of file, use the Fortran open statement.
C
C
C       <<< open and wait for completion >>>
C
C In oreder to do QIO and FORTRAN CLOSE (port requirement), all CLOSE
C statements are replaced with USRCLOS1 or USRCLOS2. These routines
C check if file was opened with USEROPEN option. If so, SYS$DASSGN(CHAN)
C is done instead of CLOSE(LUN).
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE OPENQW(LUN, I4FILE, SHRIND, DUM1, DUM2, ST)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   LUN
	BYTE	    I4FILE(*)
	INTEGER*4   SHRIND
	INTEGER*4   DUM1
	INTEGER*4   DUM2
	INTEGER*4   ST
C
	INTEGER*4   K
	INTEGER*4   EXTCNT
	INTEGER*4   XLEN
C
	CHARACTER*40 CXNAME
C
C
	EXTCNT = -1
	DO 1100 K = 1, 40
	  IF(I4FILE(K).EQ.ICHAR(' '))GOTO 1200
	  IF(I4FILE(K).EQ.ICHAR('.'))THEN
	    EXTCNT = 0
	    CXNAME(K:K) = '.'
	    GOTO 1100
	  ENDIF
	  IF(.NOT. (
     *       (I4FILE(K).GE.ICHAR('A') .AND. I4FILE(K).LE.ICHAR('Z')).OR.
     *       (I4FILE(K).GE.ICHAR('a') .AND. I4FILE(K).LE.ICHAR('z')).OR.
     *       (I4FILE(K).GE.ICHAR('0') .AND. I4FILE(K).LE.ICHAR('9')).OR.
     *	     (I4FILE(K).EQ.ICHAR(':'))
     *	       ) ) GOTO 1200
C
	  CXNAME(K:K) = CHAR(I4FILE(K))
	  IF(EXTCNT.GE.0)THEN
	    EXTCNT = EXTCNT+1
	    IF(EXTCNT.GT.3)GOTO 1200
	  ENDIF
1100	CONTINUE
	K = 41
C
1200	CONTINUE
	XLEN = MAX(K-1,1)
	CALL OPENQX(LUN, CXNAME(1:XLEN), SHRIND, DUM1, DUM2, ST)
C
10000	CONTINUE
	RETURN
	END	

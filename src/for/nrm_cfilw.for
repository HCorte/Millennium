C
C SUBROUTINE CFILW
C $Log:   GXAFXT:[GOLS]CFILW.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:30:40   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:47:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_cfilw.for **
C
C CFILW.FOR
C
C V02 13-MAR-91 TKO  ADD 1 TO # SECTORS BECAUSE FIRST BLOCK IS NOT USED
C V01 09-JUL-90 TKO  RELEASED FOR VAX
C
C This will CREATE a file
C
C It replaces the Concurrent CFILW call
C
C If file name is an array:
C	CALL CFILW(I4NAME, FILTYP, RECLEN, SIZE, ISIZE, DUM1, DUM2, STATUS)
C
C If file name is a character string
C	CALL CFILX(CXNAME, ...same as above...)
C
C FILTYP:
C	0 = Contiguous
C	2 = Indexed
C
C RECLEN:
C	Maximum record size (ignored if contiguous)
C
C SIZE:
C	Size of contiguous file (or ignored if indexed)
C
C ISIZE:
C	Block size of indexed file (ignored by this program)
C
C STATUS:
C	0 = OK, else error (if file already exists, -1 is returned)
C
C *** Note that the file is created....not cleared
C
C **** V E R Y  I M P O R T A N T ****
C
C Because a sector on a VAX is twice as long as a sector on Concurrent, the
C SIZE variable that is passed is cut in half by these routines
C
C Also, because the first block cannot be used, this will always add 1
C sector when creating a contiguous file.
C
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
	SUBROUTINE CFILW(I4FILE, FILTYP, RECLEN, SIZE, ISIZE,
     *                   DUM1, DUM2, ST)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
C
	BYTE	    I4FILE(*)
	INTEGER*4   FILTYP
	INTEGER*4   RECLEN
	INTEGER*4   SIZE
	INTEGER*4   ISIZE
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
	XLEN = K-1
	CALL CFILX(CXNAME(1:XLEN), FILTYP, RECLEN, SIZE, ISIZE,
     *             DUM1, DUM2, ST)
C
	RETURN
	END	

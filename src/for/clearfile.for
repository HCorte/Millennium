C
C SUBROUTINE CLEARFILE
C $Log:   GXAFXT:[GOLS]CLEARFILE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:34:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:52:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - clearfile.for **
C
C CLEARFILE.FOR
C
C V01 26-MAY-92 WLM INITIAL RELEASE FOR NETHERLANDS
C
C This is a subroutine which may be called to open & clear a contiguous file
C as fast as possible.
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
	SUBROUTINE CLEARFILE(LUN, FILENAME, ST)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4	LUN
	CHARACTER	FILENAME*(*)
	INTEGER*4	ST
C
	INTEGER*4	SECTORS
	INTEGER*4	FULLBLOCKS
	INTEGER*4	PARTBLOCKS
	INTEGER*4	BLOCK
	INTEGER*4	FDB(7)
	INTEGER*4	NAMLEN
	INTEGER*4	K
C
	INTEGER*4	BUFSIZE
	PARAMETER	(BUFSIZE = 512*1000)	!1000 SECTORS AT A TIME
	BYTE		BIGBUF(BUFSIZE)/BUFSIZE*0/
C
C
C
C
C Get the length of the non-blank name
C
	DO 110 K = LEN(FILENAME), 2, -1
	  IF(FILENAME(K:K).NE.' ')GOTO 120
110	CONTINUE
	K = 1
120	CONTINUE
	NAMLEN = K
C
C Open the file and fill it with zeroes
C
	CALL OPENX(LUN, FILENAME(1:NAMLEN), 4, 0, 0, ST)
	IF(ST.NE.0)THEN
	  TYPE *,IAM(),'CANNOT OPEN '//FILENAME(1:NAMLEN)
	  GOTO 9000
	ENDIF
C
C Get size of file in sectors, then in bytes
C
	CALL VAXGETFSIZ(LUN, SECTORS)
	TYPE *,IAM(),'Clearing ',SECTORS,' VAX sectors in ',
     *                FILENAME(1:NAMLEN)
C
	CLOSE(LUN)
C
C
	FULLBLOCKS = SECTORS / (BUFSIZE/512)
	PARTBLOCKS = MOD (SECTORS, BUFSIZE/512)
C
	CALL OPENQX(LUN, FILENAME(1:NAMLEN), 4, 0, 0, ST)
	IF(ST.NE.0)THEN
	  TYPE *,IAM(),'OPENQX CANNOT OPEN '//FILENAME(1:NAMLEN)
	  GOTO 9000
	ENDIF
C
	CALL IOQINIT(FDB, LUN, 512)
C
C
	BLOCK = 1
	DO 1100 K = 1, FULLBLOCKS
	  CALL WRITEQIO( FDB, BLOCK, BIGBUF, BUFSIZE, ST)
	  IF(ST.NE.0)THEN
	    TYPE *,IAM(),'CANNOT CLEAR AT BLOCK ',BLOCK,ST
	  ENDIF
	  BLOCK = BLOCK + (BUFSIZE/512)
1100	CONTINUE
C
	IF(PARTBLOCKS.NE.0)THEN
	  PARTBLOCKS = PARTBLOCKS*512
	  CALL WRITEQIO( FDB, BLOCK, BIGBUF, PARTBLOCKS, ST)
	  IF(ST.NE.0)THEN
	    TYPE *,IAM(),'CANNOT CLEAR AT BLOCK ',BLOCK,ST
	  ENDIF
	ENDIF
C
	CALL USRCLOSQ1(LUN)
	TYPE *,IAM(),'INITIALIZATION COMPLETE FOR '//FILENAME(1:NAMLEN)
	ST = 0
C
9000	CONTINUE
	RETURN
C
	END

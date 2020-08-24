C
C SUBROUTINE CHRBIN
C $Log:   GXAFXT:[GOLS]CHRBIN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:34:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:52:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_chrbin.for **
C
C CHRBIN.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This is a set of routines that are very similar to those in
C ASCBIN.MAC except that these deal with character strings, not
C I*4 arrays.
C
C CHRBIN converts an ascii string into a numeric quantity.  It
C works like ASCBIN, except you pass it a character string.
C
C Calling sequence:
C
C     CALL CHRBIN(STRING,NUM,ST)
C
C INPUT:
C     STRING - CHARACTER STRING (***NOT AN ARRAY)
C
C OUTPUT:
C     NUM    - NUMERIC EQUIVALENT (if ST is not 0, NUM is set to 0)
C     ST     - Status 0=ok, -1= error
C
C
C COPYRITF.DEF+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C COPYRIGHT 1991 GTECH CORPORATION.  ALL RIGHTS RESERVED.
C
C CONFIDENTIAL PROPRIETARY INFORMATION
C This item is the property of GTECH Corporation, W. Greenwich, Rhode
C Island, and contains confidential and trade secret information.  It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH.  Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published or disclosed, in whold or in part, directly
C or indirectly, except as expressly authorized by an officer of
C GTECH pursuant to written agreement.
C COPYRITF.DEF-------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CHRBIN(STRING,NUM,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER STRING*(*)
	INTEGER*4 NUM
	INTEGER*4 ST
C
C
	INTEGER*4 ACCUM
	INTEGER*4 XLEN
	INTEGER*4 K
C
C
	NUM=0
	XLEN=LEN(STRING)
	ST=-1
	IF(XLEN.LT.1)GOTO 9000
C
	ACCUM=0
	DO 1200 K=1,XLEN
	  IF(ACCUM.GT.214748364)GOTO 9000
	  ACCUM=ACCUM*10
	  IF(STRING(K:K).NE.' ')THEN
	    IF(STRING(K:K).LT.'0' .OR. STRING(K:K).GT.'9')GOTO 9000
	    ACCUM=ACCUM + (ICHAR(STRING(K:K))-ICHAR('0'))
	    IF(ACCUM.LT.0)GOTO 9000
	  ENDIF
1200	CONTINUE
	NUM=ACCUM
	ST=0
C
9000	CONTINUE
	RETURN
	END

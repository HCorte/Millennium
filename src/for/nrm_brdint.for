C
C SUBROUTINE BRDINT
C $Log:   GXAFXT:[GOLS]BRDINT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:22:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:45:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_brdint.for **
C
C BRDINT.FOR
C
C V01 23-MAY-91 TKO  RELEASED FOR VAX
C
C
C
C ROUTINE TO CONVERT I*4 BOARD ARRAY INTO INTERVAL CODE
C
C
C CALLING SEQUENCE:
C
C        CALL BRDINT(NUMBRD,NUMMRK,MAXSEL,INTARY,BRDARY,OUTBYTES)
C
C INPUT:
C        NUMBRD: # OF BOARDS
C        NUMMRK: # OF MARKS PER BOARD
C        MAXSEL: MAXIMUM SELECTION VALUE (EG, 49 OR 56)
C        BRDARY: START OF I*4 BOARD ARRAY, 1 SELECTION/I*4 ENTRY
C
C OUTPUT:
C        INTARY: START OF INTERVAL CODED SELECTIONS
C      OUTBYTES: -1=ERROR, ELSE POSITIVE # OF BYTES IN INTERVAL
C                          CODE
C
C
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BRDINT(NUMBRD,NUMMRK,MAXSEL,INTARY,BRDARY,OUTBYTES)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   NUMBRD	    ! # OF BOARDS
	INTEGER*4   NUMMRK	    ! # OF MARKS PER BOARD
	INTEGER*4   MAXSEL	    ! MAXIMUM SELECTION VALUE
	BYTE	    INTARY(*)	    ! ARRAY OF INTERVAL CODE
	INTEGER*4   BRDARY(*)	    ! ARRAY OF BOARDS
	INTEGER*4   OUTBYTES	    ! RESULT
C
	INTEGER*4   BRDOFF, INTOFF, NIBOFF
	INTEGER*4   BRD, MRK, DIF, BASE, SEL
C
C
C
	BRDOFF = 1		    ! NEXT WORD TO GET FROM BRDARY
	INTOFF = 1		    ! NEXT BYTE OFFSET INTO INTARY
	NIBOFF = 0		    ! NEXT NIBBLE TO LOAD (0=HIGH, 1=LOW)
C
	DO 1900 BRD = 1, NUMBRD
	  BASE = 0
	  DO 1800 MRK = 1, NUMMRK
	    SEL = BRDARY(BRDOFF)
	    BRDOFF = BRDOFF + 1
	    IF(SEL.LE.BASE .OR. SEL.GT.MAXSEL)THEN
	      OUTBYTES = -1
	      GOTO 9000
	    ENDIF
C
1100	    CONTINUE
	    DIF = SEL - BASE
	    IF(DIF.GT.15)THEN
	      DIF = 0
	      BASE = BASE + 15
	    ENDIF
	    IF(NIBOFF.EQ.0)THEN
	      INTARY(INTOFF) = ISHFT(DIF,4)
	      NIBOFF = 1
	    ELSE
	      INTARY(INTOFF) = INTARY(INTOFF) + DIF
	      INTOFF = INTOFF + 1
	      NIBOFF = 0
	    ENDIF
	    IF(DIF.EQ.0)GOTO 1100
	    BASE = SEL
1800	  CONTINUE
1900	CONTINUE
C
C Everthing is ok, set OUTBYTES to # of bytes actually written
C
	IF(NIBOFF.EQ.0)THEN
	  OUTBYTES = INTOFF - 1
	ELSE
	  OUTBYTES = INTOFF
	ENDIF
C
9000	CONTINUE
	RETURN
	END

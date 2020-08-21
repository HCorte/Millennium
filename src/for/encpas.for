C
C SUBROUTINE ENCPAS
C $Log:   GXAFXT:[GOLS]ENCPAS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:04:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:13:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_encpas.for **
C
C ENCPAS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C              CALLING SEQUENCE
C                          INPUT - UID  - identification number
C                                - INPASS - password to be either
C                                           scrambled or unscrambled
C
C                          OUTPUT - OUTPASS- password out
C                                     STS   - scramble not successful
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
	 SUBROUTINE ENCPAS(UID,INPASS,OUTPASS)
	 IMPLICIT NONE
C
	 CHARACTER*(*) INPASS, OUTPASS
	 CHARACTER*8  CPASS1,CPASS3,CKEY
	 INTEGER*4 IPASS1(2),IPASS2(2),IPASS3(2),IKEY(2),UID,I
	 EQUIVALENCE (CPASS1,IPASS1)
	 EQUIVALENCE (CPASS3,IPASS3)
	 EQUIVALENCE (CKEY,IKEY)
C
C Key is equal to massachusetts
C
	 CKEY='MASSACHU'
C
C Set input password
C
	 CPASS1=INPASS
C
C Get the exclusive or in integer form of input password and ID
C
	 DO 5 I=1,2
	   IPASS2(I)=IEOR(UID,IPASS1(I))
 5	 CONTINUE
C
C Get the exclusive or in integer form of password and KEY
C
	 DO 6 I=1,2
	   IPASS3(I)=IEOR(IKEY(I),IPASS2(I))
 6	 CONTINUE
C
C Set ouput password
C
	   OUTPASS=CPASS3
	 RETURN
	 END

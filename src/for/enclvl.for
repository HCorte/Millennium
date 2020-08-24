C
C SUBROUTINE ENCLVL
C $Log:   GXAFXT:[GOLS]ENCLVL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:04:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:12:58   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_enclvl.for **
C
C ENCLVL.FOR
C
C V02 20-JAN-92 GCAN INCREASED # DIGITS TO HOLD VISION SNAPS TO 3.
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C
C              CALLING SEQUENCE
C                          INPUT - LEVEL - level number
C                                - BUF   - character line from
C                                          the LEVEL file.
C                                          Scrambled or unscrambled
C                                          opposite of output buffer.
C
C                         OUTPUT - BUF   - character line from
C                                          from the LEVEL file.
C                                          Scrambled or unscrambled
C                                          opposite of input buffer.
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
	 SUBROUTINE ENCLVL(LEVEL,BUF)
	 IMPLICIT NONE
C
	 CHARACTER*5  INBUF,OUTBUF,CZERO
	 INTEGER*4 SUBLEN(0:5),LEVEL,TOT,ITOT,I,J,K
	 CHARACTER*80 BUF
	 DATA SUBLEN/0,3,5,2,2,2/
	 DATA (CZERO(K:K),K=1,5)/5*Z00/
C
C Initialize variables
C
	 TOT=0
	 ITOT=0
	 I=0
	 J=0
C
C Send proper section of LEVEL file to be unencrypted or encrypted
C
	 DO 1000 I=1,5
	   ITOT=I+TOT
	   INBUF=CZERO
	   OUTBUF=CZERO
	   DO 3000 K=1,SUBLEN(I)
	     INBUF(K:K)=BUF(ITOT+K-1:ITOT+K-1)
3000	   CONTINUE
	   CALL ENCPAS(LEVEL,INBUF,OUTBUF)
	   DO 2000 J=1,SUBLEN(I)
	     BUF(ITOT+J-1:ITOT+J-1)=OUTBUF(J:J)
2000	   CONTINUE
	   TOT=TOT+SUBLEN(I)
1000	 CONTINUE
C
C Return
C
	 RETURN
	 END
